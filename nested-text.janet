# Implementation of the NestedText[1] spec.
#
# The parser works as follows:
#
# - Use janet PEG to split and classify lines,
#   while also tracking the indent level.
# - Convert the indent levels to :indent and 
#   :dedent tokens using a stack based algorithm.
# - Finally, use a yacc grammar to parse the line and indent grammar.
#
# [1] https://nestedtext.org/en/stable/file_format.html

(import yacc)

(def- tokenizer-grammar
  ~{# utf8 runes
    :rune-extra
    (range "\x80\xBF")
    :1byte-rune
    (range "\x00\x7F")
    :2byte-rune
    (sequence (range "\xC0\xDF") :rune-extra)
    :3byte-rune
    (sequence (range "\xE0\xEF") :rune-extra :rune-extra)
    :4byte-rune
    (sequence (range "\xF0\xF7") :rune-extra :rune-extra :rune-extra)
    :rune
    (choice :1byte-rune :2byte-rune :3byte-rune :4byte-rune)

    # helpers
    :inline-ws
    (any (set " \t\f\v"))
    :indent
    (cmt (capture (any " ")) ,length)
    :nl
    (choice "\r\n" "\n" "\r")
    :eol
    (choice :nl (not 1))
    :not-eol
    (if-not :nl :rune)
    :rest
    (sequence (capture (any :not-eol)) :eol)
    :post-tag (choice (sequence " " :rest)
                      (sequence :eol (constant "")))
    :kv-key
    (cmt (capture (any (if-not (choice :kv-value :nl) :rune))) ,string/trimr)
    :kv-value
    (sequence ":" :post-tag)

    # inline types
    :inline-dict-string
    (cmt (capture (any (if-not (set "[]{}:,\n") 1))) ,string/trim)
    :inline-array-string
    (cmt (capture (any (if-not (set "[]{},\n") 1))) ,string/trim)
    :inline-dict-value
    (choice :inline-array :inline-dict :inline-dict-string)
    :inline-array-value
    (choice :inline-array :inline-dict :inline-array-string)
    :inline-array-values
    (opt (sequence :inline-array-value (any (sequence "," :inline-array-value))))
    :inline-array
    (cmt (sequence :inline-ws "[" :inline-array-values "]" :inline-ws) ,array)
    :inline-key-value
    (sequence :inline-dict-string ":" :inline-dict-value)
    :inline-key-values
    (opt (sequence :inline-key-value (any (sequence "," :inline-key-value))))
    :inline-dict
    (cmt (sequence :inline-ws "{" :inline-key-values "}" :inline-ws) ,table)

    # classify lines    
    :line
    (cmt
      (sequence
        (line)
        :indent
        (choice
          (cmt (sequence ">" :post-tag)
               ,(fn [v] @{:kind :multi-string :value v}))
          (cmt (sequence ":" :post-tag)
               ,(fn [v] @{:kind :multi-key :key v}))
          (cmt (sequence "-" :post-tag)
               ,(fn [v]
                  @{:kind (if (empty? v) :list... :list-value)
                    :value v}))
          (cmt (sequence "#" :post-tag) ,(fn [v] @{:kind :comment :value v}))
          (if (set "[{")
            (choice
              (cmt (sequence :inline-array :eol) ,(fn [v] @{:kind :inline-list :value v}))
              (cmt (sequence :inline-dict :eol) ,(fn [v] @{:kind :inline-dict :value v}))
              (cmt :rest ,(fn [v] @{:kind :malformed-inline-value :value v}))))

          (cmt (sequence :kv-key :kv-value) ,(fn [k v]
                                               @{:kind (if (empty? v) :key... :key-value)
                                                 :key k
                                                 :value v}))

          (cmt :eol ,(fn [] @{:kind :blank}))
          (cmt :rest ,(fn [value] @{:kind :malformed-line :value value}))))
      ,|(do
          (put $2 :line $0)
          (put $2 :indent $1)
          $2))
    :main
    (sequence (any :line) (not 1))})

(def- tokenizer
  (peg/compile tokenizer-grammar))

(defn- tokenize
  [text]
  (def tokens @[])
  (def indents @[0])
  (each token (peg/match tokenizer text)
    (def indent (token :indent))
    (unless (or (= (token :kind) :blank)
                (= (token :kind) :comment))
      (case (cmp indent (last indents))
        -1
        (do
          (while (< indent (last indents))
            (array/push tokens @{:kind :dedent :line (token :line)})
            (array/pop indents))
          (array/push tokens token))
        1
        (do
          (array/push indents indent)
          (array/push tokens @{:kind :indent :line (token :line)})
          (array/push tokens token))
        0
        (array/push tokens token))))

  (for i 0 (dec (length indents))
    (array/push tokens @{:kind :dedent :line ((last tokens) :line)}))

  tokens)

(def parser-grammar
  ~(yacc
     (%start document)
     (document () _
               (list) ,identity
               (dict) ,identity
               (inline-value) ,identity
               (multi-string) ,identity)

     (multi-string (:multi-string) ,|(buffer/push-string @"" ($0 :value))
                   (multi-string :multi-string) ,|(do
                                                    (buffer/push-string $0 "\n")
                                                    (buffer/push-string $0 ($1 :value))))

     (multi-key (:multi-key) ,|(buffer/push-string @"" ($0 :key))
                (multi-key :multi-key) ,|(do
                                           (buffer/push-string $0 "\n")
                                           (buffer/push-string $0 ($1 :key))))

     (inline-value (:inline-list) ,|($0 :value)
                   (:inline-dict) ,|($0 :value))

     (value (list) ,identity
            (dict) ,identity
            (inline-value) ,identity
            (multi-string) ,string)

     (list-item (:list-value) ,|($0 :value)
                (:list...) ,|($0 :value)
                (:list... :indent value :dedent) ,(fn [_ _ $2 _] $2))

     (list (list-item) ,array
           (list list-item) ,|(array/push $0 $1))

     (dict-item (:key-value) ,|[($0 :key) ($0 :value)]
                (:key...) ,|[($0 :key) ""]
                (:key... :indent value :dedent) ,(fn [$0 _ $2 _] [($0 :key) $2])
                (multi-key) ,|[$0 ""]
                (multi-key :indent value :dedent) ,(fn [$0 _ $2 _] [$0 $2]))

     (dict (dict-item) ,|@{($0 0) ($0 1)}
           (dict dict-item) ,|(put $0 ($1 0) ($1 1)))))

#(setdyn :yydebug stderr)
(def parser-tables (yacc/compile parser-grammar))

(defn decode
  "Decode a NestedText document returning one of
  [:syntax-error nil] on unexpected EOF.
  [:syntax-error {:kind token-kind :line line}] on unexpected token.
  [:ok value] on success.
  "
  [text]
  (def tokens (tokenize text))
  (yacc/parse parser-tables tokens))

(defn- prin-indent
  [depth]
  (def buf @"")
  (repeat depth
    (buffer/push-string buf " "))
  (prin buf))

(var- print-linev nil)

(defn- should-use-multiline
  [s]
  # This is inefficient and probably over conservative.
  (or (empty? s)
      (string/has-prefix? " " s)
      (string/find "\n" s)
      (string/find "\r" s)
      (string/find ":" s)
      (string/find "#" s)
      (string/find "-" s)
      (string/find ">" s)
      (string/find "{" s)
      (string/find "[" s)))

(defn- print-dict
  [d depth]
  (if (empty? d)
    (do
      (prin-indent depth)
      (print "{}"))
    (each k (sorted (keys d))
      (def v (d k))
      (def sk (if (bytes? k) k (string k)))
      (if (should-use-multiline sk)
        (do
          (var first true)
          (each line (string/split "\n" sk)
            (unless first
              (print))
            (set first false)
            (prin-indent depth)
            (prin ": " line))
          (print-linev v depth true))
        (do
          (prin-indent depth)
          (prin sk ":")
          (print-linev v depth false))))))

(defn- print-indexed
  [ind depth]
  (if (empty? ind)
    (do
      (prin-indent depth)
      (print "[]"))
    (each v ind
      (prin-indent depth)
      (prin "-")
      (print-linev v depth false))))

(defn- print-multiline-string
  [s depth]
  (each line (string/split "\n" s)
    (prin-indent depth)
    (print "> " line)))

(set
  print-linev
  (fn print-linev
    [v depth force-multiline]
    (cond
      (indexed? v)
      (do
        (print)
        (print-indexed v (+ depth 2)))
      (dictionary? v)
      (do
        (print)
        (print-dict v (+ depth 2)))
      (and (bytes? v)
           (not force-multiline)
           (not (should-use-multiline v)))
      (print " " v)
      (bytes? v)
      (do
        (print)
        (print-multiline-string v (+ depth 2)))
      (print-linev (string v) depth force-multiline))))

(defn print
  "Print a value as a NestedText document."
  [v &opt depth]
  (default depth 0)
  (cond
    (dictionary? v)
    (print-dict v depth)
    (indexed? v)
    (print-indexed v depth)
    (bytes? v)
    (print-multiline-string v depth)))

(defn encode
  "Encode a value as a NestedText document buffer."
  [v &opt depth]
  (def buf @"")
  (with-dyns [:out buf]
    (print v depth))
  buf)
