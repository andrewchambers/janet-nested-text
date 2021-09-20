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
  ~{
    # utf8 runes
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
    :indent
    (capture (any " "))
    :eol
    (choice "\n" (not 1))
    :key
    (sequence (capture (some (sequence (not ":") :rune))) ":")
    :not-eol
    (sequence (not "\n") :rune)
    :rest
    (sequence (capture (any :not-eol)) :eol)
    
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
    (cmt (sequence "[" :inline-array-values "]") ,array)
    :inline-key-value
    (sequence :inline-dict-string ":" :inline-dict-value)
    :inline-key-values
    (opt (sequence :inline-key-value (any (sequence "," :inline-key-value))))
    :inline-dict
    (cmt (sequence "{" :inline-key-values "}") ,table)

    # classify lines    
    :line
    (cmt
      (sequence
        (line)
        :indent
        (choice
                (cmt (sequence "#" :rest) ,(fn [value] @{:kind :comment :value value}))
                (cmt (sequence "-" :eol) ,(fn [&] @{:kind :list...}))
                (cmt (sequence "- " :rest) ,(fn [value] @{:kind :list-value :value value}))
                (cmt (sequence "> " :rest) ,(fn [value] @{:kind :multi-string :value value}))
                (cmt (sequence :inline-array :eol) ,(fn [value] @{:kind :inline-list :value value}))
                (cmt (sequence :inline-dict :eol) ,(fn [value] @{:kind :inline-dict :value value}))
                (cmt (sequence :key :eol) ,(fn [key] @{:kind :key... :key key}))
                (cmt (sequence :key " " :rest) ,(fn [key rest] @{:kind :key-value :key key :value rest}))
                (cmt :eol ,(fn [] @{:kind :blank}))
                (cmt :rest ,(fn [rest] @{:kind :bad-line :value rest}))))
      ,|(do (put $2 :line $0) [$1 $2]))
    :main
    (sequence (any :line) (not 1))
    })

(def- tokenizer
  (peg/compile tokenizer-grammar))

(defn- tokenize
  [text]
  (var lineno 0)
  (def tokens @[])
  (def indents @[""])
  (each line (peg/match tokenizer text)
    (def [indent token] line)
    (unless (or (= (token :kind) :blank)
                (= (token :kind) :comment))
      (case (cmp (length indent) (length (last indents)))
        -1
        (do
          (while (< (length indent) (length (last indents)))
            (array/push tokens {:kind :dedent})
            (array/pop indents))
          (array/push tokens token))
        1
        (do
          (array/push indents indent)
          (array/push tokens {:kind :indent})
          (array/push tokens token))
        0
        (array/push tokens token))))
  
  (for i 0 (dec (length indents))
    (array/push tokens {:kind :dedent}))
  
  tokens)

(def parser-grammar
  ~(yacc
     (%start document)
     (document () _
               (list) ,identity
               (dict) ,identity)
     
     (multi-string (:multi-string) ,|(buffer/push-string @"" ($0 :value))
                   (multi-string :multi-string) ,|(do (buffer/push-string $0 "\n")
                                                      (buffer/push-string $0 ($1 :value))))

     (value (list) ,identity
            (dict) ,identity
            (:inline-list) ,|($0 :value)
            (:inline-dict) ,|($0 :value)
            (multi-string) ,string)

     (list-item (:list-value) ,|($0 :value)
                (:list... :indent value :dedent) ,(fn [_ _ $3 _] $3))

     (list (list-item) ,array
           (list list-item) ,|(array/push $0 $1))

     (multi-key (:key...) ,|($0 :key)
                (multi-key :key...) ,|(string $0 "\n" ($1 :key)))

     (dict-item (:key-value) ,|[($0 :key) ($0 :value)]
                (multi-key :key-value) ,|[(string $0 "\n" ($1 :key)) ($1 :value)]
                (multi-key :indent value :dedent) ,(fn [$0 _ $2 _] [$0 $2]))

     (dict (dict-item) ,|@{($0 0) ($0 1)}
           (dict dict-item) ,|(put $0 ($1 0) ($1 1)))))

# (setdyn :yydebug stderr)
(def parser-tables (yacc/compile parser-grammar))

(defn parse
  [prog]
  (def tokens (tokenize prog))
  #(print "===")
  #(pp tokens)
  #(print "===")
  (yacc/parse parser-tables tokens))

# TODO test dir.
#(pp (parse ```
#-
#  [1,{a: 2, c}]
#```))