(ns vintage.core
  (:refer-clojure :exclude [get char])
  (:use [zetta.core
         :only (parse-once always do-parser <$> <* *> <|>)]
        [zetta.parser.seq
         :only (satisfy? char not-char string number any-token
                whitespace space spaces eol digit skip)]
        [zetta.combinators
         :only (many-till option choice around many sep-by skip-many)]))

(defrecord Ref [number generation])
(defrecord Indirect [number generation object])
(defrecord Stream [dictionary data])
(defrecord XrefTable [start count entries])
(defrecord XrefOffset [offset])
(defrecord Trailer [dictionary])
(defrecord Document [major minor components])
(defrecord DocumentEnd [])

;; Strings are represented as Java Strings, numbers as Java longs/doubles,
;; dictionaries as Clojure hash maps, arrays as Clojure vectors and
;; names as Clojure keywords.

(declare object)

(def whitespaces
  (many whitespace))

(def numeric-digit
  (<$> #(Character/digit % 10) digit))

(def hex-digit
  (satisfy? #(not= (Character/digit #^java.lang.Character % 16) -1)))

(def padded-number
  "Number with leading padding of 0s"
  (do-parser
   [zeros (many (char \0))
    :if (zero? (count zeros))
    :then [result number]
    :else [result (option 0 number)]]
   result))
;; todo does not parse rationals

(def number-obj
  (<$>
   *
   (option 1 (*> (char \+) (always 1)))
   (option 1 (*> (char \-) (always -1)))
   padded-number))

(def array-obj
  (*> (char \[) (<* (<$> vec (many object)) (char \]))))

(def stream
  (*>
   (string "stream")
   eol
   (<$> vec (many-till any-token (*> (option "" eol) (string "endstream"))))))
                                        ; todo byte array

(def dictionary-obj
  (*> (string "<<") (<* (<$> #(apply hash-map %) (many object)) (string ">>"))))

(def dictionary-or-stream
  (do-parser
   [stream-object (<$>
                   #(Stream. %1 %2)
                   dictionary-obj
                   (option :no-stream (*> whitespaces stream)))
    :if (= (:data stream-object) :no-stream)
    :then [result (always (:dictionary stream-object))]
    :else [result (always stream-object)]]
   result))

(def string-obj
  (*>
   (char \()
   (<*
    (<$>
     #(apply str %)
     (many
      (choice
       [(*> (string "\\n") (always \newline))
        (*> (string "\\r") (always \return))
        (*> (string "\\t") (always \tab))
        (*> (string "\\f") (always \formfeed))
        (*> (string "\\(") (always \())
        (*> (string "\\)") (always \)))
        (*> (string "\\\\") (always \\))
        (*> (char \\) (<$> str digit digit (option "" digit)))
        (not-char \))])))
    (char \)))))
;; todo handle balanced parens
;; todo stringify octal escapes

(defn hex-seq->str [s]
  (let [chars (if (odd? (count s)) (concat s '(\0)) s)
        code-points (map #(apply str %) (partition 2 chars))]
    (apply
     str
     (reduce
      concat
      []
      (map
       #(-> % (Integer/parseInt 16) (Character/toChars))
       code-points)))))

(def hex-string-obj
  (*>
   (char \<)
   (<$> hex-seq->str (<* (many hex-digit) (char \>)))))

(def name-obj
  (*>
   (char \/)
   (<$>
    keyword
    (<$>
     #(apply str %)
     (many (not-char #{\/ \] \> \[ \< \( \space \newline \return}))))))
;; todo handle escapes
;; todo any better way of handling name collision with
;; array and object end?

(def indirect-obj
  (<$>
   #(Indirect. %1 %2 %3)
   (<* number spaces)
   (<* number spaces)
   (*>
    whitespaces
    (string "obj")
    whitespaces
    (<* object whitespaces (string "endobj")))))

(def xref-table
  (<$>
   #(XrefTable. %1 %2 %3)
   (*> (string "xref") whitespaces (<* number spaces))
   (<* number whitespaces)
   (many
    (sep-by
     (<$> vector (<* padded-number space) (<* padded-number space) any-token)
     (*> spaces eol)))))
                                        ; todo multiple subsections

(def trailer
  (<$>
   #(Trailer. %1)
   (*> (string "trailer") whitespaces object)))

(def xref-offset
  (<$>
   #(XrefOffset. %1)
   (*> (string "startxref") whitespaces number)))

(def comment
  (*> (char \%) (many-till any-token eol)))

(def reference
  (<$>
   #(Ref. %1 %2)
   (<* number spaces)
   (<* number spaces (char \R))))

(def number-or-reference
  (do-parser
   [n number-obj
    generation (option :no-gen (*> spaces (<* number spaces (char \R))))
    :if (= generation :no-gen)
    :then [result (always n)]
    :else [result (always (Ref. n generation))]]
   result))

(def object
  (around
   whitespaces
   (*>
    (sep-by comment whitespaces)
    (choice
     [(*> (string "true") (always true))
      (*> (string "false") (always false))
      (*> (string "null") (always nil))
      array-obj
      name-obj
      string-obj
      number-or-reference
      dictionary-or-stream
      hex-string-obj]))))

(def document-end
  (*> (string "%%EOF") (always (DocumentEnd.))))

(def document
  "Reads a PDF to its %%EOF marker. Subsequent indirect objects are left unread."
  (<$>
   #(Document. %1 %2 %3)
   (*> (string "%PDF-") (<* numeric-digit (char \.)))
   (<* numeric-digit whitespaces)
   (*>
    (sep-by comment whitespaces)
    (many
     (around
      whitespaces
      (choice
       [document-end indirect-obj xref-table xref-offset trailer]))))))

;; (parse-once document (str "%PDF-1.5" \newline "15 0 obj" \newline "<< /Name [/First << /A /B /C [ 0 0 0 ] >> 15 -65.3] /NameOther <</Some /Thing % a comment" \newline ">> stream" \newline "blah endstream >> endobj xref 30 3" \newline "34 44 n" \newline "trailer <</Root null>> startxref 0" \newline "%%EOF"))

;; (parse-once (sep-by indirect-obj whitespaces) (slurp "/Users/karl/Projects/vintage/stream_object.txt"))