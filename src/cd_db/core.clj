(ns cd-db.core
  (:refer-clojure))

(defonce db (atom []))

(defn make-cd
  [title artist rating ripped]
  {:title title :artist artist :rating rating :ripped ripped})

(defn add-record
  [cd]
  (swap! db #(conj % cd)))

(defn- cd-to-str
  [cd]
  (let [display-field #(format "%-10s%s%n" % %2)
        kwrd-to-lbl #(format "%s:" (.toUpperCase (name %)))]
    (apply str
           (map #(display-field (kwrd-to-lbl %) (% cd))
                '(:title :artist :rating :ripped)))))

(defn dump-db
  []
  (comment (dorun (map #(println (cd-to-str %)) @db)))
  (doseq [cd @db]
    (println (cd-to-str cd))))

(defn- prompt-read
  [prompt]
  (print (format "%s:" prompt))
  (flush)
  (read-line))

(defn- parse-int
  [s]
  (try
    (Integer/parseInt s)
    (catch NumberFormatException e nil)))

(defn- y-or-n-p
  [prompt]
  (let [answer (prompt-read (format "%s (y/n) " prompt))]
    (case answer
      "y" true
      "n" false
      (recur "Please answer with y or n:"))))
  
(defn prompt-for-cd
  []
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-int (prompt-read "Rating")) 0)
   (y-or-n-p "Ripped?")))

(defn add-cds
  []
  (add-record (prompt-for-cd))
  (if (y-or-n-p "Add another?") (recur)))

(defn save-db
  [filename]
  (spit filename @db))

(defn load-db
  [filename]
  (let [contents (slurp filename)
        cds (read-string contents)]
    (reset! db cds)))

; This is made more general below. See: select and where
(comment 
  (defn select-by-artist
    [artist]
    (filter #(= artist (:artist %)) @db)))

(defn select
  [selector-fn]
  (filter selector-fn @db))

(defn where
  [& clauses]
  (fn
    [row]
    (let [pairs (partition 2 clauses)
          matches? (fn [[f v]] (= (f row) v))]
      (every? matches? pairs))))

(defn update
  [selector-fn & updates]
  (let [updates-map (apply hash-map updates)]
    (reset! db (map #(if (selector-fn %) (merge % updates-map) %) @db))))
