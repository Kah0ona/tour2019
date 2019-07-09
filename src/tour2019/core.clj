(ns tour2019.core
  (:require
   [clojure.data.csv :as csv]
   [clojure.math.combinatorics :as combo]
   [clojure.java.io :as io]))

(def teams
  (read-string (slurp "resources/teams.edn")))

(def riders
  (read-string
   (slurp "resources/riders.edn")))

(def categories
  (with-open [reader (io/reader "resources/types.csv")]
    (doall
     (csv/read-csv reader))))

(def rider-types
  {:p :puncher
   :a :aanvaller
   :s :sprinter
   :b :klimmer
   :t :tijdrijder
   :k :klassement
   :n :knecht
   :kb :kutbaguette})

(defn frequencies-by-type
  "Rider->count"
  [rider-type]
  (->> teams
       vals
       (apply concat)
       (filter (comp (partial = rider-type) :type))
       (map :name)
       frequencies
       (sort-by second)
       reverse
       (map (fn [[k v]]
              {:rider k :frequency v}))))

(defn team-normality
  "Alle frequenties van je teammembers optellen, geeft een score van hoe normaal je team is.
   Map van Player -> normality score"
  []
  (let [freqs (->> (frequencies-by-type :normal)
                   (map (fn [{r :rider f :frequency}]
                          [r f]))
                   (into {}))]
    (prn freqs)
    (->> teams
         (map
          (fn [[player riders]]
            (let [score (reduce +
                                0
                                (->>
                                 riders
                                 (filter (comp (partial = :normal) :type))
                                 (map (comp (partial get freqs) :name))))]
              [player score])))
         (sort-by second)
         reverse
         (map (fn [[k v]]
                {:player k :score v})))))

(defn parse-category
  [c]
  (let [c (->> (clojure.string/split c #",")
               (map clojure.string/trim)
               (map keyword)
               (map (partial rider-types))
               set)]
    c))

(def rider->categories
  (->> categories
       (into {} (map (fn [[r c]]
                       [r (parse-category c)])))))

(defn players-with-rider-as
  [n t]
  (->> teams
       (filter
        (fn [[pl rs]]
          (->> rs
               (filter (comp (partial = n) :name))
               (filter (comp (partial = t) :type))
               empty?
               not)))
       (map first)
       sort
       vec))

(def kluns-conflicten
  (->> riders
       (map (fn [{n :name :as r}]
              [n {:normal (players-with-rider-as n :normal)
                  :kluns  (players-with-rider-as n :kluns)}]))
       (remove (comp empty? :normal second))
       (remove (comp empty? :kluns second))
       (into {})))

(defn n-point-riders
  [n]
  (let [lookup (into {}
                     (map
                      (fn [r]
                        [(:name r) r]))
                     riders)]
    (->> teams
         (map (fn [[k rs]]
                {:name (name k)
                 :riders (->> rs
                        (filter
                         (fn [r]
                           (let [{s :score} (get lookup (:name r))]
                             (= s n))))
                        (map :name)
                        (map name)
                        sort
                        (clojure.string/join ", "))})))))

(defn csv
  [c]
  (->> c
       (map name)
       (clojure.string/join ", ")))

(defn num-of-type-in-team
  [rs k]
  (->> rs
       (filter (fn [r]
                 (let [ts (get rider->categories (:name r))]
                   (some #{k} ts))))
       count))

(def renner-verdeling
  (->> teams
       (map (fn [[pl rs]]
              (into
               {:__gasje (name pl)}
               (map
                (fn [k]
                  [k (num-of-type-in-team rs k)])
                (vals rider-types)))))))

(defn name-set-by-player
 [p]
 (->> (get teams p)
      (map :name)
      set))

(defn venn
  [p1 p2]
  (let [s1 (name-set-by-player p1)
        s2 (name-set-by-player p2)
        i (clojure.set/intersection s1 s2)
        u (clojure.set/union s1 s2)]
    {p1 (clojure.set/difference u s2)
     :beide i
     p2 (clojure.set/difference u s2)}))

(defn venn-num
  [p1 p2]
  (let [r (venn p1 p2)]
    (into {}
          (map (fn [[k v]]
                 [k (count v)]))
          r)))

(def pairs
  (->>
   (combo/combinations (keys teams) 2)
   (map set)
   set
   (map vec)))

(def differences
  (->>
   pairs
   (map
    (fn [[a b]]
      (let [v (venn-num a b)]
        {:p1 a
         :p2 b
         :overeenkomend (:beide v)})))
   (sort-by :overeenkomend)
   reverse))

(comment
  (clojure.pprint/print-table
   (frequencies-by-type :normal))

  (clojure.pprint/print-table
   (frequencies-by-type :kluns))

  (clojure.pprint/print-table
   (team-normality))

  (clojure.pprint/print-table
   (->> kluns-conflicten
        (map
         (fn [[rider {n :normal k :kluns}]]
           {:name rider
            :normal (csv n)
            :kluns  (csv k)}))
        (sort-by :name)))

  (clojure.pprint/print-table
   (n-point-riders 0))

  (clojure.pprint/print-table renner-verdeling)

  (parse-category "a, kb, b, s, p, n, k, t")

  (venn-num :joris :simon)
  (venn-num :kasper :jacob)

  (venn-num :joris :simon)
  (venn-num :joris :simon)
  (venn-num :joris :simon)


  (clojure.pprint/print-table differences)

  )
