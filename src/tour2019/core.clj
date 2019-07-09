(ns tour2019.core)

(def teams
  (read-string (slurp "resources/teams.edn")))

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

(defn sagan?
  [{n :name}]
  (= n "Peter Sagan"))

(defn no-sagan?
  [[k v]]
  (->> v (filter sagan?) empty?))



(defn ala?
  [{n :name}]
  (= n "Julian Alaphilippe"))

(defn no-ala?
  [[k v]]
  (->> v (filter ala?) empty?))

(def players-without-sagan
  (->>
   (filter no-sagan? teams)
   (map first)))


(def players-without-ala
  (->>
   (filter no-ala? teams)
   (map first)))

(->> teams
     (map second)
     (apply concat)
     (map :name)
     set
     (map (fn [r]
            {:name r
             :score 0}
            ))
     (spit "resources/riders.edn")
     )


(comment
  (clojure.pprint/print-table
   (frequencies-by-type :normal))


  (clojure.pprint/print-table
   (frequencies-by-type :kluns))

  (clojure.pprint/print-table
   (team-normality))


  (players-without-sagan teams)

  )
