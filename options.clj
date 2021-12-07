(use 'clojure.java.io)
(require '[clojure.string :as str])

(defn calc-profit-percentages [[strike last bid ask]]
  (let [strike (Float/parseFloat strike)
        last (Float/parseFloat last)
        bid (Float/parseFloat bid)
        ask (Float/parseFloat ask)
        expires (/ last 4.91)
        execute (/ (- (+ strike last) 4.91) 4.91)]
    (println (str/join ", " [strike last bid ask expires execute]))))

(with-open [file (reader "swn")]
  (println "Strike, Last, Bid, Ask, Expire, Execute")
  (doseq [line (line-seq file)]
    (calc-profit-percentages (subvec (str/split line #"\t") 2 6))))
