(ns aloop.test)

(defn singular?
  [el]
  (and (coll? el)
       (= 1 (count el))))

(defn multiple?
  [el]
  (and (coll? el)
       (< 1 (count el))))

(defn none
  [el]
  (and (coll? el)
       (= 0 (count el))))