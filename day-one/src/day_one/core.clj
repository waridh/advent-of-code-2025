(ns day-one.core
  (:gen-class)
  (:require clojure.string))

(defprotocol Command
  (magnitude [_]))

(defrecord Left [mag]
  Command
  (magnitude [_] mag))

(defrecord Right [mag]
  Command
  (magnitude [_] mag))

(defn Left?
  [v]
  (instance? Left v))

(defn Right?
  [v]
  (instance? Right v))

(defn string->command
  "converts a single string representation of a command to a command"
  [s]
  (assert (string? s))
  (let
   [str->mag->cons (fn
                     [cons]
                     (-> (.substring s 1)
                         (clojure.string/trim)
                         (Integer/parseInt)
                         (cons)))]
    (cond
      (= (first s) \L) (str->mag->cons ->Left)
      (= (first s) \R) (str->mag->cons ->Right)
      :else (assert false "did not expect to get to this branch"))))

(defn strings->commands
  "converts a sequence of strings to a sequence of commands"
  [s]
  (map string->command s))

(defn reversed-sub [x y] (- y x))

(defn comp-pipeline [op mag curr-state acc]
  (let [next-state (-> mag
                       (op curr-state)
                       (mod 100))]
    (if (= next-state 0)
      [(inc acc) next-state]
      [acc next-state])))

(defn count-zeros-aux [[acc curr-state] command]
  (cond
    (Left? command)
    (comp-pipeline reversed-sub
                   (magnitude command)
                   curr-state
                   acc)
    (Right? command)
    (comp-pipeline +
                   (magnitude command)
                   curr-state
                   acc)
    :else (assert false "should not be possible to enter this branch")))

(defn make-counter [init-state r-f]
  (fn [commands] (-> (reduce r-f init-state commands)
                     (get 0))))

(def count-zeros (make-counter [0 50] count-zeros-aux))

(defn count-left-clicks
  [acc shifted-state]
  (cond (< shifted-state 0)
        (recur (inc acc) (+ shifted-state 100))
        (= shifted-state 0)
        [(inc acc) shifted-state]
        :else
        [acc shifted-state]))

(defn count-right-clicks
  [acc shifted-state]
  (if (< shifted-state 100)
    [acc shifted-state]
    (recur (inc acc) (- shifted-state 100))))

(defn count-clicks-aux
  [[acc curr-state] command]
  (cond (Left? command) (let [next-state (- curr-state
                                            (magnitude command))]
                          (cond (and (= next-state 0)
                                     (= curr-state 0))
                                [acc next-state]
                                (= next-state 0)
                                [(inc acc) next-state]
                                (= curr-state 0)
                                (count-left-clicks (- acc 1)
                                                   (- curr-state (magnitude command)))
                                :else
                                (count-left-clicks acc
                                                   (- curr-state (magnitude command)))))
        (Right? command) (count-right-clicks acc
                                             (+ curr-state (magnitude command)))))

(def count-clicks (make-counter [0 50] count-clicks-aux))

(defn make-strings->commands->reduction [reduction]
  (fn [s] (-> s
              (strings->commands)
              (reduction))))
(def strings->count-zeros (make-strings->commands->reduction count-zeros))
(def strings->count-clicks (make-strings->commands->reduction count-clicks))

(defn -main
  "We are going to take a page in as a parameter, and then run our program
   on it"
  [& args]
  (let [mode (first args)
        file-name (second args)]
    (if (not mode)
      (println "did not get a mode")
      (let [process-fn (cond
                         (= mode "part-1") strings->count-zeros
                         (= mode "part-2") strings->count-clicks
                         :else nil)]
        (if (not process-fn)
          (println "invalid mode input. Use part-1 or part-2")
          (-> file-name
              (slurp)
              (clojure.string/split-lines)
              (process-fn)
              (str)
              (println)))))))
