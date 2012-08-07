(ns logic-demo.core
  (:refer-clojure :exclude [==])
  (:require [clojure.tools.macro :as macro])
  (:use clojure.core.logic
        [clojure.pprint :only [pprint]]))

(defn everyo
  "A pseudo-relation that takes a coll and ensures that the goal g
   succeeds on every element of the collection."
  [g coll]
  (if (seq coll)
    (all
     (g (first coll))
     (everyo g (next coll)))
        s#))

;; Zebra

(defne righto [x y l]
  ([_ _ [x y . ?r]])
  ([_ _ [_ . ?r]] (righto x y ?r)))

(defn nexto [x y l]
  (conde
    ((righto x y l))
    ((righto y x l))))

(defn zebra
  []
  (macro/symbol-macrolet [_ (lvar)]
                         (run 1 [houses]
                              (== houses [_ _ _ _ _])
                              (membero [:englishman _ _ _ :red] houses)
                              (membero [:spaniard _ _ :dog _] houses)
                              (membero [_ _ :coffee _ :green] houses)
                              (membero [:ukrainian _ :tea _ _] houses)
                              (righto [_ _ _ _ :ivory] [_ _ _ _ :green] houses)
                              (membero [_ :oldgolds _ :snails _] houses)
                              (membero [_ :kools _ _ :yellow] houses)
                              (== [_ _ [_ _ :milk _ _] _ _] houses)
                              (firsto houses [:norwegian _ _ _ _])
                              (nexto [_ _ _ :fox _] [_ :chesterfields _ _ _] houses)
                              (nexto [_ _ _ :horse _] [_ :kools _ _ _] houses)
                              (membero [_ :lucky-strikes :oj _ _] houses)
                              (membero [:japanese :parliaments _ _ _] houses)
                              (nexto [:norwegian _ _ _ _] [_ _ _ _ :blue] houses))))


;; Polynomials

(defn poly-mult
  "p1 * p2 == p3. p1 and p2 are binomials, p3 trinomial."
  [p1 p2 p3]
  (fresh [a b c d e f g tmp1 tmp2]
         (infd a b c d e f g tmp1 tmp2 (interval 1 100))
         (== p1 [a b])   ; a + bx
         (== p2 [c d])   ; c + dx
         (== p3 [e f g]) ; e + fx + gx^2
         (*fd a c e)
         (*fd b d g)
         (*fd a d tmp1)
         (*fd b c tmp2)
         (+fd tmp1 tmp2 f)))


;; Sudoku

(defn get-square [rows x y]
  (for [x (range x (+ x 3))
        y (range y (+ y 3))]
    (get-in rows [x y])))

(defn init [vars hints]
  (if (seq vars)
    (let [hint (first hints)]
      (all
        (if-not (zero? hint)
          (== (first vars) hint)
          succeed)
        (init (next vars) (next hints))))
    succeed))

(defn sudokufd [hints]
  (let [vars (repeatedly 81 lvar) 
        rows (->> vars (partition 9) (map vec) (into []))
        cols (apply map vector rows)
        sqs  (for [x (range 0 9 3), y (range 0 9 3)]
               (get-square rows x y))]
    (run 1 [q]
      (== q vars)
      (everyo #(infd % (domain 1 2 3 4 5 6 7 8 9)) vars)
      (init vars hints)
      (everyo distinctfd rows)
      (everyo distinctfd cols)
      (everyo distinctfd sqs))))

(defn sudoku
  []
  (sudokufd
   [0 0 3  0 2 0  6 0 0
    9 0 0  3 0 5  0 0 1
    0 0 1  8 0 6  4 0 0
    
    0 0 8  1 0 2  9 0 0
    7 0 0  0 0 0  0 0 8
    0 0 6  7 0 8  2 0 0
    
    0 0 2  6 0 9  5 0 0
    8 0 0  2 0 3  0 0 9
    0 0 5  0 1 0  3 0 0]))