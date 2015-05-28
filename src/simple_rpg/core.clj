(ns simple-rpg.core)

(def max-lvl 10)

; это - простая rpg-игра на Clojure
; сделано по видеообучалке
; https://www.youtube.com/watch?v=r0TWL5L7RE0
; name - имя персонажа
; lvl - уровень персонажа
; att - атака игрока
; def - защита игрока
; hp - количество жизней игрока

(defn calc-attack
  "given lvl calculate attack"
  [lvl]
  (* lvl 6))

(defn calc-defence
  "given def calculate defence"
  [lvl]
  (int (* lvl 1.4)))

(defn calc-hitpoints
  [lvl]
  (* lvl 20))

(defn calc-sides
  [lvl]
  (if (> lvl 5) 4 6))

(defn kill-negative
  [n]
  (if (neg? n) 0 n))

(defn calc-base-damage 
  [att def]
  (kill-negative (- att def)))

;; given sides count roll the dice
;; sides(int) -> roll dice(int)
(defn roll-dice
  [sides]
  (inc (rand-int sides)))

; функция создания персонажей
(defn create-character
  [name lvl]
  {:name name
   :lvl lvl
   :att (calc-attack lvl)
   :def (calc-defence lvl) 
   :hp (calc-hitpoints lvl)})

;; подсчет реальных потерь в зависимости от выпавшего значения кубика
(defn real-damage
  [base sides]
  (let [rd (roll-dice sides)
        s (/ sides 2)]
    (cond 
      (<= rd s) (int (/ base 2))
      (> rd s) base
      (= rd sides) (* base 2))))


;; функция боя между двуия пресонажами
(defn take-damage 
  [from to]
  (let [bd (calc-base-damage (:att from) (:def to))
        s (calc-sides (:lvl from))
        rd (real-damage bd s)]
    [rd (update-in to [:hp] #(- % rd))]))


; конфиг для игры
(def config
  {:player player
   :enemy big-troll})

(def log-template 
  "Character %s recieved %d damage.
   His new life is %d.")

(defn print-battle-log
  [damage character]
  (let [name (:name character)
        newhp (:hp character)
        s (format log-template name damage newhp)]
    (println s)))

; определение победителя
; p-hp - жизнь игрока, e-hp - жизнь противника
(defn print-winner
  [p-hp e-hp]
  (if (<= p-hp 0)
    (println "Enemy won.")
    (println "Player won.")))

; игровая логика
(defn game-logic 
  [config]
  (loop [player (:player config)
         enemy (:enemy config)
         round 1]
    (if (or (<= (:hp player) 0)
            (<= (:hp enemy) 0))
      (print-winner (:hp player) (:hp enemy))
      (let [pl->en (take-damage player enemy)
            en->pl (take-damage enemy player)]
        (do 
          (println (str "Round " round ":"))
          (print-battle-log (pl->en 0) (pl->en 1))
          (print-battle-log (en->pl 0) (en->pl 1))
          (recur (en->pl 1) (pl->en 1) (inc round)))))))


; create player Robert and enemies - trolls
(def player (create-character "Robert" 6))
(def troll (create-character "Troll" 2))
(def big-troll (create-character "Big Troll" 4))