(ns war-cardgame.core
  (:gen-class))

(def ranks [2 3 4 5 6 7 8 9 10 :jack :queen :king :ace])
(def suits [:clubs :diamonds :hearts :spades])

;; data structures
;;
;; card
;; {:rank 2      :suit :clubs}
;; {:rank :queen :suit :hearts}
;; etc.

;; deck
;; list of cards
;; '({:rank 2 :suit :clubs} {:rank 2 :suit :diamonds} ,,,)
(def deck (for [r ranks s suits] {:rank r :suit s}))

;; game
;; the state of the game
;; {:player-one-hand [vector of cards]
;;  :player-two-hand [vector of cards]
;;  :kitty '(list of cards that will go to the winner of the turn)
;;  :turn 0,1,2,3..n}
(def game (atom {:player-one-hand []
                 :player-two-hand []
                 :kitty '()
                 :turn 0}))

(defn rank
  [card]
  (:rank card))

(defn rank-value
  [rank]
  (.indexOf ranks rank))

(defn card-value
  [card]
  (rank-value (rank card)))

(defn compare-cards
  [x y]
  (compare (card-value x) (card-value y)))

(defn deal-to-player
  [[p1 p2 :as stacks] card]
  (update-in stacks [(if (= (count p1) (count p2)) 0 1)] conj card))

(defn deal
  [deck]
  (reduce deal-to-player ['() '()] deck))

(defn play-turn
  ([p1-stack p2-stack kitty]
   (let [[p1 & p1s] p1-stack
         [p2 & p2s] p2-stack
         kitty (shuffle (conj kitty p1 p2))
         winner (compare-cards p1 p2)]
     (cond
       (= 1 winner) [(concat p1s kitty) p2s]
       (= -1 winner) [p1s (concat p2s kitty)]
       (= 0 winner) (recur (rest p1s)
                           (rest p2s)
                           (conj kitty (first p1s) (first p2s))))))
  ([p1-stack p2-stack]
   (play-turn p1-stack p2-stack '())))

(defn game-over?
  [p1 p2]
  (or (= 0 (count p1)) (= 0 (count p2))))

(defn play-game
  [shuffled-deck]
  (let [[player1 player2] (deal shuffled-deck)]
    (loop [p1 player1
           p2 player2
           turn 1]
      (if (game-over? p1 p2)
        (str "Player " (if (pos? (count p1)) "1" "2") " wins!")
        (let [[p1 p2] (play-turn p1 p2)]
          (recur p1 p2 (inc turn)))))))

(defn war
  []
  (play-game (shuffle deck)))

(defn -main
  [& args]
  (println (war)))

;; a game looks like
;; shuffle - randomize order of cards in a deck
;; deal - equally distribute cards to player 1 and player 2
;; play turns until either player 1 or player 2 has no cards left

;; * a turn looks like
;; ** select the "top" card from both player 1 and player 2 stacks
;; ** compare the ranks of the cards
;; *** one ranks higher - the player who played the higher rank moves both cards to the "bottom" of their stack
;; *** ranks are the same
;; **** each player takes the "top" card from their stack and ignore the value
;; **** take the next top card from each player's stack and re-evaluate the compare
