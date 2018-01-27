(ns war-cardgame.core-test
  (:require [clojure.test :refer :all]
            [war-cardgame.core :refer :all]))

(deftest cards-in-deck
  (testing "there are 52 cards in the deck"
    (is (= 52 (count deck))))
  (testing "spot checking rank and suits"
    (is (some #{{:rank 2 :suit :clubs}} deck))
    (is (some #{{:rank :ace :suit :hearts}} deck))
    (is (some #{{:rank 10 :suit :diamonds}} deck))
    (is (some #{{:rank :jack :suit :spades}} deck))))

(deftest get-the-rank
  (testing "can get the rank of the card"
    (is (= 2 (rank {:rank 2 :suit :hearts})))
    (is (= :king (rank {:rank :king :suit :clubs})))))

(deftest get-the-rank-value
  (testing "can get the relative value of a rank"
    (is (= 0 (rank-value 2)))
    (is (= 12 (rank-value :ace)))
    (is (= 9 (rank-value :jack)))
    (is (= 8 (rank-value 10)))))

(deftest get-the-card-value
  (testing "can get the relative value of a card"
    (is (= 0 (card-value {:rank 2 :suit :clubs})))
    (is (= 12 (card-value {:rank :ace :suit :clubs})))
    (is (= 9 (card-value {:rank :jack :suit :clubs})))
    (is (= 8 (card-value {:rank 10 :suit :clubs})))))

(deftest can-compare-cards
  (testing "can compare the relative values of two cards"
    (is (= 0 (compare-cards {:rank 2 :suit :clubs}
                            {:rank 2 :suit :spades})))
    (is (= 1 (compare-cards {:rank 3 :suit :clubs}
                            {:rank 2 :suit :spades})))
    (is (= -1 (compare-cards {:rank 2 :suit :clubs}
                            {:rank 3 :suit :spades})))))

(deftest can-deal-cards
  (testing "can distribute cards evenly between players"
    (let [[p1 p2] (deal deck)]
      (is (= 26 (count p1)))
      (is (= 26 (count p2)))
      (is (= {:rank :ace :suit :hearts} (first p1)))
      (is (= {:rank 2 :suit :clubs} (last p1)))
      (is (= {:rank :ace :suit :spades} (first p2)))
      (is (= {:rank 2 :suit :diamonds} (last p2))))))

(deftest can-play-simple-turn
  (testing "can determine a winner and move played cards to winners stack based on first card flipped"
    (let [winner '({:rank :ace :suit :hearts})
          loser  '({:rank 2    :suit :hearts})
          [p1w p2l] (play-turn winner loser)
          [p1l p2w] (play-turn loser  winner)]
      (is (= 2 (count p1w)))
      (is (= 0 (count p2l)))
      (is (= 0 (count p1l)))
      (is (= 2 (count p2w)))))) 

(deftest cards-move-to-end-of-stack
  (testing "when a player wins, the kitty moves to the end of the stack"
    (let [winner '({:rank :ace :suit :spades} {:rank 3 :suit :hearts})
          loser  '({:rank :king :suit :hearts} {:rank 4 :suit :clubs})
          [p1 p2] (play-turn winner loser)
          [p3 p4] (play-turn loser  winner)]
      (is (= {:rank 3 :suit :hearts} (first p1)))
      (is (= {:rank 3 :suit :hearts} (first p4))))))

(deftest can-play-single-war
  (testing "can determine a winner if both players first card has the same rank"
    (let [winner '({:rank 5 :suit :clubs}  {:rank 7 :suit :clubs} {:rank :ace :suit :spades})
          loser  '({:rank 5 :suit :spades} {:rank 6 :suit :spades } {:rank :king :suit :hearts})
          [p1w p2l] (play-turn winner loser)
          [p1l p2w] (play-turn loser  winner)]
      (is (= 6 (count p1w)))
      (is (= 0 (count p2l)))
      (is (= 0 (count p1l)))
      (is (= 6 (count p2w))))))

(deftest can-play-multiple-wars
  (testing "can determine a winner if both players first and third cards have the same rank"
    (let [winner '({:rank 5 :suit :clubs} {:rank 6 :suit :clubs} {:rank :king :suit :clubs} {:rank 7 :suit :clubs} {:rank :ace :suit :hearts})
          loser  '({:rank 5 :suit :spades} {:rank 7 :suit :spades} {:rank :king :suit :hearts} {:rank 2 :suit :diamonds} {:rank :2 :suit :hearts})
          [p1w p2l] (play-turn winner loser)
          [p1l p2w] (play-turn loser  winner)]
      (is (= 10 (count p1w)))
      (is (= 0 (count p2l)))
      (is (= 0 (count p1l)))
      (is (= 10 (count p2w))))))

(deftest not-enough-cards-war
  (testing "A player runs out of cards during a war"))

(deftest end-game
  (testing "game ends when a player runs out of cards"
    (is (true? (game-over? '() '({:rank :ace :suit :spades}))))
    (is (true? (game-over? '({:rank :ace :suit :spades}) '())))
    (is (false? (game-over? '({:rank :ace :suit :spades}) '({:rank :king :suit :hearts}))))))

(deftest we-have-a-winner
  (testing "can play a game to completion with winner"
    (let [deck '({:rank :ace :suit :spades} {:rank 2 :suit :hearts})
          g1   (play-game deck)
          g2   (play-game (reverse deck))]
      (is (= "Player 1 wins!" g1))
      (is (= "Player 2 wins!" g2)))))
