{-# LANGUAGE FlexibleContexts #-}

module Data where

data Suit = Clubs | Diamonds | Hearts | Spades deriving (Ord, Enum, Eq, Show, Bounded)
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | 
  Jack | Queen | King | Ace deriving (Ord, Enum, Eq, Show, Bounded)
data Card = Card Suit Rank Bool deriving Show --Bool is shown/hidden status

data HandStatus = Pending | Blackjack | NaturalLoss | NaturalTie |
  SplitNonAcesUndrawn | SplitNonAcesDrawn |
  SplitAcesUndrawn | SplitAcesDrawn |
  DoubleDownUndrawn | DoubleDownDrawn |
  SplitPairs | DoubleDowned |
  Standing | Hit21 | HandBust |
  DealerBust | LowerThanDealer | SameAsDealer | HigherThanDealer deriving (Eq, Show)

data MainOrSplitHand = Main | Split deriving (Eq, Show) --Main is main hand; Split is split hand

data Phase = Bets | DealOrigCardToPlayer | DealOrigCardToDealer |
  CheckIfDealerHasBlackjack | NaturalsWithDealerBlackjack | NaturalsWithoutDealerBlackjack |
  Splits | DoubleDown | Insurance |
  PlayerHits | DealerHits | FinalResults | Settle deriving Eq

type Shoe = [Card]

type Hand = ([Card], HandStatus)
type HandPair = (Hand, Hand) --(main hand, split hand)
type Player = (Int, HandPair, Int, Int)

type DealerHand = [Card]
