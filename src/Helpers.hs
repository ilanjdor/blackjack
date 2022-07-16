{-# LANGUAGE FlexibleContexts #-}

module Helpers where

import System.Random
import Data.Array.IO
import Control.Monad.State
import Control.Concurrent

import Data

--Taken from:
--https://wiki.haskell.org/Random_shuffle
-- Randomly shuffle a list
-- /O(N)/
shuffle :: [a] -> IO [a]
shuffle xs = do
  ar <- newArray n xs
  forM [1..n] $ \i -> do
    j <- randomRIO (i,n)
    vi <- readArray ar i
    vj <- readArray ar j

    writeArray ar j vi
    return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs

--Taken from:
--https://www.reddit.com/r/haskell/comments/3r8x5m/comment/cwlxnvy/?utm_source=share&utm_medium=web2x&context=3
allSuits = [minBound..maxBound] :: [Suit]
allRanks = [minBound..maxBound] :: [Rank]
allCards = Card <$> allSuits <*> allRanks

getSuit :: Card -> Suit
getSuit (Card suit _ _) = suit

getRank :: Card -> Rank
getRank (Card _ rank _) = rank

getValue :: Card -> Int
getValue (Card _ rank _) = case rank of
  Two -> 2
  Three -> 3
  Four -> 4
  Five -> 5
  Six -> 6
  Seven -> 7
  Eight -> 8
  Nine -> 9
  Ten -> 10
  Jack -> 10
  Queen -> 10
  King -> 10
  Ace -> 11

getShownStatus :: Card -> Bool
getShownStatus (Card _ _ shownStatus) = shownStatus

setShownStatus :: Card -> Bool -> Card
setShownStatus (Card suit rank _) shownStatus = (Card suit rank shownStatus)

determineHandStatus :: Int -> Int -> Phase -> HandStatus -> HandStatus
determineHandStatus sumOfPlayerHand sumOfDealerHand phase playerHandStatus
  | sumOfPlayerHand < 21 && (phase == NaturalsWithoutDealerBlackjack || phase == PlayerHits)          = Pending
  | playerHandStatus == Blackjack || sumOfPlayerHand == 21 && phase == NaturalsWithoutDealerBlackjack = Blackjack
  | sumOfPlayerHand < 21 && sumOfDealerHand == 21 && phase == NaturalsWithDealerBlackjack             = NaturalLoss
  | sumOfPlayerHand == 21 && sumOfDealerHand == 21 && phase == NaturalsWithDealerBlackjack            = NaturalTie
  | sumOfPlayerHand == 21 && phase == PlayerHits                                                      = Hit21
  | sumOfPlayerHand > 21                                                                              = HandBust
  | sumOfDealerHand > 21                                                                              = DealerBust
  | sumOfPlayerHand < sumOfDealerHand                                                                 = LowerThanDealer
  | sumOfPlayerHand == sumOfDealerHand                                                                = SameAsDealer
  | sumOfPlayerHand > sumOfDealerHand                                                                 = HigherThanDealer

setMainBetAmtForPlayer :: Int -> [Player] -> Int -> [Player]
setMainBetAmtForPlayer currPlayerNum [] newMainBetAmt =
  [(currPlayerNum, (([], Pending :: HandStatus), ([], Pending :: HandStatus)), newMainBetAmt, 0)]
setMainBetAmtForPlayer currPlayerNum
  ((playerNum, handPair, mainBetAmt, insuranceBetAmt) : ys) newMainBetAmt =

  case currPlayerNum == playerNum of
    True  -> (playerNum, handPair, newMainBetAmt, insuranceBetAmt) : ys
    False -> (playerNum, handPair, mainBetAmt, insuranceBetAmt) :
      (setMainBetAmtForPlayer currPlayerNum ys newMainBetAmt)

getMainBetAmtForPlayer :: Int -> [Player] -> Int
--getMainBetAmtForPlayer currPlayerNum [] _ = 0
getMainBetAmtForPlayer currPlayerNum
  ((playerNum, _, mainBetAmt, _) : ys) =

  case currPlayerNum == playerNum of
    True  -> mainBetAmt
    False -> getMainBetAmtForPlayer currPlayerNum ys

setInsuranceBetAmtForPlayer :: Int -> [Player] -> Int -> [Player]
setInsuranceBetAmtForPlayer currPlayerNum
  ((playerNum, handPair, mainBetAmt, insuranceBetAmt) : ys) newInsuranceBetAmt =

  case currPlayerNum == playerNum of
    True  -> (playerNum, handPair, mainBetAmt, newInsuranceBetAmt) : ys
    False -> (playerNum, handPair, mainBetAmt, insuranceBetAmt) :
      (setInsuranceBetAmtForPlayer currPlayerNum ys newInsuranceBetAmt)

getInsuranceBetAmtForPlayer :: Int -> [Player] -> Int
--getMainBetAmtForPlayer currPlayerNum [] _ = 0
getInsuranceBetAmtForPlayer currPlayerNum
  ((playerNum, _, _, insuranceBetAmt) : ys) =

  case currPlayerNum == playerNum of
    True  -> insuranceBetAmt
    False -> getInsuranceBetAmtForPlayer currPlayerNum ys

countAcesInHand :: [Card] -> Int
countAcesInHand []       = 0
countAcesInHand (x : xs) = case (getRank x) == Ace of
  True  -> 1 + countAcesInHand xs
  False -> 0 + countAcesInHand xs

getHighSumOfHand :: [Card] -> Int
getHighSumOfHand [] = 0
getHighSumOfHand xs = foldr (+) 0 (map getValue xs)

getNewSumOfHand :: Int -> Int -> Int
getNewSumOfHand highAcesInHand highSumOfHand = case (highAcesInHand > 0 && highSumOfHand > 21) of
  True  -> getNewSumOfHand (highAcesInHand - 1) (highSumOfHand - 10)
  False -> highSumOfHand

getSumOfHand :: [Card] -> Int
getSumOfHand xs = getNewSumOfHand (countAcesInHand xs) (getHighSumOfHand xs)

getSumOfHandForPlayer :: Int -> MainOrSplitHand -> [Player] -> Int
getSumOfHandForPlayer currPlayerNum mainOrSplitHand
  ((playerNum, ((xs, mainHandStatus), (ys, splitHandStatus)), _, _) : zs) =
  case currPlayerNum == playerNum of
    True  -> case mainOrSplitHand of
      Main  -> getSumOfHand xs
      Split -> getSumOfHand ys
    False -> getSumOfHandForPlayer currPlayerNum mainOrSplitHand zs

addCardToPlayerHand :: Card -> Bool -> Int -> MainOrSplitHand -> [Player] -> [Player]
addCardToPlayerHand card shownStatus currPlayerNum mainOrSplitHand
  ((playerNum, ((xs, mainHandStatus), (ys, splitHandStatus)), mainBetAmt, insuranceBetAmt) : zs) =

  case currPlayerNum == playerNum of
    True  -> case mainOrSplitHand of
      Main  -> (playerNum,
        (((setShownStatus card shownStatus): xs, mainHandStatus), (ys, splitHandStatus)), mainBetAmt, insuranceBetAmt) : zs
      Split -> (playerNum,
        ((xs, mainHandStatus), ((setShownStatus card shownStatus): ys, splitHandStatus)), mainBetAmt, insuranceBetAmt) : zs
    False -> (playerNum, ((xs, mainHandStatus), (ys, splitHandStatus)), mainBetAmt, insuranceBetAmt) :
      (addCardToPlayerHand card shownStatus currPlayerNum mainOrSplitHand zs)

revealCard :: Card -> Card
revealCard (Card suit rank _) = (Card suit rank True)

revealHiddenCardInHandCards :: [Card] -> [Card]
revealHiddenCardInHandCards [x]      = [revealCard x]
revealHiddenCardInHandCards (x : xs) = x : (revealHiddenCardInHandCards xs)

revealHiddenCardInHand :: Hand -> Hand
revealHiddenCardInHand (cards, handStatus) = ((revealHiddenCardInHandCards cards), handStatus)

revealHiddenCardInPlayerHand :: Int -> MainOrSplitHand -> [Player] -> [Player]
revealHiddenCardInPlayerHand currPlayerNum mainOrSplitHand
  ((playerNum, (mainHand, splitHand), mainBetAmt, insuranceBetAmt) : zs) =

  case currPlayerNum == playerNum of
    True  -> case mainOrSplitHand of
      Main  -> (playerNum, ((revealHiddenCardInHand mainHand), splitHand), mainBetAmt, insuranceBetAmt) : zs
      Split -> (playerNum, (mainHand, (revealHiddenCardInHand splitHand)), mainBetAmt, insuranceBetAmt) : zs
    False -> (playerNum, (mainHand, splitHand), mainBetAmt, insuranceBetAmt) :
      (revealHiddenCardInPlayerHand currPlayerNum mainOrSplitHand zs)

getStatusForPlayerHand :: Int -> MainOrSplitHand -> [Player] -> HandStatus
getStatusForPlayerHand currPlayerNum mainOrSplitHand ((playerNum,
  ((xs, mainHandStatus), (ys, splitHandStatus)), _, _) : zs) =
  
  case currPlayerNum == playerNum of
    True  -> case mainOrSplitHand of
      Main  -> mainHandStatus
      Split -> splitHandStatus
    False -> getStatusForPlayerHand currPlayerNum mainOrSplitHand zs

setStatusForPlayerHand :: HandStatus -> Int -> MainOrSplitHand -> [Player] -> [Player]
setStatusForPlayerHand newHandStatus currPlayerNum mainOrSplitHand ((playerNum,
  ((xs, mainHandStatus), (ys, splitHandStatus)), mainBetAmt, insuranceBetAmt) : zs) =
  
  case currPlayerNum == playerNum of
    True  -> case mainOrSplitHand of
      Main  -> (playerNum,
        ((xs, newHandStatus), (ys, splitHandStatus)), mainBetAmt, insuranceBetAmt) : zs
      Split -> (playerNum,
        ((xs, mainHandStatus), (ys, newHandStatus)), mainBetAmt, insuranceBetAmt) : zs
    False -> (playerNum, ((xs, mainHandStatus), (ys, splitHandStatus)), mainBetAmt, insuranceBetAmt) :
      (setStatusForPlayerHand newHandStatus currPlayerNum mainOrSplitHand zs)

canSplit :: Int -> [Player] -> Bool
canSplit currPlayerNum ((playerNum, ((xs, _), _), _, _) : zs) =
  case currPlayerNum == playerNum of
    True  -> getValue (head xs) == getValue (head $ tail xs)
    False -> canSplit currPlayerNum zs

splitHand :: Int -> [Player] -> [Player]
splitHand currPlayerNum  ((playerNum,
  ((xs, mainHandStatus), (ys, splitHandStatus)), mainBetAmt, insuranceBetAmt) : zs) =

  case currPlayerNum == playerNum of
    True  -> let newHandStatus = (if (getRank $ head xs) == Ace then SplitAcesUndrawn else SplitNonAcesUndrawn) in
      (playerNum, (([head xs], newHandStatus), ([head xs], newHandStatus)), mainBetAmt, insuranceBetAmt) : zs
    False -> (playerNum, ((xs, mainHandStatus), (ys, splitHandStatus)), mainBetAmt, insuranceBetAmt) :
      splitHand currPlayerNum zs

doubleDownHand :: Int -> MainOrSplitHand -> [Player] -> [Player]
doubleDownHand currPlayerNum mainOrSplitHand ((playerNum,
  ((xs, mainHandStatus), (ys, splitHandStatus)), mainBetAmt, insuranceBetAmt) : zs) =

  case currPlayerNum == playerNum of
    True  -> let newHandStatus = (if (getStatusForPlayerHand currPlayerNum mainOrSplitHand ((playerNum,
                                  ((xs, mainHandStatus), (ys, splitHandStatus)), mainBetAmt, insuranceBetAmt) : zs) ==
                                  SplitAcesDrawn)
                                  then DoubleDownDrawn else DoubleDownUndrawn) in
      case mainOrSplitHand of
        Main -> (playerNum, ((xs, newHandStatus), (ys, splitHandStatus)), mainBetAmt * 2, insuranceBetAmt) : zs
        Split -> (playerNum, ((xs, mainHandStatus), (ys, newHandStatus)), mainBetAmt * 2, insuranceBetAmt) : zs
    False -> (playerNum, ((xs, mainHandStatus), (ys, splitHandStatus)), mainBetAmt, insuranceBetAmt) :
      doubleDownHand currPlayerNum mainOrSplitHand zs

canDoubleDownHand :: Int -> MainOrSplitHand -> [Player] -> Bool
canDoubleDownHand currPlayerNum mainOrSplitHand ((playerNum, ((xs, _), (ys, _)), _, _) : zs) = do
  let hand = (if (mainOrSplitHand == Main) then xs else ys)
  if currPlayerNum == playerNum then do
    let sum = getSumOfHand hand in (sum >= 9 && sum <= 11)
  else canDoubleDownHand currPlayerNum mainOrSplitHand zs

-- IO functions to display game output
showHandStatus :: HandStatus -> IO ()
showHandStatus handStatus = case handStatus of
  Pending  -> return ()
  _        -> putStr $ " (" ++ (show handStatus) ++ ")"

showSuit :: Suit -> IO ()
showSuit suit = case suit of
  Clubs    -> putStr "\9827" -- ♣
  Diamonds -> putStr "\9830" -- ♦
  Hearts   -> putStr "\9829" -- ♥
  Spades   -> putStr "\9824" -- ♠

showCard :: Card -> IO ()
showCard card = case (getShownStatus card) of
  True  -> let rank = (getRank card) in
    case rank == Jack || rank == Queen || rank == King || rank == Ace of
      True  -> do
        putStr $ [head (show rank)] ++ " "
        showSuit $ getSuit card
      False -> do
        putStr $ (show $ getValue card) ++ " "
        showSuit $ getSuit card
  False -> putStr "(Hidden)"

showHand :: Hand-> IO ()
showHand ([x], handStatus)     = do
  showCard x
  showHandStatus handStatus
showHand (x : xs, handStatus)  = do
  showCard x
  putStr ", "
  showHand (xs, handStatus)

showHandPair :: HandPair -> IO ()
showHandPair (([], _), ([], _)) = return ()
showHandPair ((xs, mainHandStatus), ([], _))               = do
  putStr "Main Hand: "
  showHand ((reverse xs), mainHandStatus)
showHandPair ((xs, mainHandStatus), (ys, splitHandStatus)) = do
  putStr "Main Hand: "
  showHand ((reverse xs), mainHandStatus)
  putStr "; Split Hand: "
  showHand ((reverse ys), splitHandStatus)

showPlayer :: Player -> IO ()
showPlayer (playerNum, handPair, mainBetAmt, insuranceBetAmt) = do
  showPlayerNum playerNum
  showHandPair handPair
  putStr $ "Main Bet Amt: " ++ (show mainBetAmt)
  if insuranceBetAmt > 0 then
    putStrLn $ "; Insurance Bet Amt: " ++ (show insuranceBetAmt)
  else
    putStrLn ""
  --showResult result
  
showPlayerNum :: Int -> IO ()
showPlayerNum playerNum = case playerNum < 9 of
  True  -> putStr $ "Player  " ++ (show $ playerNum + 1) ++ ": "
  False -> putStr $ "Player " ++ (show $ playerNum + 1) ++ ": "

showPlayers :: [Player] -> IO ()
showPlayers [player]       = do
  showPlayer player
showPlayers (x : xs)       = do
  showPlayer x
  showPlayers xs

showDealerHand :: DealerHand-> IO ()
showDealerHand [x]            = do
  showCard x
showDealerHand (x : xs)       = do
  showCard x
  putStr ", "
  showDealerHand xs

showPlayersAndDealerHand :: [Player] -> [Card] -> Bool -> Bool -> IO ()
showPlayersAndDealerHand players dealerHand playersFinalized dealerFinalized = do
  putStrLn "******************************"
  putStr "Players' hands"
  case playersFinalized of
    True  -> putStrLn " (finalized):"
    False -> putStrLn ":"
  putStrLn "******************************\n"
  showPlayers players
  putStrLn "\n******************************"
  putStr "Dealer's hand"
  case dealerFinalized of
    True  -> putStrLn " (finalized):"
    False -> putStrLn ":"
  putStrLn "******************************\n"
  (showDealerHand . reverse) dealerHand
  putStrLn "\n\n******************************\n"

showPlayerHandStatus :: Int -> HandStatus -> IO ()
showPlayerHandStatus playerNum handStatus = case handStatus of
  Blackjack  -> do
    putStrLn $ "Player " ++ (show $ playerNum + 1) ++ ", congratulations, you scored Blackjack (3:2 payoff)!\n"
    threadDelay 1000000
  Hit21      -> do
    putStrLn $ "Player " ++ (show $ playerNum + 1) ++ ", congratulations, you hit 21!\n"
    threadDelay 1000000
  HandBust -> do
    putStrLn $ "Player " ++ (show $ playerNum + 1) ++ ", sorry, you busted!\n"
    threadDelay 1000000
  _ -> return ()

showDealerHandStatus :: Int -> IO ()
showDealerHandStatus sumOfDealerHand
  | sumOfDealerHand <= 16                             = do
      putStrLn $ "\nDealer hits (16 or below):\n"
      threadDelay 1000000
  | (sumOfDealerHand > 16) && (sumOfDealerHand <= 21) = do
      putStrLn $ "\nDealer stands (17 or above):\n"
      threadDelay 1000000
  | sumOfDealerHand > 21                              = do
      putStrLn $ "\nDealer busts!\n"
      threadDelay 1000000

showFinalHandStatus :: Int -> HandStatus -> IO ()
showFinalHandStatus playerNum handStatus = do
  showPlayerNum playerNum
  case handStatus of
    Blackjack        -> putStrLn "Scored Blackjack and already won a 3:2 payoff (+150%)"
    HandBust         -> putStrLn "Busted and already lost bet amount (-100%)"
    DealerBust       -> putStrLn "Survived dealer bust and wins a 1:1 payoff (+100%)"
    NaturalLoss      -> putStrLn "Scored lower than dealer's Blackjack and loses bet amount (-100%)"
    NaturalTie       -> putStrLn "Ties dealer's Blackjack and reclaims bet amount (+0%)"
    LowerThanDealer  -> putStrLn "Scored lower than dealer and loses bet amount (-100%)"
    SameAsDealer     -> putStrLn "Ties dealer and reclaims bet amount (+0%)"
    HigherThanDealer -> putStrLn "Scored higher than dealer and wins a 1:1 payoff (+100%)"
    _                -> putStrLn "Missing case!"
