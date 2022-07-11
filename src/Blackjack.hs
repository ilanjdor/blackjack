{-# LANGUAGE FlexibleContexts #-}

module Main where

import System.Random
import Data.Array.IO
import Control.Monad.State
import Control.Concurrent

data Suit = Clubs | Diamonds | Hearts | Spades deriving (Ord, Enum, Eq, Show, Bounded)
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | 
  Jack | Queen | King | Ace deriving (Ord, Enum, Eq, Show, Bounded)
data Card = Card Suit Rank deriving Show
type Shoe = [Card]
type Hand = [Card]

data Result = Pending | Blackjack | NaturalLoss | NaturalTie | NaturalWin |Standing | Hit21 | PlayerBust | 
  DealerBust | LowerThanDealer | SameAsDealer | HigherThanDealer deriving (Eq, Show)
type Player = (Int, Result, Hand)

data Phase = DealOrigCardToPlayer | DealOrigCardToDealer |
  CheckIfDealerHasBlackjack | NaturalsWithDealerBlackjack | NaturalsWithoutDealerBlackjack |
  PlayersHit | DealerHits | FinalResults | Settle deriving Eq

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
getSuit (Card suit _) = suit

getRank :: Card -> Rank
getRank (Card _ rank) = rank

getValue :: Card -> Int
getValue (Card _ rank) = case rank of
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

determineResult :: Int -> Int -> Phase -> Result
determineResult sumOfPlayerHand sumOfDealerHand phase
  | (sumOfPlayerHand /= 21) && (sumOfDealerHand == 21) && (phase == NaturalsWithDealerBlackjack) = NaturalLoss
  | (sumOfPlayerHand == 21) && (sumOfDealerHand == 21) && (phase == NaturalsWithDealerBlackjack) = NaturalTie
  | (sumOfPlayerHand == 21) && (sumOfDealerHand /= 21) && (phase == NaturalsWithDealerBlackjack) = NaturalWin
  | sumOfPlayerHand > 21 = PlayerBust 
  | sumOfDealerHand > 21 = DealerBust
  | (sumOfPlayerHand < sumOfDealerHand) && (phase /= NaturalsWithDealerBlackjack) = LowerThanDealer
  | (sumOfPlayerHand == sumOfDealerHand) && (phase /= NaturalsWithDealerBlackjack)= SameAsDealer
  | (sumOfPlayerHand > sumOfDealerHand) && (phase /= NaturalsWithDealerBlackjack)= HigherThanDealer

determineHittingResult :: Int -> Result
determineHittingResult sumOfHand
  | sumOfHand < 21 = Pending
  | sumOfHand == 21 = Hit21
  | sumOfHand > 21 = PlayerBust

determineFinalResult :: Int -> Result -> Int -> Result
determineFinalResult sumOfPlayerHand playerResult sumOfDealerHand
  | playerResult == PlayerBust || playerResult == Blackjack = playerResult
  | sumOfDealerHand > 21 = DealerBust
  | sumOfPlayerHand < sumOfDealerHand = LowerThanDealer
  | sumOfPlayerHand == sumOfDealerHand = SameAsDealer
  | sumOfPlayerHand > sumOfDealerHand = HigherThanDealer

countAcesInHand :: Hand -> Int
countAcesInHand [] = 0
countAcesInHand (x : xs) = case (getRank x) == Ace of
  True -> 1 + countAcesInHand xs
  False -> 0 + countAcesInHand xs

getHighSumOfHand :: Hand -> Int
getHighSumOfHand xs = foldr (+) 0 (map getValue xs)

getNewSumOfHand :: Int -> Int -> Int
getNewSumOfHand highAcesInHand highSumOfHand = case (highAcesInHand > 0 && highSumOfHand > 21) of
  True -> getNewSumOfHand (highAcesInHand - 1) (highSumOfHand - 10)
  False -> highSumOfHand

getSumOfHand :: Hand -> Int
getSumOfHand xs = getNewSumOfHand (countAcesInHand xs) (getHighSumOfHand xs)

getSumOfHandForPlayer :: Int -> [Player] -> Int
getSumOfHandForPlayer currPlayerNum ((playerNum, _, hand) : xs) =
  case currPlayerNum == playerNum of
    True -> getSumOfHand hand
    False -> getSumOfHandForPlayer currPlayerNum xs

addCardToPlayerHand :: Card -> Int -> [Player] -> [Player]
addCardToPlayerHand card currPlayerNum [] = [(currPlayerNum, Pending :: Result, [card])]
addCardToPlayerHand card currPlayerNum ((playerNum, result, xs) : ys) = 
  case currPlayerNum == playerNum of
    True -> (playerNum, result, (card : xs)) : ys
    False -> (playerNum, result, xs) : (addCardToPlayerHand card currPlayerNum ys)

getResultForPlayer :: Int -> [Player] -> Result
getResultForPlayer currPlayerNum ((playerNum, result, hand) : xs) =
  case currPlayerNum == playerNum of
    True -> result
    False -> getResultForPlayer currPlayerNum xs

setResultForPlayer :: Result -> Int -> [Player] -> [Player]
setResultForPlayer newResult currPlayerNum ((playerNum, result, hand) : xs) =
  case currPlayerNum == playerNum of
    True -> (playerNum, newResult, hand) : xs
    False -> (playerNum, result, hand) : (setResultForPlayer newResult currPlayerNum xs)

playRound :: Int -> Int -> Shoe -> [Player] -> Hand -> Phase -> Bool -> IO ()
playRound currPlayerNum numberOfPlayers shoe players dealerHand phase secondOrigCardDealt =
  case phase of
    DealOrigCardToPlayer -> do
      let (card, updatedShoe) = (head shoe, tail shoe)
      let updatedPlayers = addCardToPlayerHand card currPlayerNum players
      let updatedPlayerNum = currPlayerNum + 1
      playRound updatedPlayerNum numberOfPlayers updatedShoe updatedPlayers dealerHand 
        (if updatedPlayerNum < numberOfPlayers then DealOrigCardToPlayer else DealOrigCardToDealer) secondOrigCardDealt
    DealOrigCardToDealer -> do
      let (card, updatedShoe) = (head shoe, tail shoe)
      let updatedDealerHand = (card : dealerHand)
      playRound 0 numberOfPlayers updatedShoe players updatedDealerHand 
        (if secondOrigCardDealt then CheckIfDealerHasBlackjack else DealOrigCardToPlayer) True
    CheckIfDealerHasBlackjack -> do
      case (getSumOfHand dealerHand) == 21 of
        True -> do
          putStrLn ""
          putStrLn "Dealer has Blackjack."
          putStrLn ""
          showPlayersAndDealerHand players dealerHand True True
          playRound 0 numberOfPlayers shoe players dealerHand NaturalsWithDealerBlackjack True
        False -> do
          putStrLn ""
          showPlayersAndDealerHand players dealerHand False False
          playRound 0 numberOfPlayers shoe players dealerHand NaturalsWithoutDealerBlackjack True
    NaturalsWithDealerBlackjack -> do
      let sumOfPlayerHand = getSumOfHandForPlayer currPlayerNum players
      let result = determineResult sumOfPlayerHand 21 phase
      let updatedPlayers = setResultForPlayer result currPlayerNum players
      showFinalResult currPlayerNum result
      let updatedPlayerNum = currPlayerNum + 1
      case updatedPlayerNum < numberOfPlayers of
        True -> playRound updatedPlayerNum numberOfPlayers shoe updatedPlayers dealerHand NaturalsWithDealerBlackjack True
        False -> do
          putStrLn ""
          return ()
    NaturalsWithoutDealerBlackjack -> do
      let sumOfPlayerHand = getSumOfHandForPlayer currPlayerNum players
      let result = if sumOfPlayerHand == 21 then Blackjack else Pending
      showPlayerResult currPlayerNum result
      let updatedPlayers = setResultForPlayer result currPlayerNum players
      let updatedPlayerNum = currPlayerNum + 1
      case updatedPlayerNum < numberOfPlayers of
        True -> playRound updatedPlayerNum numberOfPlayers shoe players dealerHand NaturalsWithoutDealerBlackjack True
        False -> playRound 0 numberOfPlayers shoe players dealerHand PlayersHit True
    PlayersHit -> do
      case (getResultForPlayer currPlayerNum players) of
        Pending -> do
          putStrLn $ "Player " ++ (show $ currPlayerNum + 1) ++ ", would you like to hit? (0 to stand; any other number to hit)"
          move <- getLine
          putStrLn ""
          let hit = read move
          case hit of
            0 -> do
              let updatedPlayers = setResultForPlayer (Standing :: Result) currPlayerNum players
              playRound currPlayerNum numberOfPlayers shoe updatedPlayers dealerHand PlayersHit True
            _ -> do
              let (card, updatedShoe) = (head shoe, tail shoe)
              let updatedPlayers = addCardToPlayerHand card currPlayerNum players
              let sumOfHand = getSumOfHandForPlayer currPlayerNum updatedPlayers
              let result = determineHittingResult sumOfHand
              let updatedPlayers2 = setResultForPlayer result currPlayerNum updatedPlayers
              putStrLn ""
              showPlayersAndDealerHand updatedPlayers2 dealerHand False False
              putStrLn ""
              showPlayerResult currPlayerNum result
              playRound currPlayerNum numberOfPlayers updatedShoe updatedPlayers2 dealerHand PlayersHit True
        _ ->  do
          let updatedPlayerNum = currPlayerNum + 1
          case updatedPlayerNum < numberOfPlayers of
            True -> playRound updatedPlayerNum numberOfPlayers shoe players dealerHand PlayersHit True
            False -> do
              putStrLn $ "\nDealer (revealing hidden card) has:\n"
              (showHand . reverse) dealerHand
              threadDelay 1000000
              putStrLn ""
              playRound 0 numberOfPlayers shoe players dealerHand DealerHits True
    DealerHits -> do
      let dealerSum = getSumOfHand dealerHand
      case dealerSum <= 16 of
        True -> do
          showDealerResult dealerSum
          let (card, updatedShoe) = (head shoe, tail shoe)
          let updatedDealerHand = (card : dealerHand)
          let updatedDealerSum = getSumOfHand updatedDealerHand
          showPlayersAndDealerHand players updatedDealerHand True False
          playRound 0 numberOfPlayers updatedShoe players updatedDealerHand DealerHits True
        False -> do
          showDealerResult dealerSum
          showPlayersAndDealerHand players dealerHand True True
          playRound 0 numberOfPlayers shoe players dealerHand FinalResults True
    FinalResults -> do
      let result = determineFinalResult (getSumOfHandForPlayer currPlayerNum players) (getResultForPlayer currPlayerNum players) (getSumOfHand dealerHand)
      let updatedPlayers = setResultForPlayer result currPlayerNum players
      let updatedPlayerNum = currPlayerNum + 1
      case updatedPlayerNum < numberOfPlayers of
        True -> playRound updatedPlayerNum numberOfPlayers shoe updatedPlayers dealerHand FinalResults True
        False -> do
          putStrLn $ "\nDealer settles with players:"
          putStrLn "\n******************************"
          putStrLn "Player payoffs:"
          putStrLn "******************************\n"
          playRound 0 numberOfPlayers shoe updatedPlayers dealerHand Settle True
    Settle -> do
      showFinalResult currPlayerNum (getResultForPlayer currPlayerNum players)
      let updatedPlayerNum = currPlayerNum + 1
      case updatedPlayerNum < numberOfPlayers of
        True -> playRound updatedPlayerNum numberOfPlayers shoe players dealerHand Settle True
        False -> do
          putStrLn "\n******************************\n"
          putStrLn "End of round\n"

showResult :: Result -> IO ()
showResult result = case result of
  Pending -> return ()
  _ -> putStr $ " (" ++ (show result) ++ ")"

showSuit :: Suit -> IO ()
showSuit suit = case suit of
  Clubs -> putStr "\9827" -- ♣
  Diamonds -> putStr "\9830" -- ♦
  Hearts -> putStr "\9829" -- ♥
  Spades -> putStr "\9824" -- ♠

showCard :: Card -> IO ()
showCard card = let rank = (getRank card) in
  case rank == Jack || rank == Queen || rank == King || rank == Ace of
    True -> do
      putStr $ [head (show rank)] ++ " "
      showSuit $ getSuit card
    False -> do
      putStr $ (show $ getValue card) ++ " "
      showSuit $ getSuit card

showHand :: Hand -> IO ()
showHand [x]       = do
  showCard x
showHand (x : xs)  = do
  showCard x
  putStr ", "
  showHand xs

showPlayerNum :: Int -> IO ()
showPlayerNum playerNum = case playerNum < 9 of
  True  -> putStr $ "Player  " ++ (show $ playerNum + 1) ++ ": "
  False -> putStr $ "Player " ++ (show $ playerNum + 1) ++ ": "

showPlayers :: [Player] -> IO ()
showPlayers [(playerNum, result, hand)]       = do
  showPlayerNum playerNum
  (showHand . reverse) hand
  showResult result
  putStr "\n"
showPlayers ((playerNum, result, hand) : xs)  = do
  showPlayerNum playerNum
  (showHand . reverse) hand
  showResult result
  putStr "\n"
  showPlayers xs

showDealerHalfHiddenHand :: Hand -> IO ()
showDealerHalfHiddenHand [x]       = do
  putStr "(Hidden)"
showDealerHalfHiddenHand (x : xs)  = do
  showCard x
  putStr ", "
  showDealerHalfHiddenHand xs

showPlayersAndDealerHand :: [Player] -> Hand -> Bool -> Bool -> IO ()
showPlayersAndDealerHand players dealerHand playersFinalized dealerFinalized = do
  putStrLn "******************************"
  putStr "Players' hands"
  case playersFinalized of
    True -> putStrLn " (finalized):"
    False -> putStrLn ":"
  putStrLn "******************************\n"
  showPlayers players
  putStrLn "\n******************************"
  putStr "Dealer's hand"
  case dealerFinalized of
    True -> putStrLn " (finalized):"
    False -> putStrLn ":"
  putStrLn "******************************\n"
  case playersFinalized of
    True -> (showHand . reverse) dealerHand
    False -> (showDealerHalfHiddenHand . reverse) dealerHand
  putStrLn "\n\n******************************\n"

showPlayerResult :: Int -> Result -> IO ()
showPlayerResult playerNum result = case result of
  Blackjack -> do
    putStrLn $ "Player " ++ (show $ playerNum + 1) ++ ", congratulations, you scored Blackjack (3:2 payoff)!\n"
    threadDelay 1000000
  Hit21 -> do
    putStrLn $ "Player " ++ (show $ playerNum + 1) ++ ", congratulations, you hit 21!\n"
    threadDelay 1000000
  PlayerBust -> do
    putStrLn $ "Player " ++ (show $ playerNum + 1) ++ ", sorry, you busted!\n"
    threadDelay 1000000
  _ -> return ()

showDealerResult :: Int -> IO ()
showDealerResult sumOfDealerHand
  | sumOfDealerHand <= 16 = do
      putStrLn $ "\nDealer hits (16 or below):\n"
      threadDelay 1000000
  | (sumOfDealerHand > 16) && (sumOfDealerHand <= 21) = do
      putStrLn $ "\nDealer stands (17 or above):\n"
      threadDelay 1000000
  | sumOfDealerHand > 21 = do
      putStrLn $ "\nDealer busts!\n"
      threadDelay 1000000

showFinalResult :: Int -> Result -> IO ()
showFinalResult playerNum result = do
  showPlayerNum playerNum
  case result of
    Blackjack -> putStrLn "Scored Blackjack and already won a 3:2 payoff (+150%)"
    PlayerBust -> putStrLn "Busted and already lost bet amount (-100%)"
    DealerBust -> putStrLn "Survived dealer bust and wins a 1:1 payoff (+100%)"
    NaturalLoss -> putStrLn "Scored lower than dealer's Blackjack and loses bet amount (-100%)"
    NaturalTie -> putStrLn "Ties dealer's Blackjack and reclaims bet amount (+0%)"
    NaturalWin -> putStrLn "Scored Blackjack and already won a 3:2 payoff (+100%)"
    LowerThanDealer -> putStrLn "Scored lower than dealer and loses bet amount (-100%)"
    SameAsDealer -> putStrLn "Ties dealer and reclaims bet amount (+0%)"
    HigherThanDealer -> putStrLn "Scored higher than dealer and wins a 1:1 payoff (+100%)"
    _ -> putStrLn "Missing case!"

main = do
  putStrLn "Enter the number of players:"
  numberOfPlayers <- getLine
  let p = (read numberOfPlayers :: Int)
  putStrLn "Enter number of decks to include in shoe:"
  numberOfDecks <- getLine
  let d = (read numberOfDecks :: Int)
  deck <- shuffle allCards
  shoe <- shuffle (concat $ take d $ repeat deck)
  playRound 0 p shoe ([]) ([]) DealOrigCardToPlayer False
