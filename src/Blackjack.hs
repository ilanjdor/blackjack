{-# LANGUAGE FlexibleContexts #-}

module Main where

import System.Random
import Data.Array.IO
import Control.Monad.State
import Control.Concurrent

data Suit = Clubs | Diamonds | Hearts | Spades deriving (Ord, Enum, Eq, Show, Bounded)
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Ord, Enum, Eq, Show, Bounded)
data Card = Card Suit Rank deriving Show
type Hand = [Card]
type Round = [[Card]]
data Phase = Begin | Deal1stCard | Deal1stCardToDealer | Deal2ndCard | Deal2ndCardToDealer | PlayersHit | DealerHits | FinalResults | Settle
data Result = Pending | Blackjack | Standing | Hit21 | PlayerBust | DealerBust | LowerThanDealer | SameAsDealer | HigherThanDealer deriving Eq

--Taken from:
--https://wiki.haskell.org/Random_shuffle
-- | Randomly shuffle a list
--   /O(N)/
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

setResult :: Result -> Int -> [Result] -> [Result]
setResult result 0 [] = [result]
setResult result 0 (x : xs) = result : xs
setResult result player (x : xs) = x : (setResult result (player - 1) xs)

determineResult :: Int -> Bool -> Result
determineResult sumOfHand isHitting
  | sumOfHand < 21 = Pending
  | sumOfHand == 21 && not (isHitting) = Blackjack
  | sumOfHand == 21 && isHitting = Hit21
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

getSumOfHandForPlayer :: Int -> Round -> Int
getSumOfHandForPlayer 0 (xs : xss) = getSumOfHand xs
getSumOfHandForPlayer player (xs : xss) = getSumOfHandForPlayer (player - 1) xss

addCardToRound :: Card -> Int -> Round -> Round
addCardToRound card 0 [] = [[card]]
addCardToRound card 0 (xs : xss) = (card : xs) : xss
addCardToRound card player (xs : xss) = xs : (addCardToRound card (player - 1) xss)

playRound :: Int -> Int -> [Card] -> Round -> Hand -> [Result] -> Phase -> IO ()
playRound player numberOfPlayers shoe round dealerHand results phase = do
  case phase of
    Begin -> do
      deck <- shuffle allCards
      let updatedShoe = concat $ take 2 $ repeat deck 
      --putStrLn "Here is the shoe:\n"
      --showHand updatedShoe
      --putStrLn ""
      playRound player numberOfPlayers updatedShoe round dealerHand results Deal1stCard
    Deal1stCard -> do
      --putStrLn $ "Deal first card to Player " ++ (show $ player + 1) ++ ":\n"
      let card = head shoe
      let updatedShoe = tail shoe
      let updatedRound = addCardToRound card player round
      --showRound updatedRound
      --putStrLn ""
      let updatedPlayer = player + 1
      case updatedPlayer < numberOfPlayers of
        True -> playRound updatedPlayer numberOfPlayers updatedShoe updatedRound dealerHand results Deal1stCard
        False -> playRound updatedPlayer numberOfPlayers updatedShoe updatedRound dealerHand results Deal1stCardToDealer
    Deal1stCardToDealer -> do
      --putStrLn $ "Deal first card to dealer\n"
      let card = head shoe
      let updatedShoe = tail shoe
      let updatedDealerHand = (card : dealerHand)
      --(showHand . reverse) updatedDealerHand
      --putStrLn ""
      playRound 0 numberOfPlayers updatedShoe round updatedDealerHand results Deal2ndCard
    Deal2ndCard -> do
      --putStrLn $ "Deal second card to Player " ++ (show $ player + 1) ++ ":\n"
      let card = head shoe
      let updatedShoe = tail shoe
      let updatedRound = addCardToRound card player round
      let sumOfHand = getSumOfHandForPlayer player updatedRound
      let result = determineResult sumOfHand False
      let updatedResults = setResult result player results
      --showRound updatedRound
      --putStrLn ""
      showResult player result
      let updatedPlayer = player + 1
      case updatedPlayer < numberOfPlayers of
        True -> playRound updatedPlayer numberOfPlayers updatedShoe updatedRound dealerHand updatedResults Deal2ndCard 
        False -> playRound updatedPlayer numberOfPlayers updatedShoe updatedRound dealerHand updatedResults Deal2ndCardToDealer
    Deal2ndCardToDealer -> do
      --putStrLn $ "Deal second card to dealer" ++ ":\n"
      let card = head shoe
      let updatedShoe = tail shoe
      let updatedDealerHand = (card : dealerHand)
      --(showDealerHalfHiddenHand . reverse) updatedDealerHand
      --putStrLn ""
      showRoundAndHand round updatedDealerHand False False
      putStrLn ""
      playRound 0 numberOfPlayers updatedShoe round updatedDealerHand results PlayersHit
    PlayersHit -> do
      case (results !! player) of
        Pending -> do
          putStrLn $ "Player " ++ (show $ player + 1) ++ ", would you like to hit? (0 to stand; any other number to hit)"
          move <- getLine
          let hit = read move
          case hit of
            0 -> do
              let updatedResults = setResult Standing player results
              playRound player numberOfPlayers shoe round dealerHand updatedResults PlayersHit
            _ -> do
              let card = head shoe
              let updatedShoe = tail shoe
              let updatedRound = addCardToRound card player round
              let sumOfHand = getSumOfHandForPlayer player updatedRound
              let result = determineResult sumOfHand True
              let updatedResults = setResult result player results
              putStrLn ""
              showRoundAndHand updatedRound dealerHand False False
              putStrLn ""
              showResult player result
              playRound player numberOfPlayers updatedShoe updatedRound dealerHand updatedResults PlayersHit
        _ ->  do
          let updatedPlayer = player + 1
          case updatedPlayer < numberOfPlayers of
            True -> playRound updatedPlayer numberOfPlayers shoe round dealerHand results PlayersHit
            False -> do
              putStrLn $ "\nDealer (revealing hidden card) has:\n"
              (showHand . reverse) dealerHand
              playRound 0 numberOfPlayers shoe round dealerHand results DealerHits
    DealerHits -> do
      let dealerSum = getSumOfHand dealerHand
      case dealerSum <= 16 of
        True -> do
          showDealerResult dealerSum
          let card = head shoe
          let updatedShoe = tail shoe
          let updatedDealerHand = (card : dealerHand)
          let updatedDealerSum = getSumOfHand updatedDealerHand
          showRoundAndHand round updatedDealerHand True False
          playRound 0 numberOfPlayers updatedShoe round updatedDealerHand results DealerHits
        False -> do
          showDealerResult dealerSum
          showRoundAndHand round dealerHand True True
          playRound 0 numberOfPlayers shoe round dealerHand results FinalResults
    FinalResults -> do
      let result = determineFinalResult (getSumOfHandForPlayer player round) (results !! player) (getSumOfHand dealerHand)
      let updatedResults = setResult result player results
      let updatedPlayer = player + 1
      case updatedPlayer < numberOfPlayers of
        True -> playRound updatedPlayer numberOfPlayers shoe round dealerHand updatedResults FinalResults
        False -> do
          putStrLn $ "\nDealer settles with players:"
          putStrLn "\n******************************"
          putStrLn "Player payoffs:"
          putStrLn "******************************\n"
          playRound 0 numberOfPlayers shoe round dealerHand updatedResults Settle
    Settle -> do
      showFinalResult player (results !! player)
      let updatedPlayer = player + 1
      case updatedPlayer < numberOfPlayers of
        True -> playRound updatedPlayer numberOfPlayers shoe round dealerHand results Settle 
        False -> do
          putStrLn "\n******************************\n"
          putStrLn "End of round\n"

showCard :: Card -> String
showCard card = let rank = (getRank card) in
  case rank == Jack || rank == Queen || rank == King || rank == Ace of
    True -> [head (show $ rank)] -- ++ " " ++ [head (show $ getSuit card)]
    False -> (show $ getValue card) -- ++ " " ++ [head (show $ getSuit card)]

showHand :: Hand -> IO ()
showHand [x]       = do
  putStr $ showCard x
  putStr "\n"
showHand (x : xs)  = do
  putStr $ showCard x
  putStr ", "
  showHand xs

showDealerHalfHiddenHand :: Hand -> IO ()
showDealerHalfHiddenHand [x]       = do
  putStr "(Hidden)\n"
showDealerHalfHiddenHand (x : xs)  = do
  putStr $ showCard x
  putStr ", "
  showDealerHalfHiddenHand xs

showRound :: Round -> IO ()
showRound = sequence_ . (map (showHand . reverse))

showRoundAndHand :: Round -> Hand -> Bool -> Bool -> IO ()
showRoundAndHand round hand roundFinalized handFinalized = do
  putStrLn "******************************"
  putStr "Players' hands"
  case roundFinalized of
    True -> putStrLn " (finalized):"
    False -> putStrLn ":"
  putStrLn "******************************\n"
  showRound round
  putStrLn "\n******************************"
  putStr "Dealer's hand"
  case handFinalized of
    True -> putStrLn " (finalized):"
    False -> putStrLn ":"
  putStrLn "******************************\n"
  case roundFinalized of
    True -> (showHand . reverse) hand
    False -> (showDealerHalfHiddenHand . reverse) hand
  putStrLn "\n******************************"

showResult :: Int -> Result -> IO ()
showResult player result = case result of
  Blackjack -> do
    putStrLn $ "Player " ++ (show $ player + 1) ++ ", congratulations, you scored Blackjack (3:2 payoff)!\n"
    threadDelay 1000000
  Hit21 -> do
    putStrLn $ "Player " ++ (show $ player + 1) ++ ", congratulations, you hit 21!\n"
    threadDelay 1000000
  PlayerBust -> do
    putStrLn $ "Player " ++ (show $ player + 1) ++ ", sorry, you busted!\n"
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
showFinalResult player result = do
  putStr $ "Player " ++ (show $ player + 1)
  case result of
    Blackjack -> putStrLn " scored Blackjack and already won a 3:2 payoff (+150%)"
    PlayerBust -> putStrLn " busted and already lost bet amount (-100%)"
    DealerBust -> putStrLn " survived dealer bust and wins a 1:1 payoff (+100%)"
    LowerThanDealer -> putStrLn " scored lower than dealer and loses bet amount (-100%)"
    SameAsDealer -> putStrLn " ties dealer and reclaims bet amount (+0%)"
    HigherThanDealer -> putStrLn " scored higher than dealer and wins a 1:1 payoff (+100%)"
    _ -> putStrLn "Missing case!"

main = do
  putStrLn "Enter the number of players:"
  numberOfPlayers <- getLine
  let p = (read numberOfPlayers :: Int)
  putStrLn ""
  playRound 0 p ([]) ([]) ([]) (take p $ repeat Pending) Begin
