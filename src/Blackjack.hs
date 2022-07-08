{-# LANGUAGE FlexibleContexts #-}

module Main where

import System.Random
import Data.Array.IO
import Control.Monad.State

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

data Suit = Clubs | Diamonds | Hearts | Spades deriving (Ord, Enum, Eq, Read, Show, Bounded)
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Ord, Enum, Eq, Read, Show, Bounded)
data Card = Card Suit Rank deriving Show
type Hand = [Card]
type Round = [[Card]]
data Phase = Begin | Deal1stCard | Deal2ndCard | PlayersHit | DealerHits | Settle deriving (Enum, Read, Show)

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

getSumOfHand :: [Card] -> Int
getSumOfHand xs = getNewSumOfHand (countAcesInHand xs) (getHighSumOfHand xs)

getNewSumOfHand :: Int -> Int -> Int
getNewSumOfHand highAcesInHand highSumOfHand = case (highAcesInHand == 0) of
  True -> highSumOfHand
  False -> case highSumOfHand > 21 of
    True -> getNewSumOfHand (highAcesInHand - 1) (highSumOfHand - 10)

countAcesInHand :: [Card] -> Int
countAcesInHand [] = 0
countAcesInHand (x : xs) = case (getRank x) == Ace of
  True -> 1 + countAcesInHand xs
  False -> 0 + countAcesInHand xs

getHighSumOfHand :: [Card] -> Int
getHighSumOfHand xs = foldr (+) 0 (map getValue xs)

getSumOfHandForPlayer :: Int -> [[Card]] -> Int
getSumOfHandForPlayer 0 (xs : xss) = getSumOfHand xs
getSumOfHandForPlayer player (xs : xss) = getSumOfHandForPlayer (player - 1) xss

addCardToRound :: Card -> Int -> [[Card]] -> [[Card]]
addCardToRound card 0 [] = [[card]]
addCardToRound card 0 (xs : xss) = (card : xs) : xss
addCardToRound card player (xs : xss) = xs : (addCardToRound card (player - 1) xss)

playRound :: Int -> Int -> [Card] -> [[Card]] -> [Card] -> [Bool] -> Phase -> IO ()
playRound player numberOfPlayers shoe round dealerHand results phase = do
  case phase of
    Begin -> do
      shoe <- shuffle allCards
      putStrLn "Here is the shoe:\n"
      showHand shoe
      putStrLn ""
      playRound player numberOfPlayers shoe round dealerHand results Deal1stCard
    Deal1stCard -> do
      putStrLn $ "Deal first card to Player " ++ (show $ player + 1) ++ ":\n"
      let card = head shoe
      let updatedShoe = tail shoe
      let updatedRound = addCardToRound card player round
      showRound updatedRound
      putStrLn ""
      let updatedPlayer = player + 1
      case updatedPlayer < numberOfPlayers of
        True -> playRound updatedPlayer numberOfPlayers updatedShoe updatedRound dealerHand results Deal1stCard
        False -> do
          putStrLn $ "Deal first card to dealer\n"
          let card = head updatedShoe
          let updatedShoe2 = tail updatedShoe
          let updatedDealerHand = (card : dealerHand)
          (showHand . reverse) updatedDealerHand
          putStrLn ""
          playRound 0 numberOfPlayers updatedShoe2 updatedRound updatedDealerHand results Deal2ndCard
    Deal2ndCard -> do
      putStrLn $ "Deal second card to Player " ++ (show $ player + 1) ++ ":\n"
      let card = head shoe
      let updatedShoe = tail shoe
      let updatedRound = addCardToRound card player round
      showRound updatedRound
      putStrLn ""
      let updatedPlayer = player + 1
      case updatedPlayer < numberOfPlayers of
        True -> playRound updatedPlayer numberOfPlayers updatedShoe updatedRound dealerHand results Deal2ndCard 
        False -> do
          putStrLn $ "Deal second card to dealer" ++ ":\n"
          let card = head updatedShoe
          let updatedShoe2 = tail updatedShoe
          let updatedDealerHand = (card : dealerHand)
          (showDealerHalfHiddenHand . reverse) updatedDealerHand
          putStrLn ""
          putStrLn "******************************"
          putStrLn "Players' hands:"
          putStrLn "******************************\n"
          showRound updatedRound
          putStrLn "\n******************************"
          putStrLn "Dealer's hand:"
          putStrLn "******************************\n"
          (showDealerHalfHiddenHand . reverse) updatedDealerHand
          putStrLn ""
          playRound 0 numberOfPlayers updatedShoe2 updatedRound updatedDealerHand results PlayersHit
    PlayersHit -> do
      putStrLn $ "Player " ++ (show $ player + 1) ++ ", would you like to hit? (0 to hit; any other number to stand)"
      move <- getLine
      let hit = read move
      case hit of
        0 -> do
          putStrLn $ "\nPlayer " ++ (show $ player + 1) ++ " hits:\n"
          let card = head shoe
          let updatedShoe = tail shoe
          let updatedRound = addCardToRound card player round
          let updatedPlayerHand = getSumOfHandForPlayer player updatedRound
          putStrLn "******************************"
          putStrLn "Players' hands:"
          putStrLn "******************************\n"
          showRound updatedRound
          putStrLn "\n******************************"
          putStrLn "Dealer's hand:"
          putStrLn "******************************\n"
          (showDealerHalfHiddenHand . reverse) dealerHand
          putStrLn ""
          case updatedPlayerHand <= 21 of
            True -> playRound player numberOfPlayers updatedShoe updatedRound dealerHand results PlayersHit
            False -> do
              putStrLn $ "\nPlayer " ++ (show $ player + 1) ++ " busts.\n"
              let updatedPlayer = player + 1
              case updatedPlayer < numberOfPlayers of
                True -> playRound updatedPlayer numberOfPlayers updatedShoe updatedRound dealerHand results PlayersHit
                False -> do
                  putStrLn $ "Dealer (revealing hidden card) has:\n"
                  (showHand . reverse) dealerHand
                  playRound 0 numberOfPlayers updatedShoe updatedRound dealerHand results DealerHits
        _ -> do
          putStrLn $ "\nPlayer " ++ (show $ player + 1) ++ " stands\n"
          let updatedPlayer = player + 1
          case updatedPlayer < numberOfPlayers of
            True -> playRound updatedPlayer numberOfPlayers shoe round dealerHand results PlayersHit
            False -> do
              putStrLn $ "Dealer (revealing hidden card) has:\n"
              (showHand . reverse) dealerHand
              playRound 0 numberOfPlayers shoe round dealerHand results DealerHits
    DealerHits -> do
      let dealerSum = getSumOfHand dealerHand
      case dealerSum <= 16 of
        True -> do
          putStrLn $ "\nDealer hits (16 or below):\n"
          let card = head shoe
          let updatedShoe = tail shoe
          let updatedDealerHand = (card : dealerHand)
          let updatedDealerSum = getSumOfHand updatedDealerHand
          putStrLn "******************************"
          putStrLn "Players' hands (finalized):"
          putStrLn "******************************\n"
          showRound round
          putStrLn "\n******************************"
          putStrLn "Dealer's hand:"
          putStrLn "******************************\n"
          (showHand . reverse) updatedDealerHand
          putStrLn ""
          case updatedDealerSum <= 21 of
            True -> playRound 0 numberOfPlayers updatedShoe round updatedDealerHand results DealerHits
            False -> do
              putStrLn $ "\nDealer busts:\n"
              putStrLn "******************************"
              putStrLn "Players' hands (finalized):"
              putStrLn "******************************\n"
              showRound round
              putStrLn "\n******************************"
              putStrLn "Dealer's hand (finalized):"
              putStrLn "******************************\n"
              (showHand . reverse) updatedDealerHand
              putStrLn ""
              playRound 0 numberOfPlayers updatedShoe round updatedDealerHand results Settle
        False -> do
          putStrLn $ "\nDealer stands (17 or above):\n"
          putStrLn "******************************"
          putStrLn "Players' hands (finalized):"
          putStrLn "******************************\n"
          showRound round
          putStrLn "\n******************************"
          putStrLn "Dealer's hand (finalized):"
          putStrLn "******************************\n"
          (showHand . reverse) dealerHand
          putStrLn ""
          playRound 0 numberOfPlayers shoe round dealerHand results Settle
    Settle -> do
      putStrLn $ "Dealer settles with Player " ++ (show $ player + 1) ++ ":\n"
      let updatedPlayer = player + 1
      case updatedPlayer < numberOfPlayers of
        True -> playRound updatedPlayer numberOfPlayers shoe round dealerHand results Settle 
        False -> do
          putStrLn "End of round\n"

showCard :: Card -> String
showCard card = let rank = (getRank card) in
  case rank == Jack || rank == Queen || rank == King || rank == Ace of
    True -> [head (show $ rank)] ++ " " ++ [head (show $ getSuit card)]
    False -> (show $ getValue card) ++ " " ++ [head (show $ getSuit card)]

showHand :: Hand -> IO ()
showHand [x]       = do
  putStr $ showCard x
  putStr "\n"
showHand (x : xs)  = do
  putStr $ showCard x
  putStr ", "
  showHand xs

showRound :: Round -> IO ()
showRound = sequence_ . (map (showHand . reverse))

showDealerHalfHiddenHand :: Hand -> IO ()
showDealerHalfHiddenHand [x]       = do
  putStr "(Hidden)\n"
showDealerHalfHiddenHand (x : xs)  = do
  putStr $ showCard x
  putStr ", "
  showDealerHalfHiddenHand xs

main = do
  putStrLn "Enter the number of players:"
  numberOfPlayers <- getLine
  let p = (read numberOfPlayers :: Int)
  putStrLn ""
  playRound 0 p ([]) ([]) ([]) ([]) Begin
