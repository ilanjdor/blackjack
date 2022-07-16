{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad.State
import Control.Concurrent

import Data
import Helpers

-- recursive function that progresses the game play through the round
playRound :: Int -> Int -> Shoe -> [Player] -> DealerHand -> Phase -> Bool -> MainOrSplitHand -> IO ()
playRound currPlayerNum numberOfPlayers shoe players dealerHand phase secondOrigCardDealt mainOrSplitHand =
  case phase of
    Bets -> do
      putStrLn $ "Player " ++ (show $ currPlayerNum + 1) ++ 
        ", place your bet. (Enter currency amount between 2 and 500)"
      move <- getLine
      putStrLn ""
      let betAmt = read move
      case (betAmt >= 2 && betAmt <= 500) of
        True -> do
          let updatedPlayers = addMainBetAmtToPlayer currPlayerNum players betAmt
          case updatedPlayerNum < numberOfPlayers of
            True -> do
              playRound updatedPlayerNum numberOfPlayers shoe updatedPlayers dealerHand
                phase secondOrigCardDealt mainOrSplitHand
            False -> do
              putStrLn "Round begins..."
              threadDelay 1000000
              playRound 0 numberOfPlayers shoe updatedPlayers dealerHand
                (DealOrigCardToPlayer :: Phase) secondOrigCardDealt mainOrSplitHand
        False -> do
          putStrLn $ "The amount you entered is invalid."
          playRound currPlayerNum numberOfPlayers shoe players dealerHand phase secondOrigCardDealt mainOrSplitHand
    DealOrigCardToPlayer -> do
      let (card, updatedShoe) = (head shoe, tail shoe)
      let updatedPlayers = addCardToPlayerHand card True currPlayerNum (Main :: MainOrSplitHand) players
      let updatedPlayerNum = currPlayerNum + 1
      playRound updatedPlayerNum numberOfPlayers updatedShoe updatedPlayers dealerHand 
        (if updatedPlayerNum < numberOfPlayers then (DealOrigCardToPlayer :: Phase) 
        else (DealOrigCardToDealer :: Phase)) secondOrigCardDealt mainOrSplitHand
    DealOrigCardToDealer -> do
      let (card, updatedShoe) = (head shoe, tail shoe)
      let updatedDealerHand = ((setShownStatus card (not secondOrigCardDealt)) : dealerHand)
      playRound 0 numberOfPlayers updatedShoe players updatedDealerHand 
        (if secondOrigCardDealt then (Insurance :: Phase)
        else (DealOrigCardToPlayer :: Phase)) True mainOrSplitHand
    Insurance -> do
      case (getRank $ head (tail dealerHand)) of
        Ace -> do
          let insuranceBetAmt = round (getMainBetAmtForPlayer / 2)
          putStrLn $ "Player " ++ (show $ currPlayerNum + 1) ++ 
            ", the dealer is showing an Ace. Would you like to buy insurance? " ++
            "(0 for no insurance; any other number to bet an additional $" ++
            (insuranceBetAmt) ++ " towards insurance)"
          move <- getLine
          putStrLn ""
          let updatedPlayers = setInsuranceBetAmtForPlayer currPlayerNum players (insuranceBetAmt $ read move == 0)
          if (getInsuranceBetAmtForPlayer currPlayerNum players > 0) then
            showPlayersAndDealerHand updatedPlayers dealerHand False False
          else
            putStr ""
          let updatedPlayerNum = currPlayerNum + 1
          case updatedPlayerNum < numberOfPlayers of
            True  ->
              playRound updatedPlayerNum numberOfPlayers shoe updatedPlayers dealerHand
                (Insurance :: Phase) True mainOrSplitHand
            False ->
              playRound 0 numberOfPlayers shoe updatedPlayers dealerHand
                (CheckIfDealerHasBlackjack :: Phase) True mainOrSplitHand
        _ ->
          playRound 0 numberOfPlayers shoe players dealerHand
                (NaturalsWithoutDealerBlackjack :: Phase) True mainOrSplitHand
    CheckIfDealerHasBlackjack -> do
      putStrLn $ "\nDealer surreptitiously checks for Blackjack...\n"
      threadDelay 1000000
      let dealerHasBlackjack = (getSumOfHand dealerHand) == 21
      if dealerHasBlackjack then
        putStrLn "\nDealer has Blackjack.\n" else putStrLn "\nDealer doesn't have Blackjack.\n"
      let updatedDealerHand = ((setShownStatus card dealerHasBlackjack): dealerHand)
      showPlayersAndDealerHand players updatedDealerHand dealerHasBlackjack dealerHasBlackjack
      playRound 0 numberOfPlayers shoe players dealerHand 
        (if dealerHasBlackjack then (NaturalsWithDealerBlackjack :: Phase) 
        else (NaturalsWithoutDealerBlackjack :: Phase)) True mainOrSplitHand
    NaturalsWithDealerBlackjack -> do
      let sumOfPlayerHand = getSumOfHandForPlayer currPlayerNum mainOrSplitHand players
      let handStatus = determineHandStatus sumOfPlayerHand 21 phase (Pending :: HandStatus)
      let updatedPlayers = setStatusForPlayerHand handStatus currPlayerNum mainOrSplitHand players
      putStr (show mainOrSplitHand) ++ " hand: "
      showFinalHandStatus currPlayerNum handStatus
      let updatedPlayerNum = currPlayerNum + 1
      case updatedPlayerNum < numberOfPlayers of
        True -> playRound updatedPlayerNum numberOfPlayers shoe 
          updatedPlayers dealerHand (NaturalsWithDealerBlackjack :: Phase) True mainOrSplitHand
        False -> do
          putStrLn ""
          return ()
    NaturalsWithoutDealerBlackjack -> do
      let sumOfPlayerHand = getSumOfHandForPlayer currPlayerNum players

      --Using 0 for sumOfDealerHand to denote that
      --it isn't used by determineHandStatus in the NaturalsWithoutDealerBlackjack phase
      let handStatus = determineHandStatus sumOfPlayerHand 0 phase (Pending :: HandStatus)
      let updatedPlayers = setStatusForPlayerHand handStatus currPlayerNum (Main :: MainOrSplitHand) players
      showPlayerHandStatus currPlayerNum handStatus
      let updatedPlayerNum = currPlayerNum + 1
      case updatedPlayerNum < numberOfPlayers of
        True -> playRound updatedPlayerNum numberOfPlayers shoe updatedPlayers 
          dealerHand (NaturalsWithoutDealerBlackjack :: Phase) True mainOrSplitHand
        False -> playRound 0 numberOfPlayers shoe updatedPlayers dealerHand (Splits :: Phase) True mainOrSplitHand
    Splits -> do
      if (canSplit currPlayerNum players) then do
        putStrLn $ "Player " ++ (show $ currPlayerNum + 1) ++
          ", would you like to split your hand? Splitting will cost you an additional $" ++
          (show getMainBetAmtForPlayer) ++ ". (Enter 0 to not split; any other number to split)"
        move <- getLine
        putStrLn ""
        let split = (read move == 0)
      else do
        let split = False
      if split then do
        putStr $ "Splitting hand..."
        threadDelay 1000000
        let updatedPlayers = splitHand currPlayerNum players
        let handStatus = getStatusForPlayerHand currPlayerNum mainOrSplitHand updatedPlayers
        if handStatus == (SplitAcesUndrawn :: HandStatus) then do
          putStr $ "Split Aces get exactly one more card, face up..."
          threadDelay 1000000
          let (card, updatedShoe) = (head shoe, tail shoe)
          let updatedPlayers2 = addCardToPlayerHand card True
                                  currPlayerNum (Main :: MainOrSplitHand) updatedPlayers
          let (nextCard, updatedShoe2) = (head updatedShoe, tail updatedShoe)
          let updatedPlayers3 = addCardToPlayerHand nextCard True
                                  currPlayerNum (Split :: MainOrSplitHand) updatedPlayers2
          let updatedPlayers4 = setStatusForPlayerHand (SplitAcesDrawn :: HandStatus) 
                                  currPlayerNum (Main :: MainOrSplitHand) updatedPlayers3
          let updatedPlayers5 = setStatusForPlayerHand (SplitAcesDrawn :: HandStatus)
                                   currPlayerNum (Split :: MainOrSplitHand) updatedPlayers4
          showPlayersAndDealerHand updatedPlayers5 dealerHand False False
        else
          showPlayersAndDealerHand updatedPlayers dealerHand False False
      else playRound currPlayerNum numberOfPlayers show players dealerHand
        (DoubleDown :: Phase) True mainOrSplitHand
    DoubleDown -> do
      if (canDoubleDownHand currPlayerNum mainOrSplitHand players) then do
        putStrLn $ "Player " ++ (show $ currPlayerNum + 1) ++
          ", would you like to double down on your " ++ (show mainOrSplitHand) ++
          " hand? Doubling down will cost you an additional $" ++
          (show getMainBetAmtForPlayer) ++ ". (Enter 0 to not double down; any other number to double down)"
        move <- getLine
        putStrLn ""
        let doubledDown = (read move == 0)
      else do
        let doubledDown = False
      if doubledDown then do
        let updatedPlayers = doubleDownHand currPlayerNum mainOrSplitHand players
        showPlayersAndDealerHand updatedPlayers dealerHand False False
        playRound currPlayerNum numberOfPlayers show updatedPlayers dealerHand (PlayerHits :: Phase) secondOrigCardDealt mainOrSplitHand
      else
        playRound currPlayerNum numberOfPlayers show updatedPlayers dealerHand (PlayerHits :: Phase) secondOrigCardDealt mainOrSplitHand
    PlayerHits -> do
      getStatusForPlayerHand
      case (getStatusForPlayerHand currPlayerNum players) of
        Pending -> do
          if secondOrigCardDealt mainOrSplitHand then
            let mainOrSplitHand = (Main :: MainOrSplitHand)
          else do
            let mainOrSplitHand = (Split :: MainOrSplitHand)
          putStrLn $ "Player " ++ (show $ currPlayerNum + 1) ++ 
            ", would you like to hit? (0 to stand; any other number to hit)"
          move <- getLine
          putStrLn ""
          let hit = read move
          if (read move == 0) then do
            let updatedPlayers = setResultForPlayer (Standing :: Result) currPlayerNum players
          else
            updatedPlayers = players
          case hit of
            0 -> do
              let updatedPlayers = setResultForPlayer (Standing :: Result) currPlayerNum players
              playRound currPlayerNum numberOfPlayers shoe updatedPlayers dealerHand (PlayerHits :: Phase) secondOrigCardDealt mainOrSplitHand
            _ -> do
              let (card, updatedShoe) = (head shoe, tail shoe)
              let updatedPlayers = addCardToPlayerHand card currPlayerNum players
              let sumOfPlayerHand = getSumOfHandForPlayer currPlayerNum updatedPlayers

              -- Using 0 for sumOfDealerHand to denote that it isn't used by determineResult in the PlayerHits phase
              let result = determineResult sumOfPlayerHand 0 phase (Pending :: Result)
              let updatedPlayers2 = setResultForPlayer result currPlayerNum updatedPlayers
              putStrLn ""
              showPlayersAndDealerHand updatedPlayers2 dealerHand False False
              putStrLn ""
              showPlayerHandStatus currPlayerNum result
              playRound currPlayerNum numberOfPlayers updatedShoe updatedPlayers2 dealerHand (PlayerHits :: Phase) True
        _ ->  do
          let updatedPlayerNum = currPlayerNum + 1
          case updatedPlayerNum < numberOfPlayers of
            True -> playRound updatedPlayerNum numberOfPlayers shoe players dealerHand (Splits :: Phase) True
            False -> do
              putStrLn $ "\nDealer (revealing hidden card) has:\n"
              (showHand . reverse) dealerHand
              threadDelay 1000000
              putStrLn ""
              playRound 0 numberOfPlayers shoe players dealerHand (DealerHits :: Phase) True
    DealerHits -> do
      let dealerSum = getSumOfHand dealerHand
      case dealerSum <= 16 of
        True -> do
          showDealerHandStatus dealerSum
          let (card, updatedShoe) = (head shoe, tail shoe)
          let updatedDealerHand = (card : dealerHand)
          let updatedDealerSum = getSumOfHand updatedDealerHand
          showPlayersAndDealerHand players updatedDealerHand True False
          playRound 0 numberOfPlayers updatedShoe players updatedDealerHand (DealerHits :: Phase) True
        False -> do
          showDealerHandStatus dealerSum
          showPlayersAndDealerHand players dealerHand True True
          playRound 0 numberOfPlayers shoe players dealerHand (FinalResults :: Phase) True
    FinalResults -> do
      let sumOfPlayerHand = getSumOfHandForPlayer currPlayerNum players
      let result = getResultForPlayer currPlayerNum players
      let sumOfDealerHand = getSumOfHand dealerHand
      let updatedResult = determineResult sumOfPlayerHand sumOfDealerHand phase result
      let updatedPlayers = setResultForPlayer updatedResult currPlayerNum players
      let updatedPlayerNum = currPlayerNum + 1
      case updatedPlayerNum < numberOfPlayers of
        True -> playRound updatedPlayerNum numberOfPlayers shoe updatedPlayers dealerHand (FinalResults :: Phase) True
        False -> do
          putStrLn $ "\nDealer settles with players:"
          putStrLn "\n******************************"
          putStrLn "Player payoffs:"
          putStrLn "******************************\n"
          playRound 0 numberOfPlayers shoe updatedPlayers dealerHand (Settle :: Phase) True
    Settle -> do
      showFinalHandStatus currPlayerNum (getResultForPlayer currPlayerNum players)
      let updatedPlayerNum = currPlayerNum + 1
      case updatedPlayerNum < numberOfPlayers of
        True -> playRound updatedPlayerNum numberOfPlayers shoe players dealerHand (Settle :: Phase) True
        False -> do
          putStrLn "\n******************************\n"
          putStrLn "End of round\n"

main = do
  putStrLn "Enter the number of players:"
  numberOfPlayers <- getLine
  let p = (read numberOfPlayers :: Int)
  putStrLn "Enter number of decks to include in shoe:"
  numberOfDecks <- getLine
  let d = (read numberOfDecks :: Int)
  deck <- shuffle allCards
  shoe <- shuffle (concat $ take d $ repeat deck)
  playRound 0 p shoe ([]) ([]) (Bets :: Phase) False (Main :: MainOrSplitHand)
