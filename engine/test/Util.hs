{-# LANGUAGE TupleSections #-}

module Util (gameOf, passes, stonePlaced, atPlaces, endedGame, isOkay, emptyGame) where

import Core

gameOf :: [((Int, Int), Stone)] -> [Event]
gameOf = reverse . map (\((x', y'), stone) -> StonePlaced stone (Pos x' y'))

stonePlaced :: Stone -> (Int, Int) -> Event
stonePlaced stone = StonePlaced stone . uncurry Pos

atPlaces :: Stone -> [(Int, Int)] -> [Event]
atPlaces stone positions = gameOf $ map (, stone) positions

passes :: IO ()
passes = pure ()

endedGame :: [Event]
endedGame = [TurnPassed, TurnPassed]

isOkay :: Either a b -> Bool
isOkay (Right _) = True
isOkay _ = False

emptyGame :: [Event]
emptyGame = []
