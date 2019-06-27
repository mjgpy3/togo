{-# LANGUAGE TupleSections #-}

module Util (gameOf, passes, stonePlaced, atPlaces, endedGame, isOkay) where

import Core

gameOf = summarize . map (\((x, y), stone) -> StonePlaced stone (Pos x y))

stonePlaced stone = StonePlaced stone . uncurry Pos

atPlaces stone positions = gameOf $ map (, stone) positions

passes :: IO ()
passes = pure ()

endedGame :: State
endedGame = summarize [TurnPassed, TurnPassed]

isOkay :: Either a b -> Bool
isOkay (Right _) = True
isOkay _ = False

