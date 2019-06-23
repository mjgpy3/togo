module Render
  ( render
  , renderWithColRow
  , blackPiece
  , whitePiece
  ) where

import Core (widthAndHeight, State, Stone(..), pieceAt, Position(..))
import Data.List (intersperse)

renderWithColRow :: State -> String
renderWithColRow state =
  let
    width = fst $ widthAndHeight state
    colIndicators =
      take (3 + 2*width) "   0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1\n" ++
      take (3 + 2*width) "   1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9\n"
    rawLines = do
      y' <- [0..snd (widthAndHeight state)-1]
      (if y' >= 9 then "1" else "0") ++ show ((y' + 1) `mod` 10) ++ " " ++ padRow state y'
  in
    colIndicators ++ rawLines

render :: State -> String
render state = [0..snd (widthAndHeight state)-1] >>= padRow state

padRow :: State -> Int -> String
padRow state y' = intersperse '-' rawRow ++ "\n"
  where
    rawRow = do
      x' <- [0..fst (widthAndHeight state)-1]
      case pieceAt (Pos {x=x', y=y'}) state of
         Just White -> [whitePiece]
         Just Black -> [blackPiece]
         _ -> "+"

blackPiece :: Char
blackPiece = '○'

whitePiece :: Char
whitePiece = '⏺'
