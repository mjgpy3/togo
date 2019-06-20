module Render
  ( render
  , blackPiece
  , whitePiece
  ) where

import Core (widthAndHeight, State, Stone(..), pieceAt)
import Data.List (intersperse)

render :: State -> String
render state = do
  y <- [0..snd (widthAndHeight state)-1]
  intersperse '-' (genRow y) ++ "\n"

  where
    genRow y = do
      x <- [0..fst (widthAndHeight state)-1]
      case pieceAt (x, y) state of
         Just White -> [whitePiece]
         Just Black -> [blackPiece]
         _ -> "+"

blackPiece = '○'
whitePiece = '⏺'
