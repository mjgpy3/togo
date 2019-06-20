module Core
  ( summarize
  , emptyGame
  , gameOf
  , Stone(..)
  , Event(..)
  , GameSize(..)
  , State
  , widthAndHeight
  , pieceAt
  ) where

import qualified Data.Map.Strict as M

data Stone = Black | White deriving (Eq, Show)

data GameSize = Standard deriving (Eq, Show)

data Event = StonePlaced Stone (Int, Int)
type State = (M.Map (Int, Int) Stone, GameSize)

widthAndHeight :: State -> (Int, Int)
widthAndHeight (_, Standard) = (13, 13)

pieceAt :: (Int, Int) -> State -> Maybe Stone
pieceAt point (game, _) = M.lookup point game

emptyGame :: State
emptyGame = (M.empty, Standard)

gameOf :: [((Int, Int), Stone)] -> State
gameOf vs = (M.fromList vs, Standard)

summarize :: [Event] -> State
summarize = standardSized . foldr track M.empty
  where
    track (StonePlaced color pos) board = M.insert pos color board
    standardSized board = (board, Standard)
