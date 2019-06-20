module Core
  ( summarize
  , track
  , emptyGame
  , gameOf
  , Position
  , Stone(..)
  , Event(..)
  , GameSize(..)
  , State
  , widthAndHeight
  , pieceAt
  ) where

import qualified Data.Map.Strict as M

type Position = (Int, Int)

data Stone = Black | White deriving (Eq, Show)

data GameSize = Standard deriving (Eq, Show)

data Event = StonePlaced Stone Position deriving (Eq, Show)
type Board = M.Map Position Stone
type State = (Board, GameSize)

widthAndHeight :: State -> Position
widthAndHeight (_, Standard) = (13, 13)

pieceAt :: Position -> State -> Maybe Stone
pieceAt point (game, _) = M.lookup point game

emptyGame :: State
emptyGame = (M.empty, Standard)

gameOf :: [(Position, Stone)] -> State
gameOf vs = (M.fromList vs, Standard)

track :: Event -> State -> State 
track event state = (track' event $ fst state, snd state)

track' :: Event -> Board -> Board
track' (StonePlaced color pos) board = M.insert pos color board

summarize :: [Event] -> State
summarize = standardSized . foldr track' M.empty
  where
    standardSized board = (board, Standard)
