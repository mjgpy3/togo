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
  , nextTurn
  , turn
  , withTurn
  ) where

import qualified Data.Map.Strict as M

type Position = (Int, Int)

data Stone = Black | White deriving (Eq, Show)

data GameSize = Standard deriving (Eq, Show)

data Event = StonePlaced Stone Position deriving (Eq, Show)
type Board = M.Map Position Stone
type State = (Board, GameSize, Stone)

widthAndHeight :: State -> Position
widthAndHeight (_, Standard, _) = (19, 19)

turn :: State -> Stone
turn (_, _, t) = t

withTurn :: Stone -> State -> State
withTurn event (b, s, t) = (b, s, nextTurn t)

pieceAt :: Position -> State -> Maybe Stone
pieceAt point (game, _, _) = M.lookup point game

emptyGame :: State
emptyGame = (M.empty, Standard, Black)

gameOf :: [(Position, Stone)] -> State
gameOf vs = (M.fromList vs, Standard, Black)

track :: Event -> State -> State
track event (b, s, t) = (track' event b, s, nextTurn t)

nextTurn :: Stone -> Stone
nextTurn Black = White
nextTurn White = Black

track' :: Event -> Board -> Board
track' (StonePlaced color pos) = M.insert pos color

summarize :: [Event] -> State
summarize = foldr track emptyGame
