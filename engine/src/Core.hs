module Core
  ( summarize
  , track
  , emptyGame
  , isEndGame
  , gameOf
  , Position
  , Stone(..)
  , Event(..)
  , GameSize(..)
  , GameState(..)
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

data Event
  = StonePlaced Stone Position
  | TurnPassed
  | PlayerResigned
  deriving (Eq, Show)

data GameState
  = InProgress
  | PassedInProgress
  | EndGame
  deriving (Eq, Show)

type Board = M.Map Position Stone
type State = (Board, GameSize, Stone, GameState)

isEndGame :: State -> Bool
isEndGame (_, _, _, EndGame) = True
isEndGame _ = False

gameSize :: State -> GameSize
gameSize (_, s, _, _) = s

board :: State -> Board
board (b, _, _, _) = b

widthAndHeight :: State -> Position
widthAndHeight state =
  case gameSize state of
    Standard -> (19, 19)

turn :: State -> Stone
turn (_, _, t, _) = t

withTurn :: Stone -> State -> State
withTurn stone (b, s, _, gs) = (b, s, stone, gs)

pieceAt :: Position -> State -> Maybe Stone
pieceAt point state = M.lookup point (board state)

emptyGame :: State
emptyGame = (M.empty, Standard, Black, InProgress)

gameOf :: [(Position, Stone)] -> State
gameOf vs = (M.fromList vs, Standard, Black, InProgress)

track :: Event -> State -> State
track TurnPassed (b, s, t, InProgress) = (b, s, nextTurn t, PassedInProgress)
track TurnPassed (b, s, t, PassedInProgress) = (b, s, nextTurn t, EndGame)
track (StonePlaced s p) (b, size, t, _) = (placeStone p s b, size, nextTurn t, InProgress)
track PlayerResigned (b, s, t, _) = (b, s, nextTurn t, EndGame)
track _ s@(_, _, _, EndGame) = s

nextTurn :: Stone -> Stone
nextTurn Black = White
nextTurn White = Black

placeStone :: Position -> Stone -> Board -> Board
placeStone = M.insert

summarize :: [Event] -> State
summarize = foldr track emptyGame
