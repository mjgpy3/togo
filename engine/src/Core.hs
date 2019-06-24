{-# LANGUAGE NamedFieldPuns #-}

module Core
  ( summarize
  , track
  , emptyGame
  , isEndGame
  , gameOf
  , Position(..)
  , Stone(..)
  , Event(..)
  , GameSize(..)
  , GameState(..)
  , State
  , size
  , pieceAt
  , liberties
  , occupied
  , nextTurn
  , turn
  , withTurn
  ) where

import qualified Data.Map.Strict as M
import Data.Maybe (isJust)

data Position =
  Pos { x :: Int, y :: Int }
  deriving (Eq, Show, Ord)

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

size :: State -> Int
size state =
  case gameSize state of
    Standard -> 19

turn :: State -> Stone
turn (_, _, t, _) = t

withTurn :: Stone -> State -> State
withTurn stone (b, s, _, gs) = (b, s, stone, gs)

pieceAt :: Position -> State -> Maybe Stone
pieceAt point state = M.lookup point (board state)

data PositionClassification = Corner | NonCorner

liberties :: Stone -> Position -> State -> Int
liberties _ pos st = possibleLiberties
  where
    possibleLiberties :: Int
    possibleLiberties =
      case classify pos of
        Corner -> 2
        NonCorner -> 4

    classify :: Position -> PositionClassification
    classify Pos{x, y} =
      case (x == 0, y == 0, x == size st-1, y == size st-1) of
        (True, True, _, _) -> Corner
        (_, _, True, True) -> Corner
        (True, _, _, True) -> Corner
        (_, True, True, _) -> Corner
        _ -> NonCorner

occupied :: Position -> State -> Bool
occupied point state = isJust $ pieceAt point state

emptyGame :: State
emptyGame = (M.empty, Standard, Black, InProgress)

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x', y') = (f x', y')

gameOf :: [((Int, Int), Stone)] -> State
gameOf vs = (M.fromList (map (mapFst (uncurry Pos)) vs), Standard, Black, InProgress)

track :: Event -> State -> State
track TurnPassed (b, s, t, InProgress) = (b, s, nextTurn t, PassedInProgress)
track TurnPassed (b, s, t, PassedInProgress) = (b, s, nextTurn t, EndGame)
track (StonePlaced s p) (b, sze, t, _) = (placeStone p s b, sze, nextTurn t, InProgress)
track PlayerResigned (b, s, t, _) = (b, s, nextTurn t, EndGame)
track _ s@(_, _, _, EndGame) = s

nextTurn :: Stone -> Stone
nextTurn Black = White
nextTurn White = Black

placeStone :: Position -> Stone -> Board -> Board
placeStone = M.insert

summarize :: [Event] -> State
summarize = foldr track emptyGame
