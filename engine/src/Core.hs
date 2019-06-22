module Core
  ( summarize
  , track
  , emptyGame
  , isEndGame
  , gameOf
  , Position(..)
  , baseOnePosition
  , Stone(..)
  , Event(..)
  , GameSize(..)
  , GameState(..)
  , State(..)
  , Board(..)
  , widthAndHeight
  , pieceAt
  , nextTurn
  , turn
  , withTurn
  , whiteStoneLocations
  , blackStoneLocations
  ) where

import qualified Data.Map.Strict as M

data Position =
  Pos { x :: Int, y :: Int }
  deriving (Show, Eq, Ord)

baseOnePosition :: Int -> Int -> Position
baseOnePosition x' y' = Pos { x=x'-1, y=y'-1 }

baseZeroPosition :: Int -> Int -> Position
baseZeroPosition x' y' = Pos { x=x', y=y' }

data Stone = Black | White deriving (Eq, Show)

data GameSize = Standard deriving (Eq, Show)

data Event
  = StonePlaced Stone Position
  | TurnPassed
  deriving (Eq, Show)

data GameState
  = InProgress
  | PassedInProgress
  | EndGame
  deriving (Eq, Show)

type Board = M.Map Position Stone
newtype State =
  Game (Board, GameSize, Stone, GameState)
  deriving (Eq, Show)


locationsOfStones :: Stone -> State -> [Position]
locationsOfStones s (Game (board, _, _, _)) = map fst $ filter ((== s) . snd) $ M.toList board

whiteStoneLocations :: State -> [Position]
whiteStoneLocations = locationsOfStones White

blackStoneLocations :: State -> [Position]
blackStoneLocations = locationsOfStones Black

isEndGame :: State -> Bool
isEndGame (Game (_, _, _, EndGame)) = True
isEndGame _ = False

gameSize :: State -> GameSize
gameSize (Game (_, s, _, _)) = s

board :: State -> Board
board (Game (b, _, _, _)) = b

widthAndHeight :: State -> (Int, Int)
widthAndHeight state =
  case gameSize state of
    Standard -> (19, 19)

turn :: State -> Stone
turn (Game (_, _, t, _)) = t

withTurn :: Stone -> State -> State
withTurn stone (Game (b, s, _, gs)) = Game (b, s, stone, gs)

pieceAt :: Position -> State -> Maybe Stone
pieceAt point state = M.lookup point (board state)

emptyGame :: State
emptyGame = Game (M.empty, Standard, Black, InProgress)

mapFst f (x, y) = (f x, y)

gameOf :: [((Int, Int), Stone)] -> State
gameOf vs = Game (M.fromList $ map (mapFst (uncurry baseZeroPosition)) vs, Standard, Black, InProgress)

track :: Event -> State -> State
track TurnPassed (Game (b, s, t, InProgress)) = Game (b, s, nextTurn t, PassedInProgress)
track TurnPassed (Game (b, s, t, PassedInProgress)) = Game (b, s, nextTurn t, EndGame)
track (StonePlaced s p) (Game (b, size, t, _)) = Game (placeStone p s b, size, nextTurn t, InProgress)
track _ s@(Game (_, _, _, EndGame)) = s

nextTurn :: Stone -> Stone
nextTurn Black = White
nextTurn White = Black

placeStone :: Position -> Stone -> Board -> Board
placeStone = M.insert

summarize :: [Event] -> State
summarize = foldr track emptyGame
