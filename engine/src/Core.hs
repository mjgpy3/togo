{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Core
  ( summarize
  , track
  , isEndGame
  , Position(..)
  , Stone(..)
  , Event(..)
  , GameSize(..)
  , GameState(..)
  , State
  , size
  , board
  , stoneAt
  , stonesCapturedBy
  , collectCaptures
  , liberties
  , hasLiberties
  , occupied
  , nextTurn
  , placeStone
  , turn
  , withTurn
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (isJust, isNothing)
import GHC.Generics
import Data.Aeson

data Position =
  Pos { x :: Int, y :: Int }
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

data Stone = Black | White deriving (Eq, Show, Generic, ToJSON, FromJSON)

data GameSize = Standard deriving (Eq, Show)

instance ToJSON GameSize where
  toJSON sz = object [ "width" .= dimension sz,
                       "height" .= dimension sz
                     ]

data Event
  = StonePlaced Stone Position
  | TurnPassed
  | PlayerResigned
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data GameState
  = InProgress
  | PassedInProgress
  | EndGame
  deriving (Eq, Show, Generic, ToJSON)

type Board = M.Map Position Stone
data State =
  Game { board :: Board
       , gameSize :: GameSize
       , turn :: Stone
       , gameState :: GameState
       , blackCaptures :: Int
       , whiteCaptures :: Int
       }
  deriving (Eq, Show)

instance ToJSON State where
  toJSON Game{board, gameSize, turn, gameState, blackCaptures, whiteCaptures} =
    object [ "gameSize" .= gameSize
           , "turn" .= turn
           , "gameState" .= gameState
           , "white" .= object [ "captures" .= whiteCaptures
                               , "positions" .= M.keys (M.filter (== White) board) ]

           , "black" .= object [ "captures" .= blackCaptures
                               , "positions" .= M.keys (M.filter (== Black) board) ]
           ]

isEndGame :: State -> Bool
isEndGame = (== EndGame) . gameState

size :: State -> Int
size = dimension . gameSize

dimension :: GameSize -> Int
dimension Standard = 19

withTurn :: Stone -> State -> State
withTurn stone state = state { turn=stone }

stoneAt :: Position -> State -> Maybe Stone
stoneAt point state = M.lookup point (board state)

stonesCapturedBy :: Stone -> State -> Int
stonesCapturedBy Black Game{blackCaptures} = blackCaptures
stonesCapturedBy White Game{whiteCaptures} = whiteCaptures

addCapturedStone :: Stone -> State -> State
addCapturedStone Black g@Game{whiteCaptures, turn=White} = g { whiteCaptures=whiteCaptures + 1 }
addCapturedStone White g@Game{blackCaptures, turn=Black} = g { blackCaptures=blackCaptures + 1 }
addCapturedStone _ g = g

collectCaptures :: State -> State
collectCaptures g@Game{board, turn} = foldr addCapturedStone (g { board=withoutCaptures }) captures
  where
    captures :: [Stone]
    captures = M.elems $ board M.\\ withoutCaptures

    withoutCaptures :: Board
    withoutCaptures = M.filterWithKey (\pos st -> hasLiberties g pos st || st == turn) board

hasLiberties :: State -> Position -> Stone -> Bool
hasLiberties state p s = not $ S.null $ liberties s p state

liberties :: Stone -> Position -> State -> S.Set Position
liberties stone p = go (S.singleton p) [] p
  where
    go :: S.Set Position -> [Position] -> Position -> State -> S.Set Position
    go visited toVisit pos@Pos{x, y} state = S.union unoccupiedNeighbors alliedNeighborsLiberties
      where
        alliedNeighborsLiberties :: S.Set Position
        alliedNeighborsLiberties =
          case nextToVisit of
            [] -> S.empty
            (nextPos:remainingToVist) -> go nextVisited remainingToVist nextPos (placeStone pos stone state)

        nextToVisit :: [Position]
        nextToVisit = filter (not . (`S.member` nextVisited)) $ (++) toVisit $ map fst $ filter ((==) stone . snd) occupiedNeighbors

        nextVisited :: S.Set Position
        nextVisited = S.insert pos visited

        unoccupiedNeighbors :: S.Set Position
        unoccupiedNeighbors =
          S.fromList $ filter (isNothing . (`stoneAt` state)) neighboringPositions

        occupiedNeighbors :: [(Position, Stone)]
        occupiedNeighbors = do
          neighbor <- neighboringPositions
          case stoneAt neighbor state of
            Just st -> [(neighbor, st)]
            _ -> []

        neighboringPositions :: [Position]
        neighboringPositions = filter withinBoard $ uncurry Pos <$> [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

        withinBoard :: Position -> Bool
        withinBoard Pos{x=x', y=y'} = withinBounds x' && withinBounds y'

        withinBounds :: Int -> Bool
        withinBounds dim = 0 <= dim && dim < size state

occupied :: Position -> State -> Bool
occupied point state = isJust $ stoneAt point state

emptyGame :: State
emptyGame = Game { board=M.empty
                 , gameSize=Standard
                 , turn=Black
                 , gameState=InProgress
                 , blackCaptures=0
                 , whiteCaptures=0
                 }

track :: Event -> State -> State
track TurnPassed g@Game{gameState=InProgress} = nextTurn $ g { gameState=PassedInProgress }
track TurnPassed g@Game{gameState=PassedInProgress} = nextTurn $ g { gameState=EndGame }
track (StonePlaced s p) g = nextTurn $ collectCaptures $ placeStone p s $ g { gameState=InProgress }
track PlayerResigned g = nextTurn $ g { gameState=EndGame }
track _ g@Game{gameState=EndGame} = g

nextTurn :: State -> State
nextTurn g@Game{turn=Black} = g { turn=White }
nextTurn g@Game{turn=White} = g { turn=Black }

placeStone :: Position -> Stone -> State -> State
placeStone pos st g@Game{board} = g { board=M.insert pos st board }

summarize :: [Event] -> State
summarize = foldr track emptyGame
