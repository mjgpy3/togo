{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}

module Serialization
  (
  ) where

import Core (State(..), Position(..), widthAndHeight, isEndGame, Stone(..), whiteStoneLocations, blackStoneLocations)
import Data.Aeson

instance ToJSON Position where
  toJSON (Pos {x, y}) =
    object [ "x" .= x
           , "y" .= y
           ]

stoneName :: Stone -> String
stoneName Black = "Black"
stoneName White = "White"

instance ToJSON State where
 toJSON s@(Game (board, _, stone, _)) =
    let
      (width, height) = widthAndHeight s
    in
      object [ "width" .= width
             , "height" .= height
             , "isEndGame" .= isEndGame s
             , "turn" .= stoneName stone
             , "whiteStoneLocations" .= whiteStoneLocations s
             , "blackStoneLocations" .= blackStoneLocations s
             ]
