{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Match
  ( Match(..)
  ) where

import GHC.Generics
import Data.Aeson

newtype Match = Match { identifier :: String }
  deriving (Generic, ToJSON)
