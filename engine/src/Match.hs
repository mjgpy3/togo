module Match
  ( Match(..)
  , Guid(..)
  ) where

newtype Guid = Guid String

data Match = Match Guid
