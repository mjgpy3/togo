module Match
  ( identifiedBy
  , identifier
  , Match
  ) where

newtype GameIdentifier = GameIdentifier String

newtype Match = Match GameIdentifier

identifiedBy :: String -> Match
identifiedBy = Match . GameIdentifier

identifier :: Match -> String
identifier (Match (GameIdentifier ident)) = ident
