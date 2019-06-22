module Serialization
  ( 
  ) where

import Core (State(..))
import Data.Aeson

instance ToJSON State where
 toJSON (Game (board, size, stone, state)) =
    object [

           {- "firstName"  .= firstName
           , "lastName"   .= lastName
           , "age"        .= age
           , "likesPizza" .= likesPizza-}
             ]
