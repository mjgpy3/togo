module Effects.Tty
  ( Tty(..)
  , runTtyIo
  , readTty
  , writeTty
  , clearTty
  ) where

import Polysemy
import System.Console.ANSI (clearScreen)

data Tty m a where
  ReadTty :: Tty m String
  WriteTty :: String -> Tty m ()
  ClearTty :: Tty m ()
makeSem ''Tty

runTtyIo :: Member (Lift IO) r => Sem (Tty ': r) a -> Sem r a
runTtyIo = interpret $ \case
  ReadTty -> sendM getLine
  WriteTty text -> sendM $ putStrLn text
  ClearTty -> sendM clearScreen
