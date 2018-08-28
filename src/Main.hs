{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import           System.Exit
import           System.Environment
import           Control.Concurrent.Extra
import           Control.Monad.Reader
import           Data.Conduit

main :: IO ()
main = join . onceFork $ do
  args <- getArgs
  case args of
    ["plain"] -> mainPlain
    ["conduit"] -> mainConduit
    _ -> die "usage: reader (plain|conduit)"

mainPlain :: IO ()
mainPlain = runReaderT (action 0 :: ReaderT () IO ()) ()

mainConduit :: IO ()
mainConduit = runReaderT (runConduit (action 0 :: ConduitM () Void (ReaderT () IO) () )) ()

action :: MonadReader () m => Int -> m ()
action n
  | n > 2000000 = ask
  | otherwise = local id (action $ succ n)
