module Main where

import Lib
import Data.Map as Map
import Control.Monad.State.Lazy

seedDb :: (UserAlg f, Monad f) => f ()
seedDb = do
    createUser 0 "Tim" 25
    createUser 0 "Hao" 24
    return ()

program :: (UserAlg f, Monad f, MonadIO f) => f ()
program = do
    seedDb
    user <- liftIO getLine >>= getUser
    liftIO $ print user
    return ()

main :: IO ()
main = evalStateT (runTestApp program) mempty