{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import Data.Map as Map
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Database.PostgreSQL.Simple (connectPostgreSQL)

seedDb :: (UserAlg f, Monad f) => f ()
seedDb = do
    initDb
    createUser "Tim" 25
    createUser "Hao" 24
    return ()

program :: (UserAlg f, Monad f, MonadIO f) => f ()
program = do
    seedDb
    user <- liftIO getLine >>= getUser
    liftIO $ print user
    deleteAllUsers
    return ()

testRunner :: IO ()
testRunner = evalStateT (runTestApp program) mempty

prodRunner :: IO ()
prodRunner = do
    conn <- connectPostgreSQL "host='localhost' port=5432 user='postgres' password='example'"
    runReaderT (runProdApp program) (Env conn)

main = prodRunner