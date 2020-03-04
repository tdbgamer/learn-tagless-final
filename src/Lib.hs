{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( UserAlg(..)
    , User
    , TestApp(..)
    , ProdApp(..)
    , Env(..)
    ) where

import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe (listToMaybe)
import Data.Int (Int64)
import Data.IORef
import Control.Monad.State.Lazy
import Control.Monad.Reader (ReaderT, MonadReader, asks)
import GHC.Generics (Generic)
import GHC.IO.Unsafe (unsafePerformIO)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField

newtype Env = Env { connection :: Connection }

newtype ProdApp f = ProdApp { runProdApp :: ReaderT Env IO f }
        deriving (Monad, Functor, Applicative, MonadIO, MonadReader Env)

newtype TestApp f = TestApp { runTestApp :: StateT (Map Int User) IO f }
        deriving (Monad, Functor, Applicative, MonadState (Map Int User), MonadIO)

data User = User { userId :: Int, userName :: String, userAge :: Int }
        deriving (Generic, Show, Eq)

instance FromRow User where
    fromRow = User <$> field <*> field <*> field

instance ToRow User where
    toRow (User id name age) = [toField id, toField name, toField age]

class UserAlg f where
    initDb :: f ()
    getUser :: String -> f (Maybe User)
    createUser :: String -> Int -> f User
    deleteAllUsers :: f ()

{-# NOINLINE currentId #-}
currentId :: IORef Int
currentId = unsafePerformIO $ newIORef 0

instance UserAlg TestApp where
    initDb = return ()
    getUser name = listToMaybe . filter ((== name) . userName) . M.elems <$> get
    createUser name age = do
        id <- liftIO (readIORef currentId)
        let user = User id name age
        users <- get
        put $ M.insert id user users
        liftIO (writeIORef currentId (id + 1))
        return user
    deleteAllUsers = void $ put mempty

instance UserAlg ProdApp where
    initDb = do
        conn <- asks connection
        liftIO (void $ execute_ conn "create table if not exists users (id serial, name text, age integer)")

    getUser name = do
        conn <- asks connection
        user <- liftIO (query conn "select * from users where name = ?" (Only name) :: IO [User])
        return $ listToMaybe user

    createUser name age = do
        conn <- asks connection
        id <- liftIO (execute conn "insert into users (name, age) values (?, ?)" (name, age) :: IO Int64)
        let user = User (fromIntegral id) name age
        return user
    
    deleteAllUsers = do
        conn <- asks connection
        id <- liftIO (execute_ conn "delete from users")
        return ()