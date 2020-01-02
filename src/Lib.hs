{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Lib
    ( UserAlg(..)
    , User
    , TestApp(..)
    ) where

import Data.Map as Map
import Control.Monad.State.Lazy

newtype TestApp f = TestApp { runTestApp :: StateT (Map String User) IO f }
        deriving (Monad, Functor, Applicative, MonadState (Map String User), MonadIO)

data User = User { userId :: Int, userName :: String, userAge :: Int }
        deriving (Show, Eq)

class UserAlg f where
    getUser :: String -> f (Maybe User)
    createUser :: Int -> String -> Int -> f User

instance UserAlg TestApp where
    getUser name = Map.lookup name <$> get
    createUser id name age = do
        let user = User id name age
        users <- get
        put $ Map.insert name user users
        return user