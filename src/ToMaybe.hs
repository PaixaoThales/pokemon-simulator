{-# LANGUAGE InstanceSigs #-}

module ToMaybe (ToMaybe (..)) where

class (Monad m) => ToMaybe m where
  toMaybe :: m b -> Maybe b

instance ToMaybe (Either c) where
  toMaybe :: Either c b -> Maybe b
  toMaybe (Right value) = Just value
  toMaybe _ = Nothing
