{-# LANGUAGE InstanceSigs #-}

module ToBoolean (ToBoolean (..)) where

class (Functor m) => ToBoolean m where
  toBoolean :: (b -> Bool) -> m b -> Bool

instance ToBoolean (Either c) where
  toBoolean :: (b -> Bool) -> Either a b -> Bool
  toBoolean predicate (Right value) = predicate value
  toBoolean _ _ = False

instance ToBoolean Maybe where
  toBoolean :: (b -> Bool) -> Maybe b -> Bool
  toBoolean predicate (Just value) = predicate value
  toBoolean _ _ = False
