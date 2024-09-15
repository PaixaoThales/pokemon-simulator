{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Domain.Commons.ToBoolean where

class (Functor m) => Boolean m where
  toBoolean :: (b -> Bool) -> m b -> Bool

instance Boolean (Either c) where
  toBoolean :: (b -> Bool) -> Either a b -> Bool
  toBoolean predicate (Right value) = predicate value
  toBoolean _ _ = False

instance Boolean Maybe where
  toBoolean :: (b -> Bool) -> Maybe b -> Bool
  toBoolean predicate (Just value) = predicate value
  toBoolean _ _ = False
