{-# LANGUAGE InstanceSigs #-}

module Commons
  ( Zipper (..),
    focus,
    applyOnFocus,
    right,
    left,
    Commons.all,
    find,
    ToBoolean (..),
    Switch (..),
    current,
    switch,
  )
where

import Control.Applicative (Alternative ((<|>)))

data Zipper a = Zipper [a] a [a] deriving (Show)

focus :: Zipper a -> a
focus (Zipper _ x _) = x

applyOnFocus :: Zipper a -> (a -> a) -> Zipper a
applyOnFocus (Zipper lft x rgt) fx = Zipper lft (fx x) rgt

right, left :: Zipper a -> Zipper a
right z@(Zipper _ _ []) = z
right (Zipper lft f (x : rgt)) = Zipper (f : lft) x rgt
left z@(Zipper [] _ _) = z
left (Zipper (x : lft) f rgt) = Zipper lft x (f : rgt)

all :: (a -> Bool) -> Zipper a -> Bool
all predicate (Zipper lft f rgt) = Prelude.all predicate lft && predicate f && Prelude.all predicate rgt

find :: (a -> Bool) -> Zipper a -> Maybe (Zipper a)
find predicate zipper@(Zipper _ f _)
  | predicate f = Just zipper
  | otherwise = find predicate (left zipper) <|> find predicate (right zipper)

class (Functor m) => ToBoolean m where
  mapToBoolean :: m b -> (b -> Bool) -> Bool

instance ToBoolean (Either c) where
  mapToBoolean :: Either a b -> (b -> Bool) -> Bool
  mapToBoolean (Right value) predicate = predicate value
  mapToBoolean _ _ = False

instance ToBoolean Maybe where
  mapToBoolean :: Maybe b -> (b -> Bool) -> Bool
  mapToBoolean (Just value) predicate = predicate value
  mapToBoolean _ _ = False

data Switch a = Switch a a deriving (Show)

current :: Switch a -> a
current (Switch c _) = c

switch :: Switch a -> Switch a
switch (Switch c n) = Switch n c
