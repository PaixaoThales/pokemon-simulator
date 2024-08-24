{-# LANGUAGE InstanceSigs #-}

module Zipper (Zipper (..), focus, apply, select, Zipper.all) where

import Control.Applicative (Alternative ((<|>)))

data Zipper a = Zipper [a] a [a]

focus :: Zipper a -> a
focus (Zipper _ x _) = x

apply :: Zipper a -> (a -> a) -> Zipper a
apply (Zipper lft x rgt) fx = Zipper lft (fx x) rgt

right, left :: Zipper a -> Zipper a
right z@(Zipper _ _ []) = z
right (Zipper lft f (x : rgt)) = Zipper (f : lft) x rgt
left z@(Zipper [] _ _) = z
left (Zipper (x : lft) f rgt) = Zipper lft x (f : rgt)

findInLeft :: (a -> Bool) -> Zipper a -> Maybe (Zipper a)
findInLeft predicate zipper@(Zipper [] f _) = if predicate f then Just zipper else Nothing
findInLeft predicate zipper@(Zipper _ f _) =
  if predicate f
    then Just zipper
    else findInLeft predicate (left zipper)

findInRight :: (a -> Bool) -> Zipper a -> Maybe (Zipper a)
findInRight predicate zipper@(Zipper _ f []) = if predicate f then Just zipper else Nothing
findInRight predicate zipper@(Zipper _ f _) =
  if predicate f
    then Just zipper
    else findInRight predicate (right zipper)

all :: (a -> Bool) -> Zipper a -> Bool
all predicate (Zipper lft x rgt) = Prelude.all predicate lft && predicate x && Prelude.all predicate rgt

select :: (a -> Bool) -> Zipper a -> Maybe (Zipper a)
select predicate zipper@(Zipper _ f _)
  | predicate f = Just zipper
  | otherwise = findInLeft predicate (left zipper) <|> findInRight predicate (right zipper)

instance Functor Zipper where
  fmap :: (a -> b) -> Zipper a -> Zipper b
  fmap fx (Zipper lft x rgt) = Zipper (fx <$> lft) (fx x) (fx <$> rgt)

instance Foldable Zipper where
  foldMap :: (Monoid m) => (a -> m) -> Zipper a -> m
  foldMap fx (Zipper lft x rgt) = foldMap fx lft <> fx x <> foldMap fx rgt

instance (Show a) => Show (Zipper a) where
  show :: Zipper a -> String
  show (Zipper lft x rgt) = "[" ++ lftItns ++ ", " ++ show x ++ ", " ++ rgtItns ++ "]"
    where
      lftItns = unwords (map show lft)
      rgtItns = unwords (map show rgt)
