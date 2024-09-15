{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Domain.Commons.Zipper where

import Domain.Commons.ToBoolean ( Boolean(..) )
import Control.Applicative ( Alternative((<|>)) )

data Zipper a = Zipper [a] a [a]

focus :: Zipper a -> a
focus (Zipper _ x _) = x

sides :: Zipper a -> [a]
sides (Zipper l _ r) = l ++ r

apply :: Zipper a -> (a -> a) -> Zipper a
apply (Zipper lft x rgt) fx = Zipper lft (fx x) rgt

right, left :: Zipper a -> Maybe (Zipper a)
right (Zipper _ _ []) = Nothing
right (Zipper lft f (x : rgt)) = Just $ Zipper (f : lft) x rgt

left (Zipper [] _ _) = Nothing
left (Zipper (x : lft) f rgt) = Just $ Zipper lft x (f : rgt)

focusOn :: (a -> Bool) -> Zipper a -> Maybe (Zipper a)
focusOn accept z
  | accept . focus $ z = Just z
  | otherwise = (left z >>= focusOn accept) <|> (right z >>= focusOn accept)

focusOn' :: Boolean m => (a -> Bool) -> Zipper (m a) -> Maybe (Zipper (m a))
focusOn' accept = focusOn (toBoolean accept)

instance Functor Zipper where
  fmap :: (a -> b) -> Zipper a -> Zipper b
  fmap fx (Zipper lft x rgt) = Zipper (fx <$> lft) (fx x) (fx <$> rgt)

instance Foldable Zipper where
  foldMap :: (Monoid m) => (a -> m) -> Zipper a -> m
  foldMap fx (Zipper lft x rgt) = foldMap fx lft <> fx x <> foldMap fx rgt

instance Traversable Zipper where
  traverse :: Applicative f => (a -> f b) -> Zipper a -> f (Zipper b)
  traverse fx (Zipper lft f rgt) = Zipper <$> lft' <*> fx f <*> rgt'
    where
      lft' = traverse fx lft
      rgt' = traverse fx rgt

instance (Show a) => Show (Zipper a) where
  show :: Zipper a -> String
  show (Zipper [] x []) = "[" ++  "\9654 " ++ show x ++ "]"
  show (Zipper [] x rgt) = "[" ++ "\9654 " ++ show x ++  ", " ++ unwords (map show rgt) ++ "]"
  show (Zipper lft x []) = "[" ++ unwords (map show lft) ++ ", \9654 " ++ show x ++ "]"
  show (Zipper lft x rgt) = "[" ++ lftItns ++ ", \9654 " ++ show x ++ ", " ++ rgtItns ++ "]"
    where
      lftItns = unwords (map show lft)
      rgtItns = unwords (map show rgt)