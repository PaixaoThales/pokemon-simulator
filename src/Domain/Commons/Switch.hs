{-# LANGUAGE InstanceSigs #-}

module Domain.Commons.Switch (Switch, mkSwitch, switch, current, next) where
import Data.Bifunctor (Bifunctor (bimap))

data Switch a b = Switch b a deriving (Show)

mkSwitch :: a -> b -> Switch a b
mkSwitch current' next' = Switch next' current'

current :: Switch a b -> a
current (Switch _ c) = c

next :: Switch a b -> b
next (Switch n _) = n

switch :: Switch a b -> Switch b a
switch (Switch c n) = Switch n c

instance Functor (Switch n) where
  fmap :: (a -> b) -> Switch n a -> Switch n b
  fmap f (Switch x y) = Switch (f x)  y

instance Bifunctor Switch where
  bimap :: (a -> b) -> (c -> d) -> Switch a c -> Switch b d
  bimap f g (Switch n c) = Switch (g n) (f c)
