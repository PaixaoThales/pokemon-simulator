module Switch (Switch (..), switch, current) where

data Switch a = Switch a a deriving (Show)

current :: Switch a -> a
current (Switch c _) = c

switch :: Switch a -> Switch a
switch (Switch c n) = Switch n c
