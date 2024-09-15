{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Domain.Team where

import Domain.Commons.Zipper ( Zipper, focus, apply, focusOn )
import qualified Domain.Pokemon as Pokemon
import Data.Maybe (fromMaybe)

type Name = String

type Defender = Team

type Attacker = Team

type Team = Zipper Pokemon.Pokemon

selected :: (Pokemon.Pokemon -> Bool) -> Team -> Bool
selected predicate = predicate . focus

set :: (Pokemon.Pokemon -> Bool) -> Team -> Team
set predicate team = fromMaybe team $ focusOn predicate team

attack :: Attacker -> Defender -> Defender
attack attacker' defender' = apply defender' attack'
  where
    attack' = Pokemon.attack $ focus attacker'