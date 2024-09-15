{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Domain.Pokemon
  ( Pokemon(..),
    Attributes,
    Attack,
    mkAttack,
    mkAttributes,
    mkPokemon,
    isAlive,
    isFainted,
    attack,
  )
where

import Domain.Commons.Zipper (Zipper, focus)

type Name = String

type Hp = Double

type Defense = Double

type Damage = Double

type Attacker = Pokemon

type Defender = Pokemon

data Attributes = Attributes {hp :: Hp, dmg :: Damage, defense :: Defense}

data Attack = Attack {aname :: Name, admg :: Damage}

data Pokemon
  = Pokemon {name :: Name, attributes :: Attributes, attacks :: Zipper Attack}

mkAttributes :: Hp -> Damage -> Defense -> Either String Attributes
mkAttributes hp' dmg' dfs'
  | hp' <= 0 = Left "Hp must be greater to zero"
  | dmg' <= 0 = Left "Damage must be greater to zero"
  | dfs' <= 0 = Left "Defesa must be greater to zero"
  | otherwise = Right $ Attributes hp' dmg' dfs'

mkAttack :: String -> Double -> Either String Attack
mkAttack name' damage'
  | length name' < 5 = Left "Attack name lenght must be greater or equals 5"
  | damage' <= 0 = Left "Attack damage must be positive"
  | otherwise = Right $ Attack name' damage'

mkPokemon :: Name -> Attributes -> Zipper Attack -> Either String Pokemon
mkPokemon name' attributes' attacks'
  | length name' < 2 = Left "Pokemon name lenght must greater or equals 2"
  | length attacks' /= 4 = Left $ "Pokemon '" ++ name' ++ "' must have 4 attacks"
  | otherwise = Right $ Pokemon name' attributes' attacks'

damage :: Attacker -> Defender -> Damage
damage attacker' defensor' = (42 * attackerDmg * attackDmg / defense') / 50 + 2
  where
    attackerDmg = dmg . attributes $ attacker'
    attackDmg = admg . focus . attacks $ attacker'
    defense' = defense . attributes $ defensor'

defend :: Defender -> Damage -> Defender
defend defender' damage' = defender' {attributes = updated}
  where
    attributes' = attributes defender'
    updated = attributes' {hp = max 0 (hp attributes' - damage')}

attack :: Attacker -> Defender -> Defender
attack attacker' defender' = defend defender' $ damage attacker' defender'

isFainted :: Pokemon -> Bool
isFainted pokemon = (hp . attributes $ pokemon) <= 0

isAlive :: Pokemon -> Bool
isAlive = not . isFainted

instance Eq Pokemon where
  (==) :: Pokemon -> Pokemon -> Bool
  (==) some other = name some == name other

instance Show Attributes where
  show :: Attributes -> String
  show attr = " HP " ++ (show . hp) attr

instance Show Attack where
  show :: Attack -> String
  show = show . aname

instance Show Pokemon where
  show :: Pokemon -> String
  show pkmn =
    name pkmn
      ++ (show . attributes) pkmn
      ++ " Attacks "
      ++ show (aname <$> attacks pkmn)
