{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Pokemon (Pokemon (..), Attributes (..), Attack (..), mkPokemon, mkAttributes, defense, isFainted, selectAttack, pkmnName, pkmnAttr, pkmnAttacks, pkmnAttackName) where

import Zipper (Zipper (..), focus, select)

type Name = String

type Hp = Double

type Defense = Double

type Damage = Double

data Attributes = Attributes Hp Damage Defense

data Attack = Attack Name Damage

data Pokemon = Pokemon Name Attributes (Zipper Attack)

mkAttributes :: Hp -> Damage -> Defense -> Either String Attributes
mkAttributes hp dmg dfs
  | hp <= 0 = Left "Hp must be greater to zero"
  | dmg <= 0 = Left "Damage must be greater to zero"
  | dfs <= 0 = Left "Defesa must be greater to zero"
  | otherwise = Right $ Attributes hp dmg dfs

mkPokemon :: String -> Attributes -> Zipper Attack -> Either String Pokemon
mkPokemon name attributes attacks
  | length name < 2 = Left "Pokemon name lenght must greater or equals 2"
  | length attacks /= 4 = Left $ "Pokemon '" ++ name ++ "' must have 4 attacks"
  | otherwise = Right $ Pokemon name attributes attacks

pkmnName :: Pokemon -> Name
pkmnName (Pokemon name _ _) = name

pkmnHp :: Pokemon -> Hp
pkmnHp (Pokemon _ (Attributes hp _ _) _) = hp

pkmnAttr :: Pokemon -> Attributes
pkmnAttr (Pokemon _ attr _) = attr

pkmnAttack :: Pokemon -> Attack
pkmnAttack (Pokemon _ _ attacks) = focus attacks

pkmnAttacks :: Pokemon -> Zipper Attack
pkmnAttacks (Pokemon _ _ attacks) = attacks

pkmnAttackName :: Attack -> Name
pkmnAttackName (Attack name _) = name

pkmnDamage :: Pokemon -> Damage
pkmnDamage (Pokemon _ (Attributes _ dmg _) _) = dmg

pkmnDefense :: Pokemon -> Defense
pkmnDefense (Pokemon _ (Attributes _ _ dfs) _) = dfs

damage :: Pokemon -> Pokemon -> Damage
damage attacker defensor =
  (42 * pkmnDamage attacker * (pkmnAttackDamage . pkmnAttack) attacker / pkmnDefense defensor) / 50 + 2
  where
    pkmnAttackDamage (Attack _ attackDamage) = attackDamage

defense :: Pokemon -> Pokemon -> Pokemon
defense attacker defensor@(Pokemon name (Attributes hp dmg dfs) attacks) =
  Pokemon name (Attributes newHp dmg dfs) attacks
  where
    takenDamage = damage attacker defensor
    newHp = max 0 (hp - takenDamage)

isFainted :: Pokemon -> Bool
isFainted pokemon = pkmnHp pokemon <= 0

selectAttack :: Pokemon -> Name -> Maybe (Zipper Attack)
selectAttack (Pokemon _ _ attacks) name =
  select (\(Attack attckName _) -> attckName == name) attacks

instance Eq Pokemon where
  (==) :: Pokemon -> Pokemon -> Bool
  (==) some other = pkmnName some == pkmnName other

instance Show Pokemon where
  show :: Pokemon -> String
  show pokemon = name ++ " HP " ++ hp ++ " Attacks " ++ attacks
    where
      name = pkmnName pokemon
      hp = (show . pkmnHp) pokemon
      attacks = show (pkmnAttackName <$> pkmnAttacks pokemon)
