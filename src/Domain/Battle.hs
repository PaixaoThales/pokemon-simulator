{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Domain.Battle (Battle(..), Player, mkBattle, mkPlayer, battle, finished, name, team) where

import Domain.Commons.Zipper (focus, sides)
import Domain.Pokemon (isFainted, isAlive)
import Domain.Team (Team, attack, selected, set)

type Name = String

type Attacker = Player

type Defensor = Player

data Player = Player {name :: Name, team :: Team}

data Battle = Battle {attk :: Player, dfs :: Player}

mkPlayer :: String -> Team -> Either String Player
mkPlayer name' team'
  | length name' < 3 = Left "Player name must be 3 or more chars"
  | otherwise = Right $ Player name' team'

mkBattle :: Player -> Player -> Battle
mkBattle = Battle

turn :: Battle -> Battle
turn (Battle p1 p2) = Battle p2 p1

defend :: Attacker -> Defensor -> Defensor
defend atk' df' = df' {team = attack attacker' defensor'}
  where
    attacker' = team atk'; defensor' = team df'

-- Temos que mexer aqui se o dano nao funcionar
fight :: Battle -> Battle
fight battle' = mkBattle attacker' (defend attacker' defensor')
  where
    attacker' = attk battle'
    defensor' = dfs battle'

consolidate :: Battle -> Battle
consolidate battle'
  | selected isFainted (team . dfs $ battle') =
      mkBattle attacker' (defensor' {team = set isAlive $ team defensor'})
  | otherwise = battle'
  where
    attacker' = attk battle'
    defensor' = dfs battle'

battle :: Battle -> Battle
battle = turn . consolidate . fight 

finished :: Battle -> Bool
finished battle' = all isFainted (team . attk $ battle')

instance Show Player where
  show :: Player -> String
  show player' =
    "\nPlayer: "
      ++ name player'
      ++ "\nSelected Pokemon: " ++ show ((focus . team) player')
      ++ "\n\t "
      ++ foldr ((\actual othrs -> actual ++ "\n\t " ++ othrs) . show) "" ((sides . team) player')