module Mock (player) where

import Battle (Player (..))
import Pokemon (Attack (..), Attributes (..), Pokemon, mkPokemon)
import Team (Team)
import Zipper (Zipper (..))

type Name = String

charizard :: Either String Pokemon
charizard = mkPokemon "Charizard" (Attributes 5 84 78) (Zipper [Attack "Flamethrower" 90] (Attack "Air Slash" 65) [Attack "Dragon Claw" 90, Attack "A" 70])

lucario :: Either String Pokemon
lucario = mkPokemon "Lucario" (Attributes 5 110 70) (Zipper [Attack "Aura Sphere" 90] (Attack "Extremespeed" 80) [Attack "Dragon Pulse" 90, Attack "B" 100])

jolteon :: Either String Pokemon
jolteon = mkPokemon "Jolteon" (Attributes 5 110 95) (Zipper [Attack "Thunder" 120] (Attack "Quick Attack" 40) [Attack "Signal Beam" 75, Attack "C" 150])

gengar :: Either String Pokemon
gengar = mkPokemon "Gengar" (Attributes 5 65 60) (Zipper [Attack "Dream Eater" 100] (Attack "Hyper Beam" 150) [Attack "Mega Punch" 80, Attack "D" 90])

dragonite :: Either String Pokemon
dragonite = mkPokemon "Dragonite" (Attributes 5 134 95) (Zipper [Attack "Hyper Beam" 150] (Attack "Slam" 80) [Attack "Blizzard" 120, Attack "E" 80])

gyarados :: Either String Pokemon
gyarados = mkPokemon "Gyarados" (Attributes 5 125 79) (Zipper [Attack "Hydropump" 120] (Attack "Hyperbeam" 65) [Attack "Strenght" 80, Attack "F" 95])

team :: String -> Team
team name
  | name == "Gustavo" = Zipper [gyarados] gengar [dragonite]
  | otherwise = Zipper [lucario] charizard [jolteon]

player :: Name -> Player
player name = Player name $ team name
