module Mock (player) where

import Battle (Player (Player))
import Commons (Zipper (Zipper))
import Pokemon (Attributes (..), PokemonAttack (..), PokemonType (..), Type (..), mkPokemon)
import Team (Team)

type Name = String

charizard = mkPokemon "Charizard" (Dual Fire Flying) (Attributes 78 84 78 100) [PokemonAttack "Flamethrower" Fire 95, PokemonAttack "Air slash" Flying 65, PokemonAttack "Dragon Claw" Dragon 90, PokemonAttack "Slash" Normal 70]

lucario = mkPokemon "Lucario" (Dual Steel Fighting) (Attributes 70 110 70 90) [PokemonAttack "Aura Sphere" Fighting 90, PokemonAttack "Extremespeed" Normal 80, PokemonAttack "Earthquake" Dragon 100, PokemonAttack "Shadow Claw" Ghost 75]

jolteon = mkPokemon "Jolteon" (Mono Electric) (Attributes 65 110 95 130) [PokemonAttack "Discharge" Electric 80, PokemonAttack "Signal Beam" Bug 65, PokemonAttack "Quick Attack" Normal 40, PokemonAttack "Thunder" Electric 120]

team :: String -> Team
team name
  | name == "Gustavo" = Zipper [lucario] charizard [jolteon]
  | otherwise = Zipper [jolteon, charizard] lucario []

player :: Name -> Player
player name = Player name $ team name
