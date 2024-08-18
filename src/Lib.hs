module Lib
    ( someFunc
    ) where
import Pokemon

pokemonOne = mkPokemon "Charizard" (Dual Fire Flying) (Attributes 78 84 78 100) [PokemonAttack "Flamethrower" Fire 90, PokemonAttack "Air slash" Flying 65, PokemonAttack "Dragon Claw" Dragon 90, PokemonAttack "Slash" Normal 70]

pokemonTwo = mkPokemon "Lucario" (Dual Steel Fighting) (Attributes 70 110 70 90) [PokemonAttack "Flamethrower" Fire 90, PokemonAttack "Air slash" Flying 65, PokemonAttack "Dragon Claw" Dragon 90, PokemonAttack "Slash" Normal 70]

pokemonThree = mkPokemon "Jolteon" (Mono Electric) (Attributes 65 110 95 130) [PokemonAttack "Flamethrower" Fire 90, PokemonAttack "Air slash" Flying 65, PokemonAttack "Dragon Claw" Dragon 90, PokemonAttack "Slash" Normal 70]

someFunc :: IO ()
someFunc = putStrLn "someFunc"
