module Infrastructure.Mock (player) where

import Domain.Battle (Player, mkPlayer)
import Domain.Pokemon (Pokemon, mkPokemon, mkAttributes, mkAttack, Attributes, Attack)
import Domain.Team (Team)
import Domain.Commons.Zipper (Zipper(..))

mk :: String -> Either String Attributes -> Either String (Zipper Attack) -> Either String Pokemon
mk name attributes attacks = attributes >>= \a -> sequence (a, attacks) >>= uncurry (mkPokemon name)

charizard :: Either String Pokemon
charizard = mk "Charizard" attributes attacks
  where
    attributes = mkAttributes 5 84 78
    attks = [mkAttack "Flamethrower" 90, mkAttack "Dragon Claw" 90, mkAttack "Attack" 70]
    attacks = sequence $ Zipper [head attks] (mkAttack "Air Slash" 65) (tail attks)

lucario :: Either String Pokemon
lucario = mk "Lucario" attributes attacks
  where
    attributes = mkAttributes 5 110 70
    attks = [mkAttack "Extremespeed" 80, mkAttack "Dragon Pulse" 90, mkAttack "Boomm" 100]
    attacks = sequence $ Zipper [head attks] (mkAttack "Aura Sphere" 90) (tail attks)

jolteon :: Either String Pokemon
jolteon = mk "Jolteon" attributes attacks
  where
    attributes = mkAttributes 5 110 95
    attks = [mkAttack "Thunder" 120, mkAttack "Signal Beam" 75, mkAttack "Calamar" 150]
    attacks = sequence $ Zipper [head attks] (mkAttack "Quick Attack" 40) (tail attks)

team :: String -> Either String Team
team name
  | name == "Gustavo" = sequence $ Zipper [charizard] lucario [jolteon]
  | otherwise = sequence $ Zipper [lucario] jolteon [charizard]

player :: String -> Either String Player
player name = team name >>= mkPlayer name
