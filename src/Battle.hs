module Battle where

import Commons (Switch (Switch), switch)
import Pokemon (Pokemon, PokemonAttack)
import Team (Team, receiveAttack, teamFainted)

type Name = String

data Player = Player String Team | Winner Name | Loser Name deriving (Show)

data Battle = Battle (Switch Player) | Finished Player Player

turn :: Battle -> Battle
turn (Battle players) = Battle $ switch players
turn battle = battle

attack :: Battle -> Pokemon -> PokemonAttack -> Battle
attack (Battle (Switch current (Player name defense))) pokemonAttk attk =
  Battle $ Switch current (Player name (receive defense))
  where
    receive = receiveAttack pokemonAttk attk
attack battle _ _ = battle

finish :: Battle -> Battle
finish battle@(Battle (Switch (Player name team) (Player other _)))
  | teamFainted team = Finished (Loser name) (Winner other)
  | otherwise = battle
finish battle = battle
