module Team (Team, teamFainted, changePokemon, Team.receiveAttack) where

import Commons (ToBoolean (mapToBoolean), Zipper, all, applyOnFocus, find, focus)
import Pokemon (Name, Pokemon, PokemonAttack, isFainted, isPokemon, receiveAttack)
import Prelude hiding (all)

type Team = Zipper (Either String Pokemon)

teamFainted :: Team -> Bool
teamFainted = all (`mapToBoolean` isFainted)

changePokemon :: Name -> Team -> Maybe Team
changePokemon nameToFind team
  | mapToBoolean (focus team) (isPokemon nameToFind) = pure team
  | otherwise = find (\pokemon -> mapToBoolean pokemon (isPokemon nameToFind)) team

receiveAttack :: Pokemon -> PokemonAttack -> Team -> Team
receiveAttack attacker attack team =
  applyOnFocus team (\pokemon -> Pokemon.receiveAttack attacker attack <$> pokemon)
