module Team (Team, teamFainted, changePokemon, Team.receiveAttack) where

import Commons (ToBoolean (toBoolean), Zipper, all, applyOnFocus, find, focus)
import Data.Maybe
import Pokemon (Pokemon, PokemonAttack, isFainted, isPokemon, receiveAttack)
import Prelude hiding (all)

type Team = Zipper (Either String Pokemon)

teamFainted :: Team -> Bool
teamFainted = all (`toBoolean` isFainted)

changePokemon :: String -> Team -> Team
changePokemon nameToFind team
  | toBoolean (focus team) (isPokemon nameToFind) = team
  | otherwise =
      fromMaybe
        team
        (find (\pokemon -> toBoolean pokemon (isPokemon nameToFind)) team)

receiveAttack :: Pokemon -> PokemonAttack -> Team -> Team
receiveAttack attacker attack team =
  applyOnFocus team (\pokemon -> Pokemon.receiveAttack attacker attack <$> pokemon)
