module Team (Team, teamFainted, selectPokemon, receiveAttack, Team.selectAttack) where

import Pokemon (Pokemon, defense, isFainted, mkPokemon, pkmnAttr, pkmnName, selectAttack)
import ToBoolean (ToBoolean (toBoolean))
import ToMaybe (ToMaybe (toMaybe))
import Zipper (Zipper (..), all, apply, focus, select)
import Prelude hiding (all)

type Name = String

type Team = Zipper (Either String Pokemon)

teamFainted :: Team -> Bool
teamFainted = all $ toBoolean isFainted

selectPokemon :: Name -> Team -> Maybe Team
selectPokemon nameToFind team
  | withSameName (focus team) = Just team
  | otherwise = select withSameName team
  where
    withSameName pokemon = toBoolean (nameToFind ==) (pkmnName <$> pokemon)

selectAttack :: Name -> Team -> Maybe Team
selectAttack nameToFind (Zipper lft p rgt) = do
  pokemon <- toMaybe p
  attacks <- Pokemon.selectAttack pokemon nameToFind
  newPokemon <- toMaybe (mkPokemon (pkmnName pokemon) (pkmnAttr pokemon) attacks)
  pure $ Zipper lft (Right newPokemon) rgt

receiveAttack :: Pokemon -> Team -> Team
receiveAttack attacker team =
  apply team (\pokemon -> defense attacker <$> pokemon)
