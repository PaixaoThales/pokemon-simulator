{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Battle (Battle (..), Player (..), mkPlayer, battle, playerName) where

import Control.Concurrent
import Data.Functor ((<&>))
import GHC.Base (failIO)
import Pokemon (Pokemon, isFainted, pkmnName)
import Switch (Switch (Switch), current, switch)
import Team (Team, receiveAttack, selectAttack, selectPokemon, teamFainted)
import ToBoolean (toBoolean)
import Zipper (Zipper (..), focus, select)

type Name = String

data Player = Player Name Team | Winner Name | Loser Name

data Battle = Battle (Switch Player) | Finished Player Player deriving (Show)

mkPlayer :: Name -> Team -> Player
mkPlayer = Player

playerName :: Player -> Name
playerName (Winner n) = n
playerName (Loser n) = n
playerName (Player n _) = n

playerTeam :: Player -> Team
playerTeam (Player _ t) = t

turn :: Battle -> Battle
turn (Battle players) = Battle $ switch players
turn b = b

attack :: Pokemon -> Battle -> Battle
attack attacker (Battle (Switch actual (Player name defense))) =
  Battle $ Switch actual (Player name newDefenseTeam)
  where
    newDefenseTeam = receiveAttack attacker defense
attack _ b = b

finish :: Battle -> Battle
finish b@(Battle (Switch (Player name team) (Player other _)))
  | teamFainted team = Finished (Loser name) (Winner other)
  | otherwise = b
finish b = b

prepare :: Battle -> IO Battle
prepare (Battle (Switch (Player pName team) next)) = do
  putStrLn $ "\n " ++ pName ++ " deseja mudar de pokemon (Y/n)? "
  shouldChangePokemon <- getLine
  requestedPokemonName <- if shouldChangePokemon == "Y" then getLine else return pokemonName
  putStrLn $ "\n " ++ pName ++ " qual ataque do pokemon " ++ requestedPokemonName ++ " deseja usar? "
  requestedAttackName <- getLine
  let newTeam = selectPokemon requestedPokemonName team >>= selectAttack requestedAttackName
  let newPlayer = fmap (Player pName) newTeam
  let newBattle = Battle <$> newPlayers newPlayer
  threadDelay 1000000
  case newBattle of
    Just b -> return b
    _ -> failIO "ERROR"
  where
    pokemonName = either (const "ERROR") pkmnName (focus team)
    newPlayers newCurrent = Switch <$> newCurrent <*> pure next
prepare b = return b

consolida :: Battle -> IO Battle
consolida (Battle (Switch attacker (Player otherName otherTeam))) = do
  let focusIsFainted = either (const False) isFainted (focus otherTeam)
  let newDefenseTeam = if focusIsFainted then select (toBoolean (not . isFainted)) otherTeam else pure otherTeam
  let newBattle = newDefenseTeam <&> (Battle . Switch attacker . Player otherName)
  case newBattle of
    Just b -> return b
    _ -> return $ Finished (Loser otherName) (Winner (playerName attacker))
consolida b = return b

battle :: Battle -> IO Battle
battle game@(Finished _ _) = return game
battle game@(Battle players) = attack <$> attacker <*> prepare game >>= consolida >>= \g -> (return . finish . turn) g
  where
    attacker = case (focus . playerTeam . current) players of
      Left _ -> failIO "ERROR"
      Right pokemon -> return pokemon

instance Show Player where
  show :: Player -> String
  show (Winner name) = "Winner: " ++ name
  show (Loser name) = "Loser: " ++ name
  show (Player name (Zipper lft selected rgt)) = player ++ pokemon ++ others
    where
      player = "\nPlayer: " ++ name
      pokemon = "\nSelected Pokemon: " ++ (drop 6 . show) selected ++ "\n\t "
      others = foldr ((\p l -> p ++ "\n\t " ++ l) . drop 6 . show) "" (lft ++ rgt)
