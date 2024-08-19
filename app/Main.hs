module Main (main) where

import Data.Foldable (foldr)
import Commons (Zipper(Zipper), Switch(Switch), ToMaybe(toMaybe), focus)
import Pokemon (name, attacks, attackName, findAttackByName, attributes, hitPoints)
import Battle(Battle(..), Player(..), attack, finish, turn)
import Mock(player)
import Team(changePokemon)

clearConsole = putStr "\ESC[2J"

presentPlayer :: Player -> IO ()
presentPlayer (Player player (Zipper lft selected rgt)) = do
    putStrLn $ "Player: " ++ player
    putStrLn $ ("Selected Pokemon: " ++ " " ++ showPokemonAndAttacks selected)
    putStrLn "Others: "
    putStrLn $ foldr (\p l -> p ++ "\n" ++ l) "" (map (showPokemonAndAttacks) lft)
    putStrLn $ foldr (\p l -> p ++ "\n" ++ l) "" (map (showPokemonAndAttacks) rgt)
    where
        showAttacks pokemon = show (map attackName (attacks pokemon))
        showPokemonAndAttacks pokemon = case pokemon of
            Left _ -> "Error on parsing pokemon"
            Right pokemon -> "\t " ++ name pokemon ++ " HP " ++ (show . hitPoints . attributes) pokemon ++ " Attacks: " ++ showAttacks pokemon
presentPlayer player = print player

play :: Battle -> IO ()
play battle@(Battle (Switch one@(Player name team) second)) = do
    
    presentPlayer one
    presentPlayer second
    putStrLn $ "\n " ++ name ++ " deseja mudar de pokemon (Y/n)? "
    shouldChangePokemon <- getLine
    if shouldChangePokemon == "Y" then
        do
            putStrLn "\n Digite o nome do pokemon que deseja escolher: "
            requestedNamePokemon <- getLine
            let newTeam = changePokemon requestedNamePokemon team
            putStrLn "Digite o nome do ataque que quer fazer: "
            requestedNameAttack <- getLine
            let requestedPokemon = toMaybe (focus newTeam)
            let requestedAttack = requestedPokemon >>= (\x -> findAttackByName x requestedNameAttack)
            let newBattle = Battle (Switch (Player name $ newTeam) second)
            let makeAttack = attack newBattle
            let newState = ((fmap makeAttack requestedPokemon) <*> requestedAttack) >>= (pure . finish)
            case newState of
                Just s -> play (turn s)
                Nothing -> play (turn battle)
    else 
        do
            putStrLn "Digite o nome do ataque que quer fazer: "
            requestedNameAttack <- getLine
            let requestedPokemon = toMaybe (focus team)
            let requestedAttack = requestedPokemon >>= (\x -> findAttackByName x requestedNameAttack)
            let makeAttack = attack battle
            let newState = ((fmap makeAttack requestedPokemon) <*> requestedAttack) >>= (pure . finish)
            case newState of
                Just s -> play (turn s) 
                Nothing -> play (turn battle)
play (Finished (Loser loser) (Winner winner)) = putStrLn $ "Winner: " ++ winner ++ " Loser: " ++ loser

main :: IO ()
main = do
    let one = player "Gustavo"
    let second = player "Thales" 
    play (Battle (Switch one second))
