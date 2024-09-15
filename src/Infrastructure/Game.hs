{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant where" #-}
module Infrastructure.Game where

import Domain.Battle
    ( Battle(..), Player(team, name), mkBattle, battle)
import qualified Infrastructure.Mock as Mock
import qualified Domain.Pokemon as Pokemon
import qualified Domain.Commons.Zipper as Z
import Data.Maybe (fromMaybe)
import Control.Concurrent
type Game = Battle

control :: Z.Zipper a -> Char -> IO (Z.Zipper a)
control zipper input
    | input == 'a'  = return $ fromMaybe zipper (Z.left zipper)
    | input == 'd' = return $ fromMaybe zipper (Z.right zipper)
    | otherwise = return zipper

config :: Game -> IO Game
config game = do
    putStr "\ESC[2J"
    let attacker' = attk game
    let defensor' = dfs game
    print . attk $ game
    print . dfs $ game
    putStrLn $ Domain.Battle.name  attacker' ++ " selecione um pokemon:"
    print (fmap Pokemon.name . team $ attacker')
    input <- getChar
    team' <- control (team attacker') input

    if input == 'e' then return game    
    else config (mkBattle (attacker'{team = team'}) defensor')

play :: Game -> IO Game
play game = do
        config game >>= return . battle
