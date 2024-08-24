module Main (main) where

import Battle (Battle (..), battle, playerName)
import Mock (player)
import Switch (Switch (..))

play :: Battle -> IO Battle
play b@(Battle (Switch one second)) = do
  putStr "\ESC[2J"
  putStrLn $ "Atacante: " ++ show one
  putStrLn $ "Defensor: " ++ show second
  newBattle <- battle b
  play newBattle
play b@(Finished loser winner) = do
  putStrLn $ "Winner: " ++ playerName winner
  putStrLn $ "Loser: " ++ playerName loser
  return b

main :: IO ()
main = do
  let first = player "Gustavo"
  let other = player "Thales"
  _ <- play $ Battle (Switch first other)
  print "A batalha acabou"
