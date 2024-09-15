module Main (main) where

import qualified Infrastructure.Mock as Mock
import Domain.Battle (mkBattle, finished)
import Infrastructure.Game (Game, play)

p1 = Mock.player "Gustavo"
p2 = Mock.player "Thales"
game = mkBattle <$> p1 <*> p2

run :: Game -> IO Game
run g = if (not . finished) g then play g >>= run else return g

main :: IO ()
main = do
  g <- either fail run game
  putStrLn "Acabou"

