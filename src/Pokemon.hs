module Pokemon
  ( Type (..),
    PokemonType (..),
    Attributes (..),
    PokemonAttack (..),
    Pokemon (..),
    isFainted,
    damageMultiplier,
    evaluateDamage,
    mkPokemon,
    stab,
    pokemonAttackDamage,
    mkAttributesWithNewHp,
    receiveAttack,
    isPokemon,
    findAttackByName,
  )
where

import Data.List (find)

data Type
  = Normal
  | Fire
  | Water
  | Grass
  | Electric
  | Ice
  | Fighting
  | Poison
  | Ground
  | Flying
  | Psychic
  | Bug
  | Rock
  | Ghost
  | Dragon
  | Dark
  | Steel
  | Fairy
  deriving (Show, Eq)

data PokemonType = Mono Type | Dual Type Type
  deriving (Show)

data Attributes = Attributes {hitPoints :: Double, attackAttribute :: Double, defenseAttribute :: Double, speedAttribute :: Double}
  deriving (Show)

data PokemonAttack = PokemonAttack {attackName :: String, attackType :: Type, attackDamage :: Double} deriving (Show)

data Pokemon
  = Pokemon
      { name :: String,
        types :: PokemonType,
        attributes :: Attributes,
        attacks :: [PokemonAttack]
      }
  | Fainted {name :: String}
  deriving (Show)

isFainted :: Pokemon -> Bool
isFainted (Fainted _) = True
isFainted _ = False

type Multiplier = Double

damageMultiplier :: Type -> Type -> Multiplier
damageMultiplier Normal Rock = 0.5
damageMultiplier Normal Ghost = 0
damageMultiplier Normal Steel = 0.5
damageMultiplier Fighting Normal = 2
damageMultiplier Fighting Flying = 0.5
damageMultiplier Fighting Poison = 0.5
damageMultiplier Fighting Rock = 2
damageMultiplier Fighting Bug = 0.5
damageMultiplier Fighting Ghost = 0
damageMultiplier Fighting Steel = 2
damageMultiplier Fighting Psychic = 0.5
damageMultiplier Fighting Ice = 2
damageMultiplier Fighting Dark = 2
damageMultiplier Fighting Fairy = 0.5
damageMultiplier Flying Fighting = 2
damageMultiplier Flying Rock = 0.5
damageMultiplier Flying Bug = 2
damageMultiplier Flying Steel = 0.5
damageMultiplier Flying Grass = 2
damageMultiplier Flying Electric = 0.5
damageMultiplier Poison Poison = 0.5
damageMultiplier Poison Ground = 0.5
damageMultiplier Poison Rock = 0.5
damageMultiplier Poison Ghost = 0.5
damageMultiplier Poison Steel = 0
damageMultiplier Poison Grass = 2
damageMultiplier Poison Fairy = 2
damageMultiplier Ground Flying = 0
damageMultiplier Ground Poison = 2
damageMultiplier Ground Rock = 2
damageMultiplier Ground Bug = 0.5
damageMultiplier Ground Steel = 2
damageMultiplier Ground Fire = 2
damageMultiplier Ground Grass = 0.5
damageMultiplier Ground Electric = 2
damageMultiplier Rock Fighting = 0.5
damageMultiplier Rock Flying = 2
damageMultiplier Rock Ground = 0.5
damageMultiplier Rock Bug = 2
damageMultiplier Rock Steel = 0.5
damageMultiplier Rock Fire = 2
damageMultiplier Rock Ice = 2
damageMultiplier Bug Fighting = 0.5
damageMultiplier Bug Flying = 0.5
damageMultiplier Bug Poison = 0.5
damageMultiplier Bug Ghost = 0.5
damageMultiplier Bug Steel = 0.5
damageMultiplier Bug Fire = 0.5
damageMultiplier Bug Grass = 2
damageMultiplier Bug Psychic = 2
damageMultiplier Bug Dark = 2
damageMultiplier Bug Fairy = 0.5
damageMultiplier Ghost Normal = 0
damageMultiplier Ghost Ghost = 2
damageMultiplier Ghost Psychic = 2
damageMultiplier Ghost Dark = 0.5
damageMultiplier Steel Rock = 2
damageMultiplier Steel Steel = 0.5
damageMultiplier Steel Fire = 0.5
damageMultiplier Steel Water = 0.5
damageMultiplier Steel Electric = 0.5
damageMultiplier Steel Ice = 0.5
damageMultiplier Steel Fairy = 0.5
damageMultiplier Fire Rock = 0.5
damageMultiplier Fire Bug = 2
damageMultiplier Fire Steel = 2
damageMultiplier Fire Fire = 0.5
damageMultiplier Fire Water = 0.5
damageMultiplier Fire Grass = 2.0
damageMultiplier Fire Ice = 2
damageMultiplier Fire Dragon = 0.5
damageMultiplier Grass Fire = 0.5
damageMultiplier Water Ground = 2
damageMultiplier Water Rock = 2
damageMultiplier Water Fire = 2
damageMultiplier Water Water = 0.5
damageMultiplier Water Grass = 0.5
damageMultiplier Water Dragon = 0.5
damageMultiplier Electric Flying = 2
damageMultiplier Electric Ground = 0
damageMultiplier Electric Water = 2
damageMultiplier Electric Grass = 0.5
damageMultiplier Electric Electric = 0.5
damageMultiplier Electric Dragon = 0.5
damageMultiplier Psychic Fighting = 2
damageMultiplier Psychic Poison = 2
damageMultiplier Psychic Steel = 0.5
damageMultiplier Psychic Psychic = 0.5
damageMultiplier Psychic Dark = 0
damageMultiplier Ice Flying = 2
damageMultiplier Ice Ground = 2
damageMultiplier Ice Steel = 0.5
damageMultiplier Ice Fire = 0.5
damageMultiplier Ice Water = 0.5
damageMultiplier Ice Grass = 2
damageMultiplier Ice Ice = 0.5
damageMultiplier Ice Dragon = 2
damageMultiplier Dragon Steel = 0.5
damageMultiplier Dragon Dragon = 2
damageMultiplier Dragon Fairy = 0
damageMultiplier Dark Fighting = 0.5
damageMultiplier Dark Ghost = 2
damageMultiplier Dark Psychic = 2
damageMultiplier Dark Dark = 0.5
damageMultiplier Dark Fairy = 0.5
damageMultiplier Fairy Fighting = 2
damageMultiplier Fairy Poison = 0.5
damageMultiplier Fairy Steel = 0.5
damageMultiplier Fairy Fire = 0.5
damageMultiplier Fairy Dragon = 2
damageMultiplier Fairy Dark = 2
damageMultiplier _ _ = 1.0

evaluateDamage :: Type -> PokemonType -> Multiplier
evaluateDamage offense (Mono defense) = damageMultiplier offense defense
evaluateDamage offense (Dual defenseOne defenseTwo) =
  damage defenseOne * damage defenseTwo
  where
    damage = damageMultiplier offense

mkPokemon :: String -> PokemonType -> Attributes -> [PokemonAttack] -> Either String Pokemon
mkPokemon pokemonName pokemonTypes pokemonAttributes pokemonAttacks
  | length pokemonName < 2 = Left "Pokemon name lenght must greater or equals 2"
  | length pokemonAttacks /= 4 = Left $ "Pokemon '" ++ pokemonName ++ "' must have 4 attacks"
  | otherwise =
      Right
        Pokemon
          { name = pokemonName,
            types = pokemonTypes,
            attributes = pokemonAttributes,
            attacks = pokemonAttacks
          }

stab :: Pokemon -> PokemonAttack -> Multiplier
stab pokemon attack = case types pokemon of
  Mono monoType -> if monoType == attackType attack then 1.5 else 1
  Dual typeOne typeTwo -> if typeOne == attackType attack || typeTwo == attackType attack then 1.5 else 1

pokemonAttackDamage :: Pokemon -> Pokemon -> PokemonAttack -> Double
pokemonAttackDamage pokemonDefender attackingPokemon attackOfAttackingPokemon =
  ((42 * attackAttribute (attributes attackingPokemon) * attackDamage attackOfAttackingPokemon / defenseAttribute (attributes pokemonDefender)) / 50 + 2)
    * stab attackingPokemon attackOfAttackingPokemon
    * evaluateDamage (attackType attackOfAttackingPokemon) (types pokemonDefender)

-- Damage = (((42 * AttackStat * AttackPower / DefenseStat) / 50) + 2) * STAB * Weakness/Resistance * 1

mkAttributesWithNewHp :: Attributes -> Double -> Attributes
mkAttributesWithNewHp attr hp = attr {hitPoints = hp}

receiveAttack :: Pokemon -> PokemonAttack -> Pokemon -> Pokemon
receiveAttack attackingPokemon attackOfAttackingPokemon pokemonDefender =
  let damage = pokemonAttackDamage pokemonDefender attackingPokemon attackOfAttackingPokemon
      newHP = hitPoints (attributes pokemonDefender) - damage
      deffenderNewAttributes = mkAttributesWithNewHp (attributes pokemonDefender) newHP
   in if newHP <= 0
        then Fainted {name = name pokemonDefender}
        else pokemonDefender {attributes = deffenderNewAttributes}

isPokemon :: String -> Pokemon -> Bool
isPokemon targetPokemonName pokemon = name pokemon == targetPokemonName

findAttackByName :: Pokemon -> String -> Maybe PokemonAttack
findAttackByName pokemon attackNameToFind =
  find (\attack -> attackName attack == attackNameToFind) (attacks pokemon)
