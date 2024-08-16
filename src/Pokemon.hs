module Lib
  (
  )
where

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

type Name = String

data PokemonAttack = PokemonAttack {attackName :: Name, attackType :: Type, attackDamage :: Double} deriving (Show)

data Pokemon
  = Pokemon
      { name :: Name,
        types :: PokemonType,
        attributes :: Attributes,
        attackOne :: PokemonAttack,
        attackTwo :: PokemonAttack,
        attackThree :: PokemonAttack,
        attackFour :: PokemonAttack
      }
  | Fainted {name :: Name}
  deriving (Show)

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

-- >>> evaluateDamage Fairy (Dual Dragon Fire)
-- 1.0

createPokemon :: String -> PokemonType -> Attributes -> PokemonAttack -> PokemonAttack -> PokemonAttack -> PokemonAttack -> Either String Pokemon
createPokemon pokemonName pokemonTypes pokemonAttributes pokemonAttackOne pokemonAttackTwo pokemonAttackThree pokemonAttackFour =
  if length pokemonName > 1
    then
      Right
        Pokemon
          { name = pokemonName,
            types = pokemonTypes,
            attributes = pokemonAttributes,
            attackOne = pokemonAttackOne,
            attackTwo = pokemonAttackTwo,
            attackThree = pokemonAttackThree,
            attackFour = pokemonAttackFour
          }
    else Left $ "Pokemon '" ++ pokemonName ++ "' must have name"

pokemonOne = createPokemon "Charizard" (Dual Fire Flying) (Attributes 78 84 78 100) (PokemonAttack "Flamethrower" Fire 90) (PokemonAttack "Air slash" Flying 65) (PokemonAttack "Dragon Claw" Dragon 90) (PokemonAttack "Slash" Normal 70)

pokemonTwo = createPokemon "Lucario" (Dual Steel Fighting) (Attributes 70 110 70 90) (PokemonAttack "Flamethrower" Fire 90) (PokemonAttack "Air slash" Flying 65) (PokemonAttack "Dragon Claw" Dragon 90) (PokemonAttack "Slash" Normal 70)

pokemonThree = createPokemon "Jolteon" (Mono Electric) (Attributes 65 110 95 130) (PokemonAttack "Flamethrower" Fire 90) (PokemonAttack "Air slash" Flying 65) (PokemonAttack "Dragon Claw" Dragon 90) (PokemonAttack "Slash" Normal 70)

-- Damage = (((42 * AttackStat * AttackPower / DefenseStat) / 50) + 2) * STAB * Weakness/Resistance * 1

stab :: Pokemon -> PokemonAttack -> Multiplier
stab pokemon attack = case types pokemon of
  Mono monoType -> if monoType == attackType attack then 1.5 else 1
  Dual typeOne typeTwo -> if typeOne == attackType attack || typeTwo == attackType attack then 1.5 else 1

pokemonAttackDamage :: Pokemon -> Pokemon -> PokemonAttack -> Double
pokemonAttackDamage pokemonDefender attackingPokemon attackOfAttackingPokemon =
  ((42 * attackAttribute (attributes attackingPokemon) * attackDamage attackOfAttackingPokemon / defenseAttribute (attributes pokemonDefender)) / 50 + 2)
    * stab attackingPokemon attackOfAttackingPokemon
    * evaluateDamage (attackType attackOfAttackingPokemon) (types pokemonDefender)

pokemonAttacksPokemon :: Pokemon -> Pokemon -> PokemonAttack -> Pokemon
pokemonAttacksPokemon pokemonDefender attackingPokemon attackOfAttackingPokemon =
  Pokemon
    { name = name pokemonDefender,
      types = types pokemonDefender,
      attributes =
        Attributes
          { hitPoints = hitPoints (attributes pokemonDefender) - pokemonAttackDamage pokemonDefender attackingPokemon attackOfAttackingPokemon,
            attackAttribute = attackAttribute (attributes pokemonDefender),
            defenseAttribute = defenseAttribute (attributes pokemonDefender),
            speedAttribute = speedAttribute (attributes pokemonDefender)
          },
      attackOne = attackOne pokemonDefender,
      attackTwo = attackTwo pokemonDefender,
      attackThree = attackThree pokemonDefender,
      attackFour = attackFour pokemonDefender
    }

data Zipper a = Z [a] a [a] deriving (Show)

focus :: Zipper a -> a
focus (Z _ x _) = x

walkRight, walkLeft :: Zipper a -> Zipper a
walkRight z@(Z _ _ []) = z
walkRight (Z lft f (x : rgt)) = Z (f : lft) x rgt
walkLeft z@(Z [] _ _) = z
walkLeft (Z (x : lft) f rgt) = Z lft x (f : rgt)

test = Z [] pokemonOne [pokemonTwo, pokemonThree]

testR = walkRight test

testRR = walkRight testR

testRRR = walkRight testRR

printPokemon :: Pokemon -> String
printPokemon = name

-- (fmap name (focus test))

--- >>> show (name <$> focus testRR)
-- "Right \"Jolteon\""

-- swtich :: Name -> Zipper a -> Zipper a
-- swtich name z@(Z left focus right) = z

isPokemon :: Name -> Pokemon -> Bool
isPokemon targetPokemonName pokemon = name pokemon == targetPokemonName

switchPokemon :: Name -> Zipper Pokemon -> Zipper Pokemon
switchPokemon n z
  | isPokemon n (focus z) = z
  | otherwise = case moveToLeft n z of
      Just z' -> z'
      Nothing -> case moveToRight n z of
        Just z' -> z'
        Nothing -> z

moveToLeft :: Name -> Zipper Pokemon -> Maybe (Zipper Pokemon)
moveToLeft n z@(Z [] _ _) = Nothing
moveToLeft n z@(Z (x : lft) f rgt)
  | isPokemon n x = Just $ Z lft x (f : rgt)
  | otherwise = moveToLeft n (walkLeft z)

moveToRight :: Name -> Zipper Pokemon -> Maybe (Zipper Pokemon)
moveToRight n z@(Z _ _ []) = Nothing
moveToRight n z@(Z lft f (x : rgt))
  | isPokemon n x = Just $ Z (f : lft) x rgt
  | otherwise = moveToRight n (walkRight z)

extractRight :: Either a b -> b
extractRight (Right x) = x

team = Z [extractRight pokemonTwo] (extractRight pokemonOne) [extractRight pokemonThree]

teamL = switchPokemon "Lucario" team

-- >>> printPokemon $ focus (switchPokemon "Jolteon" teamL)
-- "Jolteon"
