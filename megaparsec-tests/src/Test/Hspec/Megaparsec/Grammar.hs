-- | This module provides a way to generate arbitrary grammars and convert them --   to Megaparsec parsers and to QuickCheck input generators that the parser --   can parse completely.
module Test.Hspec.Megaparsec.Grammar
  ( -- * Grammar data type
    Grammar (..),

    -- * Grammar converter functions
    toParsec,
    toInputGen,

    -- * Grammar analysis functions
    startDifferent,
  )
where

import Data.Foldable (find)
import Data.Maybe (isJust, isNothing)
import Data.Void (Void)
import Test.QuickCheck
  ( Arbitrary,
    Gen,
    arbitrary,
    chooseInt,
    frequency,
    oneof,
    resize,
    shrink,
    sized,
    suchThat,
    vectorOf,
  )
import Text.Megaparsec (Parsec)
import qualified Text.Megaparsec as MP

-- | A data type representing a grammar.
-- The grammar has two primitive constructors:
--   - AnyToken: matches any token in the input.
--   - Token t: matches the token t in the input.
-- To compose more complex grammars, the following constructors are provided:
--   - Many g: matches zero or more repetitions of the grammar g.
--   - Choice gs: matches the first possible of the grammars in the list gs.
--   - Sequence gs: matches the grammars in the list gs in order.
-- The grammar works for generic token types.
data Grammar t
  = AnyToken
  | Token t
  | Many (Grammar t)
  | Choice [Grammar t]
  | Sequence [Grammar t]
  deriving (Show, Read, Eq)

instance Functor Grammar where
  fmap _ AnyToken = AnyToken
  fmap f (Token t) = Token (f t)
  fmap f (Many g) = Many (fmap f g)
  fmap f (Choice gs) = Choice (map (fmap f) gs)
  fmap f (Sequence gs) = Sequence (map (fmap f) gs)

instance (Arbitrary t, Eq t) => Arbitrary (Grammar t) where
  arbitrary = resize 10 $ sized arbitraryGrammar

  shrink (Sequence [g]) = shrink g
  shrink (Sequence gs) = concatMap shrink gs
  shrink (Choice gs) = concatMap shrink gs
  shrink (Many g) = shrink g
  shrink _ = []

-- | Converts a grammar to a Megaparsec parser.
toParsec :: (Ord t) => Grammar t -> Parsec Void [t] [t]
toParsec AnyToken = MP.anySingle >>= \token -> return [token]
toParsec (Token t) = MP.single t >>= \token -> return [token]
toParsec (Many g) = concat <$> MP.many (MP.try . toParsec $ g)
toParsec (Choice gs) = MP.choice (fmap (MP.try . toParsec) gs)
toParsec (Sequence gs) = concat <$> mapM toParsec gs

-- Obtain an input generator for the given grammar.
-- If the
toInputGen :: (Arbitrary t, Ord t) => Grammar t -> Gen [t]
toInputGen AnyToken = (: []) <$> arbitrary
toInputGen (Token t) = return [t]
toInputGen (Many g) = concat <$> (reps >>= flip vectorOf (toInputGen g))
  where
    -- arbirary number, big enough for covering the edge cases,
    -- small enough to keep performance stable
    reps = chooseInt (0, 4)
toInputGen (Choice gs) = oneof (map toInputGen gs)
toInputGen (Sequence gs) = concat <$> mapM toInputGen gs

-- Determine whether the two grammars start with a disjoint set of first tokens.
startDifferent :: (Eq t) => Grammar t -> Grammar t -> Bool
startDifferent g1 g2 = disjointSets set1 set2
  where
    set1 = findFirstToken g1
    set2 = findFirstToken g2

    tokenEq _ AnyToken = True
    tokenEq AnyToken _ = True
    tokenEq a b = a == b

    disjointSets s1 s2 =
      isNothing $ find (\t1 -> isJust $ find (\t2 -> tokenEq t1 t2) s1) s2

-- Find the set of first tokens that can be recognized by the grammar.
-- Returns a list of `Token` and `AnyToken`.
findFirstToken :: Grammar t -> [Grammar t]
findFirstToken t@(Token _) = [t]
findFirstToken t@AnyToken = [t]
findFirstToken (Choice gs) = concatMap findFirstToken gs
findFirstToken (Sequence []) = []
findFirstToken (Sequence (g : gs)) =
  findFirstToken g
    ++ if (not . consumesInput) g
      then findFirstToken (Sequence gs)
      else []
findFirstToken (Many g) = findFirstToken g

-- Whether the grammar might not consume input
consumesInput :: Grammar t -> Bool
consumesInput (Many _) = False
consumesInput (Token _) = True
consumesInput AnyToken = True
consumesInput (Sequence gs) = any consumesInput gs
consumesInput (Choice gs) = all consumesInput gs

arbitraryGrammar :: (Arbitrary t, Eq t) => Int -> Gen (Grammar t)
arbitraryGrammar size | size <= 1 = Token <$> arbitrary
arbitraryGrammar size =
  frequency
    [ (1, return AnyToken),
      (1, Token <$> arbitrary),
      (1, some <$> suchThat (vectorOf 2 subGrammar) areDifferent),
      (1, Choice <$> suchThat (vectorOf 2 subGrammar) areDifferent),
      (1, Sequence <$> vectorOf 2 subGrammar)
    ]
  where
    -- Limit the size of grammars
    subGrammar = arbitraryGrammar (size - 1)

    -- A sequence of many g1 and g2
    some [g1, g2] = Sequence [Many g1, g2]
    some _ = error "Some takes two subgrammars"

    -- Call `startDifferent` with the two grammars
    areDifferent [g1, g2] = startDifferent g1 g2
    areDifferent _ = error "choice must have two options"
