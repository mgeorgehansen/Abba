module Development.Abba.Types
  ( Rule (..)
  , Dependency
  , Recipe
  ) where

import qualified Data.Set as Set

import Text.Printf

-- |Make-like rule definition, designed to be interpreted to produce one or
--  more targets from zero or more dependencies, optionally using the supplied
--  'Recipe'.
data Rule = Rule {
    targets      :: [Dependency]
  -- ^List of targets that this 'Rule' builds.
  , dependencies :: [Dependency]
  -- ^List of dependencies that the targets require.
  , recipe       :: Maybe Recipe
  -- ^'Recipe' to build the targets from the dependencies, if any.
  }

-- Define a simple method for showing a 'Rule' to make debugging easier.
instance Show Rule where
    show (Rule {targets, dependencies, recipe})
        = printf "Rule {targets=%s, dependencies=%s, recipe=%s}"
            (show targets) (show dependencies) shownRecipe
      where
        shownRecipe = case recipe of
            Just _  -> "Just Recipe(..)"
            Nothing -> "Nothing"

-- 'Rule' equality is uniquely determined by its targets to prevent conflicts.
instance Eq Rule where
    (Rule {targets=targetsA}) == (Rule {targets=targetsB})
        = targetsA == targetsB

-- 'Rule's are ordered by their targets first, then by their dependencies.
instance Ord Rule where
    compare
        (Rule {targets=targetsA, dependencies=dependenciesA})
        (Rule {targets=targetsB, dependencies=dependenciesB})
        = case compare targetsA targetsB of
            EQ   -> compare dependenciesA dependenciesB
            comp -> comp

-- |A function which builds a list of targets from a list of dependencies,
--  often producing filesystem side effects.
type Recipe
    =  [Dependency]
    -- ^List of targets to build.
    -> [Dependency]
    -- ^List of dependencies required to build the targets.
    -> IO ()

-- |A target or dependency within a 'Rule'.
type Dependency
    =  String

