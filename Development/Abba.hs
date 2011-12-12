{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables #-}
module Development.Abba (
    Recipe
  , Rule (..)
  , constructDependencyGraph
  , buildTargets
  , shell
  , clean
  ) where

import Data.Graph
import Data.Maybe
import Data.Ord
import Data.Set (
    Set
  )
import qualified Data.Set as Set
import Data.Tree

import Foreign.Marshal.Error

import System.Directory
import System.Process hiding (shell)

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

-- |Construct a dependency 'Graph' from a set of 'Rule's.
constructDependencyGraph
    :: Set Rule
    -- ^Set of 'Rule's from which to construct the dependency graph.
    -> (Graph, NodeMapper Dependency Rule, VertexMapper Dependency)
    -- ^Three-Tuple containing the complete 'Graph', a 'NodeMapper' from
    --  'Dependency's to 'Rule's, and a 'VertexMapper' over 'Dependency's.
constructDependencyGraph rules
    = graphFromEdges nodes
  where
    nodes :: [GraphNode Dependency Rule]
        = concat $ map constructNodes (Set.elems rules)

-- |Construct a 'GraphNode' from a 'Rule'.
constructNodes
    :: Rule
    -- ^'Rule' from which to construct the 'GraphNode'.
    -> [GraphNode Dependency Rule]
    -- ^Resulting list of 'GraphNode's, one for each target of the 'Rule'.
constructNodes rule@(Rule {targets, dependencies})
    = map (\target -> (rule, target, dependencies)) targets

-- |A node within a 'Graph'; alternatively, an association between edges
--  within a 'Graph' (depending upon your viewpoint).
type GraphNode key content
    = (content, key, [key])
    -- ^Three-Tuple containing the content of a node, it's key and the keys
    --  of all child nodes, respectively.

-- |Mapping function from keys to 'Vertex's for a particular 'Graph'.
type VertexMapper key
    =  key
    -- ^Key to map.
    -> Maybe Vertex
    -- ^Vertex mapped to the key.

-- |Mapping function from 'Vertex's to 'GraphNode's for a particular 'Graph'.
type NodeMapper key content
    =  Vertex
    -- ^'Vertex' referring to the node we want to retrieve.
    -> GraphNode key content
    -- ^'GraphNode' referred to by the 'Vertex'.

buildTargets
    :: Graph
    -> VertexMapper Dependency
    -> NodeMapper Dependency Rule
    -> [Dependency]
    -> IO ()
buildTargets dependencyGraph vertexMapper nodeMapper targets = do
    let
        orderedVertices :: [[Vertex]]
            = map (reverse . flatten) $ dfs dependencyGraph startVertices
        orderedNodes :: [[GraphNode Dependency Rule]]
            = map (map nodeMapper) orderedVertices
        orderedRules :: [[Rule]]
            = map (map (\(rule, _, _) -> rule)) orderedNodes
    mapM_ (mapM_ build) orderedRules
  where
    startVertices
        :: [Vertex]
        = map getStartVertex targets
    
    getStartVertex
        :: String
        -> Vertex
    getStartVertex target
        = (errorOnNothing (printf "no rule to build target %s" target))
            $ vertexMapper target

build
    :: Rule
    -> IO ()
build (Rule {targets, dependencies, recipe=maybeRecipe})
    = case maybeRecipe of
        Just recipe -> do
            anyOutOfSync :: Bool
                <- fmap or $ mapM isOutOfSync (dependencies ++ targets)
            if anyOutOfSync
              then
                recipe targets dependencies
              else
                return ()
        
        Nothing -> return ()

isTargetOf
    :: String
    -> Rule
    -> Bool
isTargetOf target (Rule {targets})
    = target `elem` targets

-- |Determine if a node is out of sync with other nodes.
isOutOfSync
    :: String
    -> IO Bool
isOutOfSync _ = return True

errorOnNothing
    :: String
    -> Maybe a
    -> a
errorOnNothing errorMessage maybeValue
    = case maybeValue of
        Just value -> value
        Nothing -> error errorMessage

shell
    :: String
    -> [String]
    -> IO ()
shell command arguments
    = do
        putStrLn $ printf "executing: %s %s" command (unwords arguments)
        void $ rawSystem command arguments

-- |Remove a file or directory, logging the action.
--  
--  This is intended to be used in "clean" rules, where built files are
--  removed.
clean
    :: FilePath
    -> IO ()
clean filePath = do
    fileExists <- doesFileExist filePath
    directoryExists <- doesDirectoryExist filePath
    if fileExists || directoryExists
        then do
            putStrLn $ printf "cleaning %s..." filePath
            if fileExists
                then removeFile filePath
                else removeDirectory filePath
        else return ()


