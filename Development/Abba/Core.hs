module Development.Abba.Core
  ( buildTargets
  , shell
  , clean
  ) where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Query.BFS
import Data.List
import Data.Maybe
import Data.Ord
import Data.Set
  ( Set
  )
import qualified Data.Set as Set

import Foreign.Marshal.Error

import System.Directory
import System.Process hiding (shell)

import Text.Printf

import Development.Abba.DependencyGraph
import Development.Abba.Types

buildTargets
    :: Set Rule
    -> DependencyGraph
    -> [Dependency]
    -> IO ()
buildTargets rules dependencyGraph targets = do
    let
        orderedNodes
            :: [[Node]]
            = map (\n -> reverse $ (bfs n dependencyGraph)) startNodes
        orderedTargets
            :: [[Dependency]]
            = (map . map) (\n -> fromJust $ lookup n lNodes) orderedNodes
        orderedRules
            :: [[Rule]]
            = (map . map)
                (\t -> fromJust $ find (t `isTargetOf`) (Set.toList rules))
                orderedTargets
    mapM_ (mapM_ build) orderedRules
  where
    startNodes
        :: [Node]
        = map (\t -> errorOnNothing (printf "no rule to build target %s" t)
                   $ getNodeFromDependency lNodes t)
              targets
    
    lNodes
        :: [DependencyNode]
        = labNodes dependencyGraph

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



