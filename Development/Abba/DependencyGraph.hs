module Development.Abba.DependencyGraph
  ( DependencyGraph
  , DependencyNode
  , DependencyEdge
  , constructDependencyGraph
  , getNodeFromDependency
  ) where

import Data.Graph.Inductive
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

import Ix

import Development.Abba.Types

type DependencyGraph = Gr Dependency ()
type DependencyNode  = LNode Dependency
type DependencyEdge  = UEdge

constructDependencyGraph
    :: Set Rule
    -> DependencyGraph
constructDependencyGraph rules
    = mkGraph lNodes uEdges
  where
    lNodes
        :: [DependencyNode]
        = fst $ mkLNodes 0 (Set.toList rules)
    
    uEdges
        :: [DependencyEdge]
        = concat $ map (mkUEdge lNodes) (Set.toList rules)

mkUEdge
    :: [DependencyNode]
    -> Rule
    -> [DependencyEdge]
mkUEdge lNodes (Rule {targets, dependencies})
    = concat
        [ [ (targetNode, dependencyNode, ())
          | targetNode <- targetNodes
          ]
        | dependencyNode <- dependencyNodes
        ]
  where
    targetNodes
        :: [Node]
        = map (fromJust . getNodeFromDependency lNodes) targets
    dependencyNodes
        :: [Node]
        = mapMaybe (getNodeFromDependency lNodes) dependencies

mkLNodes
    :: Node
    -> [Rule]
    -> ([DependencyNode], Node)
mkLNodes startNode [] = ([], startNode)
mkLNodes startNode (Rule {targets}:xs)
    = (allLNodes, endNode)
  where
    lNodes
        :: [DependencyNode]
        = [ (node, target)
          | target <- targets
          | node   <- range (startNode, endNode - 1) ]
    
    allLNodes
        :: [DependencyNode]
        = lNodes ++ (fst $ mkLNodes endNode xs)
    
    endNode
        :: Node
        = startNode + (length targets)

getNodeFromDependency
    :: [DependencyNode]
    -> Dependency
    -> Maybe Node
getNodeFromDependency lNodes dependency
    = lookup dependency $ map reverseTuple lNodes

reverseTuple
    :: (a, b)
    -> (b, a)
reverseTuple (a, b)
    = (b, a)

