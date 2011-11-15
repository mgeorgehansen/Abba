module Main (
    main
  ) where
import qualified Data.Set as Set

import Development.Abba

import System.Environment

import Text.Printf

main = do
    let
        cxxFlags = ["-std=c++0x"]
        
        rules = Set.fromList [
            Rule ["all"] ["foo"] Nothing
          , Rule ["clean"] [] $ Just $ \_ _ -> do
                clean "foo"
                clean "foo.o"
                clean "main.o"
          , Rule ["foo"] ["main.o", "foo.o"] $ Just
                $ buildCxxExecutable cxxFlags
          , Rule ["foo.o"] ["foo.cxx"] $ Just
                $ buildCxxObject cxxFlags
          , Rule ["main.o"] ["main.cxx"] $ Just
                $ buildCxxObject cxxFlags
          ]
    initialTargets <- getArgs
    let
        (dependencyGraph, nodeMapper, vertexMapper)
            = constructDependencyGraph rules
    buildTargets dependencyGraph vertexMapper nodeMapper initialTargets
  where
    buildCxxExecutable cxxFlags targets dependencies
        = shell "g++"
            $  [ "-o", targets !! 0
               ]
            ++ cxxFlags
            ++ dependencies
    buildCxxObject cxxFlags targets dependencies
        = shell "g++"
            $  [ "-c"
               , "-o", targets !! 0
               ]
            ++ cxxFlags
            ++ dependencies

