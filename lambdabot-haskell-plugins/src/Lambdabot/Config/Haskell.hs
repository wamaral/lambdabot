{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
module Lambdabot.Config.Haskell
    ( evalPrefixes
    , languageExts
    , trustedPackages
    
    , djinnBinary
    , ghcBinary
    , ghciBinary
    , hoogleBinary
    , muevalBinary

    , maxPasteLength
    ) where

import Lambdabot.Config

config "evalPrefixes"       [t| [String]                |] [| [">"]         |]

trustedPkgs :: [String]
trustedPkgs =
    [ "array"
    , "base"
    , "bytestring"
    , "containers"
    , "random"
    , "lambdabot-trusted"
    , "text"
    , "parallel"
    , "stm"
    , "unordered-containers"
    , "hashable"
    , "exceptions"
    , "prelude-extras"
    , "base-orphans"
    , "bifunctors"
    , "profunctors"
    , "reflection"
    , "adjunctions"
    , "kan-extensions"
    , "lens"
    , "semigroups"
    , "tagged"
    , "comonad"
    ]

configWithMerge [| (++) |] "trustedPackages"    [t| [String] |] [| trustedPkgs   |]

-- extensions to enable for the interpreted expression
-- (and probably also L.hs if it doesn't already have these set)
defaultExts :: [String]
defaultExts =
    [ "ImplicitPrelude" -- workaround for bug in hint package
    , "ExtendedDefaultRules"
    , "BangPatterns"
    , "ConstrainedClassMethods"
    , "ConstraintKinds"
    , "DataKinds"
    , "DeriveDataTypeable"
    , "DeriveFoldable"
    , "DeriveFunctor"
    , "DeriveGeneric"
    , "DeriveTraversable"
    , "EmptyCase"
    , "EmptyDataDecls"
    , "ExistentialQuantification"
    , "FlexibleContexts"
    , "FlexibleInstances"
    , "FunctionalDependencies"
    , "GADTs"
    , "ImplicitParams"
    , "KindSignatures"
    , "LiberalTypeSynonyms"
    , "MagicHash"
    , "MultiParamTypeClasses"
    , "NoMonomorphismRestriction"
    , "PackageImports"
    , "ParallelListComp"
    , "PatternGuards"
    , "PolyKinds"
    , "PolymorphicComponents"
    , "PostfixOperators"
    , "RankNTypes"
    , "ScopedTypeVariables"
    , "StandaloneDeriving"
    , "TupleSections"
    , "TypeFamilies"
    , "TypeOperators"
    , "TypeSynonymInstances"
    , "UnboxedTuples"
    , "UndecidableInstances"
    , "UnicodeSyntax"
    , "ViewPatterns"
    ]

configWithMerge [| (++) |] "languageExts"   [t| [String] |] [| defaultExts |]


config "djinnBinary"        [t| String                  |] [| "djinn"       |]
config "ghcBinary"          [t| String                  |] [| "ghc"         |]
config "ghciBinary"         [t| String                  |] [| "ghci"        |]
config "hoogleBinary"       [t| String                  |] [| "hoogle"      |]
config "muevalBinary"       [t| String                  |] [| "mueval"      |]

config "maxPasteLength"     [t| Int                     |] [| 4096 :: Int   |]

