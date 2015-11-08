-- The code is mostly ripped from
-- https://github.com/ekmett/lens/blob/d1d97469f0e93c1d3535308954a060e8a04e37aa/tests/doctests.hsc
import BasePrelude
import System.Directory
import System.FilePath
import Test.DocTest
import Build_doctest (deps)

main :: IO ()
main = do
  sources <- getSources
  doctest $ dfltParams ++ map ("-package="++) deps ++ sources
  where
    dfltParams = 
      [
        "-isrc",
        "-idist/build/autogen",
        "-optP-include",
        "-optPdist/build/autogen/cabal_macros.h",
        "-XArrows",
        "-XBangPatterns",
        "-XConstraintKinds",
        "-XDataKinds",
        "-XDefaultSignatures",
        "-XDeriveDataTypeable",
        "-XDeriveFunctor",
        "-XDeriveGeneric",
        "-XEmptyDataDecls",
        "-XFlexibleContexts",
        "-XFlexibleInstances",
        "-XFunctionalDependencies",
        "-XGADTs",
        "-XGeneralizedNewtypeDeriving",
        "-XImpredicativeTypes",
        "-XLambdaCase",
        "-XLiberalTypeSynonyms",
        "-XMultiParamTypeClasses",
        "-XMultiWayIf",
        "-XNoImplicitPrelude",
        "-XNoMonomorphismRestriction",
        "-XOverloadedStrings",
        "-XPatternGuards",
        "-XParallelListComp",
        "-XQuasiQuotes",
        "-XRankNTypes",
        "-XRecordWildCards",
        "-XScopedTypeVariables",
        "-XStandaloneDeriving",
        "-XTemplateHaskell",
        "-XTupleSections",
        "-XTypeFamilies",
        "-XTypeOperators",
        "-hide-all-packages"
      ]

getSources :: IO [FilePath]
getSources = filter (isSuffixOf ".hs") <$> go "library"
  where
    go dir = do
      (dirs, files) <- getFilesAndDirectories dir
      (files ++) . concat <$> mapM go dirs

getFilesAndDirectories :: FilePath -> IO ([FilePath], [FilePath])
getFilesAndDirectories dir = do
  c <- map (dir </>) . filter (`notElem` ["..", "."]) <$> getDirectoryContents dir
  (,) <$> filterM doesDirectoryExist c <*> filterM doesFileExist c
