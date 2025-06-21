{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_hasql (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude


#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [1,9,1,2] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/deepak/.cabal/bin"
libdir     = "/home/deepak/.cabal/lib/aarch64-linux-ghc-9.12.2-632a/hasql-1.9.1.2-inplace"
dynlibdir  = "/home/deepak/.cabal/lib/aarch64-linux-ghc-9.12.2-632a"
datadir    = "/home/deepak/.cabal/share/aarch64-linux-ghc-9.12.2-632a/hasql-1.9.1.2"
libexecdir = "/home/deepak/.cabal/libexec/aarch64-linux-ghc-9.12.2-632a/hasql-1.9.1.2"
sysconfdir = "/home/deepak/.cabal/etc"

getBinDir     = catchIO (getEnv "hasql_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "hasql_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "hasql_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "hasql_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hasql_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hasql_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
