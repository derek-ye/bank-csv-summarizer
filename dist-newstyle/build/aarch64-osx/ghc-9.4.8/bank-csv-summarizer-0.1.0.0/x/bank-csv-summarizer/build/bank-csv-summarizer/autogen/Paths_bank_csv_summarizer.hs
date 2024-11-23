{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_bank_csv_summarizer (
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
version = Version [0,1,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/derek/.cabal/bin"
libdir     = "/Users/derek/.cabal/lib/aarch64-osx-ghc-9.4.8/bank-csv-summarizer-0.1.0.0-inplace-bank-csv-summarizer"
dynlibdir  = "/Users/derek/.cabal/lib/aarch64-osx-ghc-9.4.8"
datadir    = "/Users/derek/.cabal/share/aarch64-osx-ghc-9.4.8/bank-csv-summarizer-0.1.0.0"
libexecdir = "/Users/derek/.cabal/libexec/aarch64-osx-ghc-9.4.8/bank-csv-summarizer-0.1.0.0"
sysconfdir = "/Users/derek/.cabal/etc"

getBinDir     = catchIO (getEnv "bank_csv_summarizer_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "bank_csv_summarizer_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "bank_csv_summarizer_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "bank_csv_summarizer_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "bank_csv_summarizer_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "bank_csv_summarizer_sysconfdir") (\_ -> return sysconfdir)



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
