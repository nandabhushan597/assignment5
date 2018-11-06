{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_javai (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
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
version = Version [0,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/nandabhushan/Library/Haskell/bin"
libdir     = "/Users/nandabhushan/Library/Haskell/ghc-8.4.3-x86_64/lib/javai-0.1"
dynlibdir  = "/Users/nandabhushan/Library/Haskell/ghc-8.4.3-x86_64/lib/x86_64-osx-ghc-8.4.3"
datadir    = "/Users/nandabhushan/Library/Haskell/share/ghc-8.4.3-x86_64/javai-0.1"
libexecdir = "/Users/nandabhushan/Library/Haskell/libexec/x86_64-osx-ghc-8.4.3/javai-0.1"
sysconfdir = "/Users/nandabhushan/Library/Haskell/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "javai_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "javai_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "javai_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "javai_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "javai_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "javai_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
