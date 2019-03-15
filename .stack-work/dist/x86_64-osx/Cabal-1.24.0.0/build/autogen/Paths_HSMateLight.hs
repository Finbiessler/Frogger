{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_HSMateLight (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
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
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/finbiess/Documents/GitHub/Frogger/.stack-work/install/x86_64-osx/lts-7.14/8.0.1/bin"
libdir     = "/Users/finbiess/Documents/GitHub/Frogger/.stack-work/install/x86_64-osx/lts-7.14/8.0.1/lib/x86_64-osx-ghc-8.0.1/HSMateLight-0.1.0.0-GOpBE4sQsWf2XLl7DpmCkA"
datadir    = "/Users/finbiess/Documents/GitHub/Frogger/.stack-work/install/x86_64-osx/lts-7.14/8.0.1/share/x86_64-osx-ghc-8.0.1/HSMateLight-0.1.0.0"
libexecdir = "/Users/finbiess/Documents/GitHub/Frogger/.stack-work/install/x86_64-osx/lts-7.14/8.0.1/libexec"
sysconfdir = "/Users/finbiess/Documents/GitHub/Frogger/.stack-work/install/x86_64-osx/lts-7.14/8.0.1/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "HSMateLight_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "HSMateLight_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "HSMateLight_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "HSMateLight_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "HSMateLight_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
