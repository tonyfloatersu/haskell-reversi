{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_haskell_reversi (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "D:\\Programming\\haskell\\haskell-reversi\\.stack-work\\install\\5600629f\\bin"
libdir     = "D:\\Programming\\haskell\\haskell-reversi\\.stack-work\\install\\5600629f\\lib\\x86_64-windows-ghc-8.0.2\\haskell-reversi-0.1.0.0"
dynlibdir  = "D:\\Programming\\haskell\\haskell-reversi\\.stack-work\\install\\5600629f\\lib\\x86_64-windows-ghc-8.0.2"
datadir    = "D:\\Programming\\haskell\\haskell-reversi\\.stack-work\\install\\5600629f\\share\\x86_64-windows-ghc-8.0.2\\haskell-reversi-0.1.0.0"
libexecdir = "D:\\Programming\\haskell\\haskell-reversi\\.stack-work\\install\\5600629f\\libexec"
sysconfdir = "D:\\Programming\\haskell\\haskell-reversi\\.stack-work\\install\\5600629f\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "haskell_reversi_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "haskell_reversi_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "haskell_reversi_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "haskell_reversi_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "haskell_reversi_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "haskell_reversi_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
