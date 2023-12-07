{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_monomer_starter (
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
bindir     = "/home/streitmancm/project/calculator/.stack-work/install/x86_64-linux/3541ba6709209ff489332d91899a6dd2bd7e178f8e3e85c5094d7817dc6b4d42/9.2.5/bin"
libdir     = "/home/streitmancm/project/calculator/.stack-work/install/x86_64-linux/3541ba6709209ff489332d91899a6dd2bd7e178f8e3e85c5094d7817dc6b4d42/9.2.5/lib/x86_64-linux-ghc-9.2.5/monomer-starter-0.1.0.0-9olCwsnGiUkEj2ykVhw1ZW-app"
dynlibdir  = "/home/streitmancm/project/calculator/.stack-work/install/x86_64-linux/3541ba6709209ff489332d91899a6dd2bd7e178f8e3e85c5094d7817dc6b4d42/9.2.5/lib/x86_64-linux-ghc-9.2.5"
datadir    = "/home/streitmancm/project/calculator/.stack-work/install/x86_64-linux/3541ba6709209ff489332d91899a6dd2bd7e178f8e3e85c5094d7817dc6b4d42/9.2.5/share/x86_64-linux-ghc-9.2.5/monomer-starter-0.1.0.0"
libexecdir = "/home/streitmancm/project/calculator/.stack-work/install/x86_64-linux/3541ba6709209ff489332d91899a6dd2bd7e178f8e3e85c5094d7817dc6b4d42/9.2.5/libexec/x86_64-linux-ghc-9.2.5/monomer-starter-0.1.0.0"
sysconfdir = "/home/streitmancm/project/calculator/.stack-work/install/x86_64-linux/3541ba6709209ff489332d91899a6dd2bd7e178f8e3e85c5094d7817dc6b4d42/9.2.5/etc"

getBinDir     = catchIO (getEnv "monomer_starter_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "monomer_starter_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "monomer_starter_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "monomer_starter_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "monomer_starter_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "monomer_starter_sysconfdir") (\_ -> return sysconfdir)




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
