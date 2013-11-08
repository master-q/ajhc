{- Jahm: Jhc and Ajhc Haskell libraries Manager -}
module Main where
import Data.Maybe
import Data.List
import Control.Monad
import System.Environment (getArgs)
import System.Directory
import System.FilePath ((</>))
import System.Exit
import Network.URI (parseURI)
import Distribution.Client.HttpUtils (downloadURI)
import Distribution.Verbosity (verbose)
import GenUtil (iocatch)
import Support.CompatMingw32 (systemCompat)
import Support.Cabal
import Version.Version

main :: IO ()
main = getArgs >>= mainWorker

mainWorker :: [String] -> IO ()
mainWorker ("downloadURI":u:f:[]) = do
  curDir <- getCurrentDirectory
  let Just url = parseURI u
  retry $ downloadURI verbose url $ curDir </> f
    where retry io = io `iocatch` const io

mainWorker ("make":a) = do
  mayGmake <- findExecutable "gmake"
  e <- systemCompat $ fromMaybe "make" mayGmake ++ " " ++ unwords a
  when (e /= ExitSuccess) $ exitWith e

mainWorker ("makefile":_) = do -- xxx for test
  -- create dir
  home <- getHomeDirectory
  let f ' ' = '_'
      f a   = a
      basedir  = home </> ".ajhc" </> "jahm" </> fmap f (delete '(' . delete ')' $ versionSimple)
      libdir   = basedir </> "lib"
      builddir = basedir </> "build"
  createDirectoryIfMissing True libdir
  createDirectoryIfMissing True builddir
  -- find cabal and yaml files

mainWorker _ = print "help"
