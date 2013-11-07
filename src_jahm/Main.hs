{- Jahm: Jhc and Ajhc Haskell libraries Manager -}
module Main where
import Data.Maybe
import Control.Monad
import System.Environment (getArgs)
import System.Directory (getCurrentDirectory, findExecutable)
import System.FilePath ((</>))
import System.Exit
import Network.URI (parseURI)
import Distribution.Client.HttpUtils (downloadURI)
import Distribution.Verbosity (verbose)
import GenUtil (iocatch)
import Support.CompatMingw32 (systemCompat)

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

mainWorker _ = print "help"
