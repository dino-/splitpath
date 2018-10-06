module Shutils.Opts
  (handleHelp, exitWithMsg )
  where

import Control.Monad ( when )
import System.Console.Docopt ( Arguments, Docopt, isPresent, longOption, usage )
import System.Exit ( exitFailure, exitSuccess )
import System.IO ( hPutStrLn, stderr )


handleHelp :: Docopt -> Arguments -> IO ()
handleHelp patterns args =
  when (isPresent args $ longOption "help") $ do
    putStrLn $ usage patterns
    exitSuccess


exitWithMsg :: String -> IO a
exitWithMsg msg = hPutStrLn stderr msg >> exitFailure
