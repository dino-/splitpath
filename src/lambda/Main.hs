{-# LANGUAGE QuasiQuotes #-}

import Control.Monad ( when )
import qualified System.Console.Docopt as DO
import System.Environment ( getArgs )
import System.Exit ( exitFailure, exitSuccess, exitWith )
import System.IO ( hPutStrLn, stderr )
import System.Process ( system )
import Text.Printf ( printf )


patterns :: DO.Docopt
patterns = [DO.docopt|
lambda v1.0

Usage:
  lambda [-h]
  lambda [-v] <COMMAND> [<ARG>] [<ARG>] ...

Options:
  -h, --help      This help info
  -v, --verbose   Be verbose
|]


main :: IO ()
main = do
  args <- DO.parseArgsOrExit patterns =<< getArgs
  handleHelp patterns args

  userCommand <- maybe (exitWithMsg "ERROR: COMMAND required") return
    $ DO.getArg args (DO.argument "COMMAND")

  let userArgs = unwords $ DO.getAllArgs args (DO.argument "ARG")

  when (DO.isPresent args $ DO.longOption "verbose") $ do
    putStrLn $ "COMMAND: " ++ userCommand
    putStrLn $ "ARGS: " ++ userArgs

  let script = printf "function lambdaf() { %s; }; lambdaf %s"
        userCommand userArgs
  exitCode <- system script
  exitWith exitCode


handleHelp :: DO.Docopt -> DO.Arguments -> IO ()
handleHelp patterns' args =
  when (DO.isPresent args $ DO.longOption "help") $ do
    putStrLn $ DO.usage patterns'
    exitSuccess


exitWithMsg :: String -> IO a
exitWithMsg msg = hPutStrLn stderr msg >> exitFailure
