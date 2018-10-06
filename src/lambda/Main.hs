{-# LANGUAGE QuasiQuotes #-}

import Control.Monad ( when )
import Data.Time ( formatTime, getCurrentTime )
import Data.Time.Format ( defaultTimeLocale )
import qualified System.Console.Docopt as DO
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess, exitWith )
import System.IO ( hPutStrLn, stderr )
import System.Posix.Process ( getProcessID )
import System.Process ( system )
import Text.Printf ( printf )


patterns :: DO.Docopt
patterns = [DO.docopt|
lambda v1.0

Wrap shell commands in a lambda function

Usage:
  lambda [-h]
  lambda [-v | -vv] <COMMAND> [<ARG>] [<ARG>] ...

Options:
  -h, --help      This help info
  -v, --verbose   Echo back the command and args list before executing
                  Pass twice to echo back the script function as well

This is useful when you need to repeatedly modify an argument buried in the
middle of a long command. lambda lets you pull the arguments out to the end of
the command line where it's easier to edit them. It can also be used as a
`flip` function to reorder arguments.

Use the customary $1, $2... variables. Quoting can be difficult. For debugging
your commands, put 'echo ' in front of them and try running with --verbose

Example:

  $ lambda 'find $2 -name $1' *.log /var/log

The exit code will be that of COMMAND
|]


main :: IO ()
main = do
  args <- DO.parseArgsOrExit patterns =<< getArgs
  handleHelp args

  userCommand <- maybe (exitWithMsg "ERROR: COMMAND required") return
    $ DO.getArg args (DO.argument "COMMAND")

  let userArgs = unwords $ DO.getAllArgs args (DO.argument "ARG")

  tempName <- mkTempName
  let script = printf "function %s() { %s; }; %s %s"
        tempName userCommand tempName userArgs

  when (DO.isPresent args $ DO.longOption "verbose") $ do
    putStrLn $ "COMMAND: " ++ userCommand
    putStrLn $ "ARGS: " ++ userArgs
    when ((DO.getArgCount args $ DO.longOption "verbose") > 1) $ do
      putStrLn $ "SCRIPT: " ++ script
    putStrLn ""

  system script >>= exitWith


handleHelp :: DO.Arguments -> IO ()
handleHelp args =
  when (DO.isPresent args $ DO.longOption "help") $ do
    putStrLn $ DO.usage patterns
    exitSuccess


exitWithMsg :: String -> IO a
exitWithMsg msg = hPutStrLn stderr msg >> exitFailure


mkTempName :: IO String
mkTempName = printf "%s_%s_%s"
  <$> getProgName
  <*> (show <$> getProcessID)
  <*> (formatTime defaultTimeLocale "%s" <$> getCurrentTime)
