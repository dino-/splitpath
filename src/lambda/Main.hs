{-# LANGUAGE QuasiQuotes #-}

import Control.Monad ( when )
import System.Console.Docopt
import System.Environment ( getArgs )
import System.Exit ( exitSuccess, exitWith )
import System.Process ( system )
import Text.Printf ( printf )


patterns :: Docopt
patterns = [docopt|
lambda v1.0

Usage:
  lambda [-h]
  lambda [-v] <COMMAND> [<ARG>] [<ARG>] ...

Options:
  -h, --help      This help info
  -v, --verbose   Be verbose
|]

getArgOrExit = getArgOrExitWith patterns


main :: IO ()
main = do
  args <- parseArgsOrExit patterns =<< getArgs
  print args

  when (isPresent args (longOption "help")) $ do
    putStrLn $ usage patterns
    exitSuccess

  --(command : args) <- getArgs
  print $ getArg args (argument "COMMAND")
  print $ getAllArgs args (argument "ARG")

  {- For debugging
  putStr "Command: "
  print command
  putStr "Arguments: "
  print . unwords $ args
  -}

  {-
  let script = printf "function lambdaf() { %s; }; lambdaf %s" command (unwords args)
  exitCode <- system script
  exitWith exitCode
  -}
