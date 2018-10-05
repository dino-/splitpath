import System.Environment ( getArgs )
import System.Exit ( exitWith )
import System.Process ( system )
import Text.Printf ( printf )


main :: IO ()
main = do
  (command : args) <- getArgs

  {- For debugging
  putStr "Command: "
  print command
  putStr "Arguments: "
  print . unwords $ args
  -}

  let script = printf "function lambdaf() { %s; }; lambdaf %s" command (unwords args)
  exitCode <- system script
  exitWith exitCode
