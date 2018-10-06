{-# LANGUAGE QuasiQuotes #-}

import Control.Monad ( when )
import System.Console.Docopt (
  Arguments, Docopt,
  argument, docopt, getArg, isPresent,
  longOption, parseArgsOrExit, usage )
import System.Environment ( getArgs )
import System.Exit ( exitFailure, exitSuccess )
import System.FilePath (
  dropExtension, dropExtensions, dropFileName, takeBaseName,
  takeDirectory, takeExtension, takeExtensions, takeFileName )
import System.IO ( hPutStrLn, stderr )


patterns :: Docopt
patterns = [docopt|
splitpath v1.0

Comprehensive path splitting utility

Usage:
  splitpath [options] <PATH>

Options:
  --takedirectory     Get the directory name (move up one level)
  --dropfilename      Drop the filename, leave the trailing path separator
  --takeextension     Get the extension
  --dropextension     Drop the extension
  --takeextensions    Get all extensions
  --dropextensions    Drop all extensions
  --takebasename      Get the base name, without an extention or path
  --takefilename      Get the file name
  -h, --help          This help info

The standard tools for separating file paths in bash are primitive and lacking
features. There are also problems when spaces are present in the path. On the
other hand, many programming language standard libraries have powerful tools
for breaking apart file paths. This utility wraps the functionality present in
Haskell's System.FilePath library, exposing this behavior as a simple utility
with switches.

Examples:

  $ splitpath --takedirectory  /foo/bar/baz.tar.gz  # /foo/bar
  $ splitpath --dropfilename   /foo/bar/baz.tar.gz  # /foo/bar/
  $ splitpath --takeextension  /foo/bar/baz.tar.gz  # .gz
  $ splitpath --dropextension  /foo/bar/baz.tar.gz  # /foo/bar/baz.tar
  $ splitpath --takeextensions /foo/bar/baz.tar.gz  # .tar.gz
  $ splitpath --dropextensions /foo/bar/baz.tar.gz  # /foo/bar/baz
  $ splitpath --takebasename   /foo/bar/baz.tar.gz  # baz.tar
  $ splitpath --takefilename   /foo/bar/baz.tar.gz  # baz.tar.gz
|]


main :: IO ()
main = do
  args <- parseArgsOrExit patterns =<< getArgs
  handleHelp args

  path <- maybe (exitWithMsg "ERROR: PATH required") return
    $ getArg args (argument "PATH")

  either exitWithMsg putStrLn $ extractPart args path


extractPart :: Arguments -> FilePath -> Either String FilePath
extractPart args path
  | isPresent args $ longOption "takedirectory" = Right . takeDirectory $ path
  | isPresent args $ longOption "dropfilename" = Right . dropFileName $ path
  | isPresent args $ longOption "takeextension" = Right . takeExtension $ path
  | isPresent args $ longOption "dropextension" = Right . dropExtension $ path
  | isPresent args $ longOption "takeextensions" = Right . takeExtensions $ path
  | isPresent args $ longOption "dropextensions" = Right . dropExtensions $ path
  | isPresent args $ longOption "takebasename" = Right . takeBaseName $ path
  | isPresent args $ longOption "takefilename" = Right . takeFileName $ path
  | otherwise = Left "ERROR: An option must be specified"


handleHelp :: Arguments -> IO ()
handleHelp args =
  when (isPresent args $ longOption "help") $ do
    putStrLn $ usage patterns
    exitSuccess


exitWithMsg :: String -> IO a
exitWithMsg msg = hPutStrLn stderr msg >> exitFailure
