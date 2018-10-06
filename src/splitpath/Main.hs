{-# LANGUAGE QuasiQuotes #-}

import Control.Monad ( when )
import System.Console.Docopt ( Arguments, Docopt, argument, docopt, getArg, isPresent, longOption, parseArgsOrExit, usage )
import System.Environment ( getArgs )
import System.Exit ( exitFailure, exitSuccess )
import System.FilePath ( dropFileName, takeBaseName, takeDirectory, takeExtension, takeExtensions, takeFileName )
import System.IO ( hPutStrLn, stderr )


patterns :: Docopt
patterns = [docopt|
splitpath v1.0

FIXME

Usage:
  splitpath [-h]
  splitpath [-d | -D | -f | -F | -e | -E] <PATH>

Options:
  -d, --directory       The directory
  -D, --dir-with-sep    The directory including a trailing separator
  -e, --ext             The shortest extension from the end of the path
  -E, --ext-all
  -f, --file            The filename only, no path, no extension
  -F, --file-with-ext   The filename with extension, no path
  -h, --help            This help info

The standard tools for separating file paths in bash are primitive and lacking
features. There are also problems when spaces are present in the path. On the
other hand, many programming language standard libraries have powerful tools
for breaking apart file paths. This utility wraps the functionality present in
Haskell's System.Directory and System.FilePath libraries, exposing this
behavior as a simple utility with switches.

Examples:

  $ splitpath -d /foo/bar/baz.tar.gz  # /foo/bar
  $ splitpath -D /foo/bar/baz.tar.gz  # /foo/bar/
  $ splitpath -f /foo/bar/baz.tar.gz  # baz.tar
  $ splitpath -F /foo/bar/baz.tar.gz  # baz.tar.gz
  $ splitpath -e /foo/bar/baz.tar.gz  # .gz
  $ splitpath -E /foo/bar/baz.tar.gz  # .tar.gz
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
  | isPresent args $ longOption "directory" = Right . takeDirectory $ path
  | isPresent args $ longOption "dir-with-sep" = Right . dropFileName $ path
  | isPresent args $ longOption "file" = Right . takeBaseName $ path
  | isPresent args $ longOption "file-with-ext" = Right . takeFileName $ path
  | isPresent args $ longOption "ext" = Right . takeExtension $ path
  | isPresent args $ longOption "ext-all" = Right . takeExtensions $ path
  | otherwise = Left "ERROR: An option must be specified"


handleHelp :: Arguments -> IO ()
handleHelp args =
  when (isPresent args $ longOption "help") $ do
    putStrLn $ usage patterns
    exitSuccess


exitWithMsg :: String -> IO a
exitWithMsg msg = hPutStrLn stderr msg >> exitFailure
