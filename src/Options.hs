{-# LANGUAGE QuasiQuotes #-}

module Options
  ( Options (..)
  , parseOptions
  , versionInfo
  )
  where

import Data.Version ( showVersion )
import Options.Applicative
import Paths_splitpath ( version )
import Text.Heredoc ( here )
import Text.PrettyPrint.ANSI.Leijen ( string )
import Text.Printf ( printf )


data Options
  = TakeDirectory FilePath
  | DropFilename FilePath
  | TakeExtension FilePath
  | DropExtension FilePath
  | TakeExtensions FilePath
  | DropExtensions FilePath
  | TakeBasename FilePath
  | TakeFilename FilePath
  | MakeAbsolute FilePath
  | MakeRelative FilePath
  | Version
  | NoArguments


-- A helper to differentiate between --version and no arguments at all
determineIfVersion :: Bool -> Options
determineIfVersion True  = Version
determineIfVersion False = NoArguments


parser :: Parser Options
parser =
  TakeDirectory <$> strOption
    (  long "takedirectory"
    <> metavar "PATH"
    <> help "Get the directory name (move up one level)"
    ) <|>
  DropFilename <$> strOption
    (  long "dropfilename"
    <> metavar "PATH"
    <> help "Drop the filename, leave the trailing path separator"
    ) <|>
  TakeExtension <$> strOption
    (  long "takeextension"
    <> metavar "PATH"
    <> help "Get the extension"
    ) <|>
  DropExtension <$> strOption
    (  long "dropextension"
    <> metavar "PATH"
    <> help "Drop the extension"
    ) <|>
  TakeExtensions <$> strOption
    (  long "takeextensions"
    <> metavar "PATH"
    <> help "Get all extensions"
    ) <|>
  DropExtensions <$> strOption
    (  long "dropextensions"
    <> metavar "PATH"
    <> help "Drop all extensions"
    ) <|>
  TakeBasename <$> strOption
    (  long "takebasename"
    <> metavar "PATH"
    <> help "Get the base name, without an extention or path"
    ) <|>
  TakeFilename <$> strOption
    (  long "takefilename"
    <> metavar "PATH"
    <> help "Get the file name"
    ) <|>
  MakeAbsolute <$> strOption
    (  long "makeabsolute"
    <> metavar "PATH"
    <> help "Convert a path into an absolute path"
    ) <|>
  MakeRelative <$> strOption
    (  long "makerelative"
    <> metavar "PATH"
    <> help "Construct a path relative to the current directory"
    ) <|>
  determineIfVersion <$> switch
    (  long "version"
    <> help "Display version"
    )


parseOptions :: IO Options
parseOptions = execParser $ info (parser <**> helper)
  (  header "splitpath - Comprehensive path splitting utility"
  <> footer'
  )


footer' :: InfoMod a
footer' = footerDoc . Just . string $ printf content (showVersion version)
  where content = tail . init $ [here|
The standard tools and techniques in bash for separating file paths into
directories, filenames and extensions are confusing. For example, these things:
"${FOO##*.}" and "${BAR%%.*}" Also, quoting gets complicated when paths
contain spaces.

On the other hand, many programming language standard libraries have powerful
tools for breaking apart file paths.

This software wraps the functionality present in Haskell's System.FilePath and
System.Directory libraries, exposing these functions in a simple utility with
clearly-named switches.

Examples:

  $ splitpath --takedirectory  /foo/bar/baz.tar.gz    # /foo/bar
  $ splitpath --dropfilename   /foo/bar/baz.tar.gz    # /foo/bar/
  $ splitpath --takeextension  /foo/bar/baz.tar.gz    # .gz
  $ splitpath --dropextension  /foo/bar/baz.tar.gz    # /foo/bar/baz.tar
  $ splitpath --takeextensions /foo/bar/baz.tar.gz    # .tar.gz
  $ splitpath --dropextensions /foo/bar/baz.tar.gz    # /foo/bar/baz
  $ splitpath --takebasename   /foo/bar/baz.tar.gz    # baz.tar
  $ splitpath --takefilename   /foo/bar/baz.tar.gz    # baz.tar.gz
  $ splitpath --makeabsolute   somefile               # /current/dir/somefile
  $ splitpath --makerelative   /current/dir/somefile  # somefile

Version %s  Dino Morelli <dino@ui3.info>
|]


versionInfo :: String
versionInfo = "splitpath " ++ showVersion version
