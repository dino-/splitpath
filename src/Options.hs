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

Version %s  Dino Morelli <dino@ui3.info>
|]


versionInfo :: String
versionInfo = "splitpath " ++ showVersion version
