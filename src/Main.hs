import System.Exit ( die )
import System.FilePath (
  dropExtension, dropExtensions, dropFileName, takeBaseName,
  takeDirectory, takeExtension, takeExtensions, takeFileName )

import Options
  ( Options (DropExtension, DropExtensions, DropFilename,
      NoArguments, TakeBasename, TakeDirectory, TakeExtension,
      TakeExtensions, TakeFilename, Version),
    parseOptions, versionInfo
  )


main :: IO ()
main = do
  eresult <- extractPart <$> parseOptions
  either die putStrLn eresult


extractPart :: Options -> Either String FilePath
extractPart (TakeDirectory  path) = Right $ takeDirectory  path
extractPart (DropFilename   path) = Right $ dropFileName   path
extractPart (TakeExtension  path) = Right $ takeExtension  path
extractPart (DropExtension  path) = Right $ dropExtension  path
extractPart (TakeExtensions path) = Right $ takeExtensions path
extractPart (DropExtensions path) = Right $ dropExtensions path
extractPart (TakeBasename   path) = Right $ takeBaseName   path
extractPart (TakeFilename   path) = Right $ takeFileName   path
extractPart Version               = Right   versionInfo
extractPart NoArguments           = Left "No arguments given"
