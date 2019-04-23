import System.FilePath (
  dropExtension, dropExtensions, dropFileName, takeBaseName,
  takeDirectory, takeExtension, takeExtensions, takeFileName )

import Options
  ( Options (DropExtension, DropExtensions, DropFilename,
      TakeBasename, TakeDirectory, TakeExtension,
      TakeExtensions, TakeFilename, Version),
    parseOptions, versionInfo
  )


main :: IO ()
main = parseOptions >>= putStrLn . extractPart


extractPart :: Options -> FilePath
extractPart (TakeDirectory  path) = takeDirectory  path
extractPart (DropFilename   path) = dropFileName   path
extractPart (TakeExtension  path) = takeExtension  path
extractPart (DropExtension  path) = dropExtension  path
extractPart (TakeExtensions path) = takeExtensions path
extractPart (DropExtensions path) = dropExtensions path
extractPart (TakeBasename   path) = takeBaseName   path
extractPart (TakeFilename   path) = takeFileName   path
extractPart Version               = versionInfo
