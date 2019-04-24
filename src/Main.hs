import System.Directory
  ( makeAbsolute, makeRelativeToCurrentDirectory )
import System.Exit ( die )
import System.FilePath (
  dropExtension, dropExtensions, dropFileName, takeBaseName,
  takeDirectory, takeExtension, takeExtensions, takeFileName )

import Options
  ( Options (DropExtension, DropExtensions, DropFilename,
      MakeAbsolute, MakeRelative, NoArguments, TakeBasename,
      TakeDirectory, TakeExtension, TakeExtensions, TakeFilename,
      Version),
    parseOptions, versionInfo
  )


main :: IO ()
main = do
  eresult <- extractPart =<< parseOptions
  either die putStrLn eresult


-- The Either type here determines whether this program exits with failure or
-- success.
extractPart :: Options -> IO (Either String FilePath)
extractPart (TakeDirectory  path) = return . Right . takeDirectory         $ path
extractPart (DropFilename   path) = return . Right . dropFileName          $ path
extractPart (TakeExtension  path) = return . Right . takeExtension         $ path
extractPart (DropExtension  path) = return . Right . dropExtension         $ path
extractPart (TakeExtensions path) = return . Right . takeExtensions        $ path
extractPart (DropExtensions path) = return . Right . dropExtensions        $ path
extractPart (TakeBasename   path) = return . Right . takeBaseName          $ path
extractPart (TakeFilename   path) = return . Right . takeFileName          $ path
extractPart (MakeAbsolute   path) = Right <$> makeAbsolute                   path
extractPart (MakeRelative   path) = Right <$> makeRelativeToCurrentDirectory path
extractPart Version               = return . Right $ versionInfo
extractPart NoArguments           = return . Left  $ "No arguments given"
