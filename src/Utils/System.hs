module Utils.System where

import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import qualified Data.ByteString as B

createAndWriteFile :: FilePath -> String -> IO ()
createAndWriteFile path content = do
  createDirectoryIfMissing True $ takeDirectory path
  writeFile path content

createAndWriteFileB :: FilePath -> B.ByteString -> IO ()
createAndWriteFileB path content = do
  createDirectoryIfMissing True $ takeDirectory path
  B.writeFile path content
