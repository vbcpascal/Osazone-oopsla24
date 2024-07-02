{-# LANGUAGE QuasiQuotes #-}
module Target.Haskell.Files.MetaFile where

import Text.RawString.QQ

idDotHs :: (FilePath, String)
idDotHs = ("Meta/Identifier.hs", f)
  where f = [r|module Meta.Identifier where

type Id = String

inc :: Int -> Int
inc = (+) 1
|]

substDotHs :: (FilePath, String)
substDotHs = ("Meta/Substitution.hs", f)
  where f = [r|module Meta.Substitution where|]
