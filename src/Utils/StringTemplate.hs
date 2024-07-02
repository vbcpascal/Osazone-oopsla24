{-# LANGUAGE OverloadedStrings #-}

module Utils.StringTemplate
  ( (<~~)
  , PreSTMP (..)
  , module Text.StringTemplate
  ) where

import Text.StringTemplate

(<~~) :: (ToSElem a, Stringable b) => StringTemplate b -> (String, a) -> StringTemplate b
(<~~) stmp (k, v) = setAttribute k v stmp

infixl 6 <~~

class PreSTMP a where
  putInSTMP :: a -> String -> String
