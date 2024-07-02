{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}
module Config.YamlReader where

import Utils.AnsiPretty (Pretty)
import Utils.StringTemplate

import Data.Char as C
import Data.Map as M
import Data.Yaml
import GHC.Generics (Generic)

data YamlConfig = YamlConfig
  { name      :: String
  , version   :: String
  , extension :: String
  , lib       :: String
  , modules   :: M.Map String String
  , repl      :: M.Map String String
  , extra     :: Maybe [String]
  } deriving (Eq, Show, Ord, Generic)

instance FromJSON YamlConfig where
instance ToJSON YamlConfig where

data LiftingConfig = LiftingConfig
  { surfName :: String
  , corePath :: String
  , extFiles :: [String]
  } deriving (Eq, Show, Ord)

instance FromJSON LiftingConfig where
  parseJSON (Object v) = LiftingConfig
    <$> v .: "name"
    <*> v .: "core"
    <*> v .: "extensions"
  parseJSON _ = undefined

class LangConfig cfg where
  getName :: cfg -> String

instance LangConfig YamlConfig where
  getName = name

instance LangConfig LiftingConfig where
  getName = surfName

instance LangConfig a => PreSTMP a where
  putInSTMP config stmp =
    render $ newSTMP stmp
      <~~ ("lang", n)
      <~~ ("name", (\(h:t) -> C.toLower h:t) n)
      <~~ ("Name", (\(h:t) -> C.toUpper h:t) n)
    where n = getName config
