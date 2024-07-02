{-# LANGUAGE QuasiQuotes #-}
module Target.Haskell.Files.IntrpDotHs (intrpDotHsPath, intrpDotHs) where

import Text.RawString.QQ

intrpDotHsPath :: FilePath
intrpDotHsPath = "Intrp.hs"

intrpDotHs :: String
intrpDotHs = [r|{-# LANGUAGE OverloadedStrings #-}
module Intrp (intrp) where

import Repl
import Runtime

import Data.List
import Data.Map
import Data.String
import Control.Exception
import Control.Monad.Trans.State
import Control.Monad.Trans.Class

cmdMap :: Map String (String -> IO ())
cmdMap = fromList ($cmds$[])

data IntrpSt = IntrpSt
  { current :: Int
  , behaviors :: [String]
  , label :: Maybe String
  }

type S = StateT IntrpSt IO

intrp :: [String] -> IO ()
intrp ls = evalStateT (mapM f ls >> pure ()) (IntrpSt 1 [] Nothing)
  where f l = intrpLine l >> incCurr

intrpLine :: String -> S ()
intrpLine l | isPrefixOf "--*" l = do
  let lbl = unwords (tail (words l))
  st <- get
  put (st { label = Just lbl })
intrpLine l | isPrefixOf "--" l = return ()
intrpLine l | isPrefixOf "[config] " l = do
  let rs = tail (words l)
  mapM_ checkr rs
  st <- get
  put (st { behaviors = rs })
intrpLine l | Data.List.null (words l) = return ()
intrpLine l = do
  st <- get
  lbl <- getLabel l
  lift (putDoc (colored Blue ("Line" <+> pretty (current st) <> ":") <+> pretty lbl <> line))
  mapM (intrpItem l) (behaviors st)
  removeLabel

intrpItem :: String -> String -> S ()
intrpItem l r = do
  lbl <- getLabel l
  let f = cmdMap ! r
  lift (putDoc (colored Blue (" [" <> pretty r <> "] ")))
  lift (catch (f l) handler)

incCurr :: S ()
incCurr = do
  st <- get
  put (st { current = current st + 1 })

getLabel :: String -> S String
getLabel l = do
  st <- get
  case label st of
    Nothing -> return l
    Just lbl -> return lbl

removeLabel :: S ()
removeLabel = do
  st <- get
  put (st { label = Nothing })

checkr :: String -> S ()
checkr c | member c cmdMap = return ()
checkr c = do
  st <- get
  error ("Unknown command " ++ c ++ " in configuration in line " ++ show (current st) ++ ".")

|]
