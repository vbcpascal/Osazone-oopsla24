{-# LANGUAGE OverloadedStrings #-}
module Utils.ErrorMessage where

import Utils.AnsiPretty

import Control.Exception (throw)
import Control.Monad.Catch
import Control.Monad.Trans.Class
import Data.Map

data ErrMsg
  = ErrMsg String
  | ErrCode Int DocAnsi
  | ErrIns ErrorInstance DocAnsi

data GeneralErr
  = GeneralErr

instance Show ErrMsg where
  show (ErrMsg msg) = msg
  show (ErrCode code msg) = show $ colored Red ("error" <+> viaShow code <> ":") <> line <> indent 2 msg
  show (ErrIns errins msg) = show $ colored Red ("error" <+> viaShow errins <> ":") <|> indent 2 msg

instance Show GeneralErr where
  show GeneralErr = "Visit for more details."

instance Exception ErrMsg

instance Exception GeneralErr

raiseErr :: String -> Either ErrMsg a
raiseErr = Left . ErrMsg

err :: MonadTrans mt => String -> mt (Either ErrMsg) a
err = lift . raiseErr

throwErr :: MonadThrow m => Either ErrMsg a -> m a
throwErr (Left err) = throwM err
throwErr (Right x)  = return x

err' :: MonadTrans mt => Int -> DocAnsi -> mt (Either ErrMsg) a
err' code msg = lift (Left (ErrCode code msg))

throwEitherIO :: Either ErrMsg a -> IO a
throwEitherIO (Left err@(ErrMsg _))     = throwM err
throwEitherIO (Left (ErrCode code msg)) = raiseErrIO code msg
throwEitherIO (Left (ErrIns code msg))  = raiseErrIO 0 msg
throwEitherIO (Right x)                 = return x

sure :: Either ErrMsg a -> IO a
sure = throwEitherIO

raiseErrIO :: Int -> DocAnsi -> IO a
raiseErrIO code msg = do
  putDocLn (colored Red ("Error" <+> viaShow code <> ":") <|> indent 2 msg)
  throw GeneralErr

titles :: Map Int DocAnsi
titles = fromList
  [ (11, "NoDefinition")
  , (12, "MultipleDefinition")
  , (13, "NotSpecified")
  , (20, "YamlError")
  , (21, "ModuleNotFound")
  , (31, "UndefinedSymbol")
  , (32, "AmbigiousSymbol")
  , (33, "NoDefinition")
  , (34, "NotImported")
  , (99, "NotSpecified")
  ]

throwError :: ErrorInstance -> DocAnsi -> Either ErrMsg a
throwError code msg = Left (ErrIns code msg)

raiseError :: MonadTrans mt => ErrorInstance -> DocAnsi -> mt (Either ErrMsg) a
raiseError code msg = lift $ throwError code msg

raiseErrorIO :: ErrorInstance -> DocAnsi -> IO a
raiseErrorIO code msg = do
  putDocLn (colored Red ("Error" <+> viaShow code <> ":") <|> indent 2 msg)
  throw GeneralErr

data ErrorInstance
  -- Language configuration
  = YamlParseError
  | OsaParseError
  | ExtParseError
  -- Modules
  | ModuleNotFound
  | ModuleMultiDef
  -- Language definition
  | NoDefinition
  | MultiDefinition
  | UnknownAnnotation
  | MissForeignType
  | Internal InternalError
  deriving Show

data InternalError
  = NotCallPass
  | GenForeignType
  deriving Show
