{-# LANGUAGE QuasiQuotes #-}
module Target.Haskell.Files.MetaMonadFile where

import Text.RawString.QQ

transDotHs :: (FilePath, String)
transDotHs = ("Meta/Monad/Trans.hs", f)
  where f = [r|module Meta.Monad.Trans where

import qualified Control.Monad.Trans.Class as T
import qualified Data.Functor.Identity as I

lift :: (T.MonadTrans t, Monad m) => m a -> t m a
lift = T.lift

type Identity = I.Identity

runId :: Identity a -> a
runId = I.runIdentity
|]

envDotHs :: (FilePath, String)
envDotHs = ("Meta/Monad/Env.hs", f)
  where f = [r|module Meta.Monad.Env where

import qualified Control.Monad.Trans.State as ST
import Data.Functor.Identity (Identity)

type Env r = ST.StateT [r]
type EnvP r = Env r Identity

getEnv :: Monad m => Env r m r
getEnv = do
  envs <- ST.get
  return (head envs)

putEnv :: Monad m => r -> Env r m ()
putEnv env = do
  envs <- ST.get
  ST.put (env : envs)

restoreEnv :: Monad m => Env r m ()
restoreEnv = do
  envs <- ST.get
  ST.put (tail envs)

runEnv :: Monad m => Env r m a -> r -> m a
runEnv s env = ST.evalStateT s [env]

|]

errDotHs :: (FilePath, String)
errDotHs = ("Meta/Monad/Err.hs", f)
  where f = [r|module Meta.Monad.Err where

import qualified Control.Monad.Trans.Maybe as MT
import Data.Functor.Identity (Identity(..))

type Err = MT.MaybeT
type ErrP = Err Identity

error :: Monad m => Err m a
error = MT.MaybeT $ return Nothing

runErr :: Monad m => Err m a -> m (Maybe a)
runErr = MT.runMaybeT
|]

stateDotHs :: (FilePath, String)
stateDotHs = ("Meta/Monad/State.hs", f)
  where f = [r|module Meta.Monad.State where

import qualified Control.Monad.Trans.State as ST
import Data.Functor.Identity (Identity(..))

type State = ST.StateT
type StateP s = State s Identity

get :: Monad m => State s m s
get = ST.get

put :: Monad m => s -> State s m ()
put = ST.put

runState :: Monad m => State s m a -> s -> m a
runState = ST.evalStateT

|]

ioDotHs :: (FilePath, String)
ioDotHs = ("Meta/Monad/Io.hs", f)
  where f = [r|module Meta.Monad.Io where

import qualified Control.Monad.Trans.State as ST
import Data.Functor.Identity (Identity(..))
import Debug.Trace (trace)

type Io = ST.StateT String
type IoP = Io Identity

println :: Monad m => String -> Io m ()
println s = do
  buffer <- ST.get
  ST.put (buffer ++ "\n | " ++ s)

printShow :: (Monad m, Show a) => a -> Io m ()
printShow x = println (show x)

runIo :: Monad m => Io m a -> m a
runIo s = do
  (x, buffer) <- ST.runStateT s ""
  if buffer == ""
    then return x
    else return (trace ("output" ++ buffer) x)

|]
