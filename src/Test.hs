{-# LANGUAGE ScopedTypeVariables, RankNTypes, UnicodeSyntax #-}
module Test (


) where

--

import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import qualified Control.Monad.Identity as I
import qualified Control.Monad.State as S
import qualified Control.Monad.Reader as R
import qualified Control.Monad.Writer as W
import qualified Control.Monad.Trans.Maybe as Y
import Data.Either
import qualified Data.Maybe as B


data AppState = AppState Int deriving Show
data AppConfig = AppConfig String deriving Show

type App m = ReaderT AppConfig (StateT AppState m)

runApp :: App m a -> String -> m (a, AppState)
runApp app conf =
    let config = AppConfig conf
        state = AppState 0
    -- in runStateT (runReaderT app config) state
    in runApp' app config state

runApp' :: App m a -> AppConfig -> AppState -> m (a, AppState)
runApp' app = runStateT . runReaderT app

makeApp :: (Monad m) => String -> App m String
makeApp what = do
  (AppConfig config) <- R.ask
  (AppState st) <- lift S.get
  S.put (AppState $ st + 1)
  return $ "got " ++ what ++ " " ++ config ++ " " ++ show st

appInstance :: (Monad m) => App m String
appInstance = makeApp ""


appWithLog :: App (W.Writer String) String
appWithLog = W.tell "duck2" >> appInstance
runappWithLog = W.runWriter $ runApp appWithLog "hello"

appWithReadAndLog :: App (W.Writer String) String
appWithReadAndLog = do
  (AppConfig conf) <- R.ask
  W.tell $ "got config: " ++ conf
  appInstance

runappWithReadAndLog = W.runWriter $ runApp appWithReadAndLog "hello"



appIfJust2 :: String -> App Maybe String
appIfJust2 = makeApp
runAppIfJust2 what = what >>= \d -> runApp (appIfJust2 d) "hello"
