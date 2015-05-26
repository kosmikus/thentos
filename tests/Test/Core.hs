{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE ImpredicativeTypes         #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

module Test.Core
where

import Control.Applicative ((<*), (<$>))
import Control.Concurrent.Async (async, cancel)
import Control.Concurrent.MVar (MVar, newMVar)
import Control.Lens ((^.))
import Control.Monad (when, void)
import Crypto.Random (SystemRNG, createEntropyPool, cprgCreate)
import Crypto.Scrypt (Pass(Pass), encryptPass, Salt(Salt), scryptParams)
import Data.Acid.Advanced (update')
import Data.Acid (openLocalStateFrom, closeAcidState)
import Data.ByteString (ByteString)
import Data.CaseInsensitive (mk)
import Data.Configifier ((>>.), Tagged(Tagged))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromJust)
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (LBS, SBS, cs)
import Filesystem (isDirectory, removeTree)
import GHC.Exts (fromString)
import Network.HTTP.Types.Header (Header)
import Network.HTTP.Types.Method (Method)
import Network.HTTP.Types.Status (statusCode)
import Network.Wai (Application, StreamingBody, requestMethod, requestBody, strictRequestBody, requestHeaders)
import Network.Wai.Internal (Response(ResponseFile, ResponseBuilder, ResponseStream, ResponseRaw))
import Network.Wai.Test (runSession, setPath, defaultRequest, srequest, simpleBody, simpleStatus)
import Network.Wai.Test (Session, SRequest(SRequest))
import System.Directory (removeDirectoryRecursive)
import Text.Show.Pretty (ppShow)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Parser as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Test.WebDriver as WD

import Thentos.Action.Core
import Thentos.Backend.Api.Simple as Simple
import Thentos.Config
import Thentos.Frontend (runFrontend)
import Thentos.Transaction
import Thentos.Types

import Test.Config
import Test.Types


user1, user2, user3, user4, user5 :: User
user1 = User "name1" (encryptTestSecret "passwd") "em@il" Set.empty Map.empty
user2 = User "name2" (encryptTestSecret "passwd") "em38@il" Set.empty Map.empty
user3 = User "name3" (encryptTestSecret "3") "3" Set.empty Map.empty
user4 = User "name4" (encryptTestSecret "4") "4" Set.empty Map.empty
user5 = User "name5" (encryptTestSecret "5") "5" Set.empty Map.empty


encryptTestSecret :: ByteString -> HashedSecret a
encryptTestSecret pw =
    HashedSecret $
        encryptPass (fromJust $ scryptParams 2 1 1) (Salt "") (Pass pw)


setupDB :: IO DBTS
setupDB = do
    tcfg <- testConfig
    st <- openLocalStateFrom (tcfg ^. tcfgDbPath) emptyDB
    createGod st
    Right (UserId 1) <- update' st $ AddUser user1
    Right (UserId 2) <- update' st $ AddUser user2
    rng :: MVar SystemRNG <- createEntropyPool >>= newMVar . cprgCreate
    return $ DBTS tcfg (ActionState (st, rng, testThentosConfig tcfg))

teardownDB :: DBTS -> IO ()
teardownDB (DBTS tcfg (ActionState (st, _, _))) = do
    closeAcidState st
    removeDirectoryRecursive (tcfg ^. tcfgTmp)


-- | Test backend does not open a tcp socket, but uses hspec-wai
-- instead.  Comes with a session token and authentication headers
-- headers for default god user.
setupTestBackend :: Command -> IO BTS
setupTestBackend cmd = do
    DBTS tcfg asg <- setupDB
    case cmd of
        Run -> do
            let testBackend = Simple.serveApi asg
            (tok, headers) <- loginAsGod testBackend
            return $ BTS tcfg asg testBackend tok headers
{-
        RunA3 ->
            let e = error "setupTestBackend: no god credentials!"
            in return (asg, Adhocracy3.serveApi asg, e, e)
-}
        bad -> error $ "setupTestBackend: bad command: " ++ show bad

teardownTestBackend :: BTS -> IO ()
teardownTestBackend bts = teardownDB $ DBTS (bts ^. btsCfg) (bts ^. btsActionState)


-- | Set up both frontend and backend on real tcp sockets (introduced
-- for webdriver testing, but may be used elsewhere).
setupTestServerFull :: IO FTS
setupTestServerFull = do
    DBTS tcfg asg <- setupDB

    let Just (beConfig :: HttpConfig) = Tagged <$> testThentosConfig tcfg >>. (Proxy :: Proxy '["backend"])
        Just (feConfig :: HttpConfig) = Tagged <$> testThentosConfig tcfg >>. (Proxy :: Proxy '["frontend"])

    backend  <- async $ Simple.runApi beConfig asg
    frontend <- async $ Thentos.Frontend.runFrontend feConfig asg

    let wdConfig = WD.defaultConfig
            { WD.wdHost = tcfg ^. tcfgWebdriverHost
            , WD.wdPort = tcfg ^. tcfgWebdriverPort
            }

        wd :: forall a . WD.WD a -> IO a
        wd action = WD.runSession wdConfig . WD.finallyClose $ do
             -- running `WD.closeOnException` here is not
             -- recommended, as it hides all hspec errors behind an
             -- uninformative java exception.
            WD.setImplicitWait 1000
            WD.setScriptTimeout 1000
            WD.setPageLoadTimeout 1000
            action

    return $ FTS tcfg asg backend beConfig frontend feConfig wd

teardownTestServerFull :: FTS -> IO ()
teardownTestServerFull (FTS tcfg db backend _ frontend _ _) = do
    cancel backend
    cancel frontend
    teardownDB $ DBTS tcfg db


loginAsGod :: Application -> IO (ThentosSessionToken, [Header])
loginAsGod testBackend = debugRunSession False testBackend $ do
    response <- srequest (makeSRequest "POST" "/thentos_session" [] $ Aeson.encode (godUid, godPass))
    if (statusCode (simpleStatus response) /= 201)
        then error $ ppShow response
        else do
            let Just (tok :: ThentosSessionToken) = Aeson.decode' $ simpleBody response
            let credentials :: [Header] = [(mk "X-Thentos-Session", cs $ fromThentosSessionToken tok)]
            return (tok, credentials)

logoutAsGod :: Application -> ThentosSessionToken -> [Header] -> IO ()
logoutAsGod testBackend tok godCredentials = debugRunSession False testBackend $ do
    void . srequest . makeSRequest "DELETE" "/session" godCredentials $ Aeson.encode tok


-- | Cloned from hspec-wai's 'request'.  (We don't want to use the
-- return type from there.)
makeSRequest :: Method -> SBS -> [Header] -> LBS -> SRequest
makeSRequest method path headers body = SRequest req body
  where
    req = setPath defaultRequest { requestMethod = method, requestHeaders = headers ++ defaultHeaders } path
    defaultHeaders = [("Content-Type", "application/json")]


-- | Like `runSession`, but with re-ordered arguments, and with an
-- extra debug-output flag.  It's not a pretty function, but it helps
-- with debugging, and it is not intended for production use.
debugRunSession :: Bool -> Application -> Network.Wai.Test.Session a -> IO a
debugRunSession debug application session = runSession session (wrapApplication debug)
  where
    wrapApplication :: Bool -> Application
    wrapApplication False = application
    wrapApplication True = \ _request respond -> do
        (requestRendered, request') <- showRequest _request
        print requestRendered
        application request' (\ response -> putStrLn (showResponse response)  >> respond response)

    showRequest _request = do
        body :: LBS <- strictRequestBody _request
        bodyRef :: IORef Bool <- newIORef False

        let memoBody = do
              toggle <- readIORef bodyRef
              writeIORef bodyRef $ not toggle
              return $ if toggle then "" else cs body

        let  showRequestHeader = "\n=== REQUEST ==========================================================\n"

             showBody :: String
             showBody = showRequestHeader ++ ppShow _request ++ "\nbody:" ++ show body ++ "\n"

             request' = _request { requestBody = memoBody }

        return (showBody, request')
      where

    showResponse response = showResponseHeader ++ show_ response
      where
        showResponseHeader = "\n=== RESPONSE =========================================================\n"

        show_ :: Response -> String
        show_ (ResponseFile _ _ _ _) = "ResponseFile"
        show_ (ResponseBuilder status headers _) = "ResponseBuilder" ++ show (status, headers)
        show_ (ResponseStream status headers (_ :: StreamingBody)) = "ResponseStream" ++ show (status, headers)
        show_ (ResponseRaw _ _) = "ResponseRaw"


-- | Like 'Data.Aeson.decode' but allows all JSON values instead of just
-- objects and arrays.
--
-- Copied from https://github.com/haskell-servant/servant-client
-- (FIXME: also available from attoparsec these days.  replace!)
decodeLenient :: Aeson.FromJSON a => LBS -> Either String a
decodeLenient input = do
    v :: Aeson.Value <- AP.parseOnly (Aeson.value <* AP.endOfInput) (cs input)
    Aeson.parseEither Aeson.parseJSON v