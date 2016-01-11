{-# LANGUAGE Unsafe                      #-}

{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE PackageImports              #-}
{-# LANGUAGE ScopedTypeVariables         #-}

module Thentos.Action.Unsafe
where

import Control.Concurrent (modifyMVar)
import Control.Exception (throwIO, ErrorCall(..))
import Control.Lens ((^.))
import Control.Monad.Except (throwError, catchError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Morph
import Control.Monad.Reader (ask)
import "cryptonite" Crypto.Random (ChaChaDRG, DRG(randomBytesGenerate))
import Data.Configifier (Tagged(Tagged), (>>.))
import Data.Pool (withResource)
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions (LT, ST, SBS)
import LIO.Core (liftLIO, getClearance, setClearanceP)
import LIO.DCLabel (ToCNF, DCLabel, (%%), cFalse)
import LIO.Label (lub)
import LIO.TCB (Priv(PrivTCB), ioTCB)
import System.Log (Priority(ERROR, CRITICAL))
import Text.Hastache (MuConfig(..), MuContext, defaultConfig, emptyEscape, hastacheStr)

import qualified Data.Thyme as Thyme

import Thentos.Action.SimpleAuth
import Thentos.Action.Types
import Thentos.Config
import Thentos.Smtp as TS
import Thentos.Transaction.Core (ThentosQuery, runThentosQuery)
import Thentos.Types
import Thentos.Util as TU

import qualified System.Log.Missing as SLM
import qualified Thentos.Transaction as T


-- * labels, privileges and access rights.

extendClearanceOnLabel :: DCLabel -> Action e s ()
extendClearanceOnLabel label = liftLIO $ do
    getClearance >>= setClearanceP (PrivTCB cFalse) . (`lub` label)

extendClearanceOnPrincipals :: ToCNF cnf => [cnf] -> Action e s ()
extendClearanceOnPrincipals principals = mapM_ extendClearanceOnLabel $ [ p %% p | p <- principals ]

extendClearanceOnAgent :: Agent -> Action e s ()
extendClearanceOnAgent agent = do
    extendClearanceOnPrincipals [agent]
    unsafeAction (query $ T.agentRoles agent) >>= extendClearanceOnPrincipals

extendClearanceOnThentosSession :: ThentosSessionToken -> Action e s ()
extendClearanceOnThentosSession tok = do
    (_, session) <- unsafeAction . query . T.lookupThentosSession $ tok
    extendClearanceOnAgent $ session ^. thSessAgent


-- * making unsafe actions safe

unsafeLiftIO :: IO v -> Action e s v
unsafeLiftIO = unsafeAction . liftIO

-- | Run an 'UnsafeAction' in a safe 'Action' with extra authorization checks (performed through
-- 'assertAuth').
guardedUnsafeAction :: Action e s Bool -> UnsafeAction e s a -> Action e s a
guardedUnsafeAction utest uaction = assertAuth utest >> unsafeAction uaction

-- | Run an 'UnsafeAction' in a safe 'Action' without extra authorization checks.
unsafeAction :: forall e s a. UnsafeAction e s a -> Action e s a
unsafeAction = Action . hoist (hoist (hoist ioTCB)) . fromUnsafeAction

-- * misc

query :: ThentosQuery e v -> UnsafeAction e s v
query u = do
    ActionState _ _ connPool <- UnsafeAction ask
    liftIO (withResource connPool (`runThentosQuery` u)) >>= either throwError return

getConfig :: UnsafeAction e s ThentosConfig
getConfig = (^. aStConfig) <$> UnsafeAction ask

getCurrentTime :: UnsafeAction e s Timestamp
getCurrentTime = Timestamp <$> liftIO Thyme.getCurrentTime

-- | A relative of 'cprgGenerate' from crypto-random.
genRandomBytes :: Int -> UnsafeAction e s SBS
genRandomBytes i = do
    let f :: ChaChaDRG -> (ChaChaDRG, SBS)
        f r = case randomBytesGenerate i r of (output, r') -> (r', output)
    as <- UnsafeAction ask
    liftIO . modifyMVar (as ^. aStRandom) $ return . f

makeUserFromFormData :: UserFormData -> UnsafeAction e s User
makeUserFromFormData = liftIO . TU.makeUserFromFormData

hashUserPass :: UserPass -> UnsafeAction e s (HashedSecret UserPass)
hashUserPass = liftIO . TU.hashUserPass

hashServiceKey :: ServiceKey -> UnsafeAction e s (HashedSecret ServiceKey)
hashServiceKey = liftIO . TU.hashServiceKey

sendMail :: Maybe UserName -> UserEmail -> ST -> ST -> UnsafeAction e s ()
sendMail mName address subject msg = do
    config <- Tagged . (>>. (Proxy :: Proxy '["smtp"])) <$> Thentos.Action.Unsafe.getConfig
    result <- liftIO $ TS.sendMail config mName address subject msg
    case result of
        Right () -> return ()
        Left (SendmailError s) -> liftIO $ do
            SLM.logger CRITICAL $ "error sending mail: " ++ s
            throwIO $ ErrorCall "error sending email"

logger :: Priority -> String -> UnsafeAction e s ()
logger prio = liftIO . SLM.logger prio

logIfError  :: (Show e) => UnsafeAction e s v -> UnsafeAction e s v
logIfError  = (`catchError` \e ->               logger ERROR (show e)  >> throwError e)

logIfError' :: (Show e) => Action e s v -> Action e s v
logIfError' = (`catchError` \e -> unsafeAction (logger ERROR (show e)) >> throwError e)

-- | Render a Hastache template for plain-text output (none of the characters in context variables
-- will be escaped).
renderTextTemplate :: ST -> MuContext IO -> UnsafeAction e s LT
renderTextTemplate template context = liftIO $ hastacheStr hastacheCfg template context
  where
    hastacheCfg = defaultConfig { muEscapeFunc = emptyEscape }
