{- Safe -}

{-# LANGUAGE DeriveFunctor               #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE PackageImports              #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE PatternSynonyms             #-}

module Thentos.Action.Types where

import Control.Concurrent (MVar)
import Control.Exception (Exception, SomeException)
import Control.Lens (makeLenses)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT(ReaderT))
import Control.Monad.State (MonadState, StateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (EitherT(EitherT))
import "cryptonite" Crypto.Random (ChaChaDRG)
import Database.PostgreSQL.Simple (Connection)
import Data.Pool (Pool)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import LIO.Core (MonadLIO, LIO, liftLIO)
import LIO.DCLabel (DCLabel)
import LIO.Error (AnyLabelError)

import Thentos.Types
import Thentos.Config


data ActionState =
    ActionState
      { _aStConfig  :: ThentosConfig
      , _aStRandom  :: MVar ChaChaDRG
      , _aStDb      :: Pool Connection
      }
  deriving (Generic)

makeLenses ''ActionState


-- | The 'Action' monad transformer stack.  It contains:
--
--     - 'LIO' as a base monad.
--     - A state of polymorphic type (for use e.g. by the frontend handlers to store cookies etc.)
--     - The option of throwing @ThentosError e@.  (Not 'ActionError e', which contains
--       authorization errors that must not be catchable from inside an 'Action'.)
--     - An 'ActionState' in a reader.  The state can be used by actions for calls to 'LIO', which
--       will have authorized effect.  Since it is contained in a reader, actions do not have the
--       power to corrupt it.
newtype Action' m e s a = Action { fromAction :: ActionSyn m e s a }
  deriving
    ( Functor
    , Generic
    , Applicative
    , Monad
    , MonadReader ActionState
    , MonadError (ThentosError e)
    , MonadState s
    )

type ActionSyn m e s a = ReaderT ActionState (EitherT (ThentosError e) (StateT s m)) a
type Action = Action' (LIO DCLabel)

instance MonadLIO DCLabel (Action e s) where
    liftLIO lio = Action . ReaderT $ \_ -> EitherT (Right <$> lift lio)


-- | Errors known by 'runActionE', 'runAction', ....
--
-- The 'MonadError' instance of newtype 'Action' lets you throw and catch errors from *within* the
-- 'Action', i.e., at construction time).  These are errors are handled in the 'ActionErrorThentos'
-- constructor.  Label errors and other (possibly async) exceptions are caught (if possible) in
-- 'runActionE' and its friends and maintained in other 'ActionError' constructors.
data ActionError e =
    ActionErrorThentos (ThentosError e)
  | ActionErrorAnyLabel AnyLabelError
  | ActionErrorUnknown SomeException
  deriving (Show)

instance (Show e, Typeable e) => Exception (ActionError e)


-- | Like 'Action', but with 'IO' at the base.
type UnsafeAction = Action' IO

pattern UnsafeAction ::
  ReaderT ActionState (EitherT (ThentosError e) (StateT s IO)) a -> UnsafeAction e s a
pattern UnsafeAction a = Action a

fromUnsafeAction :: UnsafeAction e s a -> ActionSyn IO e s a
fromUnsafeAction = fromAction

deriving instance MonadIO (UnsafeAction e s)
