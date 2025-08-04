{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module RealWorld.Infra.Interpreter.Adapter.Manager.TxManager where

import Effectful (Eff, IOE, (:>))
import Effectful.Dispatch.Dynamic
import Effectful.Reader.Dynamic (Reader)
import RealWorld.Domain.Adapter.Manager.TxManager (TxManager (..))
import qualified RealWorld.Infra.Component.Database as Database
import Relude hiding (Reader)

run :: (Reader Database.State :> es, IOE :> es) => Eff (TxManager : es) a -> Eff es a
run = interpret $ \env -> \case
  WithTx action -> localSeqUnlift env $ \unlift -> Database.withTx (unlift action)
