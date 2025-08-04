{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module RealWorld.Domain.Command.Interpreter.Manager.TxManager where

import Effectful (Eff)
import Effectful.Dispatch.Dynamic (localSeqUnlift, interpret)
import RealWorld.Domain.Adapter.Manager.TxManager (TxManager (..))
import Relude

run :: Eff (TxManager : es) a -> Eff es a
run = interpret $ \env -> \case
  WithTx action -> localSeqUnlift env $ \unlift -> (unlift action)
