{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module RealWorld.Domain.Command.Interpreter.Gateway.TokenGateway where

import Effectful (Eff, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import RealWorld.Domain.Adapter.Gateway.TokenGateway (TokenGateway (..))
import Relude
import RealWorld.Domain.Command.Fixture (Fixture)
import qualified RealWorld.Domain.Command.Fixture as Fixture

run :: (IOE :> es) => Fixture IO -> Eff (TokenGateway : es) a -> Eff es a
run fixture = interpret $ \_ -> \case
  Generate ulid expiresIn -> liftIO $ Fixture._generateToken fixture ulid expiresIn
  Verify token -> liftIO $ Fixture._verifyToken fixture token