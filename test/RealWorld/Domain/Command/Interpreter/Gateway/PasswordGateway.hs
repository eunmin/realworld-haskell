{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module RealWorld.Domain.Command.Interpreter.Gateway.PasswordGateway where

import Effectful (Eff, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import RealWorld.Domain.Adapter.Gateway.PasswordGateway (PasswordGateway (..))
import Relude
import RealWorld.Domain.Command.Fixture (Fixture)
import qualified RealWorld.Domain.Command.Fixture as Fixture

run :: (IOE :> es) => Fixture IO -> Eff (PasswordGateway : es) a -> Eff es a
run fixture = interpret $ \_ -> \case
  HashPassword password -> liftIO $ Fixture._hashPassword fixture password
  IsValidPassword hashedPassword password -> liftIO $ Fixture._isValidPassword fixture hashedPassword password