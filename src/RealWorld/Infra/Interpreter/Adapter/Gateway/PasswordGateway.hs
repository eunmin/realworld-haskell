{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module RealWorld.Infra.Interpreter.Adapter.Gateway.PasswordGateway where

import Effectful (Eff, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import RealWorld.Domain.Adapter.Gateway.PasswordGateway (PasswordGateway (..))
import qualified RealWorld.Infra.Gateway.BcryptPasswordGateway as BcryptPasswordGateway
import Relude

run :: (IOE :> es) => Eff (PasswordGateway : es) a -> Eff es a
run = interpret $ \_ -> \case
  HashPassword password -> BcryptPasswordGateway.hashPassword password
  IsValidPassword hashedPassword password -> BcryptPasswordGateway.isValidPassword hashedPassword password