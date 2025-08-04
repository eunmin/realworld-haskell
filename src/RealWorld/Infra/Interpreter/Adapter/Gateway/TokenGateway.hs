{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module RealWorld.Infra.Interpreter.Adapter.Gateway.TokenGateway where

import Effectful (Eff, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Reader.Dynamic (Reader)
import RealWorld.Domain.Adapter.Gateway.TokenGateway (TokenGateway (..))
import qualified RealWorld.Infra.Gateway.JwtTokenGateway as JwtTokenGateway
import RealWorld.Infra.System (JwtSecret)
import Relude hiding (Reader)

run :: (IOE :> es, Reader JwtSecret :> es) => Eff (TokenGateway : es) a -> Eff es a
run = interpret $ \_ -> \case
  Generate ulid expiresIn -> JwtTokenGateway.generate ulid expiresIn
  Verify token -> JwtTokenGateway.verify token