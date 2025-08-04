{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module RealWorld.Domain.Adapter.Gateway.TokenGateway where

import Data.ULID (ULID)
import Effectful (Effect)
import Effectful.TH (makeEffect)
import RealWorld.Domain.Command.User.Value (Token)
import Relude

data TokenGateway :: Effect where
  Generate :: ULID -> Int -> TokenGateway m Token
  Verify :: Token -> TokenGateway m (Maybe ULID)

makeEffect ''TokenGateway
