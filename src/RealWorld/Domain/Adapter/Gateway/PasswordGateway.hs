{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module RealWorld.Domain.Adapter.Gateway.PasswordGateway where

import Effectful (Effect)
import Effectful.TH (makeEffect)
import RealWorld.Domain.Command.User.Value (HashedPassword, Password)
import Relude

data PasswordGateway :: Effect where
  HashPassword :: Password -> PasswordGateway m (Maybe HashedPassword)
  IsValidPassword :: HashedPassword -> Password -> PasswordGateway m Bool

makeEffect ''PasswordGateway