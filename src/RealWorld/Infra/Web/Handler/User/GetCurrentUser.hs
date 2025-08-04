{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module RealWorld.Infra.Web.Handler.User.GetCurrentUser where

import Data.Aeson (ToJSON)

import Control.Lens ((.~))
import Data.Aeson.Types (FromJSON)
import Data.Generics.Labels ()
import Data.Swagger (ToSchema)
import Effectful (Eff)
import qualified Effectful as Eff
import Effectful.Error.Dynamic (Error, throwError)
import RealWorld.Domain.Query.Data (User)
import qualified RealWorld.Domain.Query.Data as Query
import RealWorld.Domain.Query.QueryService (QueryService)
import qualified RealWorld.Domain.Query.QueryService as QueryService
import RealWorld.Infra.Web.Auth (ApiAuth (..))
import RealWorld.Infra.Web.ErrorResponse (notFound')
import RealWorld.Infra.Web.Schema ()
import Relude
import Servant (Get, JSON, ServerError, (:>))

type Route =
  "user"
    :> Get '[JSON] GetCurrentUserResponse

data GetCurrentUserResponse = GetCurrentUserResponse
  { user :: User
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

handler ::
  (QueryService Eff.:> es, Error ServerError Eff.:> es) =>
  ApiAuth ->
  Eff es GetCurrentUserResponse
handler (ApiAuth userId token) = do
  let params = Query.GetCurrentUserParams userId
  user <- QueryService.getCurrentUser params `whenNothingM` throwError (notFound' "User not found")
  pure $ GetCurrentUserResponse $ user & #token .~ token