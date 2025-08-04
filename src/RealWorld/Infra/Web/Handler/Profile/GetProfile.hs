{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module RealWorld.Infra.Web.Handler.Profile.GetProfile where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Effectful (Eff)
import qualified Effectful as Eff
import Effectful.Error.Dynamic (Error, throwError)
import RealWorld.Domain.Query.Data (Profile)
import qualified RealWorld.Domain.Query.Data as Query
import RealWorld.Domain.Query.QueryService (QueryService)
import qualified RealWorld.Domain.Query.QueryService as QueryService
import RealWorld.Infra.Converter.Aeson ()
import RealWorld.Infra.Web.Auth (ApiAuth (..))
import RealWorld.Infra.Web.ErrorResponse (notFound')
import RealWorld.Infra.Web.Schema ()
import Relude
import Servant (Capture, Get, JSON, ServerError, (:>))

type Route =
  "profiles"
    :> Capture "username" Text
    :> Get '[JSON] GetProfileResponse

data GetProfileResponse = GetProfileResponse
  { profile :: Profile
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

handler ::
  ( QueryService Eff.:> es
  , Error ServerError Eff.:> es
  ) =>
  ApiAuth ->
  Text ->
  Eff es GetProfileResponse
handler (ApiAuth userId _) username = do
  let params = Query.GetProfileParams (Just userId) username
  profile <- QueryService.getProfile params `whenNothingM` throwError (notFound' "User not found")
  pure $ GetProfileResponse profile