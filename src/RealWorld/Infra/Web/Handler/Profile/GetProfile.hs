{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module RealWorld.Infra.Web.Handler.Profile.GetProfile where

import Control.Monad.Except (MonadError (throwError))
import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
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

handler :: (MonadIO m, QueryService m, MonadError ServerError m) => ApiAuth -> Text -> m GetProfileResponse
handler (ApiAuth userId _) username = do
  let params = Query.GetProfileParams (Just $ show userId) username
  profile <- QueryService.getProfile params `whenNothingM` throwError (notFound' "User not found")
  pure $ GetProfileResponse profile