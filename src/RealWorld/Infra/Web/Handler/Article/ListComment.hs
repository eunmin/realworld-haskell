{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module RealWorld.Infra.Web.Handler.Article.ListComment where

import Effectful (Eff)
import qualified Effectful as Eff
import RealWorld.Domain.Query.Data (CommentList)
import qualified RealWorld.Domain.Query.Data as Query
import RealWorld.Domain.Query.QueryService (QueryService)
import qualified RealWorld.Domain.Query.QueryService as QueryService
import RealWorld.Infra.Web.Auth (ApiAuth (..))
import Relude
import Servant (Capture, Get, JSON, (:>))

type Route =
  "articles"
    :> Capture "slug" Text
    :> "comments"
    :> Get '[JSON] CommentList

handler :: (QueryService Eff.:> es) => ApiAuth -> Text -> Eff es CommentList
handler (ApiAuth userId _) slug = do
  let params =
        Query.GetCommentsParams
          { actorId = pure userId
          , slug = slug
          }
  QueryService.getComments params
