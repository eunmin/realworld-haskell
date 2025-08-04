{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module RealWorld.Infra.Web.Handler.Article.ListTag where

import Effectful (Eff)
import qualified Effectful as Eff
import RealWorld.Domain.Query.Data (TagList)
import RealWorld.Domain.Query.QueryService (QueryService)
import qualified RealWorld.Domain.Query.QueryService as QueryService
import RealWorld.Infra.Web.Auth (ApiAuth)
import Servant (Get, JSON, (:>))

type Route =
  "tags"
    :> Get '[JSON] TagList

handler :: (QueryService Eff.:> es) => ApiAuth -> Eff es TagList
handler _ = QueryService.getTags