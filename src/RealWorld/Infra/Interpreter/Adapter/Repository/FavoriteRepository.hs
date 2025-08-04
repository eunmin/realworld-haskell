{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module RealWorld.Infra.Interpreter.Adapter.Repository.FavoriteRepository where

import Effectful (Eff, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Reader.Dynamic (Reader)
import RealWorld.Domain.Adapter.Repository.FavoriteRepository (FavoriteRepository (..))
import qualified RealWorld.Infra.Component.Database as Database
import qualified RealWorld.Infra.Database.PgFavoriteRepository as PgFavoriteRepository
import Relude hiding (Reader)

run :: (Reader Database.State :> es, IOE :> es) => Eff (FavoriteRepository : es) a -> Eff es a
run = interpret $ \_ -> \case
  Save favorite -> PgFavoriteRepository.save favorite
  FindById favoriteId -> PgFavoriteRepository.findById favoriteId
  Delete favorite -> PgFavoriteRepository.delete favorite
