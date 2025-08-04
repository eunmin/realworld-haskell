{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module RealWorld.Infra.Interpreter.Adapter.Repository.CommentRepository where

import Effectful (Eff, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Reader.Dynamic (Reader)
import RealWorld.Domain.Adapter.Repository.CommentRepository (CommentRepository (..))
import qualified RealWorld.Infra.Component.Database as Database
import qualified RealWorld.Infra.Database.PgCommentRepository as PgCommentRepository
import Relude hiding (Reader)

run :: (Reader Database.State :> es, IOE :> es) => Eff (CommentRepository : es) a -> Eff es a
run = interpret $ \_ -> \case
  Save comment -> PgCommentRepository.save comment
  FindById commentId -> PgCommentRepository.findById commentId
  Delete comment -> PgCommentRepository.delete comment