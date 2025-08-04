{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module RealWorld.Domain.Adapter.Repository.CommentRepository where

import Data.ULID (ULID)
import Effectful (Effect)
import Effectful.TH (makeEffect)
import RealWorld.Domain.Command.Article.Entity.Comment (Comment)
import Relude

data CommentRepository :: Effect where
  Save :: Comment -> CommentRepository m Bool
  FindById :: ULID -> CommentRepository m (Maybe Comment)
  Delete :: Comment -> CommentRepository m Bool

makeEffect ''CommentRepository
