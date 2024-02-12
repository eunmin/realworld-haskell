{-# LANGUAGE FlexibleContexts #-}

module Conduit.Infra.Web.Routes where

import Conduit.Domain.Repo (Tx)
import Conduit.Domain.User.Gateway.Token (TokenGateway)
import Conduit.Domain.User.Repo (UserRepository)
import Conduit.Domain.User.Service.Password (PasswordService)
import qualified Conduit.Infra.Web.Controller.User as User
import Relude
import Web.Scotty.Trans

routes :: (MonadIO m, Tx m, UserRepository m, TokenGateway m, PasswordService m) => ScottyT LText m ()
routes = do
  -- middleware logStdout

  defaultHandler $ \_ -> do
    html "서버 에러 입니다"

  post "/api/users" User.register

  -- get (regex "^/hello/([0-9]+)$") $ do
  --   (Database.State pool, _) <- ask
  --   -- affected <-
  --   --   liftIO
  --   --     $ execute
  --   --       conn
  --   --       "insert into books (title, author, created_at) values (?, ?, now())"
  --   --     $ NewBookParams "호랑이를 덫에 가두면" "태캘러"
  --   -- results :: [Only Text] <- liftIO $ query conn "select author from books" ()
  --   let bookId :: Int = 1
  --   books :: [Book] <- liftIO $ withResource pool $ \conn -> do
  --     query conn "select * from books where id = ?" (Only bookId)
  --   -- (a : b : _) :: [Int] <- pure [1, 2, 3]
  --   html $ show books

  notFound $ html "<h1>페이지를 찾을 수 없습니다.</h1>"
