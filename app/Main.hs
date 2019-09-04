{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Network.Wai.Handler.Warp (run)

import Servant (type (:>), type (:<|>), Proxy(Proxy), (:<|>)(..))
import Servant.Server (serve)

import API.User (UserAPI, userAPI)
import API.Translation (TranslationAPI, translationAPI)
import API.Word (WordAPI, wordAPI)
import API.WordSet (WordSetAPI, wordSetAPI)

type ServiceAPI
  =    UserAPI
  :<|> WordAPI
  :<|> WordSetAPI
  :<|> TranslationAPI

serviceAPI :: Proxy ServiceAPI
serviceAPI = Proxy

main :: IO ()
main = do
  let server
        =    userAPI
        :<|> wordAPI
        :<|> wordSetAPI
        :<|> translationAPI

      app = serve serviceAPI server
  run 3000 app
