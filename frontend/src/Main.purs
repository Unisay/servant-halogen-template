module Main where

import Preamble

import Affjax (printResponseFormatError, request)
import App.Api.Endpoint (Endpoint(..))
import App.Api.Request (RequestMethod(..), defaultRequest, readAuthToken)
import App.Api.Utils (decodeAt)
import App.AppM (runAppM)
import App.Component.Router as Router
import App.Config (LogLevel(..))
import App.Data.Route (routeCodec)
import Data.Bifunctor (lmap)
import Data.Either (hush)
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Bus as Bus
import Effect.Class.Console (log)
import Effect.Ref as Ref
import FusionAuth as FA
import Halogen (liftAff, liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Record (merge)
import Routing.Duplex (parse)
import Routing.Hash (matchesWith)


main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody  
  currentUser <- liftEffect $ Ref.new Nothing
  userBus <- liftEffect Bus.make

  liftEffect readAuthToken >>= traverse_ \token -> do
    let requestOptions = { endpoint: User, method: Get }
    res <- liftAff $ request 
      $ defaultRequest env.apiUrl (Just token) requestOptions
    let u = decodeAt "user" =<< lmap printResponseFormatError res.body
    liftEffect $ Ref.write (hush u) currentUser

  let 
    config = env `merge`
      { logLevel: Dev
      , userEnv: { currentUser, userBus }
      }

    rootComponent :: H.Component HH.HTML Router.Query Unit Void Aff
    rootComponent = H.hoist (runAppM config) Router.component

  log $ FA.printApiKey config.fusionAuthApiKey
  halogenIO <- runUI rootComponent unit body

  -- https://github.com/slamdata/purescript-routing/blob/v8.0.0/GUIDE.md
  -- https://github.com/natefaubion/purescript-routing-duplex/blob/v0.2.0/README.md
  void $ liftEffect $ matchesWith (parse routeCodec) \old new ->
    when (old /= Just new) do
      launchAff_ $ halogenIO.query $ H.tell $ Router.Navigate new

  log $ "Application started"

foreign import env :: 
  { apiUrl :: FA.ApiUrl                           
  , applicationId :: FA.ApplicationId             
  , fusionAuthApiKey :: FA.ApiKey                 
  , fusionAuthApiUrl :: FA.ApiUrl
  }