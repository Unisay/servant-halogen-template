module Main where

import Prelude

import Affjax (printResponseFormatError, request)
import App.Api.Endpoint (Endpoint(..))
import App.Api.Request (RequestMethod(..), defaultRequest, readAuthToken)
import App.Api.Utils (decodeAt)
import App.AppM (runAppM)
import App.Component.Router as Router
import App.Data.Route (routeCodec)
import App.Config (Config, LogLevel(..))
import Data.Bifunctor (lmap)
import Data.Either (hush)
import Data.Maybe (Maybe(..), maybe)
import Data.String.NonEmpty (nes)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_, throwError)
import Effect.Aff.Bus as Bus
import Effect.Ref as Ref
import FusionAuth (mkApiKey, mkApiUrl, mkApplicationId)
import Halogen (liftAff, liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Routing.Duplex (parse)
import Routing.Hash (matchesWith)


main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody  
  let 
    apiUrl = mkApiUrl
      $ nes (SProxy :: SProxy "http://localhost:1234")
    fusionAuthApiUrl = mkApiUrl 
      $ nes (SProxy :: SProxy "http://localhost:9011/api")
    fusionAuthApiKey = mkApiKey 
      $ nes (SProxy :: SProxy "joeyL9RcHmc2ND7d52FPyLHisqNuiT882xDtx7ebZTc")
    logLevel = Dev
  applicationId <- maybe (throwError $ error "ApplicationId") pure 
    $ mkApplicationId
    $ nes (SProxy :: SProxy "f4876cf7-befa-47fe-b9da-58de0179d187")
  currentUser <- liftEffect $ Ref.new Nothing
  userBus <- liftEffect Bus.make

  liftEffect readAuthToken >>= traverse_ \token -> do
    let requestOptions = { endpoint: User, method: Get }
    res <- liftAff $ request 
      $ defaultRequest apiUrl (Just token) requestOptions
    let u = decodeAt "user" =<< lmap printResponseFormatError res.body
    liftEffect $ Ref.write (hush u) currentUser

  let 
    config :: Config
    config = 
      { apiUrl
      , applicationId 
      , fusionAuthApiUrl
      , fusionAuthApiKey
      , logLevel
      , userEnv: { currentUser, userBus }
      }

    rootComponent :: H.Component HH.HTML Router.Query Unit Void Aff
    rootComponent = H.hoist (runAppM config) Router.component

  halogenIO <- runUI rootComponent unit body

  -- https://github.com/slamdata/purescript-routing/blob/v8.0.0/GUIDE.md
  -- https://github.com/natefaubion/purescript-routing-duplex/blob/v0.2.0/README.md
  void $ liftEffect $ matchesWith (parse routeCodec) \old new ->
    when (old /= Just new) do
      launchAff_ $ halogenIO.query $ H.tell $ Router.Navigate new