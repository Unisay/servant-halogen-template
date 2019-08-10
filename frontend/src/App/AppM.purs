-- | A custom application monad that provides concrete implementations for 
-- | capabilities like logging, navigation, and resource management. 
-- | This is our production monad -- it interprets our capabilities as they are 
-- | meant to run on our production site. 

module App.AppM where

import Prelude

import App.Api.Request (writeAuthToken)
import App.Api.Request as Request
import App.Capability.LogMessages (class LogMessages, logError)
import App.Capability.Navigate (class Navigate, navigate)
import App.Capability.Now (class Now)
import App.Capability.Resource.User (class ManageUser)
import App.Config (Config, LogLevel(..))
import App.Data.Log as Log
import App.Data.Route as Route
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, never)
import Effect.Aff.Bus as Bus
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Now as Now
import Effect.Ref as Ref
import FusionAuth as Auth
import Routing.Duplex (print)
import Routing.Hash (setHash)
import Type.Equality (class TypeEquals, from)


newtype AppM a = AppM (ReaderT Config Aff a)

runAppM :: Config -> AppM ~> Aff
runAppM config (AppM m) = runReaderT m config

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM

instance monadAskAppM :: TypeEquals e Config => MonadAsk e AppM where
  ask = AppM $ asks from

instance monatThrowFusionAuthError :: MonadThrow Auth.FusionAuthError AppM where
  throwError err = logError (show err) *> liftAff never

instance nowAppM :: Now AppM where
  now = liftEffect Now.now
  nowDate = liftEffect Now.nowDate
  nowTime = liftEffect Now.nowTime
  nowDateTime = liftEffect Now.nowDateTime

instance logMessagesAppM :: LogMessages AppM where
  logMessage log = do 
    config <- ask
    liftEffect case config.logLevel, Log.reason log of
      Prod, Log.Debug -> pure unit
      _, _ -> Console.log $ Log.message log

-- | Our app uses hash-based routing, so to navigate from place to place, 
-- | we'll just set the hash.
instance navigateAppM :: Navigate AppM where
  navigate = liftEffect <<< setHash <<< print Route.routeCodec 
  logout = do
    { currentUser, userBus } <- asks _.userEnv
    liftEffect do 
      Ref.write Nothing currentUser
      Request.removeAuthToken 
    liftAff do
      Bus.write Nothing userBus
    navigate Route.Home

instance manageUserAppM :: ManageUser AppM where
  loginUser { email, password } = do
    let request = Auth.defaultLoginRequest email password
    response <- Auth.loginUser request
    for_ response \{ token, user } -> do 
      { userEnv } <- ask
      liftEffect do 
        writeAuthToken token 
        Ref.write (Just user) userEnv.currentUser
      -- any time we write to the current user ref, 
      -- we should also broadcast the change 
      liftAff $ Bus.write (Just user) userEnv.userBus
    pure response

  registerUser info = do
    applicationId <- asks _.applicationId
    let 
      user = Auth.defaultUserOut 
        { email = pure info.email 
        , password = pure info.password
        , firstName = pure info.firstName
        , lastName = pure info.lastName
        }
      registration = Auth.defaultRegistration applicationId
      request = Auth.defaultRegisterRequest user registration
    Auth.registerUser request

  findUserByEmail = Auth.findUserByEmail 