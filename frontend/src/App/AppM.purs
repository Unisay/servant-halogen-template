-- | A custom application monad that provides concrete implementations for 
-- | capabilities like logging, navigation, and resource management. 
-- | This is our production monad -- it interprets our capabilities as they are 
-- | meant to run on our production site. 

module App.AppM where

import Prelude

import App.Api.Endpoint (Endpoint(..))
import App.Api.Request (RequestMethod(..))
import App.Api.Request as Request
import App.Api.Utils (authenticate, decode, decodeWithAt, mkAuthRequest)
import App.Capability.LogMessages (class LogMessages)
import App.Capability.Navigate (class Navigate, navigate)
import App.Capability.Now (class Now)
import App.Capability.Resource.User (class ManageUser)
import App.Data.Log as Log
import App.Data.Profile (decodeProfileWithEmail)
import App.Data.Route as Route
import App.Env (Env, LogLevel(..))
import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Data.Argonaut.Encode (encodeJson)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff.Bus as Bus
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Now as Now
import Effect.Ref as Ref
import Routing.Duplex (print)
import Routing.Hash (setHash)
import Type.Equality (class TypeEquals, from)


newtype AppM a = AppM (ReaderT Env Aff a)

runAppM :: Env -> AppM ~> Aff
runAppM env (AppM m) = runReaderT m env

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM

-- | We can't write instances for type synonyms, and we defined our 
-- | environment (`Env`) as a type synonym for convenience. 
-- | To get around this, we can use `TypeEquals` to assert that 
-- | types `a` and `b` are in fact the same. 
-- |
-- | In our case, we'll write a `MonadAsk` (an alternate name for `Reader`) 
-- | instance for the type `e`, and assert it is our `Env` type. 
-- | This is how we can write a type class instance for a type synonym, 
-- | which is otherwise disallowed.
-- |
instance monadAskAppM :: TypeEquals e Env => MonadAsk e AppM where
  ask = AppM $ asks from

instance nowAppM :: Now AppM where
  now = liftEffect Now.now
  nowDate = liftEffect Now.nowDate
  nowTime = liftEffect Now.nowTime
  nowDateTime = liftEffect Now.nowDateTime

instance logMessagesAppM :: LogMessages AppM where
  logMessage log = do 
    env <- ask
    liftEffect case env.logLevel, Log.reason log of
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
      Request.removeToken 
    liftAff do
      Bus.write Nothing userBus
    navigate Route.Home

-- | Our first resource class describes what operations we have available to manage users. Logging 
-- | in and registration require manipulating a token, but we've designed the `Auth.Token` type so its 
-- | contents can't be read by any function outside the `Api.Request` module. For that reason, 
-- | the `login` and `register` implementations are directly imported. The others use our nicer 
-- | `mkRequest` and `mkAuthRequest` helpers.
instance manageUserAppM :: ManageUser AppM where
  loginUser = 
    authenticate Request.login

  registerUser = 
    authenticate Request.register

  getCurrentUser = 
    mkAuthRequest { endpoint: User, method: Get }
      >>= decode (decodeWithAt decodeProfileWithEmail "user")

  updateUser fields = 
    void $ mkAuthRequest 
      { endpoint: User, method: Post (Just (encodeJson fields)) }

