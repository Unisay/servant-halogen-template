{-# LANGUAGE TemplateHaskell #-}

module Server.Auth
  ( authHandler
  , JwtPayload(..)
  , Email(..)
  , Role(..)
  , IssuerClaim(..)
  , SubjectClaim(..)
  )
where

import Preamble hiding (exp)

import Control.Monad.Logger             (logError, runStderrLoggingT)
import Data.Aeson                       (FromJSON, ToJSON, parseJSON,
                                         withObject, (.:), (.:?))
import Data.List                        (lookup)
import Data.Time.Clock.POSIX            (POSIXTime)
import Data.UUID                        (UUID)
import Network.HTTP.Types               (hAuthorization, ok200)
import Network.Wai                      (Request, requestHeaders)
import Prelude                          (error)
import Servant                          (Handler, err401, errBody)
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)
import Types                            (Config (..), baseUrlSuffixed,
                                         baseUrlToString)

import qualified Data.ByteString     as BS
import qualified Network.HTTP.Simple as HS


newtype Email = Email Text
  deriving stock Show
  deriving newtype (Eq, Ord, FromJSON, ToJSON)

newtype Role = Role Text
  deriving stock Show
  deriving newtype (Eq, Ord, FromJSON, ToJSON)

newtype IssuerClaim = IssuerClaim Text
  deriving stock Show
  deriving newtype (Eq, Ord, FromJSON, ToJSON)

newtype SubjectClaim = SubjectClaim UUID
  deriving stock Show
  deriving newtype (Eq, Ord, FromJSON, ToJSON)

data JwtPayload
  = JwtPayload
    { email :: Email
    , exp   :: POSIXTime
    , iat   :: POSIXTime
    , iss   :: IssuerClaim
    , sub   :: SubjectClaim
    , roles :: [Role]
    } deriving stock (Eq, Show)

instance FromJSON JwtPayload where
  parseJSON = withObject "root" $ \root -> do
    jwt   <- root .: "jwt"
    email <- jwt .: "email"
    exp   <- jwt .: "exp"
    iat   <- jwt .: "iat"
    iss   <- jwt .: "iss"
    sub   <- jwt .: "sub"
    mbRoles <- jwt .:? "roles"
    return JwtPayload { email, exp, iat, iss, sub, roles = fromMaybe [] mbRoles }


authHandler :: Config -> AuthHandler Request JwtPayload
authHandler config = mkAuthHandler $ \req ->
  case lookup hAuthorization $ requestHeaders req of
    Nothing     -> unauthorized
    Just header -> do
      let authApi = _configAuthApi config
          token   = fromMaybe header $ BS.stripPrefix "Bearer " header
      url <- maybe (error "Invalid relative reference") pure
        $ baseUrlSuffixed authApi "jwt/validate"
      request  <- HS.parseRequestThrow $ baseUrlToString url
      response <-
        liftIO
        $ HS.httpJSON
        $ HS.setRequestHeader hAuthorization ["JWT " <> token]
        $ request
      let status = HS.getResponseStatus response
      if (status == ok200)
        then return $ HS.getResponseBody response
        else runStderrLoggingT $ do
          $(logError)
            $  "Failed to validate JWT token, response status = "
            <> show status
          lift unauthorized

 where
  unauthorized :: forall a . Handler a
  unauthorized = throwError $ err401 { errBody = "Unauthorized" }


