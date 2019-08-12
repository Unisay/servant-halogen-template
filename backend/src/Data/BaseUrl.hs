module Data.BaseUrl where

import Network.URI hiding (path)
import Preamble
import Refined

import Data.String          (String)
import Data.Typeable        (typeOf)
import Prelude              (error)
import Refined.Unsafe       (unsafeRefine)
import System.Envy.Extended (Var (..))


data Absolute
data NoQuery
data NoFragment

type BaseUrlPredicate = And NoFragment NoQuery
type BaseUrl = Refined BaseUrlPredicate URI

instance Predicate Absolute URI where
  validate p uri =
    unless (uriIsAbsolute uri) $ throwRefine $ RefineOtherException
      (typeOf p)
      "Invalid BaseUrl: non-absolute URI"

instance Predicate NoQuery URI where
  validate p uri =
    unless (null $ uriQuery uri) $ throwRefine $ RefineOtherException
      (typeOf p)
      "Invalid BaseUrl: non-empty query"

instance Predicate NoFragment URI where
  validate p uri =
    unless (null $ uriFragment uri) $ throwRefine $ RefineOtherException
      (typeOf p)
      "Invalid BaseUrl: non-empty fragment"

instance Var BaseUrl where
  toVar   = toS . baseUrlToString
  fromVar = parseURI >=> hush . refine

baseUriHost :: StringConv String s => BaseUrl -> s
baseUriHost uri =
  toS
    $ maybe (error "BaseUrl without authority") uriRegName
    $ uriAuthority
    $ unrefine uri

baseUriPath :: StringConv String s => BaseUrl -> s
baseUriPath uri = toS $ uriPath $ unrefine uri

baseUrlSuffixed :: BaseUrl -> String -> Maybe BaseUrl
baseUrlSuffixed url relRef = do
  path <- parseRelativeReference relRef
  return $ unsafeRefine $ nonStrictRelativeTo path (unrefine url)

baseUrlToString :: BaseUrl -> String
baseUrlToString = unrefine >>> \uri -> uriToString identity uri ""
