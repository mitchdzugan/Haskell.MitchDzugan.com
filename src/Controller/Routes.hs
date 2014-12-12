{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Controller.Routes where

import Controller.Sitemap


import Prelude hiding (id,(.))

import Control.Applicative ((<$>),(<|>))
import Control.Monad.Trans  (lift,liftIO)
import Data.Monoid (mconcat)
import qualified Data.ByteString.Char8 as B
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Control.Category (id,(.))
import Snap.Core
import Text.Boomerang.TH (makeBoomerangs)
import Snap.Snaplet.Heist (render, heistLocal)
import Heist.Interpreted (bindString)

import Application
import Web.Routes           (Site(..), RouteT(..), decodePathInfo, encodePathInfo, runSite)
import Web.Routes.Boomerang ((<>),(</>),integer,anyText,boomerangSiteRouteT)
import Snap.Snaplet
import Snap.Snaplet.Auth
import           Heist
import qualified Heist.Interpreted as I

handle :: Sitemap -> RouteT Sitemap AppHandler ()
handle url = lift $
  case url of
    HomeR -> render "_view/index"
    LoginR -> with auth handleLoginSubmit
    LogoutR -> with auth handleLogout
    RegisterR -> with auth handleNewUser
    BlogR -> with auth $ currentUser >>= (writeText . T.pack . show)
    ViewBlogPostR ind title -> writeText $ "Post: #" `T.append` title


{- TODO: Make these functions not depend on Sitemap, sitemap, and handle -}
redirectSM :: MonadSnap m => Sitemap -> m ()
redirectSM = redirect . T.encodeUtf8 . showUrl

site :: Site Sitemap (AppHandler ())
site = boomerangSiteRouteT handle sitemap

showUrl :: Sitemap -> Text
showUrl sm = 
  let url = (uncurry encodePathInfo . formatPathSegments site) sm in
  if (T.null url) || (T.head url /= '/')
    then T.cons '/' url
    else url

mkRoute :: AppHandler ()
mkRoute = do
  pps <- (decodePathInfo . B.dropWhile (== '/') . rqPathInfo) <$> getRequest
  either (const pass) id $ runSite "" site pps
{----------------------------------------------------------------------- -}

  -- | Render login form
handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError = heistLocal (I.bindSplices errs) $ render "_view/login"
  where
    errs = maybe noSplices splice authError
    splice err = "loginError" ## I.textSplice err

------------------------------------------------------------------------------
-- | Handle login submit
handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit =
    loginUser "login" "password" Nothing
              (\_ -> handleLogin err) (redirectSM HomeR)
  where
    err = Just "Unknown user or password"

------------------------------------------------------------------------------
-- | Logs out and redirects the user to the site index.
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirectSM HomeR

------------------------------------------------------------------------------
-- | Handle new user form submit
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method POST handleFormSubmit <|> method GET handleForm
  where
    handleForm = render "_view/new_user"
    handleFormSubmit = registerUser "login" "password" >> redirectSM HomeR