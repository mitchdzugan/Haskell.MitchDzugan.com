{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Controller.Sitemap
where

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

data Sitemap 
  = HomeR
  | LoginR
  | LogoutR
  | RegisterR
  | BlogR
  | NewBlogPostR
  | ViewBlogPostR Integer Text
  | EditBlogPostR Integer Text
  deriving (Eq,Show)
makeBoomerangs ''Sitemap

sitemap =  rHomeR
        <> "login" . rLoginR
        <> "logout" . rLogoutR
        <> "register" . rRegisterR
        <> "blog" . (  rBlogR
                    <> rNewBlogPostR </> "new"
                    <> rViewBlogPostR </> integer </> anyText
                    <> rEditBlogPostR </> integer </> anyText </> "edit"
                    )
