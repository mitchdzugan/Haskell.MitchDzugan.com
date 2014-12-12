{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Controller.Sitemap where

import Prelude hiding (id,(.))
import Control.Applicative ((<$>),(<|>))
import Control.Monad.Trans  (lift,liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Control.Category (id,(.))
import Snap.Core
import Text.Boomerang.TH (makeBoomerangs)
import Text.Boomerang.Texts (unparseTexts)
import Web.Routes.Boomerang ((<>),(</>),integer,anyText,boomerangSiteRouteT)

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

redirectSM :: MonadSnap m => Sitemap -> m ()
redirectSM = redirect . T.encodeUtf8 . showUrl

showUrl :: Sitemap -> Text
showUrl route = case unparseTexts sitemap route of
  Just [] -> T.pack "/"
  Just s  -> T.intercalate (T.pack "/") s
  _ -> T.pack "/InvalidRoute"