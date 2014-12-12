{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Controller.Sitemap where

import Prelude hiding (id,(.))
import Controller.Import

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
redirectSM = redirect . encodeUtf8 . showUrl

showUrl :: Sitemap -> Text
showUrl route = case unparseTexts sitemap route of
  Just [] -> pack "/"
  Just s  -> intercalate (pack "/") s
  _ -> pack "/InvalidRoute"