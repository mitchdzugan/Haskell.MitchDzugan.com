{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Data.ByteString (ByteString)
import qualified Data.Text as T
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.AcidState
import           Snap.Snaplet.Auth.Backends.SqliteSimple
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Snaplet.SqliteSimple
import           Snap.Util.FileServe
import           Heist
import qualified Heist.Interpreted as I
------------------------------------------------------------------------------
import           Application
import           Models.Blog
import           Sitemap
------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("", mkRoute)
         , ("", serveDirectory "static")
         , ("", notFound)
         ]

notFound :: MonadSnap m => m ()
notFound = do
  modifyResponse $ setResponseStatus 404 "Not Found"
  writeText "Not Found"

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h  <- nestSnaplet "" heist $ heistInit "templates"
    s  <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    d  <- nestSnaplet "db" db sqliteInit
    au <- nestSnaplet "auth" auth $ initSqliteAuth sess d
    ac <- nestSnaplet "acid" acid $ acidInit initialBlogState
    addRoutes routes
    addAuthSplices h auth
    return $ App h s au d ac

