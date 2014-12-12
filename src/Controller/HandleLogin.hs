{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Controller.HandleLogin where

import Controller.Import
import Controller.Sitemap

  -- | Render login form
handleLogin :: Maybe Text -> Handler App (AuthManager App) ()
handleLogin authError = heistLocal (bindSplices errs) $ render "_view/login"
  where
    errs = maybe noSplices splice authError
    splice err = "loginError" ## textSplice err

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