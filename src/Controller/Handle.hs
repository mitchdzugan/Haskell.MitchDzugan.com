{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Controller.Handle where

import Controller.Import
import Controller.HandleBlog
import Controller.HandleLogin
import Controller.Sitemap

handle :: Sitemap -> RouteT Sitemap AppHandler ()
handle url = lift $
  case url of
    HomeR -> render "_view/index"
    LoginR -> with auth handleLoginSubmit
    LogoutR -> with auth handleLogout
    RegisterR -> with auth handleNewUser
    BlogR -> writeText $ showUrl $ ViewBlogPostR 37 "lol"
    NewBlogPostR -> render "_view/index"
    ViewBlogPostR ind title -> writeText $ "Post: #" `append` title
    EditBlogPostR ind title -> writeText $ "Post: #" `append` title