{-# LANGUAGE OverloadedStrings #-}

module Page.Html.Login where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Page.Html.Common
import Page.Html.Index

login :: Html
login = index' [js "static/login.js", css "static/login.css"] $ do
  H.div ! A.class_ "login" $ do
    H.p "This is sacred land..."
    H.form ! A.action "/" ! A.method "post" $ do
      H.input ! A.type_ "text" ! A.name "session-key"
      H.input ! A.type_ "submit" ! A.value "enter"
