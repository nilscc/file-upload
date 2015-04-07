{-# LANGUAGE OverloadedStrings #-}

module Page.Html.NotFound where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Page.Html.Common

notFound :: Html
notFound = H.docTypeHtml $ do
  H.head $ do
    css "static/common.css"
    css "static/error.css"
  H.body $ do
    H.div ! A.class_ "not-found" $ do
      H.h1 "Not found."
      H.p "The page you requested was not found."
