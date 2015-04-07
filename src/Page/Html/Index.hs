{-# LANGUAGE OverloadedStrings #-}

module Page.Html.Index where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Acid.Files
import Page.Html.Common
import Page.Html.FileDescription

index' :: [Html] -> Html -> Html
index' hdr inner = H.docTypeHtml $ do

  H.head $ do

    -- include awesome font
    css "static/css/font-awesome.min.css"

    -- google font
    css "http://fonts.googleapis.com/css?family=Martel+Sans"

    -- jquery
    js  "static/lib/jquery-2.1.3.min.js"

    css "static/common.css"
    sequence_ hdr

  H.body $ do

    inner

index :: [(FileID, FileDescription)] -> Html
index desc = index' hdrs $ do

  H.div ! A.class_ "fileupload" $ do
    H.h1 "Upload"
    H.p "Drop files to upload."

  H.ul ! A.class_ "filelist" $ do
    mapM_ (H.li . uncurry fileDescription) desc

 where
  hdrs = [ js  "static/lib/cryptojs-md5.js"
         , js  "static/fileupload.js"
         , css "static/fileupload.css"
         , js  "static/filelist.js"
         , css "static/filelist.css"
         ]
