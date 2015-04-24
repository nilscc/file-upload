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

index
  :: [(FileID, FileDescription)]
  -> [(PartialFileStatus, FileDescription)]
  -> Html
index desc partials = index' hdrs $ do

  H.div ! A.class_ "fileupload" $ do
    H.h1 "Upload"
    if null partials then
      H.p "Drop files to upload."
     else do
      H.div ! A.class_ "uploadlist" $ do
        H.ul $ do
          mapM_ (uncurry (partialFileDescription' H.li)) partials
        H.button ! A.class_ "upload-all" $ do
          H.i ! A.class_ "fa fa-upload" $ return ()
          " Upload"

  H.ul ! A.class_ "filelist" $ do
    mapM_ (H.li . uncurry fileDescription) desc

 where
  hdrs = [ js  "static/fileupload.js"
         , css "static/fileupload.css"
         , js  "static/filelist.js"
         , css "static/filelist.css"
         ]
