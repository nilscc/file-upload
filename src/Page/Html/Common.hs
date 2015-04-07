{-# LANGUAGE OverloadedStrings #-}

module Page.Html.Common where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

type Source = AttributeValue

js, css :: Source -> Html
js  s = H.script ! A.type_ "text/javascript" ! A.src  s $ return ()
css s = H.link   ! A.rel   "stylesheet"      ! A.href s
