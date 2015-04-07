{-# LANGUAGE OverloadedStrings #-}

module Page.Html.FileDescription where

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Base32 as Base32
import Data.Char

import Text.Blaze.Html -- .Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Printf

import Data.Time

-- import Types
import Acid.Files

fileDescription :: FileID -> FileDescription -> Html
fileDescription fid fd = do

  H.div ! A.class_ "file"
        ! dataAttribute "id"   (toValue fid')
        ! dataAttribute "name" (toValue $ fileName fd)
        ! dataAttribute "size" (toValue $ fileSize fd)
        ! dataAttribute "content-type" (toValue $ fileContentType fd)
        ! dataAttribute "created-at"   (toValue $ createdAt "%c")
        $ do

    H.div ! A.class_ "file-created-at noselect" $ do
      toHtml $ createdAt "%-e. %B %Y, %T"

    H.div ! A.class_ "file-actions noselect" $ do

      -- delete file icon
      H.i ! A.class_ "remove fa fa-times clickable noselect"
          ! A.title "Delete file"
          ! A.onclick (toValue $ "deleteFile('" ++ fid' ++ "')")
          $ return ()

    H.a ! A.class_ "file-name" ! A.href (toValue $ "files/" ++ fid') $ do
      toHtml $ fileName fd

    H.div ! A.class_ "file-size noselect" $ do
      toHtml $ humanReadable (fileSize fd)

 where
  fid'          = map toLower . takeWhile ('=' /=) . B8.unpack $ Base32.encode fid
  createdAt fmt = formatTime defaultTimeLocale fmt (fileCreatedAt fd)
  humanReadable i = go 0 (fromIntegral i)
   where
    units = ["B", "KB", "MB", "GB"] :: [String]
    go :: Int -> Double -> String
    go ix j | j > 1024          = go (ix+1) (j / 1024.0)
            | ix < length units = printf "%.1f %s" j (units !! ix)
            | otherwise         = show i
