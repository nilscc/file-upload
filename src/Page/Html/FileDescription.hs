{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

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

humanReadable :: (Integral a, Show a) => a -> String
humanReadable i = go 0 (fromIntegral i)
 where
  units = ["B", "KB", "MB", "GB"] :: [String]
  go :: Int -> Double -> String
  go ix j | j > 1024          = go (ix+1) (j / 1024.0)
          | ix < length units = printf "%.1f %s" j (units !! ix)
          | otherwise         = show i

fileDescription :: FileID -> FileDescription -> Html
fileDescription fid fd = do

  H.div ! A.class_ "file"
        ! dataAttribute "id"            (toValue fid')
        ! dataAttribute "name"          (toValue $ fileName fd)
        ! dataAttribute "size"          (toValue $ fileSize fd)
        ! dataAttribute "checksum"      (toValue $ fileChecksum fd)
        ! dataAttribute "content-type"  (toValue $ fileContentType fd)
        ! dataAttribute "created-at"    (toValue $ createdAt "%c")
        $ do

    H.div ! A.class_ "file-created-at noselect" $ do
      toHtml $ createdAt "%-e. %B %Y, %T"

    H.div ! A.class_ "file-actions noselect" $ do

      -- delete file icon
      H.i ! A.class_ "remove fa fa-times clickable noselect"
          ! A.title "Delete file"
          ! A.onclick (toValue $ "deleteFile('" ++ fid' ++ "')")
          $ return ()

    H.div ! A.class_ "main-container" $ do

      H.a ! A.class_ "file-name" ! A.href (toValue $ "files/" ++ fid') $ do
        toHtml $ fileName fd

      H.div ! A.class_ "file-size noselect" $ do
        toHtml $ humanReadable (fileSize fd)

      H.div ! A.class_ "file-checksum noselect" $ do
        "MD5:"
        H.span ! A.class_ "select" $ toHtml (fileChecksum fd)

 where
  fid'          = map toLower . takeWhile ('=' /=) . B8.unpack $ Base32.encode fid
  createdAt fmt = formatTime defaultTimeLocale fmt (fileCreatedAt fd)

partialFileDescription
  :: PartialFileStatus
  -> FileDescription
  -> Html
partialFileDescription = partialFileDescription' H.div

partialFileDescription'
  :: (Html -> Html)  -- ^ HTML container element (typicall `div` or `li`)
  -> PartialFileStatus
  -> FileDescription
  -> Html
partialFileDescription' cont pfs fd = do

  cont ! A.class_ "partial-upload noselect"
       ! dataAttribute "name"             (toValue $ fileName fd)
       ! dataAttribute "size"             (toValue $ fileSize fd)
       ! dataAttribute "checksum"         (toValue $ fileChecksum fd)
       ! dataAttribute "partial-size"     (toValue $ partialFileSize pfs)
       ! dataAttribute "partial-checksum" (toValue $ partialFileChecksum pfs)
       ! dataAttribute "content-type"     (toValue $ fileContentType fd)
       ! dataAttribute "created-at"       (toValue $ createdAt "%c")
       $ do

    H.i ! A.class_ "fa fa-times clickable remove" $ return ()

    H.div ! A.class_ "main-container" $ do

      H.div ! A.class_ "filename select" $ do
        toHtml $ fileName fd
        -- progress bar
        H.span ! A.class_ "progress" ! A.style (toValue $ "width: " ++ show progress ++ "%") $
          return ()

      H.div ! A.class_ "filesize" $ do
        toHtml $ humanReadable(fileSize fd)
        -- percentage uploaded
        H.span ! A.class_ "percent noselect" $ do
          toHtml (printf "%.1f" progress :: String)
          "% uploaded"

      H.div ! A.class_ "upload-status noselect" $ do
        H.i ! A.class_ "fa fa-repeat" $ return ()
        H.div $ " This is a partial upload. Select the corresponding file to resume the upload."

 where
  createdAt fmt = formatTime defaultTimeLocale fmt (fileCreatedAt fd)
  progress      = 100 * fromIntegral (partialFileSize pfs) / fromIntegral (fileSize fd) :: Double
