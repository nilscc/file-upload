{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Page.Error where

import Happstack.Server as Happstack

import Types
import qualified Page.Html.NotFound as Html

notFound :: UpMonad m => m Response
notFound = Happstack.notFound $ toResponse Html.notFound
