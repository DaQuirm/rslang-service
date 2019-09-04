{-# LANGUAGE DataKinds #-}

module RequiredQueryParam where

import Servant (QueryParam', Required, Strict)

type RequiredQueryParam = QueryParam' '[Required, Strict]