{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GigGuide.AppM
  ( AppM(..)
  ) where

import           Control.Monad.Reader

newtype AppM r a = 
  AppM { 
    unApp :: ReaderT r IO a
  } deriving (
    Functor,
    Applicative,
    Monad,
    MonadReader r,
    MonadIO
  )