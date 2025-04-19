{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Text.HTML.Scalpel.Class where

import           Data.Either
import           Control.Monad.Except
import           Control.Monad.Trans (lift)
import           Text.HTML.Scalpel hiding (attr, attrs, html, htmls,
                                               innerHTML, innerHTMLs, text, 
                                               texts, chroot, chroots)
import qualified Text.HTML.Scalpel as C
import           Text.StringLike (StringLike)

newtype ScrapeM str e a = ScrapeM
  { unScrapeM :: ExceptT e (Scraper str) a }
  deriving(
    Functor,
    Applicative,
    Monad,
    MonadError e,
    MonadScraper str
  )

runScrapeM :: StringLike str =>
              ScrapeM str e a ->
              str ->
              Maybe (Either e a)
runScrapeM m s = C.scrapeStringLike s . runExceptT $ unScrapeM m

class (Show str, StringLike str, Monad m) => MonadScraper str m | m -> str where
  attr :: String -> Selector -> m str
  attrs :: String -> Selector -> m [str]
  html :: Selector -> m str
  htmls :: Selector -> m [str]
  innerHTML :: Selector -> m str
  innerHTMLs :: Selector -> m [str]
  text :: Selector -> m str
  texts :: Selector -> m [str]
  chroot :: Selector -> m a -> m a
  chroots :: Selector -> m a -> m [a]

instance (Show str, StringLike str) => MonadScraper str (Scraper str) where
  attr = C.attr
  attrs = C.attrs
  html = C.html
  htmls = C.htmls
  innerHTML = C.innerHTML
  innerHTMLs = C.innerHTMLs
  text = C.text
  texts = C.texts
  chroot = C.chroot
  chroots = C.chroots

instance (Show str, StringLike str, MonadScraper str m) => MonadScraper str (ExceptT e m) where
  attr a b = lift (attr a b)
  attrs a b = lift (attrs a b)
  html = lift . html
  htmls = lift . htmls
  innerHTML = lift . innerHTML
  innerHTMLs = lift . innerHTMLs
  text s = lift (text s)
  texts = lift . texts
  chroot s m = ExceptT $ chroot s (runExceptT m)
  chroots s m = ExceptT $ fmap f (chroots s (runExceptT m))
    where f [] = Right []
          f es = case partitionEithers es of
                  (l:_, _) -> Left l
                  (_,  rs) -> Right rs

class LiftScraper str t where
  liftScraper  :: Scraper str a -> t a

instance LiftScraper str (ScrapeM str e) where
  liftScraper = ScrapeM . lift

chroots' :: StringLike str => Selector -> ScrapeM str e a -> Scraper str [Either e a]
chroots' s = C.chroots s . runExceptT . unScrapeM
