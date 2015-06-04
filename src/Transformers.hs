module Transformers where

import Control.Monad.Reader
import Control.Monad.Writer

readerWriterExample :: ReaderT Int (Writer String) Int
readerWriterExample = do x <- ask
                         lift . tell $ show x
                         return $ x + 1