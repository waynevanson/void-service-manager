module Control.IOEither where

import Control.Monad.Trans.Except (ExceptT)

type IOEither e a = ExceptT e IO a