module VSM.List where

import Control.Exception (Exception, try)
import Control.IOEither (IOEither)
import Control.Monad.Trans.Except (ExceptT (ExceptT))
import Data.Bool (bool)
import Data.DisjunctBool (DisjunctBool (DisjunctBool), disjunctBool)
import qualified System.Directory as FS
import System.FilePath ((</>))
import qualified System.FilePath as FP
import qualified System.Posix.Files as FS
import VSM.Configuration (Configuration (Configuration, runSvDir, serviceDir))
import VSM.Manage (Service)
import Prelude

listDirectory :: Exception e => FilePath -> IOEither e [FilePath]
listDirectory = ExceptT . try . FS.listDirectory

listEnabled :: Exception e => Configuration -> IOEither e [Service]
listEnabled Configuration {runSvDir = dir} = fmap FP.takeBaseName <$> listDirectory dir

listAll :: Exception e => Configuration -> IOEither e [Service]
listAll Configuration {serviceDir = dir} = fmap FP.takeBaseName <$> listDirectory dir

listDisabled :: Exception e => Configuration -> IOEither e [Service]
listDisabled config = do
  enabled <- listEnabled config
  all <- listAll config
  pure $ filter (`notElem` enabled) all

isTested :: Exception e => Configuration -> Service -> IOEither e Bool
isTested Configuration {serviceDir = dir} service = ExceptT $ try $ FS.doesFileExist $ dir </> service </> "down"

isUntested :: Exception e => Configuration -> Service -> IOEither e Bool
isUntested options service = not <$> isTested options service

data ListCommandOptions = ListCommandOptions
  { enabled :: DisjunctBool,
    tested :: DisjunctBool
  }
  deriving (Show, Eq)

instance Semigroup ListCommandOptions where
  (<>) x y =
    ListCommandOptions
      { enabled = enabled x <> enabled y,
        tested = tested x <> tested y
      }

defaultListCommandOptions :: ListCommandOptions
defaultListCommandOptions =
  ListCommandOptions
    { enabled = DisjunctBool Nothing,
      tested = DisjunctBool Nothing
    }

-- list services

foldy :: Exception e => (Configuration -> Service -> IOEither e Bool) -> Configuration -> [Service] -> IOEither e [String]
foldy f baseOptions =
  foldl
    -- append service if it exists
    -- surely there's a better way
    (\b service -> b >>= (\services -> f baseOptions service >>= bool (pure $ services <> [service]) b))
    (pure ([] :: [String]))

listServices :: Exception e => ListCommandOptions -> Configuration -> IOEither e [String]
listServices ListCommandOptions {enabled = enabled, tested = tested} baseOptions = do
  services <- disjunctBool listDisabled listEnabled listAll enabled baseOptions
  disjunctBool (foldy isUntested) (foldy isTested) (\bo sz -> pure sz) tested baseOptions services
