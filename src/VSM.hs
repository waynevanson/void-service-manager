module VSM where

import Control.Exception (Exception, try)
import Control.Monad.Trans.Except (Except, ExceptT (ExceptT), except, runExceptT)
import Data.Bool (bool)
import Data.DisjunctBool (DisjunctBool (DisjunctBool), disjunctBool)
import Data.Monoid (Sum (Sum))
import qualified Data.Text as T
import qualified System.Directory as FS
import System.FilePath (FilePath, combine, dropTrailingPathSeparator, takeBaseName, (</>))
import qualified System.Posix.Files as FS
import Prelude

type IOEither e a = ExceptT e IO a

type Service = String

data BaseOptions = BaseOptions
  { force :: Bool,
    serviceDir :: FilePath,
    runSvDir :: FilePath
  }

-- Under disjunction (||)
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

data ManageCommand
  = Enable Service
  | Disable Service
  | Test Service

data DoubleUp = SIDEA BaseOptions | SIDEB ListCommandOptions

defaultBaseConfig :: BaseOptions
defaultBaseConfig =
  BaseOptions
    { force = False,
      serviceDir = "/etc/sv",
      runSvDir = "/var/service"
    }

defaultListCommandOptions :: ListCommandOptions
defaultListCommandOptions =
  ListCommandOptions
    { enabled = DisjunctBool Nothing,
      tested = DisjunctBool Nothing
    }

-- list services

listDirectory :: Exception e => FilePath -> IOEither e [FilePath]
listDirectory = ExceptT . try . FS.listDirectory

listEnabled :: Exception e => BaseOptions -> IOEither e [Service]
listEnabled BaseOptions {runSvDir = dir} = fmap takeBaseName <$> listDirectory dir

listAll :: Exception e => BaseOptions -> IOEither e [Service]
listAll BaseOptions {serviceDir = dir} = fmap takeBaseName <$> listDirectory dir

listDisabled :: Exception e => BaseOptions -> IOEither e [Service]
listDisabled config = do
  enabled <- listEnabled config
  all <- listAll config
  pure $ filter (`notElem` enabled) all

isTested :: Exception e => BaseOptions -> Service -> IOEither e Bool
isTested BaseOptions {serviceDir = dir} service = ExceptT $ try $ FS.doesFileExist $ dir </> service </> "down"

isUntested :: Exception e => BaseOptions -> Service -> IOEither e Bool
isUntested options service = not <$> isTested options service

foldy :: Exception e => (BaseOptions -> Service -> IOEither e Bool) -> BaseOptions -> [Service] -> IOEither e [String]
foldy f baseOptions =
  foldl
    -- append service if it exists
    -- surely there's a better way
    (\b service -> b >>= (\services -> f baseOptions service >>= bool (pure $ services <> [service]) b))
    (pure ([] :: [String]))

listServices :: Exception e => ListCommandOptions -> BaseOptions -> IOEither e [String]
listServices ListCommandOptions {enabled = enabled, tested = tested} baseOptions = do
  services <- disjunctBool listDisabled listEnabled listAll enabled baseOptions
  disjunctBool (foldy isUntested) (foldy isTested) (\bo sz -> pure sz) tested baseOptions services

-- manage services
isService :: Exception e => BaseOptions -> Service -> IOEither e Bool
isService BaseOptions {serviceDir = dir} service = ExceptT . try . FS.doesFileExist $ dir </> service </> "run"

-- does the target need to be a certain format?
enableService :: Exception e => BaseOptions -> Service -> IOEither e ()
enableService baseOptions service = ExceptT $ try $ FS.createSymbolicLink (serviceDir baseOptions </> service) $ runSvDir baseOptions

disableService :: Exception e => BaseOptions -> Service -> IOEither e ()
disableService baseOptions service = do
  isService baseOptions service
  ExceptT $ try $ FS.removeDirectoryLink $ runSvDir baseOptions </> service

testService :: Exception e => BaseOptions -> Service -> IOEither e ()
testService baseOptions service = do
  isService baseOptions service
  ExceptT $ try $ FS.touchFile $ serviceDir baseOptions </> service </> "down"
  enableService baseOptions service

manageCommand :: Exception e => ManageCommand -> BaseOptions -> IOEither e ()
manageCommand command baseOptions = case command of
  Enable service -> enableService baseOptions service
  Disable service -> disableService baseOptions service
  Test service -> testService baseOptions service