-- | Used to manage a service
module VSM.Manage where

import Control.Exception (Exception, try)
import Control.IOEither (IOEither)
import Control.Monad.Trans.Except (ExceptT (ExceptT))
import qualified System.Directory as FS
import System.FilePath ((</>))
import qualified System.Posix.Files as FS
import VSM.Configuration (Configuration (Configuration, runSvDir, serviceDir))

type Service = String

data Manage
  = Enable
  | Disable
  | Test

type ManageService = (Manage, Service)

isService :: Exception e => Configuration -> Service -> IOEither e Bool
isService Configuration {serviceDir = dir} service = ExceptT . try . FS.doesFileExist $ dir </> service </> "run"

-- does the target need to be a certain format?
enableService :: Exception e => Configuration -> Service -> IOEither e ()
enableService configuration service = ExceptT $ try $ FS.createSymbolicLink (serviceDir configuration </> service) $ runSvDir configuration

disableService :: Exception e => Configuration -> Service -> IOEither e ()
disableService configuration service = do
  isService configuration service
  ExceptT $ try $ FS.removeDirectoryLink $ runSvDir configuration </> service

testService :: Exception e => Configuration -> Service -> IOEither e ()
testService configuration service = do
  isService configuration service
  ExceptT $ try $ FS.touchFile $ serviceDir configuration </> service </> "down"
  enableService configuration service

manageCommand :: Exception e => ManageService -> Configuration -> IOEither e ()
manageCommand command configuration = case command of
  (Enable, service) -> enableService configuration service
  (Disable, service) -> disableService configuration service
  (Test, service) -> testService configuration service