module Main where

import Control.Exception (Exception, SomeException)
import Data.Data (Typeable)
import Data.DisjunctBool (DisjunctBool (DisjunctBool))
import Data.Maybe (Maybe (Just, Nothing))
import System.Console.GetOpt
  ( ArgDescr (NoArg, OptArg, ReqArg),
    OptDescr (Option),
  )
import VSM
  ( BaseOptions (BaseOptions, force, runSvDir, serviceDir),
    IOEither,
    ListCommandOptions (ListCommandOptions, enabled, tested),
    ManageCommand (Disable, Enable, Test),
    Service,
    defaultBaseConfig,
    defaultListCommandOptions,
    listDirectory,
    listServices,
    manageCommand,
  )
import Prelude

data MyException = MissingFlagException | AnyException
  deriving (Typeable, Show)

instance Exception MyException

baseOptionFlags :: [OptDescr (BaseOptions -> Maybe BaseOptions)]
baseOptionFlags =
  [ Option
      ['f']
      ["force"]
      (NoArg (\config -> Just $ config {force = False}))
      "Exit 0 always performs exit 1",
    Option
      ['s']
      ["service-dir"]
      (OptArg (\m config -> (\dir -> config {serviceDir = dir}) <$> m) "service")
      "the service directory, where all services exist",
    Option
      ['r']
      ["run-sv-dir"]
      (OptArg (\m config -> (\dir -> config {runSvDir = dir}) <$> m) "service")
      "the run service directory, where services live that are enabled"
  ]

-- just true and just false equals nothing
listDirectoryFlags :: [OptDescr (ListCommandOptions -> Maybe ListCommandOptions)]
listDirectoryFlags =
  [ Option
      ['e']
      ["enable"]
      (NoArg (\config -> Just $config {enabled = DisjunctBool $ Just True}))
      "List enabled services",
    Option
      ['d']
      ["disable"]
      (NoArg (\config -> Just $config {enabled = DisjunctBool $ Just False}))
      "List enabled services",
    Option
      ['t']
      ["test"]
      (NoArg (\config -> Just $config {tested = DisjunctBool $ Just True}))
      "List enabled services",
    Option
      ['u']
      ["untest"]
      (NoArg (\config -> Just $config {tested = DisjunctBool $ Just False}))
      "List untested services"
  ]

manageServiceFlags :: [OptDescr ManageCommand]
manageServiceFlags =
  [ Option
      ['e']
      ["enable"]
      (ReqArg Enable "service")
      "Enable a service",
    Option
      ['d']
      ["disable"]
      (ReqArg Disable "service")
      "Disable a service",
    Option
      ['t']
      ["test"]
      (ReqArg Test "service")
      "test a service"
  ]

--  CLI HERE!
main :: IO ()
main = pure ()