module Lib where

import Control.Applicative ((<|>))
import Control.Category ((<<<), (>>>))
import Data.Maybe (fromMaybe, isJust)
import System.Console.GetOpt (ArgDescr (NoArg, OptArg, ReqArg), ArgOrder (Permute), OptDescr (Option), getOpt)
import System.Directory (listDirectory)
import System.Environment
import System.Exit
import Prelude

type Service = String

data Configuration = Configuration
  { force :: Bool,
    serviceDir :: String,
    runSvDir :: String
  }
  deriving (Show, Eq)

defaultFlags :: Configuration
defaultFlags =
  Configuration
    { force = True,
      serviceDir = "/etc/sv",
      runSvDir = "/var/service"
    }

flagsOptional :: [OptDescr (Configuration -> Maybe Configuration)]
flagsOptional =
  [ Option
      ['f']
      ["force"]
      (NoArg (\config -> Just config {force = True}))
      "ignore errors if thrown",
    Option
      ['s']
      ["service-dir"]
      ( OptArg
          (\fa config -> fmap (\a -> config {serviceDir = a}) fa)
          "directory"
      )
      "the service directory, defaults to /etc/sv",
    Option
      ['t', 'r']
      ["target", "runsvdir"]
      ( OptArg
          (\fa config -> fmap (\runSvDir -> config {runSvDir = runSvDir}) fa)
          "directory"
      )
      "the runsvdir, where services go to be enabled and accessed by runit"
  ]

processflags :: [String] -> Either [String] Configuration
processflags argv =
  case getOpt Permute flagsOptional argv of
    (options, _, []) ->
      maybe (Left ["Could not find the flags for Configuration: Return Nothing"]) Right $
        foldl
          (\b fa -> (b >>= fa) <|> b)
          (Just defaultFlags)
          options
    (_, _, errors) -> Left errors

-- list services is unmutually, concat all,
-- maneg services is mutually, only one arg

data ManageService
  = Enable Service
  | Disable Service
  | Test Service
  | Untest Service

-- one of these
-- throw if more than one
manageServiceFlags :: [OptDescr ManageService]
manageServiceFlags =
  [ Option ['e'] ["enable"] (ReqArg Enable "service") "Enable a service",
    Option ['d'] ["disable"] (ReqArg Disable "service") "Disable a service",
    Option ['t'] ["test"] (ReqArg Test "service") "Test a service",
    Option ['u'] ["untest"] (ReqArg Untest "service") "Untest a service"
  ]

processManagedServiceFlags :: [String] -> Either [String] ManageService
processManagedServiceFlags argv =
  case getOpt Permute manageServiceFlags argv of
    ([x], _, []) -> Right x
    (multi, _, errors) -> Left $ ["Multiple arguments used, try again"] <|> errors

data ListService
  = Enabled
  | Disabled
  | Tested
  | Untested

-- enable and disable on by default
-- "filter in" list
listServiceDefaults = [Enabled, Disabled]

-- test and untested are mutually exclusive
-- "filter out" list

-- any of these
listServicesFlags :: [OptDescr ListService]
listServicesFlags =
  [ Option ['e'] ["enable"] (NoArg Enabled) "Display a list of enabled services",
    Option ['d'] ["disable"] (NoArg Disabled) "Display a list of enabled services",
    Option ['t'] ["test"] (NoArg Tested) "Display a list of enabled services",
    Option ['u'] ["untest"] (NoArg Untested) "Display a list of enabled services"
  ]

-- todo - is non empty
processListServiceFlags :: [String] -> [ListService]
processListServiceFlags argv =
  case getOpt Permute listServicesFlags argv of
    (options, _, []) -> options
    (_, _, errors) -> []
