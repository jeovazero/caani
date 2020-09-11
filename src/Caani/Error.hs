module Caani.Error (
  CaaniError(..),
  CaaniErrorType(..),
  tryIO,
  tryOrThrow,
  rightOrThrow
  ) where

import Control.Exception (Exception, try, throwIO)
import Control.Exception.Base (IOException)

data CaaniError
  = CaaniError CaaniErrorType
  deriving (Show, Eq)

instance Exception CaaniError

data CaaniErrorType
  = BoundaryLimit
  | LoadFileError
  | LoadTagError
  | LoadFontError
  | InvalidCode
  deriving (Show, Eq)

tryIO :: IO a -> IO (Either IOException a)
tryIO = try

rightOrThrow :: IO (Either a b) -> CaaniErrorType -> IO b
rightOrThrow ioEither etype =
  ioEither >>= \result ->
    case result of
      Left _ -> throwIO $ CaaniError etype
      Right r -> pure r

tryOrThrow :: IO a -> CaaniErrorType -> IO a
tryOrThrow eff = rightOrThrow (tryIO eff)