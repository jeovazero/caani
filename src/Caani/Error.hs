module Caani.Error
    ( CaaniError (..)
    , CaaniErrorType (..)
    , caanify
    , tryIO
    , tryCaani
    , tryOrThrow
    , rightOrThrow
    )
where

import Caani.Font (FreeTypeError(..))
import Control.Exception
    ( Exception
    , Handler(..)
    , SomeException
    , catch
    , catches
    , throwIO
    , try
    )
import Control.Exception.Base (IOException)

newtype CaaniError
    = CaaniError CaaniErrorType
    deriving (Show, Eq)

instance Exception CaaniError

data CaaniErrorType
    = BoundaryLimit
    | LoadFileError
    | LoadTagError
    | LoadFontError
    | UnaexpectedFontError String
    | InvalidCode
    | UnknownError String
    deriving (Show, Eq)

freetypeHandler :: FreeTypeError -> IO CaaniError
freetypeHandler (FreeTypeError e) = pure . CaaniError . UnaexpectedFontError $ e

caaniHandler :: CaaniError -> IO CaaniError
caaniHandler = pure

fallbackHandler :: (String -> CaaniErrorType) -> SomeException -> IO CaaniError
fallbackHandler errorFallback err = pure . CaaniError . errorFallback $ show err

caanify :: IO a -> IO (Either CaaniError ())
caanify effect = do
    catches
        (effect >> pure (Right ()))
        (fmap
            (fmap Left)
            [ Handler freetypeHandler,
              Handler caaniHandler,
              Handler $ fallbackHandler UnknownError
            ]
        )

tryCaani :: IO a -> (String -> CaaniErrorType) -> IO (Either CaaniError a)
tryCaani effect errorType =
    catch
        (effect >>= \a -> pure $ Right a)
        (fmap Left . fallbackHandler errorType)

tryIO :: IO a -> IO (Either IOException a)
tryIO = try

rightOrThrow :: IO (Either a b) -> CaaniErrorType -> IO b
rightOrThrow ioEither etype =
    ioEither >>= \result ->
        case result of
            Left _  -> throwIO $ CaaniError etype
            Right r -> pure r

tryOrThrow :: IO a -> CaaniErrorType -> IO a
tryOrThrow eff = rightOrThrow (tryIO eff)
