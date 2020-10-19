{-# LANGUAGE OverloadedStrings #-}

module Main where

import Caani (CaaniConfig(..), caani)
import Data.Maybe (fromMaybe)
import qualified Data.Text.IO as TIO
import System.Environment (getArgs, lookupEnv)

getResourceDir :: IO String
getResourceDir = fmap (fromMaybe "./resources") (lookupEnv "RESOURCE_DIR")

data CLI
    = CLIOutput String
    | CLIError String
    | CLIHelp
    deriving (Eq, Show)

help :: String
help =
    unlines
        [ "usage:",
          "      cat your-code.hs | caani -o image.png",
          "",
          "  -o OUTPUT_PATH        Specify the output path [Required]",
          "  -h                    Help",
          "",
          "Repository: https://github.com/jeovazero/caani",
          "XD"
        ]

parse :: [String] -> Maybe CLI -> CLI
parse ("-h":_) _ = CLIHelp
parse ("-o":x:xs) _ = parse xs (Just $ CLIOutput x)
parse _ (Just cli) = cli
parse _ _ = CLIError $ "Error: Missing or wrong arguments :(\n\n" ++ help

main :: IO ()
main = do
    args <- getArgs
    let cli = parse args Nothing
    case cli of
        CLIHelp -> putStr help
        CLIError err -> putStr err
        CLIOutput out -> do
            input <- TIO.getContents
            resourceDir <- getResourceDir
            r <-
                caani
                    ( CaaniConfig
                          { boundary = (200, 1000),
                            fontPath = resourceDir ++ "/FiraCode-Medium.ttf",
                            tagPath = resourceDir ++ "/haskell-flag.png",
                            code = input,
                            outPath = out
                          }
                    )
            case r of
                Right () ->
                    putStrLn $
                        concat
                            [ "\n",
                              "  Yay!!! The image was saved in '",
                              out,
                              "'\n"
                            ]
                Left err ->
                    putStrLn $
                        concat
                            [ "\n",
                              "Error: ",
                              show err,
                              "\n"
                            ]
