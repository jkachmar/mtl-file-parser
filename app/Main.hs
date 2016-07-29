{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import qualified Control.Exception    as E
import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Bifunctor       as BF
import qualified Data.Bool            as B
import qualified Data.Char            as C
import           Options.Applicative

-- types

data Options = Options
    { oCapitalize :: Bool
    , oExcited    :: Bool
    , oStdIn      :: Bool
    , oFileToRead :: Maybe String
    }

type AppConfig = MonadReader Options

data AppError = IOError E.IOException

newtype App a = App {
  runApp :: ReaderT Options (ExceptT AppError IO) a
} deriving (Monad, Functor, Applicative, AppConfig, MonadIO, MonadError AppError)

-- program entry point

main :: IO ()
main = runProgram =<< parseCLI

runProgram :: Options -> IO ()
runProgram = _

-- data retrieval and transformation

getSource :: App String
getSource = B.bool _ (liftIO getContents) =<< asks oStdIn

handleCapitalization :: AppConfig m => String -> m String
handleCapitalization str = B.bool str (map C.toUpper str) <$> asks oCapitalize

handleExcitedness :: AppConfig m => String -> m String
handleExcitedness str = B.bool str ("ZOMG " ++ str) <$> asks oExcited

loadContents :: Options -> IO (Either String String)
loadContents o =
    maybe defaultResponse readFileFromOptions $ oFileToRead o
  where
    readFileFromOptions f = BF.first show <$> safeReadFile f
    defaultResponse = return $ Right "This is fun!"

-- CLI parsing

parseCLI :: IO Options
parseCLI = execParser (withInfo parseOptions "File Fun")
  where
    withInfo opts h = info (helper <*> opts) $ header h

parseOptions :: Parser Options
parseOptions = Options
    <$> switch (long "capitalize")
    <*> switch (long "excited")
    <*> switch (long "stdin")
    <*> optional (strOption $ long "file")

safeReadFile :: FilePath -> IO (Either E.IOException String)
safeReadFile = E.try . readFile
