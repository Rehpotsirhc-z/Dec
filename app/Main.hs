-- [[file:../dec.org::*License][License:1]]
-- SPDX-FileCopyrightText: 2025 Rehpotsirhc
--
-- SPDX-License-Identifier: GPL-3.0-or-later
-- License:1 ends here

-- [[file:../dec.org::*Imports][Imports:1]]
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (unless, when)
import Data.Foldable (for_)
import Data.List
  ( intercalate,
    isInfixOf,
    isPrefixOf,
    stripPrefix,
  )
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as Set
import Data.String.QQ (s)
import qualified Data.Text as T
import System.Console.ANSI
  ( Color (Green, Red, Yellow),
    ColorIntensity (Dull, Vivid),
    ConsoleIntensity (BoldIntensity),
    ConsoleLayer (Foreground),
    SGR (Reset, SetColor, SetConsoleIntensity),
    setSGRCode,
  )
import System.Directory (findExecutable)
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import System.IO.Error (catchIOError)
import System.Process (callProcess, readProcess)
-- Imports:1 ends here

-- [[file:../dec.org::*Main][Main:1]]
main :: IO ()
main = do
  paruInstalled <- checkParu
  unless paruInstalled $ do
    printError "Paru is not installed"
    exitFailure
  handleArgs =<< getArgs
-- Main:1 ends here

-- [[file:../dec.org::*Paru Dependency Check][Paru Dependency Check:1]]
checkParu :: IO Bool
checkParu = fmap isJust (findExecutable "paru")
-- Paru Dependency Check:1 ends here

-- [[file:../dec.org::*Parse Arguments][Parse Arguments:1]]
getPackageList :: [String] -> Maybe String
getPackageList args =
  case filter ("--packagelist=" `isInfixOf`) args of
    [result] -> Just result
    _moreThanOneOrNone -> Nothing
-- Parse Arguments:1 ends here

-- [[file:../dec.org::*Prompt User][Prompt User:1]]
promptUser :: IO Bool
promptUser = fmap (`elem` ["Y", "y", ""]) getLine
-- Prompt User:1 ends here

-- [[file:../dec.org::*Remove Comments][Remove Comments:1]]
removeComments :: String -> String
removeComments line =
  let noComment = takeWhile (/= '#') line
      trimmed = T.unpack $ T.strip $ T.pack noComment
   in trimmed
-- Remove Comments:1 ends here

-- [[file:../dec.org::*Errors][Errors:1]]
printError :: String -> IO ()
printError str =
  hPutStrLn stderr $
    setSGRCode [SetColor Foreground Vivid Red]
      ++ str
      ++ setSGRCode [Reset]
-- Errors:1 ends here

-- [[file:../dec.org::*Headings][Headings:1]]
printHeading :: String -> IO ()
printHeading str =
  putStrLn $
    setSGRCode [SetColor Foreground Vivid Yellow, SetConsoleIntensity BoldIntensity]
      ++ str
      ++ setSGRCode [Reset]
-- Headings:1 ends here

-- [[file:../dec.org::*Information][Information:1]]
printInfo :: String -> IO ()
printInfo str =
  putStrLn $
    setSGRCode [SetColor Foreground Dull Green]
      ++ str
      ++ setSGRCode [Reset]
-- Information:1 ends here

-- [[file:../dec.org::*Prompts][Prompts:1]]
printPrompt :: String -> IO ()
printPrompt str =
  putStr $
    setSGRCode [SetColor Foreground Dull Green]
      ++ str
      ++ setSGRCode [Reset]
-- Prompts:1 ends here

-- [[file:../dec.org::*Help][Help:1]]
printHelp :: IO ()
printHelp =
  printInfo
    [s|
Usage: dec [OPTIONS]
Declarative package manager for Arch Linux

Options:
  -h, --help            Show this help message
  --packagelist=FILE    Specify the package list
  upgrade               Upgrade packages
  install               Install packages
  remove                Remove packages

Examples:
  dec upgrade
  dec --packagelist=packages.txt install
  dec remove --packagelist=packages.txt|]
-- Help:1 ends here

-- [[file:../dec.org::*Arguments][Arguments:1]]
handleArgs :: [String] -> IO ()
handleArgs args = do
  when ("--help" `elem` args || "-h" `elem` args) $ printHelp >> exitSuccess

  let validCommands = ["upgrade", "install", "remove"]
  let validFlags = ["--help", "-h"]
  let isFlag arg = arg `elem` validFlags
  let isCommand arg = arg `elem` validCommands

  let invalidArgs = filter (\arg -> not (isFlag arg || isCommand arg || "--packagelist=" `isPrefixOf` arg)) args

  for_ (NE.nonEmpty invalidArgs) $ \invalidNE -> do
    let formattedArgs = fmap (\x -> "`" ++ x ++ "`") invalidNE
    let errorMsg = case formattedArgs of
          x :| [] -> "Invalid argument: " ++ x
          x :| [y] -> "Invalid arguments: " ++ x ++ " and " ++ y
          neArgs ->
            let initArgs = NE.init neArgs
                lastArg = NE.last neArgs
             in "Invalid arguments: " ++ intercalate ", " initArgs ++ ", and " ++ lastArg
    printError errorMsg
    printHelp
    exitFailure

  let doUpgrade = "upgrade" `elem` args
  let doInstall = "install" `elem` args
  let doRemove = "remove" `elem` args

  let packageListFromArgs = stripPrefix "--packagelist=" =<< getPackageList args
  packageListFromEnv <- lookupEnv "DEC_PACKAGELIST"

  let packageList = fromMaybe "" (packageListFromArgs <|> packageListFromEnv)

  when (packageList == "") $ do
    printError "No list of packages specified"
    printInfo "You can specify one by using --packagelist= or by setting DEC_PACKAGELIST"
    exitFailure

  when doUpgrade upgrade
  when doInstall $ install packageList
  when doRemove $ remove packageList
  unless
    (doUpgrade || doInstall || doRemove)
    (upgrade >> install packageList >> remove packageList)
-- Arguments:1 ends here

-- [[file:../dec.org::*Upgrade][Upgrade:1]]
upgrade :: IO ()
upgrade = do
  printHeading "[[ Upgrading Packages ]]"
  printInfo "* paru -Syu"
  callProcess "paru" ["-Syu"] `catchIOError` paruError
-- Upgrade:1 ends here

-- [[file:../dec.org::*Install][Install:1]]
install :: FilePath -> IO ()
install packageList = do
  packageListContents <- fmap (Set.fromList . lines) (readFile packageList) `catchIOError` readPackageListError
  let packages = Set.filter (not . null) $ Set.map removeComments packageListContents
  systemPackages <-
    Set.fromList . lines
      <$> readProcess "paru" ["-Qqe"] []
        `catchIOError` paruPackageError
  let toInstall = Set.difference packages systemPackages

  printHeading "[[ Installing Packages ]]"
  if Set.null toInstall
    then printInfo "No packages need to be installed"
    else do
      printInfo $ "* paru -S --asexplicit " ++ unwords (Set.toList toInstall)
      printPrompt "About to run above command. Continue? [Y/n] "
      hFlush stdout
      userInput <- promptUser
      when userInput $ callProcess "paru" (["-S", "--asexplicit"] ++ Set.toList toInstall) `catchIOError` paruError
-- Install:1 ends here

-- [[file:../dec.org::*Remove][Remove:1]]
remove :: FilePath -> IO ()
remove packageList = do
  packageListContents <- fmap (Set.fromList . lines) (readFile packageList) `catchIOError` readPackageListError
  let packages = Set.filter (not . null) $ Set.map removeComments packageListContents
  systemPackages <-
    Set.fromList . lines
      <$> readProcess "paru" ["-Qqett"] []
        `catchIOError` paruPackageError
  let toRemove = Set.difference systemPackages packages

  printHeading "[[ Removing Packages ]]"
  if Set.null toRemove
    then printInfo "No packages need to be removed"
    else do
      printInfo $ "* paru -D --asdeps " ++ unwords (Set.toList toRemove)
      printPrompt "About to run above command. Continue? [Y/n] "
      hFlush stdout
      userInput <- promptUser
      when userInput $ do
        callProcess "paru" (["-D", "--asdeps"] ++ Set.toList toRemove) `catchIOError` paruError
        callProcess "paru" ["--clean"] `catchIOError` paruError
-- Remove:1 ends here

-- [[file:../dec.org::*Catching Errors][Catching Errors:1]]
paruError :: (Monad m) => p -> m ()
paruError _ = return () -- Left blank since it could be user decline

paruPackageError :: p -> IO b
paruPackageError _ = printError "Error running paru to query installed package list" >> exitFailure

readPackageListError :: p -> IO b
readPackageListError _ = printError "Could not read package list" >> exitFailure
-- Catching Errors:1 ends here
