{-# LANGUAGE OverloadedStrings #-}

module Utils.XdgDesktopEntry
    ( DesktopEntry (..)
    , readDescktopEntrys
    ) where

import           Data.Ini.Config              (IniParser, fieldMbOf, flag,
                                               listWithSeparator, parseIniFile,
                                               section, string)
import           Data.Maybe                   (catMaybes)
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as TIO
import           System.FilePattern.Directory (getDirectoryFiles)

xdgApplicationsPath :: FilePath
xdgApplicationsPath = "/usr/share/applications/"

data DesktopEntry = DesktopEntry
    { desktopEntryName        :: Maybe String
    , desktopEntryGenericName :: Maybe String
    , desktopEntryComment     :: Maybe String
    , desktopEntryCategories  :: Maybe [String]
    , desktopEntryExec        :: Maybe String
    , desktopEntryTerminal    :: Maybe Bool
    } deriving (Eq, Show)

readDescktopEntrys :: IO [DesktopEntry]
readDescktopEntrys = do
    files <- getDirectoryFiles xdgApplicationsPath ["*.desktop"]
    catMaybes <$> mapM (go . (xdgApplicationsPath <>)) (filter (`notElem` [".", ".."]) files)
  where
    go :: FilePath -> IO (Maybe DesktopEntry)
    go path = do
        contents <- T.unlines . filter (not . T.isInfixOf "]=") . T.lines <$> TIO.readFile path
        return . either (const Nothing) Just
               $ parseIniFile contents configParser

configParser :: IniParser DesktopEntry
configParser = do
    section "Desktop Entry" $ do
        name <- fieldMbOf "Name" string
        gname <- fieldMbOf "GenericName" string
        comment <- fieldMbOf "Comment" string
        categories <- fmap (filter (not . null))
                   <$> fieldMbOf "Categories" (listWithSeparator ";" string)
        exec <- fieldMbOf "Exec" string
        terminal <- fieldMbOf "Terminal" flag
        return DesktopEntry
            { desktopEntryName = name
            , desktopEntryGenericName = gname
            , desktopEntryComment = comment
            , desktopEntryCategories = categories
            , desktopEntryExec = exec
            , desktopEntryTerminal = terminal
            }
