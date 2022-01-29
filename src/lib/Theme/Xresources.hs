module Theme.Xresources (xprop) where

import           Control.Monad    (guard)
import           Data.Bifunctor   (bimap)
import           Data.Char        (isSpace)
import           Data.List        (dropWhileEnd, elemIndex, find)
import           Data.Maybe       (catMaybes, fromMaybe)
import           System.IO        (hClose, hGetContents)
import           System.IO.Unsafe (unsafeDupablePerformIO)
import           System.Process   (runInteractiveProcess)

unsafeRunProcessWithInput :: FilePath -> [String] -> String
unsafeRunProcessWithInput cmd args = unsafeDupablePerformIO $ do
    (hin, hout, herr, _) <- runInteractiveProcess cmd args Nothing Nothing
    out <- hGetContents hout
    guard (out == out) -- wait for exit
    hClose hin >> hClose hout >> hClose herr
    return out

xProperty :: String -> String
xProperty key = fromMaybe "" . findValue key $ unsafeRunProcessWithInput "xrdb" ["-query"]

findValue :: String -> String -> Maybe String
findValue xresKey xres = snd <$> find ((== xresKey) . fst) (catMaybes $ splitAtColon <$> lines xres)

splitAtColon :: String -> Maybe (String, String)
splitAtColon str = splitAtTrimming str <$> elemIndex ':' str

splitAtTrimming :: String -> Int -> (String, String)
splitAtTrimming str idx = bimap trim (trim . tail) $ splitAt idx str

trim, xprop :: ShowS
trim = dropWhileEnd isSpace . dropWhile isSpace
xprop = xProperty
