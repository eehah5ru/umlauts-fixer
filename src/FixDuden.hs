module FixDuden where

--import FixDuden.Internal

import System.Hclip
-- import qualified Data.ByteString.Char8 as C
import Control.Monad
import qualified Data.Char as C
import System.Environment (getEnv)

fixUmlauts :: Char -> Char
fixUmlauts '\128' = 'Ä'
fixUmlauts '\133' = 'Ö'
fixUmlauts '\134' = 'Ü'
fixUmlauts '\138' = 'ä'
fixUmlauts '\154' = 'ö'
fixUmlauts '\159' = 'ü'
fixUmlauts '\167' = 'ß'           
fixUmlauts c = c

printByteCodes :: String -> IO ()
printByteCodes = foldM_ doPrint ()
  where doPrint _ x = putStrLn $ (show . C.ord) x

fixDudenEncoding :: IO ()
fixDudenEncoding = modifyClipboard_ (map fixUmlauts)
   
-- fixDudenEncoding = do 
--                       cl <- getClipboard >>= return . map fixUmlauts
--                       lang <- getEnv "LANG"
--                       setClipboard (cl ++ " - " ++ lang)
  -- where f xs = (map fixUmlauts xs)
-- main = getClipboard >>= return . map fixUmlauts >>= putStrLn


printBytesFromClipBoard :: IO ()
printBytesFromClipBoard = getClipboard >>= printByteCodes

fixAndPrint :: IO ()
fixAndPrint = getClipboard >>= return . map fixUmlauts >>= putStrLn


