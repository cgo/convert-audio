--
-- A small wrapper that converts all the audio files you give it to another audio format
-- using ffmpeg and lame.
-- It needs ffmpeg and lame installed with the necessary codecs.
-- You can also tell it how many processes to spawn.
-- It needs the spawn library installed (cabal install spawn).
--

module Main where

import System.Process (system)
import System.Environment
import System.Console.GetOpt
import System.FilePath
import Data.List (intercalate)
import Data.Maybe (listToMaybe)

import Control.Concurrent.Spawn

data Format = Ogg | Mp3 | Flac deriving (Show, Eq)

data Arg = FormatArg Format
         | Print
         | ProcNum Int deriving (Show, Eq)

extension :: Format -> String
extension Ogg = ".ogg"
extension Mp3 = ".mp3"
extension Flac = ".flac"


-- Codecs for ffmpeg
codec :: Format -> String
codec Ogg = "libvorbis"
codec Mp3 = "lame"
codec Flac = "flac"

convertFilename :: Format -> FilePath -> FilePath
convertFilename fmt fp = replaceExtension fp $ extension fmt

conversionCommand :: Format -> FilePath -> String

conversionCommand Mp3 fp = intercalate " "
                           ["ffmpeg -i" , "\"" ++ fp ++ "\""
                           ,"-acodec pcm_s16le"
                           ,"-f s16le"
                           ,"pipe:"
                           ,"|"
                           ,"lame -r -b 192"
                           ,"-"
                           ,"\"" ++ convertFilename Mp3 fp ++ "\""]
                           
conversionCommand fmt fp = intercalate " "
                           ["ffmpeg -i", "\"" ++ fp ++ "\""
                           , "-acodec", codec fmt
                           , "-aq", quality
                           , "\"" ++ convertFilename fmt fp ++ "\"" ]
  where quality = "7"


main = do
  args <- getArgs
  let opts' = [Option [] ["ogg"]   (NoArg $ FormatArg Ogg) "Format: ogg"
              ,Option [] ["mp3"]   (NoArg $ FormatArg Mp3) "Format: mp3"
              ,Option [] ["flac"]  (NoArg $ FormatArg Flac) "Format: flac"
              ,Option ['n'] []     (ReqArg getN "<int>") "Number of processes to start"
              ,Option [] ["print"] (NoArg $ Print) "Just print the command lines"]

      getN s = ProcNum $ read s

      (opts, fnames, _) = getOpt Permute opts' args
      
      fmt = maybe Ogg id $ listToMaybe $ map getFormat $ filter formatArgs opts
        where 
              formatArgs (FormatArg _) = True
              formatArgs _ = False
              getFormat (FormatArg x) = x

      n = maybe 1 unProcNum $ listToMaybe $ filter nArg opts
        where nArg (ProcNum _) = True
              nArg _ = False
              unProcNum (ProcNum x) = x
              
      systemCommand | null $ filter (== Print) opts = \a -> system a >> return ()
                    | otherwise                     = putStrLn
                                                      
      cmds = map (conversionCommand fmt) fnames
      
  if (null fnames)
    then putStrLn $ usageInfo "convert-audio [option(s)] filenames" opts'
    else
      pool n >>= \f ->
      parMapIO_ (f . systemCommand) cmds
