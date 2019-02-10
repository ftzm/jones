module Git
    ( getGit
    ) where

import Text.ParserCombinators.ReadP
import Data.Yaml
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as BL
import System.Process
import qualified Data.Vector as V

import Types

callGit :: IO String
callGit = readProcess "git" args []
  where args =
          [ "--git-dir=/home/matt/.dolly/.git"
          , "log"
          , "--pretty=tformat:%H;%an;%s;%b;%N|"
          , "--since=\"one week ago\""
          , "-n"
          , "10000"
          , "HEAD"
          , "--notes=refs/notes/artifacts"
          ]

gitHash :: ReadP String
gitHash = count 40 $ satisfy isHexDigit

parseCommit = do
  hash <- gitHash
  char ';'
  author <- manyTill get (char ';')
  subject <- manyTill get (char ';')
  body <- manyTill get (char ';')
  listing <- mkListing <$> munch (/='|')
  char '|'
  char '\n'
  return $ CommitInfo hash author subject body listing
  where
    mkListing :: String -> Maybe Listing
    mkListing = decodeThrow . BL.toStrict . BL.pack


parseGit :: String -> V.Vector CommitInfo
parseGit s = V.fromList $ head $ fst <$> readP_to_S p s
  where p = manyTill parseCommit eof

getGit :: IO (V.Vector CommitInfo)
getGit = parseGit <$> callGit
