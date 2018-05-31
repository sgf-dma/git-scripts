#!/usr/bin/env stack
-- script
{-# LANGUAGE OverloadedStrings  #-}

import Data.Either.Combinators
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Attoparsec.Text as A
import qualified Data.Map as M
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad
import Shelly

newtype Sha = Sha {sha :: T.Text}
  deriving (Show, Eq, Ord)

newtype Tag = Tag {gitTag :: T.Text}
  deriving (Show, Eq, Ord)

-- | Parse single line of old-to-new-sha map file. Hashes should be separated
-- by comma.
parseLine :: A.Parser (Sha, Sha)
parseLine   = (,)
                <$> (Sha <$> A.takeWhile1 (/= ','))
                <*> (Sha <$> (A.string "," *> A.takeWhile (not . A.isEndOfLine)))

-- | Parse entire sha map file.
parseCommits :: A.Parser (M.Map Sha Sha)
parseCommits    = M.fromList <$> some (parseLine <* (A.endOfLine <|> A.endOfInput))

-- | Parse single word (in entire input).
singleWord :: A.Parser T.Text
singleWord  = A.takeWhile (`notElem` [' ', '\t', '\n'])
                <* many A.endOfLine <* A.endOfInput

-- | Parse single sha (in entire input).
singleSha :: A.Parser Sha
singleSha       = Sha <$> singleWord

-- | Find new Sha for tag.
findNewSha :: M.Map Sha Sha -> Tag -> Sh (Tag, Either Sha Sha)
findNewSha m t  = do
    c <- run "git" ["rev-parse", gitTag t <> "^{commit}"]
    oldSha <- either error return (A.parseOnly singleSha c)
    return (t, maybeToRight oldSha (M.lookup oldSha m))

-- | Delete old tag and create new /not/ annotated tag with the same name.
rewriteTags :: Tag -> Either Sha Sha -> Sh ()
rewriteTags t (Right newSha) = do
    liftIO . putStrLn $ "Move " ++ show t ++ " to " ++ show newSha
    run_ "git" ["tag", "-d", gitTag t]
    run_ "git" ["tag", gitTag t, sha newSha]
rewriteTags t (Left oldSha) = liftIO . putStrLn $ "  => " ++ show t ++ " remains at " ++ show oldSha

buildTagMap :: [Tag] -> T.Text -> [Tag]
buildTagMap zm tx = either error (: zm)
    $ A.parseOnly (Tag <$> singleWord) tx

main :: IO ()
main    = do
    t <- T.readFile "commit-map.txt"
    commitMap <- either error return (A.parseOnly parseCommits t)
    shelly $ do
      tags    <- runFoldLines empty buildTagMap "git" ["tag", "-l"]
      mapM_ (uncurry rewriteTags <=< findNewSha commitMap) tags
      return ()

