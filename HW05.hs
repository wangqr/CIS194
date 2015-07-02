{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)

import Control.Applicative
import Data.Bits
import Data.Function

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

import Parser

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret fa fb = (BS.pack . filter (0/=)) <$>
    (liftA2 (zipWith xor) `on` ((fmap BS.unpack) . BS.readFile)) fa fb

-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key fp = (
        BS.writeFile fp . BS.pack .
            zipWith xor (cycle $ BS.unpack key) . BS.unpack
    ) =<< BS.readFile (fp ++ ".enc")

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile = (fmap decode) . BS.readFile

-- Exercise 4 -----------------------------------------

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs fv ft = do
    vl <- parseFile fv :: IO (Maybe [TId])
    td <- parseFile ft :: IO (Maybe [Transaction])
    return $ liftA2 (
            \vt -> filter (
                \(Transaction {tid = x}) -> elem x vt
            )
        ) vl td

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow = foldr (
        \(Transaction {from = fn, to = tn, amount = am}) ->
            (Map.insertWith (+) fn (negate am))
                . (Map.insertWith (+) tn am)
    ) Map.empty

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal = fst . Map.foldrWithKey
    (\na mo (mn,mm) -> if mm<mo then (na,mo) else (mn,mm)) ("",0)

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs m ts
    | Map.null m  = []
    | amount <= 0 = []
    | ts == []    = []
    | otherwise   = tr : (undoTs new_m $ tail ts)
    where
        payer = Map.foldrWithKey
            (\na mo (mn,mm) -> if mm<mo then (na,mo) else (mn,mm)) ("",0) m
        payee = Map.foldrWithKey
            (\na mo (mn,mm) -> if mm>mo then (na,mo) else (mn,mm)) ("",0) m
        amount = min (snd payer) (negate $ snd payee)
        tr = Transaction (fst payer) (fst payee) amount $ head ts
        new_m = Map.filter (/=0) $ Map.adjust (amount +) (fst payee)
            $ Map.adjust ((-amount) +) (fst payer) m

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON f = BS.writeFile f . encode

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <-
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim

