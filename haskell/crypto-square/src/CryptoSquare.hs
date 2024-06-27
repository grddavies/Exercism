{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module CryptoSquare (encode) where

import Data.Char (isAlphaNum)
import Data.Text qualified as T

encode :: String -> String
encode xs = T.unpack $ T.intercalate " " chunked
  where
    text = T.pack xs
    normalised = T.filter isAlphaNum $ T.toLower text
    len = T.length normalised
    sqrtLen = sqrt (fromIntegral len)
    c = ceiling sqrtLen
    coded = T.concat $ T.transpose $ T.chunksOf c normalised
    -- c Chunks of length r
    r = round sqrtLen
    nPad = (c * r) - len
    ch = ' '
    (toChunk, toPad) = T.splitAt (r * (c - nPad)) coded
    padded = (`T.snoc` ch) <$> T.chunksOf (r - 1) toPad
    chunked = T.chunksOf r toChunk ++ padded
