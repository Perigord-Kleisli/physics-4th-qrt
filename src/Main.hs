{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Main (main) where

import           Reanimate.Parameters
import           Intro
import           Environment
import           Reanimate.Animation
import           Reanimate
import           Reanimate.Scene
import           Control.Monad
import           Control.Lens
import           Linear
import           Util
import           Wave
import           System.Directory
import           Data.Char (digitToInt)

main :: IO ()
main = do
  let scenes = [namedList|intro,wave|]
  mapM_ (\(i, (name, _)) -> putStrLn ("[" ++ show i ++ "] " ++ name))
    $ zip [0 ..] scenes
  getLine >>= reanimate . (map snd scenes !!) . read
  putStrLn "Done"
