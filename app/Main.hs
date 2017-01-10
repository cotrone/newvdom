{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import Lib
import Valentine
import LiveVDom
import qualified Data.Sequence as S

main :: IO ()
main = do
  container <- createContainer
  globalMb  <- spawnIO True
--  runDom container (return ()) $ display globalMb
  runDomI container (return ()) $ display globalMb


--display :: STMMailbox Bool -> Elem Identity
display :: STMMailbox Bool -> Elem IO
display globalMb = elem' "div" [] [] content
  where
--  content' =
--    let btn1 = buttonWith (modifyMailbox globalMb (const True)) [] "Set True"   :: Elem Identity
--        btn2 = buttonWith (modifyMailbox globalMb (const False)) [] "Set False" :: Elem Identity
--      in [valentine|
--          <div>
--            ${btn1}
--            ${btn2}
--            ${showContent <$> (fst globalMb)}
--         |]

-- I'm not sure if this is the right way to do it
  content = do
    yesnoMb <- spawn' True

    let btn1 = buttonWith (modifyMailbox yesnoMb (const True)) [] "Set True"
        btn2 = buttonWith (modifyMailbox yesnoMb (const False)) [] "Set False"
      in [valentine|
          <div>
            ${btn1}
            ${btn2}
            ${showChildContent yesnoMb}
         |]

showContent :: Bool -> Elem Identity
showContent True = elem' "h1" [] [] $ text' "True"
showContent False = elem' "h1" [] [] $ text' "False"


-- This won't work because I need it to be Elem Identity
showChildContent :: STMMailbox Bool -> Elem Identity
showChildContent yesnoMb = elem' "h1" [] []
  $ [valentine|
    <h1>
      ${showContent <$> (fst yesnoMb)}
    |]
