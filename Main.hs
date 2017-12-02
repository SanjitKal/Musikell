module Main where

import Text.Read

import Euterpea (play)

import Music

import CompositionMap
import qualified CompositionMap as CM

parser :: CompositionMap -> IO ()
parser m = do
    input <- getLine
    case words input of
        ["compose", t, ns] -> addComposition t ns
        ["stack", cid, ns] -> stack cid ns
        ["seq", cid, ns]   -> seq cid ns
        ["play", cid]      -> playComposition cid
        ["quit"]           -> return ()
        _                  -> (putStrLn "what's up with it?") >> parser m

    where
        addComposition t ns = let (cid, m') = CM.add m $ toComposition t ns in
                                (putStrLn ("new composition id = " ++ (show cid))) >> parser m'
        stack cid ns        = undefined
        seq   cid ns        = undefined
        playComposition cid = undefined

            readMaybe cid :: Maybe Int in
                                case cid' of
                                    Nothing    -> (putStrLn "yoooo") >> parser m
                                    Just cid'' ->
                                        let m' = CM.update m cid'' $ toNote note in
                                        case m' of
                                            Just m'' -> (putStrLn ("updated composition " ++ cid)) >> parser m''
                                            Nothing  -> (putStrLn ("id " ++ cid ++ " does not exist")) >> parser m

let cid' = readMaybe cid :: Maybe Int in
                                case cid' of
                                    Nothing    -> (putStrLn "yoooo") >> parser m
                                    Just cid'' ->
                                        let comp = CM.get m cid'' in
                                        case comp of
                                            Just ("m", c) -> (play $ toMusic (:+:) c) >> parser m
                                            Just ("h", c) -> (play $ toMusic (:=:) c) >> parser m
                                            Nothing       -> (putStrLn "no such cid") >> parser m

main :: IO ()
main = parser CM.empty




compose h a,b,c
> new cid is 1
 1 d
melodize 1 d