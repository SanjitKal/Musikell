module Main where

import Text.Read

import Euterpea

import Music

import CompositionMap
import qualified CompositionMap as CM

parser :: CompositionMap -> IO ()
parser m = do
    input <- getLine
    case words input of
        ["compose", t, note] -> let (cid, m') = CM.add m (t, [toNote note]) in
                                (putStrLn ("new composition id = " ++ (show cid))) >> parser m'
        ["add", cid, note]   -> let cid' = readMaybe cid :: Maybe Int in
                                case cid' of
                                    Nothing    -> (putStrLn "yoooo") >> parser m
                                    Just cid'' ->
                                        let m' = CM.update m cid'' $ toNote note in
                                        case m' of
                                            Just m'' -> (putStrLn ("updated composition " ++ cid)) >> parser m''
                                            Nothing  -> (putStrLn ("id " ++ cid ++ " does not exist")) >> parser m
        ["play", cid]        -> let cid' = readMaybe cid :: Maybe Int in
                                case cid' of
                                    Nothing    -> (putStrLn "yoooo") >> parser m
                                    Just cid'' ->
                                        let comp = CM.get m cid'' in
                                        case comp of
                                            Just ("m", c) -> (play $ toMusic (:+:) c) >> parser m
                                            Just ("h", c) -> (play $ toMusic (:=:) c) >> parser m
                                            Nothing       -> (putStrLn "no such cid") >> parser m
        ["quit"]             -> return ()
        _                    -> (putStrLn "what do you want?") >> parser m

main :: IO ()
main = parser CM.empty