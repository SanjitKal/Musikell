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
        "compose":ns   -> addComposition ns
        -- "stack":cid:ns -> stack cid ns
        "seq":cid:ns   -> seq cid ns -- append or prepend
        ["play", cid]  -> playComposition cid
        ["quit"]       -> return ()
        _              -> (putStrLn "what's up with it?") >> parser m

    where
        addComposition :: [String] -> IO ()
        addComposition ns = let (cid, m') = CM.add m $ toComposition ns in
                                (putStrLn ("new composition id = " ++ (show cid))) >> parser m'

        -- stack :: String -> [String] -> IO ()
        -- stack cid = combine cid (\cNew cOld -> ???) . toComposition -- HMMMMM

        seq :: String -> [String] -> IO ()
        seq cid = combine cid mappend . toComposition

        combine :: String -> (Composition -> Composition -> Composition) -> Composition -> IO ()
        combine cid f c     = let cid' = readMaybe cid in
                                case cid' of
                                    Nothing    -> putStrLn "NaN" >> parser m
                                    Just cid'' ->
                                        let m' = CM.updateWith m f cid'' c in
                                        case m' of
                                            Nothing  -> (putStrLn $ "id " ++ cid ++ " does not exist") >> parser m
                                            Just m'' -> (putStrLn $ "updated composition " ++ cid) >> parser m''
        playComposition cid = let cid' = readMaybe cid :: Maybe Int in
                                case cid' of
                                    Nothing    -> (putStrLn "NaN") >> parser m
                                    Just cid'' ->
                                        let comp = CM.get m cid'' in
                                        case comp of
                                            Just c  -> (play $ toMusic c) >> parser m
                                            Nothing -> (putStrLn "no such cid") >> parser m

main :: IO ()
main = parser CM.empty

