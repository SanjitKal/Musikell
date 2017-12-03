module Main where

import Text.Read
import Parser
import Euterpea (play)
import Music
import CompositionMap
import qualified CompositionMap as CM

parser :: CompositionMap -> IO ()
parser m = do
    input <- getLine
    case words input of
        "compose":i:ns -> addComposition i ns
        "seq":cid:i:ns -> seq cid i ns
        "stack":cid1:cid2 -> stackCompositions cid1 cid2
        ["play", cid]  -> playComposition cid
        ["quit"]       -> return ()
        _              -> (putStrLn "what's up with it?") >> parser m

    where
        addComposition :: String -> [String] -> IO ()
        addComposition i ns = let (cid, m') = CM.add m $ toComposition i ns in
                                (putStrLn ("new composition id = " ++ (show cid))) >> parser m'

        seq :: String -> String -> [String] -> IO ()
        seq cid i = combine cid mappend . toComposition i

        combine :: String -> (Composition -> Composition -> Composition) -> Composition -> IO ()
        combine cid f c     = let cid' = readMaybe cid in
                                case cid' of
                                    Nothing    -> putStrLn "NaN" >> parser m
                                    Just cid'' ->
                                        let m' = CM.updateWith m f cid'' c in
                                        case m' of
                                            Nothing  -> (putStrLn $ "id " ++ cid ++ " does not exist") >> parser m
                                            Just m'' -> (putStrLn $ "updated composition " ++ cid) >> parser m''

        stackCompositions :: String -> String -> IO ()
        stackCompositions cid1 cid2 = let (cid1', cid2') = (readMaybe cid1 :: Maybe Int, readMaybe cid2 Maybe Int) in
                            case (cid1', cid2') of
                                (Just cid1'', Just cid2'') -> let (c1, c2) = (CM.get m cid1'', CM.get m cid2'') in
                                                                case (c1, c2) of
                                                                    (Just c1', Just c2') -> let (cid, m') = CM.add m $ stack c1' c2' in
                                                                    (putStrLn ( "new composition id = " ++ (show cid))) >> parser m'
                                                                    _ -> (putStrLn $ "Cannot find one or more composition IDs") >> parser m
                                _ -> (putStrLn $ "Invalid CID") >> parser m




        playComposition :: String -> IO ()
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
