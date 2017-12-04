module Main where

import Text.Read
import Numeric (readSigned, readFloat)
import Parser
import Euterpea (play)
import Music
import CompositionMap
import qualified CompositionMap as CM

parser :: CompositionMap -> IO ()
parser m = do
    input <- getLine
    case words input of
        "compose":i:ns                   -> addComposition i ns
        ["setTempo", t, cid]             -> update cid (readRational t) setTempo
        ["modifyTempo", d, cid]          -> update cid (readRational d) modifyTempo
        ["transpose", t, cid]            -> update cid (readMaybe t :: Maybe Int) transpose
        ["setInstrument", i, cid]        -> update cid (Just i) (setInstrument . toInstrumentName)
        ["reverse", cid]                 -> update' cid Music.reverse
        ["collapse", cid]                -> update' cid collapse
        ["take", i, cid]                 -> update'' cid i Music.take
        ["drop", i, cid]                 -> update'' cid i Music.drop
        ["splitAt", i, cid]              -> addSplit cid i
        ["seq", cid1, cid2]              -> combine cid1 cid2 mappend 
        ["stack", cid1, cid2]            -> combine cid1 cid2 stack3
        ["stackCycle", cid1, cid2]       -> combine cid1 cid2 stack
        ["stackTruncate", cid1, cid2]    -> combine cid1 cid2 stack2
        ["intersperse1", cid1, cid2]     -> combine cid1 cid2 intersperse1
        ["intersperse2", cid1, cid2]     -> combine cid1 cid2 intersperse2
        ["intersperse2n", cid1, cid2, n] -> intersperse2nCompositions cid1 cid2 n
        ["play", cid]                    -> playComposition cid
        ["quit"]                         -> return ()
        _                                -> (putStrLn "what's up with it?") >> parser m

    where
        addComposition :: String -> [String] -> IO ()
        addComposition i ns = let (cid, m') = CM.add m $ toComposition i ns in
                                (putStrLn ("new composition id = " ++ (show cid))) >> parser m'

        -- Eventually:
        -- update :: String -> (Composition -> Composition) -> IO ()
        update :: Read a => String -> Maybe a -> (a -> Composition -> Composition) -> IO ()
        update cid arg f = case (readMaybe cid :: Maybe Int, arg) of
                                (Just cid', Just arg') ->
                                    case CM.updateWith cid' (f arg') m of
                                        Just m' -> (putStrLn $ "Updated " ++ cid) >> parser m'
                                        _       -> (putStrLn "Invalid") >> parser m
                                _ -> (putStrLn "Invalid, yo") >> parser m

        -- this will eventually not exist:
        update' :: String -> (Composition -> Composition) -> IO ()
        update' cid f = case readMaybe cid :: Maybe Int of
                            Just cid' ->
                                case CM.updateWith cid' f m of
                                    Just m' -> (putStrLn $ "Updated " ++ cid) >> parser m'
                                    _       -> (putStrLn "Invalid") >> parser m
                            _ -> (putStrLn "Invalid") >> parser m

        -- this will eventually not exist:
        update'' :: Read a => String -> String -> (a -> Composition -> Maybe Composition) -> IO ()
        update'' cid arg f = case (readMaybe cid :: Maybe Int, readMaybe arg) of
                                (Just cid', Just arg') ->
                                    case CM.get m cid' of
                                        Just c ->
                                            case f arg' c of
                                                Just c' ->
                                                    case CM.updateWith cid'( \_ -> c') m of
                                                        Just m' -> (putStrLn $ "Updated " ++ cid) >> parser m'
                                                        _ -> (putStrLn "Invalid") >> parser m
                                                _ -> (putStrLn "Invalid") >> parser m
                                        _ -> (putStrLn "Invalid") >> parser m
                                _ -> (putStrLn "Invalid") >> parser m

        addSplit :: String -> String -> IO ()
        addSplit cid i = case (readMaybe cid, readMaybe i) of
                            (Just cid', Just i') ->
                                case CM.get m cid' of
                                    Just c ->
                                        case Music.splitAt i' c of
                                            (Just c1, Just c2) ->
                                                let (cid1, m')  = CM.add m  c1 in
                                                let (cid2, m'') = CM.add m' c2 in
                                                (putStrLn ("Added " ++ (show cid1) ++ " and " ++ (show cid2))) >> parser m''
                                            (Just c1, _) ->
                                                let (cid1, m') = CM.add m c1 in
                                                (putStrLn $ "Added " ++ (show cid1)) >> parser m'
                                            _ -> (putStrLn "Invalid") >> parser m
                                    _ -> (putStrLn "Invalid") >> parser m
                            _ -> (putStrLn "Invalid") >> parser m

        intersperse2nCompositions :: String -> String -> String -> IO ()
        intersperse2nCompositions cid1 cid2 n = case readMaybe n of
                                                     Just n' -> combine cid1 cid2 (intersperse2n n')
                                                     Nothing -> putStrLn "Invalid step"

        combine :: String -> String -> (Composition -> Composition -> Composition) -> IO ()
        combine cid1 cid2 f = let (cid1', cid2') = (readMaybe cid1 :: Maybe Int, readMaybe cid2 :: Maybe Int) in
                                case (cid1', cid2') of
                                    (Just cid1'', Just cid2'') -> let (c1, c2) = (CM.get m cid1'', CM.get m cid2'') in
                                                                    case (c1, c2) of
                                                                        (Just c1', Just c2') -> let (cid, m') = CM.add m $ f c1' c2' in
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



        --
        -- can't wait to use monad transformers so I can do something like this:
        --
        -- ["setTempo", t, cid] -> readMaybe t >>= \t' -> update cid $ setTempo t'
        -- ["modifyTempo", d, cid] -> readMaybe d >>= \d' -> update cid $ modifyTempo d'
        -- ["transpose", t, cid] -> readMaybe t >>= \t' update cid $ transpose t'
        -- ["setInstrument", i, cid] -> readMaybe i >>= \i' -> update cid $ setInstrument i'
        -- ["reverse", cid] -> update cid reverse
        -- ["splitAt", i, cid] -> readMaybe i >>= \i' -> update cid $ splitAt i'
        -- ["take", i cid] -> readMaybe i >>= \i' -> update cid $ take i'
        -- ["drop", i, cid] -> readMaybe i >>= \i' -> update cid $ drop i'
        -- ["collapse", cid] -> update cid collapse