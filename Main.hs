module Main where

import Text.Read
import Numeric (readSigned, readFloat)
import Parser
import Euterpea (play)
import Music
import IncrMap
import qualified IncrMap as IM
import CompositionMap
import qualified CompositionMap as CM

parser :: MelodyMap -> CompositionMap -> IO ()
parser mm cm = do
    input <- getLine
    case words input of
        "melodize":i:ns                  -> addMelody i ns
        ["compose", mid1, mid2]          -> compose Music.stack mid1 mid2
        ["setTempo", t, mid]             -> update mid (readRational t) setTempo
        ["modifyTempo", d, mid]          -> update mid (readRational d) modifyTempo
        ["transpose", t, mid]            -> update mid (readMaybe t) transpose
        ["setInstrument", i, mid]        -> update mid (Just i) (setInstrument . toInstrumentName)
        -- ["reverse", t, rid]              -> updateReverse mid Music.reverseMelody
        -- ["reverse", "c", cid]            -> update' cid Music.reverseComposition
        -- ["collapse", cid]                -> update' cid collapse
        -- ["take", i, mid]                 -> update'' mid i Music.take
        -- ["drop", i, mid]                 -> update'' mid i Music.drop
        -- ["splitAt", i, mid]              -> addSplit mid i
        -- ["seq", mid1, mid2]              -> combine mid1 mid2 mappend 
        ["repl", mid1, n]                -> 
        ["stackCycle", mid1, mid2]       -> compose stackCycle mid1 mid2
        ["stackTruncate", mid1, mid2]    -> compose stackTruncate mid1 mid2
        ["stackPreserve", mid1, mid2]    -> compose stackPreserve mid1 mid2
        ["intersperse1", mid1, mid2]     -> compose intersperse1 mid1 mid2
        ["intersperse2", mid1, mid2]     -> compose intersperse2 mid1 mid2
        ["intersperse2n", mid1, mid2, n] -> intersperse2nCompositions mid1 mid2 n
        ["play", "m", mid]                    -> playMelody mid
        ["play", "c", cid]                    -> playComposition cid
        ["quit"]                         -> return ()
        _                                -> (putStrLn "what's up with it?") >> parser mm cm

    where
        addMelody :: String -> [String] -> IO ()
        addMelody i ns = let (mid, mm') = IM.add mm $ toMelody i ns in
                                (putStrLn ("new melody id = " ++ (show mid))) >> parser mm' cm



        compose :: (Melody -> Melody -> Composition) -> String -> String -> IO ()
        compose f mid1 mid2 = case (readMaybe mid1, readMaybe mid2) of
                                (Just mid1', Just mid2') ->
                                    case (IM.get mm mid1', IM.get mm mid2') of
                                        (Just m1, Just m2) ->
                                            let (cid, cm') = IM.add cm $ f m1 m2 in
                                            (putStrLn ("new composition id = " ++ (show cid))) >> parser mm cm'
                                        _ -> (putStrLn "Invalid") >> parser mm cm
                                _ -> (putStrLn "Invalid") >> parser mm cm

        -- Eventually:
        -- update :: String -> (Composition -> Composition) -> IO ()
        update :: Read a => String -> Maybe a -> (a -> Melody -> Melody) -> IO ()
        update mid arg f = case (readMaybe mid, arg) of
                                (Just mid', Just arg') ->
                                    case IM.updateWith mid' (f arg') mm of
                                        Just mm' -> (putStrLn $ "Updated " ++ mid) >> parser mm' cm
                                        _       -> (putStrLn "Invalid") >> parser mm cm
                                _ -> (putStrLn "Invalid, yo") >> parser mm cm

        intersperse2nCompositions :: String -> String -> String -> IO ()
        intersperse2nCompositions mid1 mid2 n = case readMaybe n of
                                                     Just n' -> compose (intersperse2n n') mid1 mid2
                                                     Nothing -> putStrLn "Invalid step"

        playMelody :: String -> IO ()
        playMelody mid = let mid' = readMaybe mid :: Maybe Int in
                                case mid' of
                                    Nothing    -> (putStrLn "NaN") >> parser mm cm
                                    Just mid'' ->
                                        let mel = IM.get mm mid'' in
                                        case mel of
                                            Just m  -> (play $ toMusicPitch m) >> parser mm cm
                                            Nothing -> (putStrLn "no such mid") >> parser mm cm

        playComposition :: String -> IO ()
        playComposition cid = let cid' = readMaybe cid :: Maybe Int in
                                case cid' of
                                    Nothing    -> (putStrLn "NaN") >> parser mm cm
                                    Just cid'' ->
                                        let comp = IM.get cm cid'' in
                                        case comp of
                                            Just c  -> (play $ toMusicPitch c) >> parser mm cm
                                            Nothing -> (putStrLn "no such cid") >> parser mm cm

        -- -- this will eventually not exist:
        -- update' :: String -> (Composition -> Composition) -> IO ()
        -- update' cid f = case readMaybe cid :: Maybe Int of
        --                     Just cid' ->
        --                         case CM.updateWith cid' f m of
        --                             Just m' -> (putStrLn $ "Updated " ++ cid) >> parser m'
        --                             _       -> (putStrLn "Invalid") >> parser m
        --                     _ -> (putStrLn "Invalid") >> parser m

        -- -- this will eventually not exist:
        -- update'' :: Read a => String -> String -> (a -> Composition -> Maybe Composition) -> IO ()
        -- update'' cid arg f = case (readMaybe cid :: Maybe Int, readMaybe arg) of
        --                         (Just cid', Just arg') ->
        --                             case CM.get m cid' of
        --                                 Just c ->
        --                                     case f arg' c of
        --                                         Just c' ->
        --                                             case CM.updateWith cid'( \_ -> c') m of
        --                                                 Just m' -> (putStrLn $ "Updated " ++ cid) >> parser m'
        --                                                 _ -> (putStrLn "Invalid") >> parser m
        --                                         _ -> (putStrLn "Invalid") >> parser m
        --                                 _ -> (putStrLn "Invalid") >> parser m
        --                         _ -> (putStrLn "Invalid") >> parser m

        -- addSplit :: String -> String -> IO ()
        -- addSplit cid i = case (readMaybe cid, readMaybe i) of
        --                     (Just cid', Just i') ->
        --                         case CM.get m cid' of
        --                             Just c ->
        --                                 case Music.splitAt i' c of
        --                                     (Just c1, Just c2) ->
        --                                         let (cid1, m')  = CM.add m  c1 in
        --                                         let (cid2, m'') = CM.add m' c2 in
        --                                         (putStrLn ("Added " ++ (show cid1) ++ " and " ++ (show cid2))) >> parser m''
        --                                     (Just c1, _) ->
        --                                         let (cid1, m') = CM.add m c1 in
        --                                         (putStrLn $ "Added " ++ (show cid1)) >> parser m'
        --                                     _ -> (putStrLn "Invalid") >> parser m
        --                             _ -> (putStrLn "Invalid") >> parser m
        --                     _ -> (putStrLn "Invalid") >> parser m

main :: IO ()
main = parser IM.empty IM.empty



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
