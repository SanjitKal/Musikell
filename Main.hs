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
        ["reverse", "m", mid]            -> update' mid Music.reverseMelody mm ((flip parser) cm)
        ["reverse", "c", cid]            -> update' cid Music.reverseComposition cm (parser mm)
        ["collapse", mid]                -> update' mid Music.collapseMelody mm ((flip parser) cm)
        ["collapseC1", cid]              -> update' cid Music.collapseComposition1 cm (parser mm)
        ["collapseC2", cid]              -> update' cid Music.collapseComposition2 cm (parser mm)
        ["repl", i, mid]                 -> update mid (readMaybe i) Music.repl
        ["take", i, mid]                 -> update mid (readMaybe i) Music.take
        ["drop", i, mid]                 -> update mid (readMaybe i) Music.drop
        ["splitAt", i, mid]              -> addSplit mid i
        ["seq", mid1, mid2]              -> combine mid1 mid2 mappend
        ["seq2", mid1, mid2]             -> compose Music.seq mid1 mid2
        ["stackCycle", mid1, mid2]       -> compose stackCycle mid1 mid2
        ["stackTruncate", mid1, mid2]    -> compose stackTruncate mid1 mid2
        ["stackPreserve", mid1, mid2]    -> compose stackPreserve mid1 mid2
        ["intersperse1", mid1, mid2]     -> compose intersperse1 mid1 mid2
        ["intersperse2", mid1, mid2]     -> compose intersperse2 mid1 mid2
        ["intersperse2n", mid1, mid2, n] -> intersperse2nCompositions mid1 mid2 n
        ["play", "m", mid]               -> playMelody mid
        ["play", "c", cid]               -> playComposition cid
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
                                        _ -> (putStrLn "Could not find melodies") >> parser mm cm
                                _ -> (putStrLn "Failed to read IDs") >> parser mm cm

        update :: String -> Maybe a -> (a -> Melody -> Melody) -> IO ()
        update mid arg f = case (readMaybe mid, arg) of
                                (Just mid', Just arg') ->
                                    case IM.updateWith mid' (f arg') mm of
                                        Just mm' -> (putStrLn $ "Updated melody " ++ mid) >> parser mm' cm
                                        _       -> (putStrLn "Could not update") >> parser mm cm
                                _ -> (putStrLn "Failed to read ID or argument") >> parser mm cm

        update' :: Playable a => String -> (a -> a) -> IncrMap a -> (IncrMap a -> IO ()) -> IO ()
        update' pid f m parser' = case (readMaybe pid :: Maybe Int) of
                                    Nothing   -> (putStrLn "Failed to read ID") >> parser' m
                                    Just pid' ->
                                        case IM.updateWith pid' f m of
                                            Nothing -> (putStrLn "Could not update") >> parser' m
                                            Just m' -> (putStrLn $ "Updated " ++ pid) >> parser' m'

        combine :: String -> String -> (Melody -> Melody -> Melody) -> IO ()
        combine mid1 mid2 f = case (readMaybe mid1, readMaybe mid2) of
                                (Just mid1', Just mid2') ->
                                    case (IM.get mm mid1', IM.get mm mid2') of
                                        (Just m1, Just m2) ->
                                            let (mid, mm') = IM.add mm $ f m1 m2 in
                                            (putStrLn ("new melody id = " ++ (show mid))) >> parser mm' cm
                                        _ -> (putStrLn "Could not find melodies") >> parser mm cm
                                _ -> (putStrLn "Failed to read IDs") >> parser mm cm

        intersperse2nCompositions :: String -> String -> String -> IO ()
        intersperse2nCompositions mid1 mid2 n = case readMaybe n of
                                                     Nothing -> putStrLn "Invalid, yo"
                                                     Just n' -> compose (intersperse2n n') mid1 mid2

        playMelody :: String -> IO ()
        playMelody mid = case (readMaybe mid :: Maybe Int) of
                            Nothing   -> (putStrLn "Failed to read ID") >> parser mm cm
                            Just mid' ->
                                case IM.get mm mid' of
                                    Just m  -> (play $ toMusicPitch m) >> parser mm cm
                                    Nothing -> (putStrLn "Could not find melody") >> parser mm cm

        playComposition :: String -> IO ()
        playComposition cid = case (readMaybe cid :: Maybe Int) of
                                Nothing   -> (putStrLn "Failed to read ID") >> parser mm cm
                                Just cid' ->
                                    case IM.get cm cid' of
                                        Just c  -> (play $ toMusicPitch c) >> parser mm cm
                                        Nothing -> (putStrLn "Could not find composition") >> parser mm cm

        addSplit :: String -> String -> IO ()
        addSplit mid i = case (readMaybe mid, readMaybe i) of
                            (Just mid', Just i') ->
                                case IM.get mm mid' of
                                    Nothing -> (putStrLn "Could not find melody") >> parser mm cm
                                    Just m  ->
                                        case Music.splitAt i' m of
                                            (Just m1, Just m2) ->
                                                let (mid1, mm')  = IM.add mm  m1 in
                                                let (mid2, mm'') = IM.add mm' m2 in
                                                (putStrLn ("Added melodies " ++ (show mid1) ++ " and " ++ (show mid2))) >> parser mm'' cm
                                            (Just m1, _) ->
                                                let (mid1, mm') = IM.add mm m1 in
                                                (putStrLn $ "Added emelody " ++ (show mid1)) >> parser mm' cm
                                            _ -> (putStrLn "Failed to split") >> parser mm cm
                            _ -> (putStrLn "Failed to read ID or argument") >> parser mm cm

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
