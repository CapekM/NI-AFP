module Data.Logging where

import Data.Time.Clock
import Data.List

data LogLevel = Debug | Info | Warning | Error | Fatal
              deriving (Show, Read, Eq, Ord, Enum, Bounded)

data EventSource = Internal { iesComponent   :: String
                            , iesCallID      :: String }
                 | External { eesURI         :: String
                            , eesDescription :: String }
                 | Combined [EventSource]
                 | Unknown
                 deriving (Show, Read, Eq)

data LogMessage = LogMessage
                { lmSource     :: EventSource
                , lmMessage    :: String
                , lmTimestamp  :: UTCTime
                , lmHiddenFlag :: Bool
                , lmLogLevel   :: LogLevel
                } deriving (Show, Read, Eq)

data EventSourceMatcher = Exact EventSource
                        | With EventSource
                        | AnyInternal
                        | AnyExternal
                        | Any
                        | MatchAny [EventSourceMatcher]
                        | MatchAll [EventSourceMatcher]
                        deriving (Show, Read, Eq)

-- | Change log level operator
-- TODO: implement operator which changes LogLevel of LogMessage
($=) :: LogMessage -> LogLevel -> LogMessage
($=) m l = m { lmLogLevel = l }


-- | EventSource "combinator"
-- TODO: implement operator which combines two EventSources (just 1 level for Combined, see tests)
(@@) :: EventSource -> EventSource -> EventSource
(@@) (Combined s1) (Combined s2) = Combined ( s1 ++ s2)
(@@) e (Combined s) = Combined ([e] ++ s)
(@@) (Combined s) e = Combined ((decomposition s) ++ [e])
(@@) e1 e2 = Combined ([e1] ++ [e2])

decomposition :: [EventSource] -> [EventSource]
decomposition (Combined x:xs) = (decomposition x) ++ (decomposition xs)
decomposition (x:xs) = [x] ++ (decomposition xs)
decomposition x = x

-- | Matching EventSource with EventSourceMatcher operator
-- TODO: implement matching
infixr 6 ~~
(~~) :: EventSourceMatcher -> EventSource -> Bool
(~~) (Exact e1) e2 = e1 == e2
(~~) (With e) (Combined s) = elem e s
(~~) AnyInternal (Internal _ _) = True
(~~) AnyInternal (Combined s)  = not $ null $ filter is_internal s
    where is_internal (Internal _ _) = True
          is_internal _ = False
(~~) AnyExternal (External _ _) = True
(~~) AnyExternal (Combined s) = not $ null $ filter is_external s
    where is_external (External _ _) = True
          is_external _ = False
(~~) Any _  = True
(~~) (MatchAny s) e = any (~~ e) s
(~~) (MatchAll s) e = all (~~ e) s
(~~) _ _ = False

-- | Specialized log list filter
-- TODO: implement filter function for logs with matchers, log level and hidden flag
logFilter :: EventSourceMatcher -> LogLevel -> Bool -> [LogMessage] -> [LogMessage]
logFilter m l h = (filter_hidden h) . (filter_level l) . (filter_source m)
    where filter_hidden h   = filter (\x -> h == (lmHiddenFlag x))
          filter_level l = filter (\x -> l <= (lmLogLevel x))
          filter_source m  = filter (\x -> m ~~ (lmSource x))
