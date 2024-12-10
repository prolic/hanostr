module TimeFormatter (Language(..), formatDateTime) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, formatTime, defaultTimeLocale)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

data Language = English | Spanish | German

-- Convert Int (Unix timestamp) to UTCTime
intToUTCTime :: Int-> UTCTime
intToUTCTime = posixSecondsToUTCTime . fromIntegral

-- Format UTCTime to Text based on the provided format
formatUTCTime :: String -> UTCTime -> Text
formatUTCTime format utcTime = T.pack $ formatTime defaultTimeLocale format utcTime

-- Helper function to format time
formatDateTime :: Language -> Int -> Int -> Text
formatDateTime lang currentTimestamp messageTimestamp =
    let secondsInDay = 24 * 60 * 60
        diffInDays = (currentTimestamp - messageTimestamp) `div` secondsInDay
        isToday = diffInDays == 0
    in case lang of
        English -> if isToday
            then formatUTCTime "%I:%M %p" (intToUTCTime messageTimestamp)
            else formatUTCTime "%b %d, %I:%M %p" (intToUTCTime messageTimestamp)

        German -> if isToday
            then formatUTCTime "%H:%M" (intToUTCTime messageTimestamp)
            else formatUTCTime "%d.%m., %H:%M" (intToUTCTime messageTimestamp)

        Spanish -> if isToday
            then formatUTCTime "%H:%M" (intToUTCTime messageTimestamp)
            else formatUTCTime "%d/%m, %H:%M" (intToUTCTime messageTimestamp)
