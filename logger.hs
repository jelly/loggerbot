--
-- A simple,  IRC logging bot in Haskell
-- 
-- Author: Jelle van der Waa
--
--  $ ghc -O --make -o bot bot.hs
--  $ ./bot
-- or
--  $ runhaskell bot.hs
-- or
--  $ echo main | ghci bot.hs
-- or
--  $ echo main | hugs -98 bot.hs
-- or
--  $ runhugs -98 bot.hs
--

import Data.List
import Network
import System.IO
import System.Time
import System.Locale
import System.Exit
import Control.Monad.Reader
import Control.OldException
import Control.Concurrent
import Text.Printf
import Prelude hiding (catch)

--
-- Some User Settings
--
server = "irc.freenode.org"
port   = 6667
chan   = "#flood"
nick   = "logger-bot"
fileName = "/home/jelle/test.txt"
nickcolor = "#0000FF"
msgcolor = "#FF0000"
admin = "foo"

--
-- The 'Net' monad, a wrapper over IO, carrying the bot's immutable state.
-- A socket and the bot's start time.
--
type Net = ReaderT Bot IO
data Bot = Bot { socket :: Handle, starttime :: ClockTime }

--
-- Set up actions to run on start and end, and run the main loop
--
main :: IO ()
main = bracket connect disconnect loop
  where
    disconnect = hClose . socket
    loop st    = catch (runReaderT run st) (const $ return ())

--
-- Connect to the server and return the initial bot state
--
connect :: IO Bot
connect = notify $ do
    t <- getClockTime
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    return (Bot h t)
  where
    notify a = bracket_
        (printf "Connecting to %s ... " server >> hFlush stdout)
        (putStrLn "done.")
        a

--
-- We're in the Net monad now, so we've connected successfully
-- Join a channel, and start processing commands
--
run :: Net ()
run = do
    write "NICK" nick
    write "USER" (nick++" 0 * :tutorial bot")
    write "JOIN" chan
    asks socket >>= listen

--
--get the nick out of the String
--
getNick :: String -> String
getNick = takeWhile (/= '!') . drop 1 

--
--returns true if argument is admin
--
isAdmin :: String -> Bool
isAdmin s  
    | (getNick s) == admin  = True
    | otherwise = False

--
--Get the message out of the string
--
msg :: String -> String 
msg = drop 1 . dropWhile (/= ':') . drop 1 

--
--Make the log message, output it as html
--
message :: String -> String
message str = printf " - %s: %s" (getNick str) (msg str)

--
--Read the irc channel to a logfile
--
logToFile :: String -> IO ()
logToFile str = appendFile fileName str

--
--check if it's a PRIVMSG
--
isPrivmsg :: String -> Bool
isPrivmsg str = "PRIVMSG"`isInfixOf` str


--
-- Process each line from the server
--
listen :: Handle -> Net ()
listen h = forever $ do
    s <- init `fmap` io (hGetLine h)
    
--io $ currentTime >>= logtoFile
--    io $ logtoFile  (message s)
    if isPrivmsg s then do { io $ currentTime >>= logToFile; io $ logToFile (message s) } else return ()

--    io (putStrLn s)
    if ping s then pong s else eval (clean s) s
  where
    forever a = a >> forever a
    clean     = drop 1 . dropWhile (/= ':') . drop 1
    ping x    = "PING :" `isPrefixOf` x
    pong x    = write "PONG" (':' : drop 6 x)

--
-- Dispatch a command
-- Let the bot only quit when admin calls '@quit'
--
eval :: String -> String -> Net ()
eval     "@uptime"   _          = uptime >>= privmsg
eval     "@source"   _          = privmsg "http://mydomain.com/logs/log.html" 
eval     "@version"  _          = privmsg "version 0.1" 
eval     "@quit"     nick          = if (isAdmin nick) then write "QUIT" ":Exiting" >> io (exitWith ExitSuccess) else return ()
eval     _           _          = return () -- ignore everything else

--
-- Send a privmsg to the current chan + server
--
privmsg :: String -> Net ()
privmsg s = write "PRIVMSG" (chan ++ " :" ++ s)

--
-- Send a message out to the server we're currently connected to
--
write :: String -> String -> Net ()
write s t = do
    h <- asks socket
    io $ hPrintf h "%s %s\r\n" s t
    io $ printf    "> %s %s\n" s t

--
-- Calculate and pretty print the uptime
--
uptime :: Net String
uptime = do
    now  <- io getClockTime
    zero <- asks starttime
    return . pretty $ diffClockTimes now zero

--
-- Pretty print the date in '1d 9h 9m 17s' format
--
pretty :: TimeDiff -> String
pretty td = join . intersperse " " . filter (not . null) . map f $
    [(years          ,"y") ,(months `mod` 12,"m")
    ,(days   `mod` 28,"d") ,(hours  `mod` 24,"h")
    ,(mins   `mod` 60,"m") ,(secs   `mod` 60,"s")]
  where
    secs    = abs $ tdSec td  ; mins   = secs   `div` 60
    hours   = mins   `div` 60 ; days   = hours  `div` 24
    months  = days   `div` 28 ; years  = months `div` 12
    f (i,s) | i == 0    = []
            | otherwise = show i ++ s

--
-- Convenience.
--
io :: IO a -> Net a
io = liftIO

currentTime = getClockTime >>= toCalendarTime >>= return . (formatCalendarTime defaultTimeLocale "%c")
