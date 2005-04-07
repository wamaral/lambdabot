--
-- | System module : IRC control functions
--

module SystemModule (systemModule) where

import IRC
import Util                     (join, breakOnGlue)
import qualified Map            (Map,keys,fromList,lookup)

import Data.Maybe               (fromMaybe)
import Control.Monad.State      (MonadState(get))
import Control.Monad.Reader     (mapReaderT)

------------------------------------------------------------------------

newtype SystemModule = SystemModule ()

systemModule :: SystemModule
systemModule = SystemModule ()

instance Module SystemModule () where
    moduleName   _ = return "system"
    commands     _ = return (Map.keys syscmds)
    moduleHelp _ s = return $ fromMaybe defaultHelp (Map.lookup s syscmds)
    process      _ = doSystem

------------------------------------------------------------------------

syscmds :: Map.Map String String
syscmds = Map.fromList
       [("listchans",   "show channels bot has joined")
       ,("listmodules", "show available plugins")
       ,("listcommands","listcommands [module]\n"++
                        "show all commands or command for [module]")
       ,("join",        "join <channel>")
       ,("leave",       "leave <channel>")
       ,("part",        "part <channel>")
       ,("msg",         "msg someone")
       ,("quit",        "quit [msg]")
       ,("reconnect",   "reconnect to channel")
       ,("echo",        "echo irc protocol string")]

defaultHelp :: String
defaultHelp = "system : irc management"

doSystem :: MonadIRC m => IRCMessage -> String -> [Char] -> [Char] -> m ()
doSystem msg target cmd rest = do
   s <- liftIRC get
   case cmd of
      "listchans"   -> ircPrivmsg target $ "Channels: "++pprKeys (ircChannels s)
      "listmodules" -> ircPrivmsg target $ "Modules: "++pprKeys (ircModules s)
      "listcommands" | null rest -> listAll s target
                     | otherwise -> listModule target rest

      "join"  -> checkPrivs msg target (ircJoin rest)
      "leave" -> checkPrivs msg target (ircPart rest)
      "part"  -> checkPrivs msg target (ircPart rest)

      "msg"   -> checkPrivs msg target $ ircPrivmsg tgt txt'
                      where (tgt, txt) = breakOnGlue " " rest
                            txt'       = dropWhile (== ' ') txt

      "quit" -> checkPrivs msg target $
              ircQuit $ if null rest then "requested" else rest

      "reconnect" -> checkPrivs msg target $
              ircReconnect $ if null rest then "request" else rest

      "echo" -> ircPrivmsg target $ concat 
              ["echo; msg:", show msg, " rest:", show rest]

      _unknowncmd -> ircPrivmsg target $ 
              concat ["unknown system command: ", show msg, show rest]

------------------------------------------------------------------------

listAll :: MonadIRC m => IRCRWState -> String -> m ()
listAll state target = 
        ircPrivmsg target $ "Commands: "++pprKeys (ircCommands state)

listModule :: MonadIRC m => String -> String -> m ()
listModule target modname = withModule ircCommands modname (ircPrivmsg target $ 
        "No module \""++modname++"\" loaded") (\m -> do
                cmds <- mapReaderT liftLB $ commands m
                ircPrivmsg target $ concat 
                        ["Module ", modname, 
                         " provides the following commands: ", show cmds])

pprKeys :: Show a => Map.Map a b -> String
pprKeys = join " " . map (init . tail . show) . Map.keys
