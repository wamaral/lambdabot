{-# LANGUAGE TemplateHaskell #-}

module Modules (modulesInfo) where

import Lambdabot.Main

-- to add a new plugin, one must first add a qualified import here, and also
-- add a string in the list below
import Lambdabot.Plugin.Haskell
-- import Lambdabot.Plugin.IRC
import Lambdabot.Plugin.Misc
import Lambdabot.Plugin.Novelty
import Lambdabot.Plugin.Reference
import Lambdabot.Plugin.Social

modulesInfo :: Modules
modulesInfo = $(modules $ corePlugins
    ++ haskellPlugins
    -- ++ ["irc", "localtime", "topic"] -- ircPlugins // log
    ++ ["todo"] -- miscPlugins // dummy error fresh hello stats
    ++ ["dice", "elite", "filter", "quote", "slap"] -- noveltyPlugins // bf numberwang unlambda vixen
    ++ ["dict", "metar", "oeis", "search", "spell", "ticker", "url", "where"] -- referencePlugins
    ++ ["activity", "karma", "poll"]) -- socialPlugins // seen tell
