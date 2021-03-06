-- Copyright (c) 2005-6 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)

-- | Lambdabot version information
module Lambdabot.Plugin.Core.Version (versionPlugin) where

import Lambdabot.Plugin
import Data.Version (showVersion)

versionPlugin :: Module ()
versionPlugin = newModule
    { moduleCmds = return
        [ (command "version")
            { help = say $
                "version/source. Report the version " ++
                "and git repo of this bot"
            , process = const $ do
                ver <- getConfig lbVersion
                say $ "lambdabot " ++ showVersion ver
                say "git clone https://github.com/wamaral/lambdabot"
            }
        ]
    }
