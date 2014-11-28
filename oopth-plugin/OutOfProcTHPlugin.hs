module OutOfProcTHPlugin (plugin) where

import GhcPlugins

import OutOfProcTH (installOOPTHHook)

plugin :: Plugin
plugin = defaultPlugin { installHooks = installOOPTHHook }
