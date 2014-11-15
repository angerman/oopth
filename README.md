# Out Of Process Template Haskell

This project aims to implement the out of process template haskell strategie used in
(ghcjs)[https://github.com/ghcjs/ghcjs] to haskell.  This would allow a hybrid strategie
where the compilere does not need to compile template haskell itself but defer that
to a template haskell runner, that would in turn compile the splices.  For cross-compilation
this would solve one of the main issues, which is that template haskell cross compiliation
can work properly only when there was a stage two compilere (a compiler running on the
target platform targetting the target platform).  With out of process template haskell, the
compiler could do the heavy lifting on the host platform, which in most cases is likely to
be more powerful than the target platform; a template haskell runner (or delegate) on running
on the target platform would then be used as a slave process to compile the required
template haskell pieces.

## Motivation

Template Haskell has become a very useful tool and is uesd extensively in lenses and also
in yesod.  No Template Haskell for cross compilers means that cross compilation is severely
cripled by the absents of the Template Haskell facility.

## Links

- [ghcjs](https://github.com/ghcjs/ghcjs)
  ghcjs contains the out of process template haskell facility already.  But in a tightly coupled
  solution specific to ghcjs for now.
- [Porting GHCJS Template Haskell to GHC](https://github.com/ghcjs/ghcjs/wiki/Porting-GHCJS-Template-Haskell-to-GHC)
  luite was kind enough to provide us with a detailed explaination on how he'd approach the port.

### iOS related:
- http://realmacsoftware.com/blog/dynamic-linking
- Note that you will need to sign the dylibs when deploying to a device.
