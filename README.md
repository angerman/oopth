# Out Of Process Template Haskell

This project aims to implement the out of process template haskell strategie used in
[ghcjs](https://github.com/ghcjs/ghcjs) to haskell.  This would allow a hybrid strategie
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

## Usage

You will currently need to have a patched ghc, the following patch can be used with ghc-7.8.3
<https://gist.github.com/4d3e8da8143d7986bf63>. The patch allows plugins to install hooks, which
is how the out-of-proc-th soluction currently works.

Next, I recommend using a sandbox and install the plugin there.

```{sh}
oopth $ cabal sandbox init
# add the network service, while it's not on hackage yet (https://github.com/angerman/network-service)
oopth $ cabal sandbox add-source ../network-service
# when ever cahnges on the oopth-plugin are made, reinstall (hence --reinstall)
oopth $ cabal install oopth-plugin --reinstall
# launch the runner
oopth $ .cabal-sandbox/bin/th-runner slave &
# compile some module with a TH splice.
oopth $ cabal exec ghc -- Test/Simple.hs -package ghc -package oopth-plugin -fplugin OutOfProcTHPlugin -dynamic
# or with the llvm backend:
oopth $ cabal exec ghc -- Test/Simple.hs -package ghc -package oopth-plugin -fplugin OutOfProcTHPlugin -dynamic -fllvm
# check that the binary is working.
oopth $ ./Test/Simple
```

## Links

- [ghcjs](https://github.com/ghcjs/ghcjs)
  ghcjs contains the out of process template haskell facility already.  But in a tightly coupled
  solution specific to ghcjs for now.
- [Porting GHCJS Template Haskell to GHC](https://github.com/ghcjs/ghcjs/wiki/Porting-GHCJS-Template-Haskell-to-GHC)
  luite was kind enough to provide us with a detailed explaination on how he'd approach the port.

### iOS related:
- http://realmacsoftware.com/blog/dynamic-linking
- Note that you will need to sign the dylibs when deploying to a device.
