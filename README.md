Afterburner Pascal
==================

This is Afterburner Pascal, a Pascal implementation written for two
reasons:

    1) A friend had written a Pascal program I wanted to run on my
       server, but I couldn't find a Pascal compiler that ran on
       OpenBSD.

    2) I wanted to gain some LLVM experience.

This compiler was designed and written by someone who knows neither
Pascal or LLVM (at least at the time this README was written).

Installation
============

Afterburner Pascal should be fairly easy to install, assuming you have
a decent-ish Haskell setup.

Just run `cabal install` and an executable by the name of
`afterburner-pascal` will be installed in your Cabal bin directory,
most likely `$HOME/.cabal/bin`.

Otherwise, just run `cabal configure`, followed by `cabal build`, and
the executable can be found as
`dist/build/afterburner-pascal/afterburner-pascal`.
