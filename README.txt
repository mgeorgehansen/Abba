---------------------------------
Abba: A better build architecture
---------------------------------

Welcome to Abba, a Haskell library implementing a low-level, abstract version
of Make. As the name suggests, Abba is _not_ a standalone build system like
Make: it is a collection of functions and datatypes that can be used to
easily and quicky setup a custom build system programmed in Haskell.

Abba is heavily based on Make and its functions try very hard to mimic Make's
behavior where appropriate. However, unlike Make where you have to rely on
macros to configure your build system Abba allows build system authors to
leverage the scripting power and type-safty of Haskell.

Documentation
-------------

At the moment there isn't a lot of user documentation. However, the Haddock
documentation for the Development.Abba module is quite extensive and should
be sufficient for basic use. Users should also look at the contents of the
"tests" directory, in particular at the various Buildscript.hs examples.

