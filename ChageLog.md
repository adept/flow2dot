2020-06-16
==========
Migrated to github, cleaned up sources

Version bump to 0.9.1

Pre-github Changelog
====================
tagged 0.7

  * Added Order directive (see sample.flow for usage)
  * Fix: identifier could not be empty
  * Removing useles import
  * Preserve UTF8 chars in "reflow"
  * Fix to preserve UTF8 chars in "showDot" (again)
  * Imported Dot.hs from dotgen 0.4.1
  * Fixed "lost edge" errors from graphviz

tagged 0.6.1

  * Forgot to add Text.Dot local override to cabal. Thnx to dons for spotting it

tagged 0.6

  * Fixed repo URL in README
  * Made sources compilable with QuickCheck2
  * Dotgen kills UTF-8 in graph attributes. Incorporated patched version of dotgen-0.2 into flow2dot until it will be fixed upstream
  * Added quickCheck.hs and rebuild.sh

tagged 0.5.1

  * Moved all flow diagram processing to separate module (exposed from this package)

tagged 0.4
  * Documentation fixes
  * Switched to `dotgen` package for all graphviz generation needs.
	    Dropped support of preformatted strings in flow files.

tagged 0.3.1
  * Fixed links to repo and docs

tagged 0.3
  * Ditched Text.UTF8 in favor of utf8-string library
  * Quotes inside messages are now properly escaped for Dot. Sample updated accordingly.

tagged 0.2.1
  * Version bump to 0.2.1 - ready for GHC 6.8.2
  * -Wall
  * +LANGUAGE pragmas

tagged 0.2
  * Some minor clarifications in docs
  * Dropped regex-based parser in favor of parsec-based due to issues with Unicode
  * Added QuickCheck to cabal, added LICENSE to Dot.hs
  * Added QuickCheck for Flow parser/pretty-printer
  * Moved graph generation into separate module
  * Added UTF8 module
  * cabalization

tagged 0.1
  * initial version
