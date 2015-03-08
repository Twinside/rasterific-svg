Rasterific-svg
==============

[![Build Status](https://travis-ci.org/Twinside/rasterific-svg.png?branch=master)](https://travis-ci.org/Twinside/rasterific-svg)
[![Hackage](https://img.shields.io/hackage/v/rasterific-svg.svg)](http://hackage.haskell.org/package/rasterific-svg)
SVGTiny loader/renderer/serializer based on Rasterific.

The library is available on [Hackage](http://hackage.haskell.org/package/rasterific-svg)

Current capabilities
--------------------

The current version implements SVG Tiny1.2 with the exception of:

 * non-scaling stroke.
 * textArea element.

The implementation also implements feature from SVG 1.1 like:

 * Advanced text handling (text on path, dx/dy), but with
   low support for Unicode, right to left and vertical text.
 * CSS Styling, using CSS2 styling engine.
 * `pattern` element handling
 * `marker` element hadnling

