SML# + annot
=============

DESCRIPTION
------------

'SML# + annot' is a patch and Emacs lisp for SML# compiler to show a type of an expression.

![sample](https://github.com/mzp/smlsharp-annot/raw/master/misc/sample.png "sample")

Supported version
------------------

 * v1.0.3

INSTALL
------------------

### Prepare
Downlad SML# compiler and install dependencies library. See http://www.pllab.riec.tohoku.ac.jp/smlsharp/.

Download a proper version patch from https://github.com/mzp/smlsharp-annot/tree/master/patches.(if you use 1.0.3, download 'PATCH-1.0.3'.)

### Patch

    $ tar xvzf smlsharp-xxx.tar.gz
    $ cd smlsharp-xxx
    $ patch -p2 < path/to/patches/PATCH-xxx

### Build/Install

    $ configure # with some options
    $ make minismlsharp
    $ make depend
    $ make # and wait for a long time
    $ make install

### Install Emacs lisp

Before this, install sml-mode(http://www.iro.umontreal.ca/~monnier/elisp/)

Download https://github.com/mzp/smlsharp-annot/blob/master/elisp/smlsharp.el and put `load-path`.

And add following lines to `.emacs`.

    (require 'sml-mode)
    (require 'smlsharp)
    (define-key sml-mode-map "\C-ct" #'smlsharp-query-type)

Usage
-------------------

Compile a file with annot option.

    $ smlsharp -d annot=yes fact.ml

* * * * * * * * * *
## License

Copyright 2012- MIZUNO Hiroki(mzp)

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

