# AOFORCE - My Common Lisp Playground

<p align="center">
  <img src="assets/aoforce-logo.svg" width="700" />
</p>

<p align="center">
  <img src="assets/yin-yang-lisp-logo_512_svg.png" width="200" />
</p>

Note: This project is akin to a dotfiles repo, however, centered around
Common Lisp - NOT intented to be useful to anyone besides myself, though one
may find it useful as a learning aid... The incentive and inspiration was
always for learning purposes.

This is my Commmon Lisp configuration resource tool project. This was started
as an idea to store my various common lisp libraries, each separately deployable
from one system. I also want it to setup my Common Lisp linux environment, i.e.
deploy rcfiles, dotfiles etc. that pertain to my workflow - a means of having a
reproducible Common Lisp environment.

The plan is that ideas/tools started here may later manifest themselves into
projects of their own. So this project is really just my Common Lisp
playground...


## Setup, Building & Testing

Currently, the setup of this project itself acts as a template on how to scaffold
a modern Common Lisp system along with a testing framework (FiveAM).
The Common Lisp code written herein also acts as a 'style guide' on my
functionality/aesthetic preferences. I also use
[ocicl](https://github.com/ocicl/ocicl) for a modern approach for Common Lisp
Systems Management, its a great tool (highly preferred over quicklisp, qlot,
roswell, CLPM, and vend which is discriminatory towards those using ASDF's
`package-inferred-system`). 

The most recent addition is a testing framework (FiveAM) that has an example test
template borrowed from 
[The Common Lisp Cookbook:Testing With FiveAM](https://lispcookbook.github.io/cl-cookbook/testing.html#testing-with-fiveam).


### Setup (WIP)


### Build & Test:

```lisp
* (asdf:load-system :aoforce)
* (asdf:test-system :aoforce)
```


### Play & Learn

As a fun exercise of incorporating a library, I adapted the `SDRAW` and `DTRACE` tools from
the book 
[Common Lisp: A Gentle Introduction to Symbolic Computation](https://www.cs.cmu.edu/~dst/LispBook/).
This is staged in the system/library `learn-cl`. For example, you can run the `sdraw`
learning tool (a cons cell visual aid) in this project as follows:

```lisp
;; Load Library Systems
(asdf:load-system :aoforce/libraries)

;; Enter into learn-cl/sdraw package
(in-package :learn-cl/sdraw)

(sdraw '(This (is a (test!))))
;; =>
;; [*|*]--->[*|*]--->NIL
;;  |        |
;;  v        v
;; THIS     [*|*]--->[*|*]--->[*|*]--->NIL
;;           |        |        |
;;           v        v        v
;;           IS       A       [*|*]--->NIL
;;                             |
;;                             v
;;                            TEST!

```

Another learning exercise was to create a functional calculator I wrote in Scheme,
but then translated into Common Lisp - it is also contained in the system/library
`learn-cl` under the package `:fcalc`. The CLOS based calculator can be trialed
under the package `:scalc`!

A simple calculator exhibiting the use of functional & CLOS programming 
in CL.

```lisp
;; Load Library Systems
(asdf:load-system :aoforce/libraries)

;; Enter in learn-cl/* package
(in-package :learn-cl/fcalc)
;; or (in-package :learn-cl/scalc)

(defvar *my-calc* (make-calculator 0))

(add! *my-calc* 2)
;;=> 2
;; OR
;; (add *my-calc* 2)

(multiply! *my-calc* 2)
;;=> 4
;; OR
;; (multiply *my-calc* 2)

(clear! *my-calc*)
;;=> 0
;; OR
;; (clear *my-calc*)

(get-result *my-calc*)
;;=> 0
;; OR
;; (value *my-calc*)

```


## Roadmap

 - [ ] Build a documentation system 
 - [ ] Build a configuration deployment system
 - [ ] Build database for configuration deployment system
 - [ ] Start adding unit testing
 - [ ] Build a CLI for deployment system
 - [ ] Build a GTK4 gui config system inspector?


## References:
 - TBD


# Fedora Scratch Notes

## Core Development Environment (Fedora 42)
```bash
$ sudo dnf install sbcl clisp gnome-themes-extra redhat-rpm-config cmake

```

## Building Clozure CL (ccl)
Simply follow their instructions. CCL is the most beautiful bootstrap I've seen, builds
with everything it ships with (i.e. it's minimal lisp image kernel).

- Ref: CCL (Clozure CL): https://github.com/Clozure/ccl/releases/tag/v1.13

```bash
# fetch source code into directory "ccl"
$ git clone https://github.com/Clozure/ccl.git ccl

# go into the "ccl" directory where the sources are
$ cd ccl

# download pre-compiled binaries
$ curl -L -O https://github.com/Clozure/ccl/releases/download/v1.13/linuxx86.tar.gz

# unpack binaries (lisp kernel, heap image, interface database) into "ccl" directory
$ tar xf ./linuxx86.tar.gz

# Rebuild CCL to make sure everything is up-to-date with respect to the current sources.
$ echo '(rebuild-ccl :full t)' | ./lx86cl64 -n

```

## Building ECL
Simply follow their instructions. ECL builds with the dependencies installed above.
- ECL (Embedable CL): https://common-lisp.net/project/ecl/static/manual/Building-ECL.htm

For a local install:

```bash
$ ./configure --prefix=/home/<user>/.local/   # change user to your username
$ make                                        # -jX if you have X cores
$ make install
```

## Building Nyxt (experimental)
 - Nyxt (Electron) dependencies

```bash
$ sudo dnf install sbcl openssl-devel libfixposix-devel libsqlite3x-devel \
                   wl-clipboard enchant-devel npm redhat-rpm-config

$ cd /path/to/nyxt
$ make all NYXT_RENDERER=electron
```

## Building Lem (experimental)
 - qlot "automatic installer"

```bash
 $ curl -L https://qlot.tech/installer | sh
 # uninstall
 $ ~/.qlot/qlot/scripts/qlot-uninstaller.sh
```

 - Lem dependencies

```bash
$ sudo dnf install sbcl ncurses-devel make automake gcc gcc-c++ \
                   sdl2-compat-devel SDL2_image-devel SDL2_ttf-devel fd-find \
                   redhat-rpm-config

$ cd /path/to/lem
# Build lem ncurses+sdl2
$ make sdl2-ncurses
```

## Building Clasp (experimental)

- Build dependencies (Minimal)
- Clasp to upgrade to llvm20 --> clang20 (current llvm19 --> clang19)

```bash
$ sudo dnf install sbcl ninja-build clang19-devel llvm19-devel elfutils-devel \
                   boost-devel fmt-devel gmp-devel libunwind-devel binutils-gold \
                   redhat-rpm-config

$ mkdir -p ~/.local/share/clasp/
$ cd ~/.local/share/clasp/
$ git clone https://github.com/clasp-developers/clasp.git ~/.local/share/clasp/src/
                   
```

## Building stumpwm/mahogany (experimental)

Get source

```bash
# Get source
$ git clone https://github.com/stumpwm/mahogany.git
$ cd /path/to/mahogany/
$ git submodule update --init
# test specific working branch
$ git switch fix/add-pkg-config-path-to-makefile
```

System Build Dependencies

```bash
$ sudo dnf install sbcl clang (or gcc) make meson \
                   redhat-rpm-config
$ sudo dnf builddep wlroots
```

CL Build Dependencies
```bash
# Note if you have ocicl setup this step is not necessary as it pulls
# these packages automatically from system - just listing here for completeness
$ ocicl install alexandria cl-ansi-text terminfo cl-argparse snakes ffi \
                ffi-grovel closer-mop fiaso
```

Build & Run

```bash
# Build & Run
$ make
$ make run
```
