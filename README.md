# AOFORCE - My Common Lisp Playground

<p align="center">
  <img src="assets/yin-yang-lisp-logo_512_svg.png" width="200" />
</p>

Note: This project is akin to a dotfiles repo, however, centered around
Common Lisp - NOT intented to be useful to anyone besides myself, though one
may find it useful as a learning aid... The incenntive and inspiration was
always for learning purposes.

This is my Commmon Lisp configuration resource tool project. This was started
as an idea to store my various common lisp libraries, each separately deployable
from one system, hence setting it up using ASDF 3 `package-inferred-system`. I
also want it to setup my Common Lisp linux environment, i.e. deploy rcfiles,
dotfiles etc. that pertain to my workflow - a means of having a reproducible
Common Lisp environment.

The plan is that ideas/tools started here may later manifest themselves into
projects of their own. So this project is really just my Common Lisp
playground...


## Setup, Building & Testing

Currently, the setup of this project itself acts as a template on how to scaffold
a modern Common Lisp system using the ASDF `package-inferred-system` along with a
testing framework (FiveAM). The Common Lisp code written herein also acts as a
'style guide' on my functionality/aesthetic preferences. I also use
[ocicl](https://github.com/ocicl/ocicl) for a modern approach for Common Lisp
Systems Management, its a great tool (highly preferred over quicklisp, qlot,
roswell, CLPM, and vend which is discriminatory towards those using ASDF's
`package-inferred-system`). 

The most recent addition is a testing framework (FiveAM) that has an example test
template borrowed from 
[The Common Lisp Cookbook:Testing With FiveAM](https://lispcookbook.github.io/cl-cookbook/testing.html#testing-with-fiveam).

### Setup (WIP)

```bash
$ sbcl --load setup.lisp
```

### Build & Test:

```lisp

* (asdf:load-system :aoforce)
;; or
;; (asdf:load-system :aofrc) ; system short alias
;;...

* (asdf:test-system :aoforce/test)

```

### Play & Learn

As a fun exercise of incorporating a library, I adapted the `SDRAW` and `DTRACE` tools from
the book 
[Common Lisp: A Gentle Introduction to Symbolic Computation](https://www.cs.cmu.edu/~dst/LispBook/).
This is staged in the library `learncl` (alias `lcl`). For example, you can run the `sdraw`
learning tool (a cons cell visual aid) in this project as follows:

```lisp

(lcl:sdraw '(This (is a (test!))))
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

A simple calculator exhibiting the use of functional programming in CL:

```lisp
(defvar *my-calc* ())
;;=> *MY-CALC*

(setf *my-calc* (fcalc:make-calculator 0))
;; #<FUNCTION (LAMBDA (AOFORCE/LIBRARIES/LEARN-CL/FCALC::OPERATION)
;;             :IN
;;             AOFORCE/LIBRARIES/LEARN-CL/FCALC:MAKE-CALCULATOR) {100457E9EB}>

(fcalc:add! *my-calc* 2)
;;=> 2

(fcalc:multiply! *my-calc* 2)
;;=> 4

(fcalc:clear! *my-calc*)
;;=> 0

(fcalc:get-result *my-calc*)
;;=> 0

```

## TODOs (Wish List)
 - Create a CL setup script to scaffold my prefered develop environment
 - Develop a CL Command Line tool to create Fedora RPM's for CL Libraries/Systems
 - Create a website for this project (GitHub Pages)


## Changelog

### 0.0.4 (Next Up)
  - Create pre-liminary command-line application
  - Refactor build/setup of CLI & CL Runtime
  - Further build out unit testing
  - Build out document generation
  - Website?
  
### 0.0.3 (WIP)
  - Project name finalization: confer -> `:aoforce` `(:aoforce)`
  - Create AOFORCE Logo svg
  - Build base of core utilities/helpers
  - Create simple database to track configuration changes (clsql?)
  - Add document generation secondary system
  - Build out unit test system
  - Separate this changelog into it's own separate entity


### 0.0.2
  - Refactor/Clean-up project scaffold
  - Create System alias `:aofor` & set to package nickname
  - Add unit-testing framework template (FiveAM)
  - Update README & Revision Bump

### 0.0.1
  - Initial commit
  - Added basic project scaffold

   
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


## Building Nyxt
 - Nyxt (Electron) dependencies

```bash
$ sudo dnf install sbcl openssl-devel libfixposix-devel libsqlite3x-devel \
                   wl-clipboard enchant-devel npm redhat-rpm-config

$ cd /path/to/nyxt
$ make all NYXT_RENDERER=electron
```


## Building ECL
Simply follow their instructions. ECL builds with the dependencies installed above.

- ECL (Embedable CL): https://common-lisp.net/project/ecl/static/manual/Building-ECL.htm


## Building Clasp (experimental)

- Build dependencies (Minimal)
- Clasp upgrade to llvm20 -> clang20

```bash
$ sudo dnf install sbcl ninja-build clang19-devel llvm19-devel elfutils-devel \
                   boost-devel fmt-devel gmp-devel libunwind-devel binutils-gold \
                   redhat-rpm-config
```


## Building Lem (Fedora 42)
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
$ make lem
```
