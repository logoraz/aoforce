# AOFORCE - A Collection of Common Lisp Resources


<p align="center">
  <img src="assets/yin-yang-lisp-logo_512_svg.png" width="200" />
</p>

This is my Commmon Lisp configuration tool project. It servers as a general
resource for custom libraries, RC Files, ASDF configuration settings
and a playground to develop new systems. It's in a very early stage, so not
much functionality as a 'tool' here just yet, just a working project to store my
work at the moment...

## Project Scaffold
```lisp

  ~/.config/aoforce/
    |- aoforce.asd
    |- aoforce-test.asd
    |- aofrc.asd
    |- setup.lisp
    |- assests/
    |- core/
    |- libraries/
    |- tests/
    |- files/
    |- *rcfiles/
    |- *xdg-config/
```

## Setup, Building & Testing



Currently, the setup of this project itself acts as a template on how to scaffold
a modern Common Lisp system using the ASDF `package-inferred-system` along with a testing
framework (FiveAM). The Common Lisp code written herein also acts as a 'style guide' on my
functional/aesthetic preferences. I also use [ocicl](https://github.com/ocicl/ocicl) for
a modern approach for Common Lisp Systems Management - its a great tool (highly preferred
over quicklisp, qlot, roswell, CLPM, etc.) - really wish the community would adopt this
solution over the others!

The most recent addition is a testing framework (FiveAM) that has an example test template
borrowed from 
[The Common Lisp Cookbook:Testing With FiveAM](https://lispcookbook.github.io/cl-cookbook/testing.html#testing-with-fiveam).
The test(s) can be executed, in the project directory, via:

```lisp

* (asdf:test-system :aoforce/test)

```

As a fun exercise of incorporating a library, I adapted the `SDRAW` and `DTRACE` tools from
the book 
[Common Lisp: A Gentle Introduction to Symbolic Computation](https://www.cs.cmu.edu/~dst/LispBook/).
This is staged in the library `learncl` (alias `lcl`). For example, you can run the `sdraw`
learning tool (a cons cell visual aid) in this project as follows:

```lisp

(asdf:load-system :aoforce)
;; or
;; (asdf:load-system :aofrc) ; system alias
;;...

(lcl:sdraw '(This (is a (test!))))
;; or
;; (learncl:sdraw '(This (is a (test!)))) ; short alias
;;
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

## TODOs (Wish List)
 - Create a CL setup script to scaffold my prefered develop environment
 - Develop a CL Command Line tool to create Fedora RPM's for CL Libraries/Systems
 - TBD

## Core Development Environment (Fedora 42)
```bash
$ sudo dnf install sbcl gnome-themes-extra redhat-rpm-config sbcl cmake

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

## Building Nyxt (Fedora 42)
 - Nyxt (Electron) dependencies

```bash
$ sudo dnf install sbcl openssl-devel libfixposix-devel libsqlite3x-devel \
                   wl-clipboard enchant-devel npm redhat-rpm-config

$ cd /path/to/nyxt
$ make all NYXT_RENDERER=electron
```


## Building Clasp (Fedora 42)

- Build dependencies (Minimal)

```bash
$ sudo dnf install sbcl ninja-build clang19-devel llvm19-devel elfutils-devel \
                   boost-devel fmt-devel gmp-devel libunwind-devel binutils-gold \
                   redhat-rpm-config
```


## Changelog

### 0.0.4 (Wish List)
  - Create simple database to track installed configuration elements (sqlite?)
  - Create command-line application

### 0.0.3 (WIP)
  - Project name finalization: confer -> `:aoforce` `(:aofor)`
  - Create AOFORCE Logo svg
  - Create common lisp setup script
  - Build base of utilities/helpers
  - Make Clasp compliant (target CL implementations: SBCL & CLASP)

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

