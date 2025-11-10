# AOFORCE - Common Lisp Playground

<p align="center">
  <img src="assets/aoforce-logo.svg" width="700" />
</p>

<p align="center">
  <img src="assets/lisp-logo.png" width="200" />
</p>

Note: This project is akin to a dotfiles repo, however, centered around
Common Lisp - NOT intented to be useful to anyone besides myself, though one
may find it useful as a learning aid... The incentive and inspiration was
always for learning purposes.

The plan is that ideas/tools started here may later manifest themselves into
projects of their own. So this project is really just my Common Lisp
playground...

## Setup, Building & Testing

Currently, the setup of this project itself acts as a template on how to scaffold
a modern Common Lisp system (declarative style) along with a testing framework.
The Common Lisp code written herein also acts as a 'style guide' on my
functionality/aesthetic preferences. I also use
[ocicl](https://github.com/ocicl/ocicl) for a modern approach for Common Lisp
Systems Management, its a great tool - the only one that has a CLI to tie into a
unix/linux workflow.


### Setup (WIP)
A work in progreess, plan is have a setup funtionality that auto builds and
deploys my Common Lisp tools, sets up the environment, and deploys my rc/dot-files.


### Build, Test, Create Executable, and Generate Documentation:

```lisp
;; Build System
;; will need `cl-gtk4` library manually injected in /ocicl/
;; see below section Play & Learn:ADW/GTK4 Example
(asdf:load-system :aoforce)

;; Test System
(asdf:test-system :aoforce/test)

;; Create Executable
(asdf:make :aoforce/executable)

;; Generate Documentation
(asdf:load-system :aoforce/docs)

;; Build Libraries (Extensions)
(asdf:load-system :aoforce/libraries)

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


#### ADW/GTK4 Example

Currently working on establishing a gtk4/adw frontend using `cl-gtk4`.
You can play/test it as follows. See: https://github.com/bohonghuang/cl-gtk4

Note: ocicl will install most of the dependencies, however, currently `cl-gtk4`
is not pulling/available and so `cl-gtk4` will need to be mainly placed in
the created `/ocicl/` directory:

```shell
  $ cd ./ocicl/
  $ git clone https://github.com/bohonghuang/cl-gtk4.git
```

Then you can run the adw tutorial package as follows:

```lisp
(asdf:load-system :aoforce)
(in-package :frontends/adw-tutorial)
(main)
```


## Roadmap

 - [ ] Build a documentation system 
 - [ ] Build a database based on sqlite
 - [ ] Start adding unit testing
 - [ ] Build a CLI for deployment system
 - [ ] Build a ADW/GTK4 GUI config system inspector?
 - [ ] Build a configuration deployment library/system (`:confr`)
 - [ ] Build a system to contruct rpm packagtes (`:cl-rpm`)


## References:
 - TBD
 - TBD
