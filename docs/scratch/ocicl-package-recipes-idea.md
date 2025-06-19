Had an idea to expand OCICL's reach borrowing from a concept in Guix, that is the capability of specifying custom system repos (i.e. those that are failing to build upstream or just not yet added/known upstream) as build recipes - it could also go beyond to bring in libraries/systems that other PM's don't have or haven't yet been submitted to or published..

I understand that guix acts as a container, able to download all source types and build packages in a containerized approach, but maybe OCICL could implement a portion of that, i.e. the ability to specify a repo to build (with ASDF for now) locally and make known to OCICL and systems that use it. I foresee build recipes are something that could greatly expand OCICL and even Common Lisp for that matter.

As a quick template, perhaps one wanted to install `reblocks`, or a system that doesn't have components that require to compile c source etc., one could specify to ocicl:

```bash
$ ocicl install --custom reblocks-recipe.lisp
```

where `reblocks-recipe.lisp` could look like

```lisp
(define-recipe reblocks-custom
  (package
   (name "reblocks")
   (version "0.64.0")
   (source (method git-fetch)
           (url "https://github.com/40ants/reblocks")
           (commit "e74e0253e090dc2922ad4a2e2a5918c21a290bec")
           ;; A way to maintain the safety, i.e. to ensure the contents
           ;; have not been adultered...
           (content-hash "<sha256 hash here"))
   (build-system :asdf-build-system/sbcl))
  ...)
```
OCICL would pull the source from the repo, check the projects hash with that listed in the build recipe, and then build out with ASDF, and catalogue 
This would benefit users from doing it the manual way, as it would bundle source retrieval and security checks all in one command - while still working within the OCICL ecosystem...

I've thought about as a learning project, porting Guix's `g-exp`'s to Common Lisp as `b-exp` since they are primarily build expressions and perhaps this library could even build out to expand into what is needed to create the needed docker files and a way for users to contribute to the upstream packages all within Common Lisp data/code (homoiconicity).

I understand that this idea requires a huge amount of work, and even knowledge that it is unknown to me and as I am still learning  the basics of Common Lisp and symbolic computation for that matter, but thought I would go ahead and present the idea anyways despite my ignorance and foolishness in this new area for me.

Notes from Chris:
if it were me, I would probably edit the message and make it two or three sentences like "has anyone thought of updating ocicl so that it can install sources from web-requested vcs like github and gitlab? maybe ocicl could use the same approach as guix [code-sample]"