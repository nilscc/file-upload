Simple file upload & share platform written in Haskell & JavaScript.

Requirements
---

The browser frontend is using HTML5 features, including (but not limited to):

* [FileAPI](http://www.w3.org/TR/FileAPI/)
* CSS3 transitions and animations
* UTF-8 filenames using `filename*=UTF-8''<filename>` headers

If your browser does not support these features, please [upgrade to a modern
browser](http://browsehappy.com/).

The server backend is written in Haskell, using libraries like:

* [happstack-server](https://hackage.haskell.org/package/happstack-server)
* [acid-state](http://hackage.haskell.org/package/acid-state)
* [blaze-html](http://hackage.haskell.org/package/blaze-html)

GHC version 7.8.2 or higher is required.

Installing
---

To build the server, first create a new cabal-sandbox (this step is optional,
but recommended):

```
cabal sandbox init
```

Then install and run the binary:

```
cabal install
./.cabal-sandbox/bin/file-upload-server
```
