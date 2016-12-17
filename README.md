Simple file upload & share platform written in Haskell & JavaScript.

Features
---

Implemented features:

* Secure login with randomized master key
* Drag & drop file upload UI
* Client-side and server-side file hashing before upload to guarantee
  correctness of upload
* Support for partial file uploads and resumable uploads of large files
* Anonymous download URLs with randomized file IDs
* Preservation of original file names (including all UTF-8 characters)

Planned features:

* Lightweight upload keys with limited bandwidth or number of files
* Lightweight browsing keys with restricted visibility on individual files
* Key management for given upload/browsing keys
* Logging mechanism for all up- and downloads as well as key activations

Issues:

* Please use the [issue tracker](https://github.com/nilscc/file-upload/issues)
  on GitHub for all bug reports.

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

Integrating with Other Web Servers
---

Setup your main web server as a proxy to the file-upload backend server. For
Nginx with Let's Encrypt use the following configuration inside the `http`
block of your Nginx configuration:

```
# Proxy for file-upload backend
server {
  server_name           <sub>.<domain>.<tld>;

  # only accept encrypted connections
  listen                443 ssl;
  ssl_certificate       /usr/local/etc/letsencrypt/live/<sub>.<domain>.<tld>/fullchain.pem;
  ssl_certificate_key   /usr/local/etc/letsencrypt/live/<sub>.<domain>.<tld>/privkey.pem;

  location / {

    # pass all data to backend
    proxy_pass                  http://127.0.0.1:8085;

    # disable all request buffering.
    # this is required for partial uploads to function properly.
    proxy_request_buffering     off;

    # max file size - adjust as necessary
    client_max_body_size        1G;
  }
}
```
