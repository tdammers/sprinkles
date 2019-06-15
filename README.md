# Sprinkles

A minimalist zero-programming web framework.

https://sprinkles.tobiasdammers.nl/

https://github.com/tdammers/sprinkles/

## What It Does

Sprinkles enables you to build HTML front-ends to diverse data sources,
without any programming.

You write:

- A **YAML configuration file** that defines URLs and data sources
- A set of **templates** that say how to turn data into HTML
- Some **static assets**

Sprinkles then turns that into a fully functioning dynamic website - or bakes
it into a static site.

### Supported data sources

- Local files
- HTTP(S)
- SQL databases:
  - MySQL
  - SQLite
  - PostgreSQL
- Subprocesses

### Supported data fomats

- JSON
- YAML
- Markdown
- ReStructuredText
- Textile
- LaTeX
- WikiCreole
- HTML
- Plain text
- DOCX

### Typical Use Cases

- As a **presentation layer for a headless CMS**. Sprinkles can load data from
  a JSON API, feed it directly to your templates, and serve those on the fly.
- As a **poor man's CMS**, pulling data from a set of local files. Sprinkles
  supports a wide range of data formats, including JSON, YAML, Markdown,
  ReStructuredText, DOCX, HTML, and several more.
- As a **static site generator**. Develop your website locally, rendering it
  dynamically until you are happy with it, then bake it into a static website
  ready to upload to a web server.

## Installing

### Binary Install (x64 Linux)

This is the preferred way of installing sprinkles.

1. Install the system dependencies; at the time of writing, those are:
    - `libfcgi` (for FCGI support)
    - `libgmp` (for Haskell's bignum type)
    - `libcurl` (for HTTP backends)
    - `libpcre` (for PCRE regex support)
    - `libpq5` (for the PostgreSQL backend)
    - `libmysqlclient` (for MySQL support)
    - `libssl` (for TLS support)
  The exact names of the packages that you need to install for your OS may
  vary, so unfortunately we cannot provide a definitive list here.
2. Get the binary release tarball from
  https://github.com/tdammers/sprinkles/releases/latest (both zip file and
  tarball are available, both contain the same files)
3. Unzip or untar
4. Copy the sprinkles binary in `sprinkles/bin/sprinkles` to somewhere on your
   `$PATH`.  The suggested location for single-user deployments is
   `~/.local/bin/sprinkles`, or `~/bin/sprinkles`; for system-wide installation
   as root, `/usr/local/bin/sprinkles`.
5. `cd` to the `countryInfo` example project and start a sprinkles server:
  `cd examples/countryInfo; sprinkles -serve 5000`
6. Point your browser at http://localhost:5000/.

### From Source

1. Install development versions of the system dependencies; at the time of
  writing, those are:
    - `libfcgi` (for FCGI support)
    - `libgmp` (for Haskell's bignum type)
    - `libcurl` (for HTTP backends)
    - `libpcre` (for PCRE regex support)
    - `libpq5` (for the PostgreSQL backend)
    - `libmysqlclient` (for MySQL support)
    - `libssl` (for TLS support for the HTTP backend)
  The exact names of the packages that you need to install for your OS may
  vary, so unfortunately we cannot provide a definitive list here. On
  debian-like systems, the package names tend to end in `-dev` though.
2. Clone the Sprinkles repository:
  `git clone https://github.com/tdammers/sprinkles.git`
3. Go to the project directory:
  ```
  cd sprinkles
  ```
4. Build and install the `sprinkles` binary for your user:
  - Using [Cabal](https://www.haskell.org/cabal/):
    ```
    cabal v2-install
    ```
  - Using [Stack](https://haskellstack.org/):
    ```
    stack install
    ```
5. `cd` to the `countryInfo` example project and start a sprinkles server:
  `cd examples/countryInfo; sprinkles -serve 5000`
6. Point your browser at http://localhost:5000/.

## Further Reading

- [Sprinkles Website](https://sprinkles/tobiasdammers.nl/)
- [Getting Started](https://sprinkles.tobiasdammers.nl/doc/guide/getting-started)
- [User Guide](https://sprinkles.tobiasdammers.nl/doc/guide)
