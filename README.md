# Sprinkles

A minimalist website construction tool.

https://sprinkles.tobiasdammers.nl/

https://github.com/tdammers/sprinkles/

## What It Does

Sprinkles is a tool that provides HTML front-ends to diverse data backends, based
on a YAML configuration file, and a collection of templates and static files.
With sprinkles, you can turn a basic JSON API, or a collection of Markdown, YAML
and JSON files, into a full-blown content-driven website. Sprinkles can act as a
UI layer for a complex web service, or as a front-end for a poor man's CMS, or
as a drop-in replacement for a static site generator.

## Getting Started

### Installing

#### Binary Install (x64 Linux)

On a reasonably modern x64 Linux system, this is the preferred way of
installing sprinkles.

- Install the system dependencies; at the time of writing, those are:
    - `libfcgi` (for FCGI support)
    - `libgmp` (for Haskell's bignum type)
    - `libcurl` (for HTTP backends)
    - `libpcre` (for PCRE regex support)
    - `libpq5` (for the PostgreSQL backend)
    - `libmysqlclient` (for MySQL support)
    - `libssl` (for TLS support)
  The exact names of the packages that you need to install for your OS may
  vary, so unfortunately we cannot provide a definitive list here.
- Get the binary release tarball from
  https://github.com/tdammers/sprinkles/releases/latest (both zip file and
  tarball are available, both contain the same files)
- Unzip or untar
- Copy the sprinkles binary in `sprinkles/bin/sprinkles` to somewhere on your `$PATH`
  (the suggested location for single-user deployments is
  `~/.local/bin/sprinkles`, or `~/bin/sprinkles`; for system-wide installation as
  root, `/usr/local/bin/sprinkles`).
- Go to one of the example projects and start a sprinkles server:
  `cd examples/countryInfo; sprinkles 5000`
  Then point your browser at http://localhost:5000/.

#### From Source

- Install development versions of the system dependencies; at the time of
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
- Clone the Sprinkles repository:
  `git clone https://github.com/tdammers/sprinkles.git`
- Go to the project directory:
  ```
  cd sprinkles
  ```
- Build and install the `sprinkles` binary for your user:
  - Using [Cabal](https://www.haskell.org/cabal/):
    ```
    cabal v2-install
    ```
  - Using [Stack](https://haskellstack.org/):
    ```
    stack install
    ```
- Go to one of the example projects and start a sprinkles server:
  ```
  cd examples/countryInfo; sprinkles 5000
  ```
  Then point your browser at http://localhost:5000/.

## Project structure

The core of every sprinkles project is the `project.yml` file. (See
http://yaml.org/ for details on YAML). When starting up, sprinkles parses that
file and configures itself accordingly. The most important (and currently only)
key in that file is `rules`, a list of routing rules.

### Routing Rules

A routing rule consists of the following keys:

- `pattern`, a route pattern that triggers the route if matched, and extracts
  variables
- `data`, a dictionary of context data to load from data backends. This data
  gets passed to the template. Backend definitions can use variables defined in
  patterns and redirects; more on that below.
- `template`, the name of a [ginger](https://ginger.tobiasdammers.nl/)
  template file to use for this route. If none is given, the backend data is
  served as JSON, in raw form.
- `static`, a boolean (`true` or `false`) that, if `true`, overrides the
  template and causes Sprinkles to serve the raw file found under the `data.file`
  key as-is.
- `redirect`, defines a local URL to redirect to.
- `required`, a list of backend data keys that need to be present. A missing
  backend data key that appears in this list will lead to a 404 page being
  served, while a missing backend data key that doesn't will just produce a
  `null` value, and the template will still be rendered.

The rules are sampled from top to bottom, until one is found that matches.

### Patterns

A pattern is a HTTP request path, i.e., the part of the URL after the domain
name. In their simplest form, patterns match exactly, e.g.: `'/pages/home'`
will match `http://mywebsite.com/pages/home`, and nothing else.

Apart from literally matching exact paths, the following are possible:

- **Matching any path item**: `/pages/{{*}}` matches any direct child of
  `/pages`, e.g. `/pages/pizza`, `/pages/hello`, `/pages/1`, etc.
- **Matching many path items**: `/pages/{{**}}` matches any descendant of
  `/pages`, e.g. `/pages/pizza`, `/pages/pizza/pepperoni`,
  `/pages/1/5/things-to-see-and-do`, as well as `/pages` itself.
- **Matching a regular expression**: `/pages/{{/[0-9]+/}}` matches anything under
  `/pages/` that matches the regular expression `/[0-9]+/`, e.g. `/pages/1`,
  `/pages/2378728`, etc. The regular expression dialect used here is PCRE
  (i.e., the one used in Perl).
- **Named matches**: these can take the same form as the above, but come with a
  name. That name is used as a variable under which the matched path part is
  captured, and that captured variable can then be used to parametrize backend
  definitions (see below). The syntax is
  `{{`(variable-name)`:`(match-specifier)`}}`, where variable-name is the name
  for the capture, and match-specifier is one of the above matchers. For
  example, the following matches exactly one path item and captures it under
  the name `"slug"`: `{{slug:*}}`. Another example; `{{id:/[1-9][0-9]*/}}`
  matches a positive integer and captures it under the name `"id"`.

### Backends

Backends are the things that pull data into your website. Such data can come
from diverse sources, and in various content formats. Sprinkles makes a best
effort at normalizing things into a consistent model, such that your templates
don't need to know whether the data they're handling was originally a local
.docx file, a JSON document fetched from a RESTful API, or a result set from an
SQL query.

All backends can be defined in long-hand object form or in short-hand string
form; the string form is more convenient, but doesn't offer all the options.
Both forms support interpolating captured variables from route patterns in
order to parametrize backend queries (i.e., fetch different data items based on
the variable parts of the matched route). In fact, the interpolation is
implemented using Ginger (see below, under "Templates"), and the string parts
of each backend specification is actually a tiny Ginger template - this means
that you can do a lot more than just putting captured variables into backend
specs as-is; the full Ginger language is at your disposal here.

### Further Reading

For the full glory of what Sprinkles can do, please consult the documentation,
found in the `/doc/guide` directory in the source repository, or at
[https://sprinkles.tobiasdammers.nl/guide/](https://sprinkles.tobiasdammers.nl/guide/).
