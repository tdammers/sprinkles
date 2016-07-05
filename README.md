# Templar

A minimalist website construction tool.

## What It Does

Templar is a tool that provides HTML front-ends to diverse data backends, based
on a YAML configuration file, and a collection of templates and static files.
With templar, you can turn a basic JSON API, or a collection of Markdown, YAML
and JSON files, into a full-blown content-driven website. Templar can act as a
UI layer for a complex web service, or as a front-end for a poor man's CMS, or
as a drop-in replacement for a static site generator.

## Getting Started

### Installing

#### Binary Install (x64 Linux)

On a reasonably modern x64 Linux system, this is the preferred way of
installing templar.

- Install the system dependencies; at the time of writing, those are:
    - `libfcgi` (for FCGI support)
    - `libgmp` (for Haskell's bignum type)
    - `libcurl` (for HTTP backends)
    - `libpcre` (for PCRE regex support)
- Get the binary release tarball from
  https://github.com/tdammers/templar/releases/latest (both zip file and
  tarball are available, both contain the same files)
- Unzip or untar
- Copy the templar binary in `templar/bin/templar` to somewhere on your `$PATH`
  (the suggested location for single-user deployments is
  `~/.local/bin/templar`, or `~/bin/templar`; for system-wide installation as
  root, `/usr/local/bin/templar`).
- Go to one of the example projects and start a templar server:
  `cd examples/countryInfo; templar 5000`
  Then point your browser at http://localhost:5000/.

#### From Source

The easiest way to do that is to use
[Stack](https://haskellstack.org/):

- Install development versions of the system dependencies; at the time of
  writing, those are:
    - `libfcgi` (for FCGI support)
    - `libgmp` (for Haskell's bignum type)
    - `libcurl` (for HTTP backends)
    - `libpcre` (for PCRE regex support)
- Install stack
- Clone the Templar repository:
  `git clone https://bitbucket.org/tdammers/templar.git`
- Go to the project directory: `cd templar`
- Install the templar binary for your user: `stack install`
- Go to one of the example projects and start a templar server:
  `cd examples/countryInfo; templar 5000`
  Then point your browser at http://localhost:5000/.

## Project structure

The core of every templar project is the `project.yml` file. (See
http://yaml.org/ for details on YAML). When starting up, templar parses that
file and configures itself accordingly. The most important (and currently only)
key in that file is `rules`, a list of routing rules.

### Routing Rules

A routing rule consists of the following keys:

- `pattern`, a route pattern that triggers the route if matched, and extracts
  variables
- `data`, a dictionary of context data to load from data backends. This data
  gets passed to the template. Backend definitions can use variables defined in
  patterns and redirects; more on that below.
- `template`, the name of a [ginger](https://bitbucket.org/tdammers/ginger.git)
  template file to use for this route. If none is given, the backend data is
  served as JSON, in raw form.
- `static`, a boolean (`true` or `false`) that, if `true`, overrides the
  template and causes Templar to serve the raw file found under the `data.file`
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

- **Matching any path item**: `/pages/{*}` matches any direct child of
  `/pages`, e.g. `/pages/pizza`, `/pages/hello`, `/pages/1`, etc.
- **Matching many path items**: `/pages/{**}` matches any descendant of
  `/pages`, e.g. `/pages/pizza`, `/pages/pizza/pepperoni`,
  `/pages/1/5/things-to-see-and-do`, as well as `/pages` itself.
- **Matching a regular expression**: `/pages/{/[0-9]+/}` matches anything under
  `/pages/` that matches the regular expression `/[0-9]+/`, e.g. `/pages/1`,
  `/pages/2378728`, etc. The regular expression dialect used here is PCRE
  (i.e., the one used in Perl).
- **Named matches**: these can take the same form as the above, but come with a
  name. That name is used as a variable under which the matched path part is
  captured, and that captured variable can then be used to parametrize backend
  definitions (see below). The syntax is
  `{`(variable-name)`:`(match-specifier)`}`, where variable-name is the name
  for the capture, and match-specifier is one of the above matchers. For
  example, the following matches exactly one path item and captures it under
  the name `"slug"`: `{slug:*}`. Another example; `{id:/[1-9][0-9]*/}` matches
  a positive integer and captures it under the name `"id"`.

### Backends

Backends are the things that pull data into your website. Such data can come
from diverse sources, and in various content formats. Templar makes a best
effort at normalizing things into a consistent model, such that your templates
don't need to know whether the data they're handling was originally a local
.docx file, a JSON document fetched from a RESTful API, or a result set from an
SQL query.

All backends can be defined in long-hand object form or in short-hand string
form; the string form is more convenient, but doesn't offer all the options.
Both forms support interpolating captured variables from route patterns in
order to parametrize backend queries (i.e., fetch different data items based on
the variable parts of the matched route).

#### Supported Backend Types

All backend specifications support at least the `type`, `fetch`, and `ordering`
keys in long-hand mode. `type` determines the backend type, `fetch` is one of
`one`, `all`, or an integer for a fixed maximum number of items. `all` and
numbered return a list of records, `one` returns just one record. For
`ordering`, the following are allowed:

- "arbitrary": do not reorder, use whatever the backend produces
- "random": random-shuffle results
- "shuffle": same as "random"
- "name": order by name
- "mtime": order by modification time

##### The `file` Backend

Fetches data from local files.

Longhand:

    type: 'file'
    path: '{filename}' # The filename, absolute or relative, to load

Shorthand:

    'file://{filename}'

Two variations exist: using `glob` for the type instead of `file` will
interpret the filename as a glob expression and return zero or more files
instead of exactly one; using `dir` will treat the filename as a directory and
return all direct descendants of the directory rather than the directory
itself.

Example:

    type: file
    path: '/var/www/example.org/static/{staticfile}'

##### The `http` Backend

Issues HTTP GET request to a remote server.

Longhand:

    type: 'http'
    uri: '{remote-uri}'

Shorthand:

    'http://{remote-uri}' # remote URI (without the `http://` prefix)

Using `https` instead of `http` will fetch data over an HTTPS connection.

##### The `sql` Backend

Fetches data from an SQL database. Currently supports SQLite3 and PostgreSQL.

Longhand:

      type: 'sql'
      connection:
        driver: {driver} # one of 'sqlite', 'postgres'
        dsn: {driver-specific data source name}
      query: {SQL query} # parametrized using ? for placeholders
      params: [ "{{param1}}", "{{param2}}", ... ] # parameters to put in placeholder

Shorthand:

      'sql://{driver}:{dsn}:{query}'

Note that for security reasons, the SQL query string itself does not support
variable substitution; captured variables can only be interpolated into the
`params` array. This is to prevent SQL Injection: captured variables are
user-supplied and thus potentially tainted, so we only accept them into
parameters, not into the raw query. Since the shorthand form doesn't cater for
parameters, it follows that the shorthand doesn't support injecting captured
variables.

Because SQL result sets do not come in any particular file format by
themselves, templar pretends they're JSON, and exposes them with a content type
of `text/json`, as a document containing a list of rows, each modelled as an
object of column names to values.

##### The `subprocess` Backend

Run a program locally, and return its output (read from stdout).

Longhand:

    type: 'subprocess'
    cmd: ['{program}', '{arg1}', '{arg2}', ... ]
    mime-type: '{mime-type}'

Shorthand: n/a

Particular points of interest:

- Processes do not provide any information about the format of their output;
  because of this, you have to specify it explicitly in the configuration. If
  no MIME type is defined, `text/plain` is assumed.
- To prevent shell injection vulnerabilities, the command and arguments need to
  be provided as a list; the command is run directly, without a shell. This
  means, among other things, that shell magic such as globbing, environment
  variable substitution, or shorthands like `~` won't work.

#### Supported Content Types

Templar detects the content type (file format) for backend data automatically
in most cases, depending on the backend type (see above). All the supported
MIME types are marshalled into the same format, but due to the diverse nature
of the data, details may still differ. The following types are currently
supported:

- JSON (`application/json`, `text/json`): parsed as JSON, and exposed as-is
- YAML (`application/x-yaml`, `text/x-yaml`): parsed as YAML, into the same
  kind of data structures as JSON
- Markdown (`application/x-markdown`, `text/x-markdown`): parsed as Pandoc
  Markdown. The resulting value behaves as follows:
    - When converted to JSON, generates the same output as Pandoc's JSON writer,
      i.e., a JSON representation of Pandoc's parse tree.
    - When converted to plain text, generates the same output as Pandoc's
      plaintext writer.
    - When converted to HTML, generates the same output as Pandoc's HTML5 writer.
    - When used as a list (iterating without keys, or indexing with numeric
      keys), behaves as a list of the top-level blocks, which in turn behave like
      documents themselves. This means that you can step through the parsed
      DOM-like tree using array indexing, e.g. `page[0]` will get the first block
      in the `page` document, typically a level-1 heading, or the first paragraph
      of the document.
    - When used as a dictionary, two keys are exposed: `meta`, which gives access
      to meta properties defined in the input document, and `body`, which points
      at the input document contents.
- Textile (`application/x-textile`, `text/x-textile`): parsed as Textile using
  Pandoc. The resulting value behaves like Markdown.
- ReStructuredText (`application/x-rst`, `text/x-rst`): parsed as RST using
  Pandoc. The resulting value behaves like Markdown.
- HTML (`application/html`, `text/html`): parsed as HTML using Pandoc. This
  means that while basic semantic formatting (`<p>`, `<h1>`, `<b>`, `<em>`,
  ...) is preserved, most HTML-specific features (class names, IDs, script
  tags, ...) will be stripped out. Note that no attempt is made to translate
  relative URLs referenced in the HTML, so such links are likely to not work.
- DOCX (`application/vnd.openxmlformats-officedocument.wordprocessingml.document`):
  OpenXML-style Word documents. Support is rudimentary: text content is
  preserved without problems, but most formatting will be stripped out.
- Any other content type will not be parsed, but can be served as-is using the
  `static` directive. Using such data in templates or trying to serve them as
  JSON will produce empty values / `null`.
