%Data Backends

Backends are the things that pull data into your website. Such data can come
from diverse sources, and in various content formats. Sprinkles makes a best
effort at normalizing things into a consistent model, such that your templates
don't need to know whether the data they're handling was originally a local
.docx file, a JSON document fetched from a RESTful API, or a result set from an
SQL query.

There are two forms for backend definitions: long-hand (objects) and short-hand
(strings). The string form is more convenient, but doesn't offer all the
options.

# Supported Backend Types

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

## The `file` Backend

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

## The `http` Backend

Issues HTTP GET request to a remote server.

Longhand:

    type: 'http'
    uri: '{remote-uri}'

Shorthand:

    'http://{remote-uri}' # remote URI (without the `http://` prefix)

Using `https` instead of `http` will fetch data over an HTTPS connection.

## The `sql` Backend

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
themselves, sprinkles pretends they're JSON, and exposes them with a content type
of `text/json`, as a document containing a list of rows, each modelled as an
object of column names to values.

## The `subprocess` Backend

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

## The `post` Backend

Parse the request body according to its content type.

Longhand:

    type: 'post'

Shorthand:

    'post:'

Particular points of interests:

- This backend will only produce data for POST requests, since GET requests do
  not have a body.

## The `literal` Backend

Return inline data literally.

Longhand:

    type: 'literal'
    value: {any valid YAML value}


Shorthand:

    'literal://{string value}'

Note that the shorthand form only supports string values, but the longhand can
take any valid YAML data structure, and will pass it into the template as-is.

# Variable Backends

Both definition forms (short-hand and long-hand) support interpolating captured
variables in order to parametrize backend queries (i.e., fetch different data
items based on the variable parts of the matched route). In fact, the
interpolation uses Ginger, the same language that you'll use for the page
templates.

Variables can come from captured route elements (e.g. if your route is
`/pages/{{*:page_name}}`, then writing `{{page}}` anywhere in a backend
definition will inject the page name captured from the request path); they can
also come from other backends, as long as they load before the current one.

In order to force loading order, backend definitions can be written in a
two-stage data structure, namely a list of (ideally single-element) objects,
like so:

    data:
      - meta: 'file://data/meta/{{page}}.yml'
      - content: 'file://data/content/{{meta.filename}}'

Written in JSON syntax, the structure is a bit clearer:

    "data": [
      { "meta": "file://data/meta/{{page}}.yml" },
      { "content": "file://data/content/{{meta.filename}}" }
    ]

This is because plain YAML objects are conceptually unordered, so the order in
which they are written in the file does not survive the YAML parsing step. But
by putting them in a list, we can control the order in which they are
processed.

Without this trick, the `content` backend might end up being processed before
the `meta` backend, and that would mean that interpolating `meta.filename` into
the definition for `content` could not possibly work.

# Supported Content Types

Sprinkles detects the content type (file format) for backend data automatically
in most cases, depending on the backend type (see above). All the supported
MIME types are marshalled into the same format, but due to the diverse nature
of the data, details may still differ. The following types are currently
supported:

- JSON (`application/json`, `text/json`): parsed as JSON, and exposed as-is
- YAML (`application/x-yaml`, `text/x-yaml`): parsed as YAML, into the same
  kind of data structures as JSON
- Markdown (`application/x-markdown`, `text/x-markdown`): parsed as Pandoc
  Markdown. The resulting value behaves as follows:
    - When converted to JSON, generates the same output as Pandoc's JSON
      writer, i.e., a JSON representation of Pandoc's parse tree.
    - When converted to plain text, generates the same output as Pandoc's
      plaintext writer.
    - When converted to HTML, generates the same output as Pandoc's HTML5
      writer.
    - When used as a list (iterating without keys, or indexing with numeric
      keys), behaves as a list of the top-level blocks, which in turn behave
      like documents themselves. This means that you can step through the
      parsed DOM-like tree using array indexing, e.g. `page[0]` will get the
      first block in the `page` document, typically a level-1 heading, or the
      first paragraph of the document.
    - When used as a dictionary, two keys are exposed: `meta`, which gives
      access to meta properties defined in the input document, and `body`,
      which points at the input document contents.
- Textile (`application/x-textile`, `text/x-textile`): parsed as Textile using
  Pandoc. The resulting value behaves like Markdown.
- ReStructuredText (`application/x-rst`, `text/x-rst`): parsed as RST using
  Pandoc. The resulting value behaves like Markdown.
- HTML (`application/html`, `text/html`): parsed as HTML using Pandoc. This
  means that while basic semantic formatting (`<p>`, `<h1>`, `<b>`, `<em>`,
  ...) is preserved, most HTML-specific features (class names, IDs, script
  tags, ...) will be stripped out. Note that no attempt is made to translate
  relative URLs referenced in the HTML, so such links are likely to not work.
- DOCX
  (`application/vnd.openxmlformats-officedocument.wordprocessingml.document`):
  OpenXML-style Word documents. Support is rudimentary: text content is
  preserved without problems, but most formatting will be stripped out; bold
  and italic text works though.
- Any other content type will not be parsed, but can be served as-is using the
  `static` directive. Using such data in templates or trying to serve them as
  JSON will produce empty values / `null`.

# Getting Embedded Media Out Of Complex Documents

Some Pandoc document types (currently `.docx`) contain not only a rich-text
body, but also embed media such as images, which are referenced from within the
document itself. When rendering the body, we faithfully reproduce these
references, typically as `<img>` tags, but in order to actually get those
images into the resulting web page, we need a bit of extra work in the form of
a special routing rule.

For a document served something like this:

    - pattern: '/{{page:*}}'
      data:
        - page: 'file://./data/pages/{{page}}.*'
      template: 'page.html'

...we need an additional routing rule that makes it such that relative links in
`<img>` tags end up serving the images from the document statically. For this
to work, we need two things: a suitable routing rule, such that appending the
filename of the embedded image to the parent document's URL matches, and an
extra property on the rule to tell the static file handler that we want to
serve a child item rather than the parent document. This is what such a rule
looks like:

    - pattern: '/{{page:*}}/{{mediapath:**}}'
      data:
        - file: 'file://./data/pages/{{page}}.*'
      static: true
      child: '{{mediapath}}'
