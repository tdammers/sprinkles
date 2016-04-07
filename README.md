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

At the time of writing, the only way of installing Templar is to install from
source. The easiest way to do that is to use
[Stack](https://haskellstack.org/):

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
key in that file is `Rules`, a list of routing rules.

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

On top of that, you can add variable parts, and capture them in variables:
`'/pages/{{page:*}}'` will capture *one* path segment (up to the next `/`, `?`,
`&`, or end of URL) and remember it in the variable `page` (which you can later
use in backend paths).  So in this example, a request to
`http://mywebsite.com/pages/home` will match, and store `"home"` in the
variable `page`.

At the end of a path, a variable can also capture the entire rest of the URL,
like so: `/pages/{{path:**}}`, i.e., double asterisks. The `path` variable,
here, will contain the rest of the URL after `/pages/`, including slashes and
anything else.

### Backend Definitions

Backend definitions are mini-templates where you can interpolate the variables
captured in the route pattern to build URLs for fetching data. Currently, the
following protocols are supported:

- `http://` / `https://`: fetch data over HTTP.
- `file://`: load a local file.

Backend data is parsed according to its reported MIME type (which, for the
`file://` protocol, is derived from the extension). The following types are
currently supported:

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
- Any other content type will not be parsed, but can be served as-is using the
  `static` directive. Using such data in templates or trying to serve them as
  JSON will produce empty values / `null`.
