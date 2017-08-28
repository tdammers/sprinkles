%Defining Routes

Sprinkles's routes are defined in the `rules` section in the `project.yml`
project configuration file. Routing rules are tried in order, until one of them
matches.

The following properties of a routing rule can be used to define a route's
behavior:

- `pattern`: This one is required; it defines what the URL should look like
  that the route matches on, which parts to extract from it, and how to name
  them.
- `data`: Defines data sources that are to be loaded once the rule matches.
- `template`: Sets the template (in `/templates`) to be used for this route.
- `redirect`: If this property is set to an URL, the route will not serve a
  template, but instead redirect to this URL.
- `static`: If this property is set to `true`, the template is ignored, and
  instead, the `data` item named `file` is served as-is. The purpose of this is
  to serve static assets.
- `required`: A list of `data` properties that must exist; if any of the
  required properties doesn't exist, the route raises a 404 Not Found error.

Here is an excerpt from the `rules` section of this website's `project.yml`:

```yaml
rules:
- # The overview page. Because it is the most prominent landing page, it is
  # structured differently, so we can prioritize a good first impression over
  # putting more information in the page.
  #
  # The pattern matches only the site root (/), and we load an `overview.yml`
  # file that tells the overview template which blocks to show below the top
  # content.
  pattern: '/'
  data:
    overview: 'file://./data/overview.yml'
    page: 'file://./data/pages/overview.markdown'
  template: 'overview.html'

- # We have some images that are referenced from page content; we put them in
  # `/img/`, but unlike pages, we want them to be served as static files, so
  # they get a rule for themselves.
  pattern: '/img/{{file:**}}' # match every route that starts with `/img/`
  data:
    # Append the matched filename to the /img/ root directory.
    file: 'file://./data/img/{{file}}'
  # Serve this as a static file, no template.
  static: true

- # The "User Guide" section is also special, because it has a Table Of
  # Contents on every page, and the pages themselves have numbers prefixed so
  # that ordering them by filename gives us the right ordering for the TOC. We
  # don't want the numbering to appear in links / URLs though, so there's a bit
  # of magic added to make that possible.
  #
  # The guide's index page is simple:
  pattern: '/guide'
  data:
    toc:
      type: file
      # The 'fetch: all' property makes it so that we get a list of all the
      # matching files rather than just one file.
      fetch: all
      path: './data/pages/guide/*.markdown'
  template: 'guide.html'
- # Then we have a rule that captures URLs that *do* have a three-digit number
  # prefix; we use a regular expression for that.
  pattern: '/guide/{{page:/^[0-9]{3}-.*$/}}'
  data:
    page: 'file://./data/pages/guide/{{page}}.*'
    toc:
      type: file
      fetch: all
      path: './data/pages/guide/*.markdown'
  template: 'guide.html'
- # This rule matches all the /guide/... URLs that don't start with a numeric
  # prefix.
  pattern: '/guide/{{page:*}}'
  data:
    page: 'file://./data/pages/guide/*-{{page}}.*'
    toc:
      type: file
      fetch: all
      path: './data/pages/guide/*.markdown'
  template: 'guide.html'
- # These are our regular static files (CSS, style images, JavaScript...)
  pattern: '/static/{{file:**}}'
  data:
    file: 'file://./static/{{file}}'
  static: true
- # Anything not matched by the above is a regular page:
  pattern: '/{{page:*}}'
  data:
    page: 'file://./data/pages/{{page}}.*'
  required:
    - 'page'
  template: 'page.html'
```

## Patterns

As you've seen above, route patterns form a minilanguage. Intuitively, a
pattern looks exactly like the local part of a URL (including the leading
slash), e.g. `/foo/bar?item=21`. On top of that, however, we can have dynamic
parts to match on variable URLs. Dynamic matchers come between double curly
braces (similar to template interpolations, although their purpose is
reversed), e.g.: `/foo/{{*}}?item=21`. Between the curly braces, we have:

- An optional *capturing name*, followed by a colon (`:`); if given, this name
  is the name of the variable as which the captured value can be used in the
  rest of the rule.
- A base matcher: this can be `*` to match anything, or a PCRE regular
  expression.
- An optional `*`, indicating that the part may match multiple times rather
  than exactly once.

For example, `/{{letters:/^[a-z]$/*}}` means "match many path elements that are
exactly one letter long, and capture that path under the variable `letters`.
This pattern would match, for example, `/a/f/g/a/i/k`, `/z`,
`/p/a/a/a/a/a/a/a/a/a`, but not `/a/b/cd/e`.

## Data Backends

The `data` property defines data to be loaded from backends. Each key in this
dictionary defines one variable that is passed to the template, and tells
sprinkles what to load. Most of the fields in the `data` section are passed
through Ginger, with the captured variables from the route pattern available
for interpolation. This means that you can inject captured variables into data
source definitions using `{{ varname }}`, but in fact the entire Ginger
language is available to you.

See the `Data Backends` section for a list of available backends and how to
define them.
