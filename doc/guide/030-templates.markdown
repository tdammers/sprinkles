%Templates

Templates go into the `/templates` subdirectory, and are written in the
[Ginger](https://ginger.tobiasdammers.nl) template language. Ginger is very
similar to Jinja2 or Twig; if you're familiar with Django templates, you will
also feel at home quickly.

# Ginger Basics

Ginger is basically HTML, plus some extra syntax to put dynamic stuff in your
HTML; we call this "interpolation". The syntax for interpolation is `{{ }}`,
and the simplest form just injects one value from the route's data into the
HTML as-is.

For example, if the `greeting` value from the route is `"hello"`, then the
following template snippet:

```
<div class="greeting">{{ greeting }}</div>
```

...will produce the following HTML:

```html
<div class="greeting">hello</div>
```

One thing to note here is that Ginger automatically HTML-encodes any data
you pass it, unless it is HTML-encoded already; this means that you will not
generally encounter XSS vulnerabilities, nor excessive HTML-encoding (i.e.,
seeing "special" HTML characters in the output such as \&amp; or \&gt;.

## Literal Values

### Numbers

Ginger supports numeric literals in digital notation:

```
<span>{{ 500 }}</span>
```

...renders as:

```html
<span>500</span>
```

A range of basic math operators is available, e.g.:

```
<span>{{ 5 * 5 + 1 }}</span>
```

```html
<span>26</span>
```

### Strings

String literals can use single quotes, or double quotes:

```
<span>{{ "Hello" }}, {{ 'world' }}!</span>
```

```html
<span>Hello, world!</span>
```

While this may not seem terribly useful, strings can be useful to avoid writing
out excessive HTML-encoding by hand:

```
<p>
  This is what a div tag looks like:
  {{ "<div>content</div>" }}
</p>
```

```html
<p>
  This is what a div tag looks like:
  &lt;div&gt;content&lt;/div&gt;
</p>
```

String literals are also useful in more complex expressions.

Strings can be concatenated with the `~` operator; applying it will also
automatically convert both sides into strings, so you can use it to concatenate
a number onto a string:

```
<span>{{ "the winning number is " ~ 23 }}</span>
```

```html
<span>the winning number is 23</span>
```

## Dictionaries

Not all variables are simple values that you interpolate as-is; Ginger also
supports key-value collections known as *dictionaries*. Here's an example:

```
<div class="name">{{ user.name }}</div>
```

Assuming that the `user` value from the route is something like `{"username":
"johndoe", "fullname": "John Doe", "id": 23}`, then the above will render as:

```html
<div class="name">John Doe</div>
```

Alternatively, dictionary access can be achieved using `[ ]` syntax:

```
<div class="name">{{ user["name"] }}</div>
```

Note that in this case, the property name needs to be quoted; this is because
the `[ ]` syntax accepts arbitrary expressions. This works almost exactly like
in JavaScript.

## Lists

Lists are similar to dictionaries, but they have no explicit keys. You can,
however, access elements by (0-based) index:

```
<span class="third-item">{{ items[2] }}</span>
```

With `items` bound to `[ "foo", "bar", "baz" ]`, this renders as:

```html
<span class="third-item">baz</span>
```

## Loops

Lists are most useful, however, for loops. Ginger has a loop construct called
`for`, which looks like this:

```
<ul>
{% for item in items %}
  <li>{{ item }}</li>
{% endfor %}
</ul>
```

With the same binding for `items`, this will render as:

```html
<ul>

  <li>foo</li>
  <li>bar</li>
  <li>baz</li>

</ul>
```

## Conditionals

The basic conditional construct is `if`:

```
{% if user.name == null %}
<span class="anonymous-user">Guest</span>
{% else %}
<span class="registered-user">{{ user.name }}</span>
{% endif %}
```

Which, if `user` exists and has a `name` property of value `"John Doe", renders
as:

```html
<span class="registered-user">John Doe</span>
```

## Filters

Ginger comes with a wide range of filters. You can pass variables through
filters before interpolating them using the `|` syntax. For example, the
`default` filter replaces a value with a default value if it is empty:

```
<span>{{ user.name|default("anonymous") }}</span>
```

Another useful filter is `ellipse`, which shortens its input to a maximum
length, appending an ellipsis (...) as needed:

```
<div>{{ page.content|ellipsis(100) }}</div>
```

For a full list of supported filters, please refer to the [Ginger
Documentation](https://ginger.tobiasdammers.nl/guide).

## Functions

Filters and functions are basically the same thing, the only difference is
syntax. For example, using `default` as a function in the above example would
look like this:

```
<span>{{ default(user.name, "anonymous") }}</span>
```

# Ginger in Sprinkles

Ginger is a powerful and extensible template language. On top of what it can do
out of the box, Sprinkles adds some functionality of its own, and it uses
Ginger's flexible dynamic data structures to make the data you get from
backends quite powerful.

## Special Functionality Of Data Objects

### Rich Text Content

Many data formats  that Sprinkles can read (DOCX, HTML, Markdown, Textile,
ReStructuredText, ...) are exposed as "rich text". Such documents, by default,
render as HTML, reproducing as much of the markup as possible. But they can do
more:

- There's metadata, accessible through the `meta` property. This property, in
  turn, contains a dictionary of meta-property names and values; which ones
  exactly depends on the document and document type.
- The document's body can be accessed for more in-depth processing through the
  `body` property, which in turn can act as a list of top-level blocks; each
  block acts like a rich text document itself. This means that you can render
  the first block from a rich-text document using a construct like `{{
  document.body[0] }}`, and Ginger will take care of producing the right HTML.
- When converted to a plain string, a rich text document will do the right
  thing, producing a representation that preserves as much layout as possible
  in plain text, and discarding the rest so that what you get is always clean
  text.
- Embedded media files (such as images found inside DOCX documents) can be
  accessed through the `children` property. This isn't particularly useful
  inside templates, but makes it possible to serve these files as part of a
  dynamic route (see [Defining Routes](routes) for details on routes).
- By default, links to embedded media are produced such that their URLs map to
  the current URL plus a slash plus the name of the media file. However, if you
  need to override this, you can use the `withMediaRoot` to override the base
  URL for embedded media.

### The `request` Object

Every page exposes a `request` object that contains information about the
current HTTP request, particularly:

- `httpVersion`: '1.0', '1.1', ...
- `method`: 'GET', 'POST', ...
- `path`: the raw path portion of the request URL, e.g. `'/users/login'`
- `query`: the raw query-string portion, e.g. `'language=en&country=US'`
- `pathInfo`: the path, parsed into a list, e.g. `['users', 'login']`
- `queryInfo`: the query, parsed into a dictionary, e.g. `{'language': 'en',
  'country': 'US'}`
- `headers`: the request headers, as an ordered dictionary, e.g.
  `{'Content-type': 'text/plain'}`

## Extra Functions Sprinkles Adds

- `load`: Takes a backend specification (exactly like in `project.yml`), and
  loads data just like it would if written in `project.yml`.
- `ellipse`: Shorten, and append `...` as needed.
- `json`: Convert to JSON source. The optional parameter `pretty` makes it
  pretty-print (with line breaks and indentation).
- `yaml`: Convert to YAML.
- `getlocale`: You don't normally need to bother with this, but by adding this
  global function, Sprinkles tells Ginger how to format date and time values
  correctly.
- `pandoc`: Pass the input through pandoc (the rich-text conversion library
  Sprinkles uses). The second argument is the input language to use.
- `markdown`, `textile`, `rst`, `creole`:, Shorthand for `pandoc`, selecting the
  input language directly. 
