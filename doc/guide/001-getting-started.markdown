%Getting Started

## Prerequisites

- A suitable computer with Sprinkles installed (see below). Currently, the
  easiest platform to run Sprinkles on is 64-bit GNU/Linux.
- A text editor. Popular choices include Notepad++, Atom, Nano, GEdit, Kate,
  Vim, Emacs, ...), but even Notepad will do in a pinch.

## Installation

Head over to the [Download](/download) section and grab a copy of Sprinkles.
Follow the included installation instructions.

## Your First Project

- Create a directory for your project.
- Inside that directory, create a file named `project.yml`, and paste the
  following code into it:

```yaml
rules:
- pattern: '/'
  data:
    page: 'file://./data/pages/home.markdown'
  template: 'page.html'
```

- Create a directory `data` in your project directory, and inside that, a
  directory named `pages`. Then create a file named `home.markdown` in that
  directory, and paste the following code:

```markdown
%Home

Hello, world!
```

- Create a directory `templates` in your project directory, and inside that,
  create a file named `page.html`; paste the following code:

```html
<!DOCTYPE html>
<html>
  <head>
    <title>{{ page.meta.title }}</title>
  </head>
  <body>
    <h1>{{ page.meta.title }}</h1>
    <div>
      {{ page.body }}
    </div>
  </body>
</html>
```

You should now have a directory structure like this:

```
+-- project.yml
|
+-- data
|    |
|    +-- pages
|         |
|         +-- home.markdown
|
+-- templates
     |
     +-- page.html
```

- Open a terminal in the project directory, and issue the following command:

```
sprinkles -serve 5000
```

If all goes well, you'll see a line saying something like:

```
2016-08-15 17:21:06.162955 UTC [Notice] Running server on port 5000
```

Now point your web browser at [http://localhost:5000](http://localhost:5000).
You should see a barebones HTML page titled "Home", that says "Hello, world!".

## What Just Happened?

In `project.yml`, we defined a *route*. A route defines a rule that tells
Sprinkles which URLs the rule applies to (`pattern: '/'`), and then says which
backend data to load (`data: `) and which template to apply (`template: `).

Let's add another route, but for this one, we'll add a dynamic part. Add the
following to the bottom of your `project.yml`:

```yaml
- pattern: '/{{page:*}}'
  data:
    page: 'file://./data/pages/{{page}}.markdown'
  template: 'page.html'
```

Restart your Sprinkles server, and navigate to
[http://localhost:5000/home](http://localhost:5000/home). Verify that it
displays the same page as [http://localhost:5000](http://localhost:5000).

Let's break down what happens here. The `pattern:` used in this second route
contains a dynamic part, the bit between double curly braces. Inside, we have
`page`, which is the name to which we're binding the dynamic part, and an
asterisk (`*`), which means "match any one path element in its entirety". This
means that our route pattern will match anything that starts with `/`, followed
by exactly one path item and no slashes. It will, thus, match `/foobar`,
`/pizza`, `/12345`, `/oh-no.jpg`, but not `/foo/bar`.

And then in the `data:` part, we refer to the `page` value captured from the
route pattern and inject it into the data source specifier. So when we ask for
`/home`, the "home" part gets captured as `page`, and inserting it into the
data source string gives us `file://./data/pages/home.markdown`.

Now add another markdown file in `./data/pages`, e.g.
`./data/pages/example.markdown`; write something else in it, and navigate to
[http://localhost:5000/example](http://localhost:5000/example). Observe how it
displays the text from `example.markdown`, using the same template as before.

## Adding Some Assets

The HTML we've produced so far has been rather boring, so let's add some CSS.
In order to do that, we need a stylesheet; keeping our static assets separate
from the content is kind of neat, so let's create a directory for them:
`./static`, and below it, `css`; inside that, we'll create a file named
`style.css`, and fill it with some useful CSS, e.g.:

```css
html {
  font-family: sans-serif;
  background-color: silver;
}

body {
  margin: 2em;
  padding: 1em;
  border: solid 1px black;
  background-color: white;
  box-shadow: 2px 2px 4px black;
}
```

This isn't enough, however: Sprinkles only serves things you explicitly tell it
to, so without any special rules, it will ignore the stylesheet. Here's a rule
that will match anything under `static`, and just serve the file it finds
unchanged:

```yaml
- pattern: '/static/{{path:**}}'
  data:
    file: 'file://./static/{{path}}'
  static: true
```

A few things to note here:

- The `pattern` features a dynamic part with a *double* asterisk, which means
  "match multiple path elements". This means that our route will match
  `/static/foobar`, but also `/static/foo/bar/baz.css`, capturing *all* path
  elements after `/static/` as `path`.
- There is no template; instead, we've added the property `static` with a value
  of `true`. This tells Sprinkles to treat this route as a static file, which
  means it should be served without any transformation applied, just the raw
  bytes.
- The `static: true` serving mode expects a data key called `file`, which we're
  declaring as `file://./static/{{path}}`.

Navigate to
[http://localhost:5000/static/css/style.css](http://localhost:5000/static/css/style.css),
and verify that it serves the stylesheet.

In order for the stylesheet to work, we need to link to it from the HTML, which
is best done by adding a `<link>` tag to the template. Add this to the `<head>`
section:

```html
<link rel="stylesheet" type="text/css" href="/static/css/style.css">
```

Now check the site root again; the HTML should now look a little bit less
bland.
