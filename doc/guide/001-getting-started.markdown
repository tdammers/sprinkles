%Getting Started

## Prerequisites

- A suitable computer with Sprinkles installed (see below).
- Your favorite text editor.
- A web browser.

## Installation

Head over to the [Download](/download) section and grab a copy of Sprinkles.
Follow the included installation instructions.

## Your First Project

1. Create a directory for your project.
2. Inside that directory, create a file named `project.yml`, and paste the
   following code into it:

    ```yaml
    rules:
    - pattern: '/'
      data:
        page: 'file://./data/pages/home.markdown'
      template: 'page.html'
    ```

3. Create a directory `data` in your project directory, and inside that, a
   directory named `pages`. Then create a file named `home.markdown` in that
   directory, and paste the following code:

    ```markdown
    %Home

    Hello, world!
    ```

4. Create a directory `templates` in your project directory, and inside that,
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

5. Open a terminal in the project directory, and issue the following command:

    ```
    sprinkles -serve 5000
    ```

    If all goes well, you'll see a line saying something like:

    ```
    2016-08-15 17:21:06.162955 UTC [Notice] Running server on port 5000
    ```

6. Point your web browser at [http://localhost:5000](http://localhost:5000).
    You should see a barebones HTML page titled "Home", that says "Hello,
    world!".

### What Just Happened?

In `project.yml`, you defined a **route**. A route defines a rule that tells
Sprinkles:

- which URLs the rule applies to (`pattern: '/'`)
- which backend data to load (`data: `)
- which template to apply (`template: `)

### Making Routes Dynamic

A route that always serves the same file isn't very powerful. Here's how to
parametrize routes.

1. Add the following to the bottom of your `project.yml`:

    ```yaml
    - pattern: '/{{page:*}}
      data:
        page: 'file://./data/pages/{{page}}.markdown'
      template: 'page.html'
    ```

2. Restart your Sprinkles server, and navigate to
    [http://localhost:5000/home](http://localhost:5000/home). Verify that it
    displays the same page as [http://localhost:5000](http://localhost:5000).

3. Now create another markdown file `./data/pages/example.markdown`.

4. Navigate to [http://localhost:5000/example](http://localhost:5000/example).
    Observe how it displays the text from `example.markdown`, using the same
    template as before.

The `pattern:` used in this second route contains a dynamic part, the bit
between double curly braces. Inside, the asterisk (`*`) means "match any one
path element in its entirety", and it is bound to the variable `page`.

This means that our route pattern will match anything that starts with `/`,
followed by exactly one path item and no slashes. It will, thus, match
`/foobar`, `/pizza`, `/12345`, `/oh-no.jpg`, but not `/foo/bar`.

In the `data:` part, the `page` value is injected into the data source
specifier. So when a visitor requests `/home`, the "home" part gets captured
as `page`, and inserting it into the data source string gives
`file://./data/pages/home.markdown`.

### Adding Static Assets

The HTML produced so far has been rather bland, so you will want to add CSS.

1. Create a file `./static/css/style.css`:

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

2. Add a rule to the stylesheet for serving static files:

    ```yaml
    - pattern: '/static/{{path:**}}' # note double asterisk!
      data:
        file: 'file://./static/{{path}}'
      static: true
    ```

3. Navigate to
    [http://localhost:5000/static/css/style.css](http://localhost:5000/static/css/style.css),
    and verify that it serves the stylesheet.

4. Link to it from the HTML by adding a `<link>` tag to the template. Add this
    to the `<head>` section:

    ```html
    <link rel="stylesheet" type="text/css" href="/static/css/style.css">
    ```

5. Check the site root again; the HTML should now look a little bit less
    bland.

Note that:

- The `pattern` features a dynamic part with a *double* asterisk, which means
  "match multiple path elements". The route will match `/static/foobar`, but
  also `/static/foo/bar/baz.css`, capturing *all* path elements after
  `/static/` as `path`.
- The line `static: true` tells Sprinkles that this is a static route.
  A static route expects a data key named `file`, and whatever comes out of
  that is served verbatim.
- Static routes do not have a `template:` parameter.
