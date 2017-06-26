## 0.4.3.0

- Nicer YAML syntax for SQL backend.

## 0.4.2.0

- Handle SQL NULL more leniently in the SQL backend; NULL values in the result
  set are now passed to Ginger as null, rather than failing.
- Multi-query SQL backend: the SQL backend now accepts an alternative
  spec syntax. Instead of `query` and `params`, it can now also take just one
  `queries` key, containing a list of query string + params array pairs.
  Queries are executed against the params they come with, in order, and all the
  queries in the backend are run within the same transaction.
- Turned on partial pattern match warnings, and fixed them all.
- Added a missing template in an example project
- Expose backend data in templated rule specs. This means that it is now
  possible to change, for example, the selected template, or the redirect
  target, based on backend data.

## 0.4.1.2

- Serve html as utf-8 on apache for baked site
- Bugfix: Put baked .htaccess in the correct directory

## 0.4.1.1

- Generate .htaccess when baking

## 0.4.1.0

- Make the 'bake' command also generate a 404 page

## 0.4.0.0

- Added 'bake' command, putting sprinkles in static site generator mode.

## 0.3.5.0

- Upped Ginger version, adding {% switch %}.
