%Baking

Sprinkles can also "bake" a project into a static website, acting as a static
site generator. Simple run `sprinkles -bake` in the project root, and sprinkles
will create a static website in `./baked` under the project root.

## How It Works

In order to generate a static website, sprinkles uses the same request handling
logic as it would for serving a project dynamically; it just writes the output
to a file that matches the URL path, rather than sending it over HTTP. It does
this recursively, that is, it starts at the project root (`/`), and then
follows all links and references (except for those pointing to off-site
locations).

## Caveats

- Obviously, baking only works for content that doesn't change between
  requests.  For example, a weather website that pulls weather data from an
  external API and renders it as HTML, won't work - the API would be called
  once, while baking, and then the static site would forever display the
  weather for the time of baking, rather than current weather.
- Neither `POST` requests nor query string parameters currently
  work. This is because there is no obvious mapping from either of these to
  local file names. So if you intend to bake your project into a static site,
  do not use these features.
- Sprinkles will only include URLs in the baking process that are reachable
  from the root URL and produce a `200 OK` response.
