%Deployment

In order to run a Sprinkles site on a production system, you have several
options.

## Using The Built-In Warp Server

This is what you've been doing in the [Getting Started](001-getting-started)
guide: Sprinkles has a web server built into it that you can run as a standalone
process.

For a production server, however, you will want some extra bells and whistles,
such as running the application on port 80 (which is a privileged port and thus
requires root permissions on most systems), serving over HTTPS (which Sprinkles
itself does not implement), running on a subdomain, etc. Existing web servers
like nginx or Apache are excellent for this, and a simple setup is to configure
one of them as a *Reverse Proxy*, forwarding requests to a Sprinkles process that
listens on a non-privileged port.

A bit of extra work is needed to keep this working reliably, but once
everything is in place, this is a very flexible and performant setup.

## FastCGI

With FastCGI, the main web server process fires up one or more Sprinkles
processes as needed, and keeps them running for subsequent requests. For this
to work well, you'll want to create a wrapper script like the following (put it
right next to your `project.yml`), and point your web server's FastCGI handler
to it:

```sh
#!/bin/sh
sprinkles -fcgi
```

## SCGI

SCGI is similar to FastCGI; the protocol is different, but the use cases are
mostly the same. A suitable wrapper script looks like this:

```sh
#!/bin/sh
sprinkles -scgi
```

## CGI

CGI is not your best choice, because it requires going through all the startup
routines for every request; however, if you must, Sprinkles can work as a CGI
script. Just like with FastCGI, you probably want a little wrapper to point
your web server to:

```sh
#!/bin/sh
sprinkles -cgi
```
