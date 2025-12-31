# Check whether HTTP requests are proxied. This is the case if the kernel is started from Jupyterhub and the Python package "jupyter_server_proxy" is installed.

If requests are proxied and the HTTP server of the R session listens at
port \`\<PORT\>\` then the appropriate URL is of the form
\`\<JUPYTERHUB_SERVICE_PREFIX\>/proxy/\<PORT\>\` in addition to (or
instead of) \`http://localhost:\<PORT\>\`.

## Usage

``` r
http_is_proxied()
```
