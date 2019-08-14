
Haskell Backend - Servant Web Server
===

## Haskell Setup

1. If you haven't already, [install Stack](https://haskell-lang.org/get-started)
	* On POSIX systems, this is usually `curl -sSL https://get.haskellstack.org/ | sh`

### Start server locally: 

1. Set environment variables:
```sh
API_PORT=9000 
EKG_PORT=9001 
ENV=Dev 
AUTH_API=http://localhost:9011/api/ # assumes that FusionAuth is listening there
```

2. Run server:
`stack run`

Alternatively, do it in one go:
```sh
$ API_PORT=9000 EKG_PORT=9001 ENV=Dev AUTH_API=http://localhost:9011/api/ stack run
Starting server at http://localhost:9000
```

### Continuously build project when developing
```sh
$ stack build --fast --file-watch --exec=clear
```
