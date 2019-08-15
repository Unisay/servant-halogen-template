Frontend (Browser) application
===

### Installation

#### Prerequisites
1. `yarn`
    ```sh
    $ yarn version
    yarn version v1.17.3
    ```
1. Purescript 0.13.x
    ```sh
    $ purs --version
    0.13.2
    ```
1. `spago`
    ```sh
    $ spago version
    0.9.0.0
    ```
1. `psc-package`
    ```sh
    $ psc-package --version
    0.5.1
    ```

#### Steps
1. `yarn install`

### Usage

#### Development server

Start development web-server (Parcel) in one terminal window.
```sh
yarn serve
```

Watch Purescript files and recompile (except *.js) in other terminal.
```sh
yarn watch-ps
```

## Docker Compose Dev Environment

Instead of setting up compiler and dev server directly on your machine you can run complete dev setup inside docker containers using Docker Compose:

```sh
docker-compose up
```

This command downloads/builds necessary docker images and starts all components:
* http://localhost:9011 - FusionAuth UI
* http://localhost:1234 - Template UI
* http://localhost:9000 - Backend API
* http://localhost:9001 - Backend EKG UI
* http://localhost:9003 - Frontend Supervisord UI