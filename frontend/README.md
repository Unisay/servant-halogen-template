Haskell Servant / PureScript Halogen5 Template
===

## Project Structure
* `backend` - backend code (Haskell)
* `frontend` - frontend code (PureScript)

## Frontend

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
