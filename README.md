Servant / Halogen 5 template
===
Template project with Haskell Servant on the backend and PureScript Halogen 5 on the frontend.

## Project structure

* [`backend`](/backend) directory - contains Haskell project of the backend server that exposes functionality via REST API.
* [`frontend`](/frontend) directory - contains PureScript project of the frontend application.

In order to setup/run components please refer to the corresponding README files within subdirectories.


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
