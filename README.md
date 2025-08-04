# realworld-haskell

## Stack

* [Servant](https://www.servant.dev/)
* [Effectful](https://haskell-effectful.github.io/)
* [PostgreSQL](https://www.postgresql.org/)
* [Swagger](https://swagger.io/), [Swagger UI](https://swagger.io/tools/swagger-ui/)
* [Clean Architecture](https://blog.cleancoder.com/uncle-bob/2012/08/13/the-clean-architecture.html)

## Prerequisites

- [stack](https://docs.haskellstack.org/en/stable)
- [direnv](https://direnv.net/docs/installation.html)

## Getting Started

```sh
$ git clone https://github.com/eunmin/realworld-haskell
$ cp .envrc.template .envrc
$ direnv allow .
$ stack run
```

Open your browser and enter [http://localhost:3000/swagger-ui](http://localhost:3000/swagger-ui) in
the address bar.

## nix

If you're interested in the Nix project, please refer to the [nix tag](https://github.com/eunmin/realworld-haskell/tree/nix).
