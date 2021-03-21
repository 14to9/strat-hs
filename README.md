# strat-hs

Haskell implementation of pen-and-paper football simulation

## Developer Setup

Project assumes OSX + Docker + docker-sync

Get docker-sync up to make faster file syncs into the docker image:
> docker-sync start

Build the haskell container:
> docker-compose build

Shell into the haskell container
> docker-compose run dev bash

Inside the haskell container, run:
> root@strat-hs-dev strat-hs $ stack build
> root@strat-hs-dev strat-hs $ stack run
