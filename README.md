# FROST - Backend

This is the Backend for the FROST Application.

## Build Instructions

Tested to work with:

 - GHC 7.8.4
 - Cabal 1.22.1.0
 - cabal-install 1.22.0.1

Run

````
cabal sandbox init
cabal install alex happy
cabal configure
cabal install -j4

export PORT=8000
cabal run
````

Files in **./static** will be served by name.

Snapshots can be saved and loaded from the [Admin Console](localhost:8000/admin).

## Required Setup

- PostgreSQL
- FROST-Frontend Project built in the **./static** folder.
