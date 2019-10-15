# Productivity Tool: `okuno.info`

A productivity tool that is being tuned for myself (i.e. opinionated), but is far from complete.
At the moment, it is concerned with being an external memory for small notes.

## Development Environment

### Set Up

After cloning the project, don't forget to `git submodule init`.

Install dependencies.
First, `cabal install --only-dependencies`.
Then, `cabal install neptune/neptune neptune/neptune-wai`.

Create a database called `hstest` (or another name, but you'll have to change the config for that).
Then, run `sql/setup.sql` through it.
Then, run each numbered file in `sql/` through the db.

### Enable

Source some connection options for `postgresql-typed`: `source config/dev/env.sh`.
Compilation should now be successful.
Run the server with `cabal run config/dev/`.
