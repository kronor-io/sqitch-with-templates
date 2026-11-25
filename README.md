# sqitch-with-templates

`create-migration` is a small wrapper around [`sqitch add`](https://sqitch.org/docs/manual/sqitch-add/)
for PostgreSQL projects. You pick what kind of change you are making (a table,
an index, a new non-nullable column, a column rename and so on). It asks for the
few names the template needs, gives the change a consistent name and path, and
runs `sqitch add` with deploy, revert and verify scripts filled in from the
template.

The templates hold what we learned running migrations against a busy production
database. Every lock is taken with a short `lock_timeout`, so a blocked
migration fails fast instead of queueing every query behind it. Indexes are
built `concurrently`. `NOT NULL` is added through a `NOT VALID` check
constraint. Renames and large updates are split into steps you deploy
separately. Scripts that cannot run in a transaction are written so that it is
safe to run them again.

We built this at [Kronor](https://kronor.io) for our own sqitch project and use it daily.

create-migration is one of the tools Kronor uses to keep a PostgreSQL schema in sqitch, with [sqiff](https://github.com/kronor-io/sqiff), [sqint](https://github.com/kronor-io/sqint) and [psql-retry](https://github.com/kronor-io/psql-retry). [sqitchcraft](https://kronor-io.github.io/sqitchcraft/) shows how they fit together: the repository layout they assume, one change from an edited file to a deploy, and instructions for coding agents.

## Contents

- [Installation](#installation)
- [Quick start](#quick-start)
- [Usage](#usage)
- [Subcommands](#subcommands)
- [Multi-step recipes](#multi-step-recipes)
- [Templates](#templates)
- [Shell completion](#shell-completion)
- [Development](#development)
- [License](#license)

## Installation

You need [sqitch](https://sqitch.org) with the PostgreSQL engine on your `PATH`.

### Nix

```bash
# run once
nix run github:kronor-io/sqitch-with-templates -- --help

# install into your profile
nix profile install github:kronor-io/sqitch-with-templates
```

The flake also exports `sqitchPg`, a sqitch that has these templates
installed as its own system templates. With it, plain `sqitch add --template add_column ...`
works without `create-migration`:

```bash
nix build github:kronor-io/sqitch-with-templates#sqitchPg
```

The overlay that does this is `sqitch-template-overlay.nix` if you want it in
your own nixpkgs.

Binary caches are configured in the flake (`kronor-open.cachix.org`). Pass
`--accept-flake-config` to use them.

### Cabal

```bash
git clone https://github.com/kronor-io/sqitch-with-templates
cd sqitch-with-templates
cabal install create-migration
```

The templates are installed as the package's data files, so the binary finds
them without further setup. Builds are tested with GHC 9.8.

## Quick start

Run it from the directory that holds your `sqitch.conf`, as you would run sqitch:

```console
$ create-migration add-column
Enter schema's value; name of the schema.
> billing
Enter table_name's value; name of the table.
> invoice
Enter column_name's value; name of the column.
> paid_at
Created deploy/migrations/2026/11/2026-11-18-142301-add-column-billing.invoice.paid_at.sql
Created revert/migrations/2026/11/2026-11-18-142301-add-column-billing.invoice.paid_at.sql
Created verify/migrations/2026/11/2026-11-18-142301-add-column-billing.invoice.paid_at.sql
Added "migrations/2026/11/2026-11-18-142301-add-column-billing.invoice.paid_at" to sqitch.plan
```

Or skip the prompts by passing the values on the command line:

```bash
create-migration add-column --schema billing --table-name invoice --column-name paid_at
```

Then open the generated files and fill in the placeholders (a column type, a
condition and so on). Each template says in comments what to fill in and what
to leave alone.

## Usage

```text
create-migration [--db TARGET] [--template-directory DIR] COMMAND [OPTIONS]
```

| Option | Meaning |
| --- | --- |
| `--db TARGET` | The sqitch target to add the change to, one of the `[target "..."]` sections in `sqitch.conf`. Without it, sqitch uses its default target. |
| `--template-directory DIR` | Use your own templates instead of the bundled ones. `DIR` must contain `deploy/`, `revert/` and `verify/`. See [Custom templates](#custom-templates). |
| `-h`, `--help` | Help, globally or for a subcommand (`create-migration add-index --help`). |

Any variable you leave off the command line is asked for interactively. The
prompts support line editing and history. The schema and table prompts
tab-complete from the directories under `deploy/` in your sqitch project, using
the target's `top_dir` (or `[core] top_dir`) from `sqitch.conf`.

Each generated change gets the note `<template>: <description>` in
`sqitch.plan`, for example `add_column: add column billing.invoice.paid_at`.

### Where changes go

Changes that create an object are named after it. Everything else is a
migration and goes under a date-based path, so the plan reads in the order the
work was done:

| Kind | Change name |
| --- | --- |
| table | `<schema>/table/<table>` |
| view | `<schema>/view/<view>` |
| function | `<schema>/function/<function>` |
| trigger | `<schema>/trigger/<table>/<trigger>` |
| domain | `type/<file>` |
| index | `<schema>/table/<table>/index/<index>` |
| partitioned-table index / FK | `<schema>/migrations/YYYY/MM/YYYY-MM-DD-...` |
| everything else | `migrations/YYYY/MM/YYYY-MM-DD-HHMMSS-<description>` |

To change a table, view or function after it has been deployed, use
`sqitch rework`. Do not create a new change for it.

## Subcommands

All subcommands accept `-s/--schema` and `-t/--table-name` where they apply.
Options left out are prompted for.

### Objects

| Command | Creates | Options |
| --- | --- | --- |
| `table` | A table with a `created_at` column, a commented-out foreign key and `comment on` placeholders | `-s`, `-t` |
| `view` | A view | `-s`, `-v/--view-name` |
| `function general` | A plpgsql function | `-s`, `-n/--function-name` |
| `function statechart` | A trigger function for a statechart transition, taking an `fsm_event_payload` (your schema must define that composite type) | `-s`, `-n/--function-name` |
| `trigger row` | A row-level trigger and its function | `-s`, `-n/--trigger-name`, `-t/--trigger-table` |
| `trigger statement` | A statement-level trigger and its function | `-s`, `-n/--trigger-name`, `-t/--trigger-table` |
| `domain` | A file for domain types | `-f/--file-name` |

### Changes to existing tables

| Command | What it does | Options |
| --- | --- | --- |
| `migration` | An empty migration with `lock_timeout` set and a checklist of safe-migration advice | `-d/--description` |
| `add-column` | `add column if not exists ... null` | `-s`, `-t`, `-c/--column-name` |
| `non-nullable-column` | Five changes that add a `NOT NULL` column without a long lock (see [below](#adding-a-not-null-column)) | `-s`, `-t`, `-c` |
| `add-index` | `create index concurrently if not exists` | `-s`, `-t`, `-i/--index-name` |
| `add-index-partitioned` | Builds an index concurrently on every partition, then on the parent and on the `pg_partman` template table | `-s`, `-t`, `-c/--column` |
| `add-fk-partitioned` | Three changes that add a foreign key from a partitioned table (see [below](#foreign-key-from-a-partitioned-table)) | `-s`, `-t`, `-c/--column`, `--target-schema`, `--target-table-name`, `--target-table-column` |
| `validate-constraint` | `validate constraint`, outside a transaction | `-s`, `-t`, `-c`, `--constraint-name` (default: `<column>_cannot_be_null`) |
| `bulk-update` | A batched `update` loop that commits every 100 rows and sleeps between batches | `-s`, `-t` |
| `rename-column-step-1` | First half of a column rename (see [below](#renaming-a-column)) | `-s`, `-t`, `--old-column-name`, `--new-column-name`, `--type`, `--batch-column`, `--batch-column-type` |
| `rename-column-step-2` | Second half of a column rename | `-s`, `-t`, `--old-column-name`, `--new-column-name` |

## Multi-step recipes

Some changes cannot be made safely in one step on a large table that is in use.
For these, `create-migration` writes several changes. Some must be deployed
together and some must not.

### Adding a NOT NULL column

`create-migration non-nullable-column -s billing -t invoice -c currency` adds
these five changes, in order:

1. **`add_column`**: adds the column as nullable.
2. **`add_not_null_constraint`**: adds `check (currency is not null) not valid`.
   New writes are checked from now on, but existing rows are not scanned. Before
   deploying this, make sure every insert sets the column, for example with a
   default or a `before` trigger.
3. **`bulk_update`**: backfills existing rows in batches. Fill in the `set`
   clause and the "not yet updated" condition.
4. **`validate_constraint`**: scans the table once under a
   `SHARE UPDATE EXCLUSIVE` lock, which does not block reads or writes.
5. **`make_column_not_null`**: `set not null`. Postgres uses the validated
   constraint to skip the scan, so this is instant. The check constraint is
   then dropped.

Changes 4 and 5 run outside a transaction and are written to be safe to rerun,
so do not add more statements to them.

### Renaming a column

You cannot rename a column in one deploy while application code still uses the
old name.

1. **`rename-column-step-1`** adds the new column and a trigger that keeps the
   two columns in sync in both directions. It then copies existing values over
   in batches ordered by `--batch-column`, and finally copies the old column's
   nullability and default. Deploy it, then switch the application to the new
   name.
2. **`rename-column-step-2`** drops the old column, the trigger and its
   function. Deploy it only after no running code reads or writes the old
   column.

### Foreign key from a partitioned table

Adding a foreign key to a partitioned parent locks and scans every partition at
once. `add-fk-partitioned` splits the work into three changes:

1. **part 1** adds the constraint to each partition as `NOT VALID`. This is
   quick, and new rows are checked from now on.
2. **part 2** validates the constraint on each partition, one at a time.
3. **part 3** adds the constraint to the parent. Every partition already has a
   valid matching constraint, so this is quick.

Deploy them in order. Each can go in its own release. Parts 1 and 2 use psql's
`\o` and `\i` to generate and run one statement per partition. Sqitch's
PostgreSQL engine runs scripts through `psql`, so this works with a normal
`sqitch deploy`. It does need to write a temporary file in the current
directory.

`add-index-partitioned` uses the same technique, because
`create index concurrently` cannot run inside the transaction a loop would
open. Its last statement assumes a [`pg_partman`](https://github.com/pgpartman/pg_partman)
template table named `partman.<schema>_<table>_template`. Drop that statement if
you do not use pg_partman, or fix the name if yours differs.

## Templates

Templates are [Template Toolkit](http://template-toolkit.org/) files, as sqitch
expects, in `create-migration/templates/{deploy,revert,verify}/<name>.tmpl`.
Each subcommand uses the template with the matching name (`add-column` uses
`add_column.tmpl`).

The deploy scripts follow these conventions:

- **Every lock is bounded.** Scripts begin with
  `select set_config('lock_timeout', '50ms', false)` or `set lock_timeout`.
  When a statement cannot get its lock within 50 ms it fails, and nothing waits
  behind it. Retry the deploy rather than raising the timeout.
- **Instant changes also get a short `statement_timeout`.** This applies to
  `set not null` on a validated column and to adding a `NOT VALID` constraint.
  If one of these takes longer than a few milliseconds, something is wrong, and
  the script stops.
- **Some scripts run without a transaction and must be safe to rerun.**
  `create index concurrently`, `validate constraint` and batched updates cannot
  run in one transaction. These files say so in a comment. Do not add
  statements after that comment.
- **Batched work commits between batches.** `bulk_update` and
  `rename_column_step_1` commit after each batch and sleep, so other
  transactions get a turn. Settings that must last across those commits are
  set with `set_config(..., false)`, because `SET LOCAL` is lost at the first
  `COMMIT`.
- **Verify scripts select from the new object or assert on the catalog**
  (`pg_indexes`, `information_schema`, `pg_constraint`), so `sqitch verify`
  fails when the object is missing. The exceptions are the partitioned-table
  index and foreign-key templates, whose verify scripts are empty. Write your
  own checks for those.

### Variables

Besides sqitch's own `project`, `change`, `engine`, `requires` and `conflicts`,
templates can use whatever variables the subcommand collects. `create-migration`
passes them to sqitch as `-s name=value`:

`schema`, `table_name`, `view_name`, `function_name`, `trigger_name`,
`trigger_table`, `file_name`, `index_name`, `column`, `column_name`,
`constraint_name`, `target_schema`, `target_table_name`,
`target_table_column`, `old_column_name`, `new_column_name`, `type`,
`batch_column`, `batch_column_type`.

### Custom templates

To change the SQL but keep the CLI, copy `create-migration/templates`, edit the
copy, and point `create-migration` at it:

```bash
create-migration --template-directory ./db-templates add-index -s billing -t invoice -i paid_at
```

The directory must contain a template for every name the subcommand uses. To add
a subcommand of your own, see [Development](#development).

## Shell completion

The Nix package installs completion for bash, zsh and fish. With a cabal
install, generate it yourself:

```bash
create-migration --bash-completion-script "$(command -v create-migration)" > ~/.local/share/bash-completion/completions/create-migration
```

Use `--zsh-completion-script` or `--fish-completion-script` for the other
shells. Completion knows subcommands and flags, `--db` target names from
`sqitch.conf`, schemas from `deploy/`, and, once `--schema` is on the line,
that schema's tables.

## Development

```bash
nix develop          # GHC, cabal, sqitch, pre-commit hooks
cabal run create-migration -- --help
```

The code lives in `create-migration/app`:

| Module | Role |
| --- | --- |
| `Sqitch/Template.hs` | Defines every template: its variables, how its change is named and described, and the multi-change compositions |
| `ProgramOptions.hs` | Command-line parsing: one subcommand per template |
| `Main.hs` | Collects the missing variables, then runs one `sqitch add` per template spec |
| `Sqitch/Command.hs` | Builds the `sqitch add` argument list |
| `Completion.hs`, `SqitchConf.hs` | Tab completion, and reading `sqitch.conf` |

To add a template:

1. Add `deploy/`, `revert/` and `verify/` files under `create-migration/templates`.
2. Add a constructor to `TemplateEnum` and its `TemplateSpec` in `metaOf`.
3. Add a subcommand in `ProgramOptions.hs` and map it in `resolveCommand`.

A pull request is checked by CI, which runs `nix build` for every package and
pre-commit check (fourmolu, nixpkgs-fmt, zizmor) on Linux and macOS. If you
change dependencies, regenerate `cabal.project.freeze` with
`nix build .#freezeFile` and copy the result over.

## License

LGPL-3.0-or-later.
