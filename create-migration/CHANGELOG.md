# Changelog

## 0.2.1.1

- Builds with GHC 9.12.4 and nixpkgs 26.05, on the same haskell.nix,
  nixpkgs and hackage.nix as sqiff and sqint, so they share one toolchain in
  the kronor-open cache. `sqitchPg` takes sqitch from nixpkgs 26.05's
  `perl5Packages`. No change to the templates or the tool.

## 0.2.1.0

- Templates write SQL keywords in lower case, `begin;`/`commit;`/`rollback;`
  included, like the rest of their SQL, sqiff's output and pgfmt's default.
  String values such as `'NO'` are unchanged.

## 0.2.0.0

- Subcommands replace the old `--table`/`--view`/... flags, and every template
  variable can be passed on the command line (`--schema`, `--table-name`, ...);
  anything missing is still asked for interactively.
- `--db` is now an optional flag naming the sqitch target; without it sqitch's
  default target is used.
- `--template-directory` points at your own templates instead of the bundled ones.
- Interactive prompts use haskeline: line editing, history, and tab completion
  of schema and table names from the sqitch project's `deploy/` directory.
- Shell completion for bash, zsh and fish, including target, schema and table names.
- `sqitch add` is run with an argument vector instead of through a shell, so
  quotes in descriptions no longer break the command.
- Time-based change names include the time of day (`YYYY-MM-DD-HHMMSS-...`),
  so two migrations made on the same day no longer collide.
- New templates: `add-index-partitioned` and the three-step `add-fk-partitioned`.
- Template fixes: `bulk_update` sets `lock_timeout`/`synchronous_commit` per
  session (a `SET LOCAL` is lost at the loop's first `COMMIT`); `add_column` is
  idempotent; `table` includes a `created_at` column; `add_index`'s verify only
  looks in the right schema.

## 0.1.0.0

- First release.
