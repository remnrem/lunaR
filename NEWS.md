# luna 1.8.0

* Added `p$procn()` and `p$proc_parallel()`: lunapi worker processes with
  R `ProcResult` tables, per-record errors and metadata, explicit worker
  parameters, row/ID selection, and database or text output. Strict
  failures carry the partial result in the R condition's `result` field.
* Complete rewrite as a pure-R package on top of the lunapi Python package
  (via reticulate), replacing the compiled `luna` package. No compiler or
  system libraries are needed to install.
* The API now mirrors lunapi: one exported object, `lp`, with `lp$proj()`,
  `p$inst()`, `i$eval()`, `i$table()`, `i$data()`, `p$proc()` and so on.
  The old `l*` functions (`lattach()`, `leval()`, `lx()`, `ldata()`, ...)
  are gone. See README for the differences from lunapi.
* Plots use lunapi's names: `lp$hypno()`, `lp$hypno_density()`,
  `lp$psd()`, `lp$spec()`, `lp$topo_heat()`, plus `i$hypno()`, `i$psd()`
  and `i$spec()`.
* Setup: `lunapi_available()` never starts Python unless asked;
  `options(luna.auto_install = FALSE)` / `LUNA_AUTO_INSTALL=false` turns
  off automatic installation; unsupported platforms are reported before
  any download.
* `moonlight()` removed (superseded by Lunascope).
