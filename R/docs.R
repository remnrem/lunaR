#' The Luna project: engine session and sample list
#'
#' Returned by `lp$proj()`. Mirrors lunapi's `proj` class: the same method
#' names, arguments and defaults, called with `$`.
#'
#' @section Sample lists and instances:
#' * `sample_list(filename = NULL, path = NULL, df = TRUE)`: read a sample
#'   list (`path` sets the folder relative paths are resolved against), or
#'   with no `filename`, return the current one as a data frame.
#' * `build(args)`: build a sample list by scanning folders (`--build`).
#' * `nobs()`, `validate()`, `clear()`.
#' * `inst(n)`: an [inst] for sample-list record `n` (1-based) or ID `n`;
#'   an ID not in the list gives a new, empty instance with that ID.
#' * `empty_inst(id, nr, rs, startdate = "01.01.00", starttime = "00.00.00")`:
#'   an in-memory recording of `nr` records of `rs` seconds.
#' * `get_n(id)`, `get_id(n)`, `get_edf(x)`, `get_annots(x)`. *Indices
#'   are 1-based here* (lunapi's are 0-based), consistent with `inst(n)`.
#'
#' @section Running commands:
#' * `proc(cmdstr)`: run Luna commands across the whole sample list;
#'   returns a [ProcResult]. As in lunapi, a record that cannot be loaded
#'   stops the whole run; check with `validate()` first.
#' * `silent_proc(cmdstr)`: the same, with console output off.
#' * `procn(cmdstr, workers = NULL, batch_size = NULL, params = NULL,
#'   param_file = NULL, strict = FALSE, progress = TRUE, out_db = NULL,
#'   out_text = NULL, in_memory = NULL, n1 = NULL, n2 = NULL, ids = NULL,
#'   skip = NULL)`: process a file-backed sample list using lunapi worker
#'   processes, returning a [ProcResult] with R tables and per-record metadata.
#'   `proc_parallel()` is the same method under its longer name.
#' * `strata()`, `table(cmd, strata = "BL")`, `variables(cmd, strata)`,
#'   `empty_result_set()`: the most recent in-memory results (from `proc()`, `procn()` or
#'   `import_db()`). `strata` may be `"CH_F"` or `c("F", "CH")`.
#' * `import_db(f, s = NULL)`: load a `luna -o` database (optionally only
#'   IDs `s`); returns the IDs read, with tables available via `table()`.
#' * `desc()`, `commands()`.
#'
#' @section Parallel processing:
#' `workers` defaults to half the available CPUs, capped at 10 and at the
#' number of selected records. `batch_size` controls records per worker batch.
#' Workers do not inherit the parent's project variables: pass them as a
#' named list in `params`, or in a Luna `param_file`. Values in `params`
#' override the file, and use the same conversion as `vars()`.
#' `n1`/`n2` select inclusive, 1-based sample-list rows. `ids` and `skip`
#' accept character vectors or whitespace-separated ID strings.
#' `progress` is `TRUE`, `FALSE`, or an R function receiving progress events.
#'
#' By default, failed records are collected in `r$errors` while successful
#' tables are retained; `r$ok` reports whether there were no failures.
#' With `strict = TRUE`, failures raise a `luna_proc_error` condition with
#' the partial [ProcResult] in `e$result`.
#'
#' `out_db` writes per-worker databases (`prefix-1.db`, etc.); `out_text`
#' writes per-individual text tables. They are mutually exclusive and
#' disable in-memory table collection. Leave `in_memory = NULL` for this
#' automatic choice, or set it to `FALSE` for file output. The result
#' retains `errors`, `stdout`, `records`, `workers` and `out_paths`;
#' table access raises `luna_file_output`. Read files with `import_db()`
#' or `lp$read_text_table()`.
#'
#' ```r
#' p <- lp$proj()
#' p$sample_list("s.lst")
#' r <- p$procn("PSD sig=${s} spectrum", workers = 4,
#'               params = list(s = "EEG"))
#' r$table("PSD", "CH_F")
#' ```
#'
#' @section Variables and output:
#' * `var(key, value)` / `vars(key, value)`: with no arguments all
#'   variables; with `key` its value (`NULL` if unset); with `key` and
#'   `value`, set it; `key` may be a named list.
#' * `clear_vars(key = NULL)`, `clear_ivars()`, `include(f)`, `aliases()`.
#' * `silence(b = TRUE, verbose = FALSE)`, `is_silenced()`, `flush()`.
#' * `reset()`, `reinit()`.
#' * `eng`: the underlying lunapi engine (a reticulate object).
#'
#' @name proj
#' @seealso [lp], [inst], [ProcResult]; lunapi's `proj` documentation
#'   <https://zzz-luna.org/luna/lunapi/ref/>; Luna commands
#'   <https://zzz-luna.org/luna/ref/>
NULL

#' A Luna instance: one recording
#'
#' Returned by `p$inst()` and `p$empty_inst()` (see [proj]). Mirrors
#' lunapi's `inst` class: the same method names, arguments and defaults,
#' called with `$`. Commands have cumulative effects on the in-memory data
#' (masks, filters, new channels) until `refresh()`.
#'
#' @section Data and annotations:
#' * `attach_edf(f)`, `attach_annot(annot)`, `refresh()`, `drop()`, `id()`.
#' * `stat()`, `desc()`, `channels()` / `chs()`, `headers()`, `annots()`.
#' * `fetch_annots(anns, interp = -1)` and `fetch_fulls_annots(anns,
#'   add_keys = FALSE)` (lunapi's spelling; `fetch_full_annots()` also
#'   works): events as data frames, times in seconds.
#' * `has_staging()`, `has_annots(anns)` / `has_annot()`,
#'   `has_channels(ch)` / `has()`.
#'
#' @section Running commands:
#' * `eval(cmdstr)`: run Luna commands (see
#'   <https://zzz-luna.org/luna/ref/>); returns (invisibly) the table of
#'   output (Command, Strata). Character vectors are run in sequence.
#' * `proc(cmdstr)`, `silent_proc(cmdstr)`: the same, returning a
#'   [ProcResult].
#' * `strata()`, `table(cmd, strata = "BL")`, `variables(cmd, strata)`,
#'   `empty_result_set()`. Asking for a table that does not exist is an
#'   error naming the tables that do (lunapi returns an empty table).
#' * `epoch(f = "")`, `epochs()`, `mask(f)`, `segments()`, `stages()`,
#'   `freeze(f)`, `thaw(f, remove = FALSE)`, `empty_freezer()`.
#' * `var(key, value)` / `vars()`, `clear_vars(keys = NULL)`,
#'   `refresh_channel_vars()`: individual-level variables.
#'
#' @section Signals and time:
#' * `data(chs, annots = NULL, time = FALSE)`: signals as a data frame,
#'   one row per sample; with `time = TRUE`, column `T` gives seconds.
#'   Annotation columns are 0/1. Unknown channels are an error.
#' * `e2i(epochs)`, `s2i(secs)`: epochs, or (start, stop) seconds, as
#'   intervals: a two-column matrix of Luna time-points (nanoseconds).
#' * `slice(intervals, chs, annots = NULL, time = FALSE)`: one data frame
#'   for all intervals; `slices(...)`: a list, one per interval.
#'   `intervals` is a two-column matrix (as from `e2i()`) or a list of
#'   pairs.
#' * `insert_signal(label, data, sr)`, `update_signal(label, data)`,
#'   `insert_annot(label, intervals, durcol2 = FALSE)` (intervals in
#'   seconds).
#'
#' @section Plots:
#' `hypno()`, `psd(ch, var = "PSD", minf, maxf = 25, minp, maxp, xlines,
#' ylines)`, `spec(ch, mine, maxe, minf, maxf, w = 0.025)`; see [lp].
#'
#' @section Underlying object:
#' `edf` is the lunapi instance (a reticulate object), for anything not
#' wrapped here, such as `pops()` or `extract_event_waveforms()`.
#'
#' @name inst
#' @seealso [lp], [proj], [ProcResult]; lunapi's `inst` documentation
#'   <https://zzz-luna.org/luna/lunapi/ref/>; Luna commands
#'   <https://zzz-luna.org/luna/ref/>
NULL
