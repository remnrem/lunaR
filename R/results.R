# Output tables: lookup by command and strata, and ProcResult.

# Resolve (cmd, strata) against the available (Command, Strata) pairs, as
# lunapi's coerce_strata() does, but raising informative errors where
# lunapi returns an empty table.  `strata` may be a string ("CH_F") or a
# character vector of factors in any order (c("F", "CH")); a command may be
# written with "-" or "_" (DUMP-MASK / DUMP_MASK).
.resolve_table <- function(pairs, cmd, strata = "BL") {
  if (!is.character(cmd) || length(cmd) != 1L) .luna_abort("expecting a single command name")
  if (is.null(pairs) || nrow(pairs) == 0L) {
    .luna_abort(sprintf("no output tables (looking for %s): run eval() or proc() first", cmd),
                class = "luna_no_table")
  }
  if (!cmd %in% pairs$Command) {
    alt <- gsub("-", "_", cmd, fixed = TRUE)
    if (alt %in% pairs$Command) cmd <- alt
    else .luna_abort(sprintf("no output for command %s; commands with output: %s",
                             cmd, paste(unique(pairs$Command), collapse = ", ")),
                     class = "luna_no_table")
  }
  avail <- pairs$Strata[pairs$Command == cmd]
  if (is.null(strata) || length(strata) == 0L) strata <- "BL"
  strata <- as.character(strata)
  if (length(strata) == 1L && strata %in% avail) return(c(cmd, strata))
  want <- sort(unique(unlist(strsplit(strata, "_", fixed = TRUE))))
  hit <- avail[vapply(strsplit(avail, "_", fixed = TRUE),
                      function(p) identical(sort(unique(p)), want), TRUE)]
  if (length(hit) == 1L) return(c(cmd, hit))
  if (length(hit) > 1L) {
    .luna_abort(sprintf("ambiguous strata %s for %s; matches: %s",
                        paste(strata, collapse = ","), cmd, paste(hit, collapse = ", ")))
  }
  .luna_abort(sprintf("no %s table stratified by %s; available for %s: %s",
                      cmd, paste(strata, collapse = "_"), cmd, paste(avail, collapse = ", ")),
              class = "luna_no_table")
}

# a table from an engine or instance result store
.store_table <- function(store, cmd, strata) {
  k <- .resolve_table(.strata_df(store), cmd, strata)
  .lcall(.frame_to_df(.br()$table(store, k[1], k[2])))
}

.store_variables <- function(store, cmd, strata) {
  k <- .resolve_table(.strata_df(store), cmd, strata)
  as.character(.py(store$vars(k[1], k[2])))
}

.table_key <- function(cmd, strata) paste0(cmd, ": ", strata)

#' Results of `proc()`
#'
#' Returned by `proj$proc()`, `proj$procn()`, `proj$proc_parallel()` and
#' `inst$proc()`, mirroring lunapi's
#' `ProcResult`. Unlike lunapi's, it holds its own copy of the tables, so
#' later commands do not change it. Tables are converted to data frames
#' when first accessed.
#'
#' @section Usage:
#' ```r
#' r <- i$proc("PSD sig=C3 spectrum")
#' r$keys()                   # "PSD: CH", "PSD: CH_F", ...
#' r[["PSD: CH_F"]]           # a data frame, as r['PSD: CH_F'] in Python
#' r$table("PSD", "CH_F")     # the same; strata may also be c("F", "CH")
#' r$strata(); r$commands(); r$has_table("PSD", "CH")
#' r$get("PSD: B_CH", default = NULL)
#' r$tables                   # named list of all tables
#' r$ok                       # no per-record errors
#' r$errors                   # per-record errors from parallel runs
#' r$records; r$stdout; r$workers; r$out_paths
#' ```
#'
#' @details
#' Parallel runs collect per-record errors by default. With `strict = TRUE`,
#' failures raise a `luna_proc_error` condition whose `result` field contains
#' the partial [ProcResult]. Serial failures always raise R errors.
#' With `out_db` or `out_text`, tables are written to files: metadata and
#' `out_paths` remain available, but table access raises `luna_file_output`.
#' @name ProcResult
NULL

ProcResult <- R6::R6Class("ProcResult",
  cloneable = FALSE,
  public = list(
    # @field errors per-record errors from parallel runs
    errors = NULL,
    stdout = NULL,
    records = NULL,
    out_paths = character(0),
    # @field workers number of workers used
    workers = 1L,

    # @description Wraps a lunapi results dict (internal).
    # @param ts a Python dict `{cmd: {strata: (cols, data)}}`
    initialize = function(ts, metadata = NULL) {
      private$ts <- ts
      s <- .py(.br()$dict_strata(ts))
      private$pairs <- data.frame(
        Command = vapply(s, `[[`, "", 1L), Strata = vapply(s, `[[`, "", 2L),
        stringsAsFactors = FALSE)
      private$cache <- list()
      self$errors <- data.frame(ID = character(0), error = character(0))
      self$stdout <- self$records <- data.frame()
      if (!is.null(metadata)) {
        self$errors <- .frame_to_df(metadata[["errors"]])
        self$stdout <- .frame_to_df(metadata[["stdout"]])
        self$records <- .frame_to_df(metadata[["records"]])
        self$workers <- as.integer(.py(metadata[["workers"]]))
        self$out_paths <- as.character(unlist(.py(metadata[["out_paths"]])))
        private$file_output <- isTRUE(.py(metadata[["file_output"]]))
      }
    },
    # @description Table names, as `"CMD: STRATA"`.
    keys = function() {
      private$check_tables()
      .table_key(private$pairs$Command, private$pairs$Strata)
    },
    # @description One table.
    # @param cmd command name
    # @param strata strata, e.g. `"CH_F"` or `c("CH", "F")`
    table = function(cmd, strata = "BL") {
      private$check_tables()
      k <- .resolve_table(private$pairs, cmd, strata)
      key <- .table_key(k[1], k[2])
      if (is.null(private$cache[[key]])) {
        private$cache[[key]] <- .lcall(.frame_to_df(.br()$dict_table(private$ts, k[1], k[2])))
      }
      private$cache[[key]]
    },
    # @description Available (Command, Strata) pairs.
    strata = function() {
      private$check_tables()
      if (nrow(private$pairs)) private$pairs else NULL
    },
    # @description Commands with output.
    commands = function() {
      private$check_tables()
      data.frame(Command = sort(unique(private$pairs$Command)))
    },
    # @description Is a table present?
    # @param cmd,strata as for `table()`
    has_table = function(cmd, strata = "BL") {
      if (private$file_output) return(FALSE)
      !inherits(try(.resolve_table(private$pairs, cmd, strata), silent = TRUE), "try-error")
    },
    # @description A table by key, or `default` if absent.
    # @param key `"CMD: STRATA"`
    # @param default returned if absent
    get = function(key, default = NULL) {
      if (private$file_output) return(default)
      if (!key %in% self$keys()) return(default)
      self[[key]]
    },
    # @description All tables as a named list.
    items = function() self$tables,
    # @description All tables as an unnamed list.
    values = function() unname(self$tables),
    # @description Print a summary.
    # @param ... ignored
    print = function(...) {
      if (private$file_output) {
        cat("ProcResult: output written to files\n")
        if (length(self$out_paths)) cat(paste0("  ", self$out_paths, collapse = "\n"), "\n")
        return(invisible(self))
      }
      cat(sprintf("ProcResult: %d table(s)\n", nrow(private$pairs)))
      if (nrow(private$pairs)) cat(paste0("  ", self$keys(), collapse = "\n"), "\n")
      invisible(self)
    }
  ),
  active = list(
    # @field ok whether there were no per-record errors
    ok = function() nrow(self$errors) == 0L,
    # @field tables all tables, as a named list keyed `"CMD: STRATA"`
    tables = function() {
      private$check_tables()
      stats::setNames(lapply(seq_len(nrow(private$pairs)), function(i)
        self$table(private$pairs$Command[i], private$pairs$Strata[i])), self$keys())
    }
  ),
  private = list(
    ts = NULL, pairs = NULL, cache = NULL, file_output = FALSE,
    check_tables = function() {
      if (private$file_output) {
        .luna_abort("results were written to files; use import_db() or lp$read_text_table()",
                    class = "luna_file_output")
      }
    }
  )
)

#' @export
`[[.ProcResult` <- function(x, i, ...) {
  if (length(i) == 2L) return(x$table(i[1], i[2]))
  if (!is.character(i) || length(i) != 1L) .luna_abort("index a ProcResult by \"CMD: STRATA\"")
  parts <- strsplit(i, ":[[:space:]]*")[[1]]
  if (length(parts) != 2L) {
    .luna_abort(sprintf("%s not found; available tables: %s", i, paste(x$keys(), collapse = ", ")),
                class = "luna_no_table")
  }
  x$table(parts[1], parts[2])
}

#' @export
names.ProcResult <- function(x) x$keys()

#' @export
length.ProcResult <- function(x) length(x$keys())
