# `lp`: the package's single export, standing in for `import lunapi as lp`.

#' The lunapi interface
#'
#' `lp` is the R counterpart of `import lunapi as lp`. Python `lp.f(x)`
#' becomes `lp$f(x)`; methods of the objects it returns are called with
#' `$` too, so nothing else is added to your search path. If `lp` clashes
#' with a name of your own, use `luna::lp`.
#'
#' Arguments keep lunapi's names and defaults. Python `None`/`True`/lists
#' become `NULL`/`TRUE`/vectors, and results come back as R objects:
#' typed data frames instead of pandas, R errors instead of exceptions.
#'
#' @section Projects and instances:
#' * `lp$proj(verbose = TRUE)`: the Luna engine session; see [proj].
#'   lunapi has one engine per session, so every call returns the same
#'   object (and, as in lunapi, turns console output back on).
#' * `p$inst(n)`, `p$empty_inst(...)`: individual recordings; see [inst].
#' * `proc()` results: see [ProcResult].
#'
#' @section Command and output reference:
#' `lp$fetch_doms()`, `lp$fetch_cmds(dom)`, `lp$fetch_params(cmd)`,
#' `lp$fetch_tbls(cmd)`, `lp$fetch_vars(cmd, tbl)`, and
#' `lp$fetch_desc_dom(dom)`, `lp$fetch_desc_cmd(cmd)`,
#' `lp$fetch_desc_param(cmd, param)`, `lp$fetch_desc_tbl(cmd, tbl)`,
#' `lp$fetch_desc_var(cmd, tbl, var)`.
#'
#' @section Results helpers:
#' * `lp$cmdfile(f)`: a Luna command file as one string.
#' * `lp$strata(ts)`, `lp$table(ts, cmd, strata = "BL")`, `lp$tables(ts)`,
#'   `lp$show(dfs)`: for a [ProcResult] `ts` (or a named list of tables).
#' * `lp$subset(df, ids = NULL, qry = NULL, vars = NULL)`: rows for `ids`,
#'   rows matching `qry` (an R expression as a string, e.g. `"F < 10"`),
#'   and columns `vars` (with `ID`).
#' * `lp$concat(dfs, tlab, vars = NULL, add_index = NULL)`: row-binds
#'   table `tlab` (e.g. `"PSD: CH"`) across a named list of results,
#'   optionally adding the list names as column `add_index`.
#' * `lp$list_text_tables(path, id = NULL)`, `lp$read_text_table(path,
#'   cmd_or_file, factors = NULL)`: text-table output (`luna -t`).
#' * `lp$version()`: lunapi and Luna versions.
#'
#' @section Plots:
#' Drawn with base R graphics on the current device. lunapi's `xsize`/
#' `ysize` figure-size arguments are accepted and ignored.
#' * `lp$hypno(ss, e = NULL, xsize = 20, ysize = 2, title = NULL)`
#' * `lp$hypno_density(probs, e = NULL, xsize = 20, ysize = 2, title = NULL)`
#' * `lp$psd(df, ch, var = "PSD", minf, maxf, minp, maxp, xlines, ylines, dB = FALSE)`
#' * `lp$spec(df, ch = NULL, var = "PSD", mine, maxe, minf, maxf, w = 0.025)`
#' * `lp$topo_heat(chs, z, ths = NULL, th = 0.05, topo = NULL, lmts = NULL,
#'   sz = 70, colormap = "bwr", title = "", rimcolor = "black", lab = "dB")`
#' * `lp$default_xy()`, `lp$stgcol(ss)`, `lp$stgn(ss)`
#'
#' @section Everything else:
#' `lp$py` is the lunapi Python module itself (via reticulate), for parts
#' not wrapped here, such as `moonbeam`, `destrat`, `proc_parallel()` or
#' `pops()`; their results are reticulate objects.
#'
#' @seealso [proj], [inst], [ProcResult]; the lunapi reference
#'   <https://zzz-luna.org/luna/lunapi/ref/> (which these functions mirror)
#'   and the Luna command reference <https://zzz-luna.org/luna/ref/>.
#' @format An environment of functions.
#' @export
#' @examplesIf interactive()
#' p <- lp$proj()
#' i <- p$empty_inst("demo", nr = 120, rs = 1)
#' i$insert_signal("EEG", sin(2 * pi * 10 * seq(0, 120 - 1/128, by = 1/128)), 128)
#' i$eval("PSD sig=EEG max=20 spectrum")
#' i$strata()
#' head(i$table("PSD", "CH_F"))
#' lp$fetch_tbls("PSD")
lp <- structure(new.env(parent = emptyenv()), class = "luna_module")

#' @export
print.luna_module <- function(x, ...) {
  cat("<lunapi interface> use lp$proj() to start; see ?lp\n")
  invisible(x)
}

#' @exportS3Method utils::.DollarNames
.DollarNames.luna_module <- function(x, pattern = "") {
  grep(pattern, ls(x), value = TRUE)
}

lp$proj <- function(verbose = TRUE) {
  .luna_init()
  if (is.null(.luna$proj)) .luna$proj <- Proj$new(.luna$eng)
  if (isTRUE(verbose)) message("initiated lunapi ", .luna$version, ", luna ",
                               trimws(sub("\\(.*", "", .py(.luna$lp$lunapi0$version()))))
  .luna$proj$silence(FALSE)
  .luna$proj
}

lp$version <- function() {
  .luna_init()
  list(lunapi = .luna$version, luna = as.character(.py(.luna$lp$lunapi0$version())))
}

# --- command reference ---------------------------------------------------
.l0 <- function() { .luna_init(); .luna$lp$lunapi0 }
.chr <- function(x) as.character(unlist(.py(x)))
lp$fetch_doms <- function() .lcall(.chr(.l0()$fetch_doms(TRUE)))
lp$fetch_cmds <- function(dom) .lcall(.chr(.l0()$fetch_cmds(dom, TRUE)))
lp$fetch_params <- function(cmd) .lcall(.chr(.l0()$fetch_params(cmd, TRUE)))
lp$fetch_tbls <- function(cmd) .lcall(.chr(.l0()$fetch_tbls(cmd, TRUE)))
lp$fetch_vars <- function(cmd, tbl) .lcall(.chr(.l0()$fetch_vars(cmd, tbl, TRUE)))
lp$fetch_desc_dom <- function(dom) .lcall(.chr(.l0()$fetch_desc_dom(dom)))
lp$fetch_desc_cmd <- function(cmd) .lcall(.chr(.l0()$fetch_desc_cmd(cmd)))
lp$fetch_desc_param <- function(cmd, param) .lcall(.chr(.l0()$fetch_desc_param(cmd, param)))
lp$fetch_desc_tbl <- function(cmd, tbl) .lcall(.chr(.l0()$fetch_desc_tbl(cmd, tbl)))
lp$fetch_desc_var <- function(cmd, tbl, var) .lcall(.chr(.l0()$fetch_desc_var(cmd, tbl, var)))

# --- results helpers -----------------------------------------------------
lp$cmdfile <- function(f) paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

.as_tables <- function(ts) {
  if (inherits(ts, "ProcResult")) return(ts$tables)
  if (is.list(ts) && !is.data.frame(ts)) return(ts)
  .luna_abort("expecting a ProcResult or a named list of tables")
}

lp$strata <- function(ts) {
  if (inherits(ts, "ProcResult")) return(ts$strata())
  k <- strsplit(names(.as_tables(ts)), ":[[:space:]]*")
  data.frame(Command = vapply(k, `[`, "", 1L), Strata = vapply(k, `[`, "", 2L))
}

lp$table <- function(ts, cmd, strata = "BL") {
  if (inherits(ts, "ProcResult")) return(ts$table(cmd, strata))
  .resolve_table(lp$strata(ts), cmd, strata) -> k
  .as_tables(ts)[[.table_key(k[1], k[2])]]
}

lp$tables <- function(ts) .as_tables(ts)

lp$show <- function(dfs) {
  dfs <- .as_tables(dfs)
  for (nm in names(dfs)) {
    cat(nm, "\n", sep = "")
    print(dfs[[nm]])
    cat("\n")
  }
  invisible(dfs)
}

lp$subset <- function(df, ids = NULL, qry = NULL, vars = NULL) {
  if (!is.null(ids)) df <- df[df$ID %in% ids, , drop = FALSE]
  if (is.character(qry)) {
    keep <- eval(parse(text = qry), df, parent.frame())
    df <- df[!is.na(keep) & keep, , drop = FALSE]
  }
  if (!is.null(vars)) df <- df[, unique(c("ID", vars)), drop = FALSE]
  rownames(df) <- NULL
  df
}

lp$concat <- function(dfs, tlab, vars = NULL, add_index = NULL, ignore_index = TRUE) {
  if (is.null(names(dfs))) .luna_abort("expecting a named list of results")
  out <- lapply(names(dfs), function(k) {
    t <- dfs[[k]][[tlab]]
    if (is.null(t)) return(NULL)
    if (!is.null(add_index)) t[[add_index]] <- k
    if (!is.null(vars)) t <- t[, unique(c(vars, add_index)), drop = FALSE]
    t
  })
  .rbind_fill(out)
}

lp$list_text_tables <- function(path, id = NULL) {
  .lcall(.pandas_to_df(.luna$lp$list_text_tables(path.expand(path), id)))
}

lp$read_text_table <- function(path, cmd_or_file, factors = NULL) {
  f <- if (is.null(factors)) NULL else .strs(factors)
  .lcall(.pandas_to_df(.luna$lp$read_text_table(path.expand(path), cmd_or_file, f)))
}
