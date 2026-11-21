# Conversion of bridge (lunar_bridge.py) output into R objects, and of R
# arguments into what lunapi expects.

.col_to_r <- function(col) {
  type <- .py(col[["type"]])
  v <- .py(col[["v"]])
  na <- as.logical(.py(col[["na"]]))
  v <- switch(type,
    i = as.integer(v),
    d = as.numeric(v),
    s = as.character(v),
    l = rep(NA, length(na)))
  if (any(na)) v[na] <- NA
  as.vector(v)
}

# {'cols': [...], 'data': [column dicts]} -> data.frame
.frame_to_df <- function(fr) {
  if (is.null(fr) || inherits(fr, "python.builtin.NoneType")) return(NULL)
  cols <- as.character(.py(fr[["cols"]]))
  b <- reticulate::import_builtins(convert = FALSE)
  data <- .py(b$list(fr[["data"]]))
  out <- lapply(seq_along(cols), function(i) .col_to_r(data[[i]]))
  names(out) <- cols
  if (length(out) == 0L) return(data.frame())
  data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
}

# pandas DataFrame -> data.frame (typed columns)
.pandas_to_df <- function(df) .frame_to_df(.br()$frame(df))

# (cmd, strata) pairs in a result store -> data.frame(Command, Strata)
.strata_df <- function(store) {
  s <- .py(.br()$strata(store))
  if (length(s) == 0L) return(NULL)
  data.frame(Command = vapply(s, `[[`, "", 1L), Strata = vapply(s, `[[`, "", 2L),
             stringsAsFactors = FALSE)
}

# bridge {'cols', 'data': ndarray} -> data.frame
.signal_to_df <- function(x) {
  cols <- as.character(.py(x[["cols"]]))
  .mat_to_df(.py(x[["data"]]), cols)
}

.mat_to_df <- function(m, cols) {
  if (!is.matrix(m)) m <- matrix(m, ncol = length(cols))
  colnames(m) <- cols
  as.data.frame(m, stringsAsFactors = FALSE, optional = TRUE)
}

# character vector -> Python list[str] (always a list, even of length 0/1)
.strs <- function(x) {
  x <- if (is.null(x)) character(0) else as.character(x)
  reticulate::r_to_py(as.list(x))
}

.str_set <- function(x) .br()$str_set(.strs(x))

# intervals as an (n, 2) numeric matrix: accepts a 2-column matrix or data
# frame, a list of length-2 vectors, or a single length-2 vector
.as_intervals <- function(x, what = "intervals") {
  if (is.data.frame(x)) x <- as.matrix(x[, 1:2, drop = FALSE])
  if (is.list(x)) {
    if (!all(lengths(x) == 2L)) .luna_abort(paste(what, "must be pairs of (start, stop) values"))
    x <- do.call(rbind, lapply(x, as.numeric))
  }
  if (is.null(dim(x))) {
    if (length(x) != 2L) .luna_abort(paste(what, "must be pairs of (start, stop) values"))
    x <- matrix(as.numeric(x), ncol = 2L)
  }
  if (ncol(x) != 2L) .luna_abort(paste(what, "must have two columns (start, stop)"))
  storage.mode(x) <- "double"
  if (anyNA(x)) .luna_abort(paste(what, "contain missing values"))
  if (any(x[, 2] < x[, 1])) .luna_abort(paste(what, "have a stop before a start"))
  x
}

.np_matrix <- function(x) reticulate::np_array(x, dtype = "float64")

.tp_matrix_to_r <- function(m) {
  m <- .py(m)
  if (!is.matrix(m)) m <- matrix(m, ncol = 2L)
  colnames(m) <- c("start", "stop")
  m
}

.coerce_var_value <- function(value) {
  if (is.logical(value)) return(if (isTRUE(value)) "1" else "0")
  paste(as.character(value), collapse = ",")
}

# Python dict[str, str|None] -> named list (None -> NULL dropped to NA)
.py_dict <- function(x) {
  x <- .py(x)
  if (length(x) == 0L) return(list())
  lapply(x, function(v) if (is.null(v)) NA_character_ else v)
}

# Row-bind data frames with different columns (fills with NA).
.rbind_fill <- function(dfs) {
  dfs <- Filter(Negate(is.null), dfs)
  if (length(dfs) == 0L) return(NULL)
  if (length(dfs) == 1L) return(dfs[[1L]])
  cols <- unique(unlist(lapply(dfs, names)))
  dfs <- lapply(dfs, function(d) {
    for (m in setdiff(cols, names(d))) d[[m]] <- rep(NA, nrow(d))
    d[cols]
  })
  do.call(rbind, c(dfs, list(make.row.names = FALSE, stringsAsFactors = FALSE)))
}
