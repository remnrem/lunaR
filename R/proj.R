# proj: the Luna engine session and sample list, mirroring lunapi's `proj`.
# lunapi has one engine per session, so there is one R proj object too.

.is_index <- function(n) is.numeric(n) && length(n) == 1L && !is.na(n) && n == round(n)

Proj <- R6::R6Class("proj",
  cloneable = FALSE,
  public = list(
    eng = NULL,
    n = 0L,

    initialize = function(eng) self$eng <- eng,

    print = function(...) {
      cat(sprintf("<proj> lunapi %s, %d individual(s) in sample list\n",
                  .luna$version, self$nobs()))
      invisible(self)
    },

    # --- sample lists ----------------------------------------------------
    sample_list = function(filename = NULL, path = NULL, df = TRUE) {
      if (is.null(filename)) {
        sl <- .lcall(.py(.br()$sample_list(self$eng)))
        if (!isTRUE(df)) return(sl)
        if (!length(sl)) return(data.frame(ID = character(0), EDF = character(0),
                                           Annotations = character(0)))
        return(data.frame(
          ID = vapply(sl, function(r) as.character(r[[1]]), ""),
          EDF = vapply(sl, function(r) as.character(r[[2]]), ""),
          Annotations = vapply(sl, function(r) paste(sort(unlist(r[[3]])), collapse = ","), ""),
          stringsAsFactors = FALSE))
      }
      filename <- path.expand(filename)
      if (!file.exists(filename)) .luna_abort(paste("cannot find sample list", filename))
      if (!is.null(path)) {
        path <- path.expand(path)
        if (!grepl("/$", path)) path <- paste0(path, "/")
        message("setting path to ", path)
        self$var("path", path)
      }
      .lcall({
        self$eng$clear()
        self$n <- as.integer(.py(self$eng$read_sample_list(filename)))
      }, context = filename)
      message("read ", self$n, " individuals from ", filename)
      invisible(self$n)
    },
    build = function(args) {
      .lcall({
        self$eng$clear()
        self$n <- as.integer(.py(self$eng$build_sample_list(.strs(path.expand(args)))))
      })
      invisible(self$n)
    },
    nobs = function() .lcall(as.integer(.py(self$eng$nobs()))),
    validate = function() {
      v <- .lcall(.py(self$eng$validate_sample_list()))
      data.frame(ID = vapply(v, function(r) as.character(r[[1]]), ""),
                 Filename = vapply(v, function(r) as.character(r[[2]]), ""),
                 Valid = vapply(v, function(r) isTRUE(r[[3]]), TRUE),
                 stringsAsFactors = FALSE)
    },
    clear = function() invisible(.lcall(self$eng$clear())),
    # 1-based, unlike lunapi's get_n()/get_id(), to match inst(n)
    get_n = function(id) {
      n <- .lcall(.py(self$eng$get_n(as.character(id))))
      if (is.null(n)) NA_integer_ else as.integer(n) + 1L
    },
    get_id = function(n) {
      x <- .lcall(.py(self$eng$get_id(as.integer(n) - 1L)))
      if (is.null(x)) NA_character_ else as.character(x)
    },
    get_edf = function(x) {
      if (!.is_index(x)) x <- self$get_n(x)
      .lcall(as.character(.py(self$eng$get_edf(as.integer(x) - 1L))))
    },
    get_annots = function(x) {
      if (!.is_index(x)) x <- self$get_n(x)
      as.character(unlist(.lcall(.py(.br()$annots_of(self$eng, as.integer(x) - 1L)))))
    },

    # --- instances -------------------------------------------------------
    inst = function(n) {
      if (.is_index(n)) {
        nobs <- self$nobs()
        if (n < 1 || n > nobs) {
          .luna_abort(sprintf("index out of bounds: sample list has %d record(s)", nobs))
        }
        return(Inst$new(.lcall(self$eng$inst(as.integer(n) - 1L))))
      }
      if (!is.character(n) || length(n) != 1L) .luna_abort("expecting a 1-based index or an ID")
      k <- .py(self$eng$get_n(n))
      if (!is.null(k)) return(Inst$new(.lcall(self$eng$inst(as.integer(k)))))
      Inst$new(.lcall(self$eng$inst(n)))
    },
    empty_inst = function(id, nr, rs, startdate = "01.01.00", starttime = "00.00.00") {
      if (!.is_index(nr) || nr < 0) .luna_abort("nr (number of records) must be a non-negative integer")
      if (!.is_index(rs) || rs < 0) .luna_abort("rs (record duration in seconds) must be a non-negative integer")
      Inst$new(.lcall(self$eng$empty_inst(as.character(id), as.integer(nr), as.integer(rs),
                                          startdate, starttime)))
    },

    # --- console output --------------------------------------------------
    silence = function(b = TRUE, verbose = FALSE) {
      if (isTRUE(verbose)) message(if (isTRUE(b)) "silencing console outputs" else "enabling console outputs")
      invisible(.lcall(self$eng$silence(isTRUE(b))))
    },
    is_silenced = function() .lcall(isTRUE(.py(self$eng$is_silenced()))),
    flush = function() invisible(.lcall(self$eng$flush())),

    # --- variables -------------------------------------------------------
    include = function(f) {
      f <- path.expand(f)
      if (!file.exists(f)) .luna_abort(paste("cannot find", f))
      invisible(.lcall(.py(self$eng$include(f)), context = f))
    },
    var = function(key = NULL, value = NULL) self$vars(key, value),
    vars = function(key = NULL, value = NULL) {
      if (is.null(key)) return(.py_dict(self$eng$get_all_opts()))
      if (is.list(key)) {
        for (k in names(key)) self$vars(k, key[[k]])
        return(invisible(NULL))
      }
      if (is.null(value)) {
        if (length(key) > 1L) return(.py_dict(self$eng$get_opts(.strs(key))))
        v <- .py(self$eng$get_opt(as.character(key)))
        return(if (is.null(v)) NULL else v)
      }
      .lcall(self$eng$opt(as.character(key), .coerce_var_value(value)))
      invisible(NULL)
    },
    clear_vars = function(key = NULL) {
      if (is.null(key)) {
        .lcall(self$eng$clear_all_opts())
        self$vars("sig", "")
      } else {
        .lcall(self$eng$clear_opts(.strs(key)))
      }
      invisible(NULL)
    },
    clear_ivars = function() invisible(.lcall(self$eng$clear_ivars())),
    aliases = function() {
      a <- .lcall(.py(self$eng$aliases()))
      if (!length(a)) return(data.frame())
      d <- as.data.frame(do.call(rbind, lapply(a, as.character)), stringsAsFactors = FALSE)
      names(d) <- c("Type", "Preferred", "Case-insensitive, sanitized alias")
      d
    },

    # --- running commands ------------------------------------------------
    proc = function(cmdstr) ProcResult$new(.lcall(self$eng$eval(.cmd_text(cmdstr)))),
    silent_proc = function(cmdstr) .silently(self$proc(cmdstr)),
    desc = function() {
      d <- .desc_df(.lcall(.silently(.py(self$eng$desc()))))
      print(d, right = FALSE)
      invisible(d)
    },
    commands = function() data.frame(Command = .lcall(as.character(.py(self$eng$commands())))),
    empty_result_set = function() .lcall(length(.py(self$eng$strata())) == 0L),
    strata = function() .lcall(.strata_df(self$eng)),
    table = function(cmd, strata = "BL") .store_table(self$eng, cmd, strata),
    variables = function(cmd, strata = "BL") .store_variables(self$eng, cmd, strata),
    import_db = function(f, s = NULL) {
      f <- path.expand(f)
      if (!file.exists(f)) .luna_abort(paste("cannot find", f))
      ids <- .lcall(if (is.null(s)) self$eng$import_db(f)
                    else self$eng$import_db_subset(f, .str_set(s)), context = f)
      invisible(as.character(.py(ids)))
    },
    reset = function() invisible(.lcall(self$eng$reset())),
    reinit = function() invisible(.lcall(self$eng$reinit()))
  )
)
