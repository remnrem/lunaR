# inst: one EDF and its annotations, mirroring lunapi's `inst` class.

.cmd_text <- function(cmdstr) {
  if (!is.character(cmdstr) || length(cmdstr) == 0L) {
    .luna_abort("expecting Luna commands as a character string")
  }
  paste(cmdstr[nzchar(trimws(cmdstr))], collapse = "\n")
}

# run expr with Luna's console output silenced, restoring the previous mode
.silently <- function(expr) {
  eng <- .luna$eng
  old <- .py(eng$is_silenced())
  eng$silence(TRUE)
  on.exit(eng$silence(old), add = TRUE)
  expr
}

.chk_chs <- function(edf, chs, what = "channel") {
  chs <- as.character(chs)
  have <- as.character(.py(edf$channels()))
  miss <- setdiff(chs, have)
  if (length(miss)) {
    .luna_abort(sprintf("unknown %s(s): %s; available: %s", what,
                        paste(miss, collapse = ", "), paste(have, collapse = ", ")),
                class = "luna_bad_channel")
  }
  chs
}

.chk_annots <- function(edf, annots) {
  annots <- if (is.null(annots)) character(0) else as.character(annots)
  have <- as.character(.py(edf$annots()))
  miss <- setdiff(annots, have)
  if (length(miss)) {
    .luna_abort(sprintf("unknown annotation(s): %s; available: %s",
                        paste(miss, collapse = ", "), paste(have, collapse = ", ")),
                class = "luna_bad_annot")
  }
  annots
}

.desc_cols <- c("ID", "Gapped", "Date", "Start(hms)", "Stop(hms)", "Dur(hms)",
                "Dur(s)", "# sigs", "# annots", "Signals")

.desc_df <- function(rows) {
  if (length(rows) == 0L) return(data.frame())
  m <- do.call(rbind, lapply(rows, as.character))
  d <- as.data.frame(m, stringsAsFactors = FALSE)
  names(d) <- .desc_cols
  d
}

Inst <- R6::R6Class("inst",
  cloneable = FALSE,
  public = list(
    edf = NULL,

    initialize = function(edf) self$edf <- edf,

    print = function(...) {
      cat("<inst>", .py(reticulate::py_str(self$edf)), "\n")
      invisible(self)
    },

    # --- attaching -------------------------------------------------------
    id = function() .lcall(as.character(.py(self$edf$get_id()))),
    attach_edf = function(f) {
      f <- path.expand(f)
      if (!file.exists(f)) .luna_abort(paste("cannot find EDF", f))
      invisible(.lcall(.py(self$edf$attach_edf(f)), context = f))
    },
    attach_annot = function(annot) {
      annot <- path.expand(annot)
      if (!file.exists(annot) && !dir.exists(annot)) .luna_abort(paste("cannot find annotation file", annot))
      invisible(.lcall(.py(self$edf$attach_annot(annot)), context = annot))
    },
    refresh = function() {
      .lcall({ self$edf$refresh(); .luna$eng$reset() })
      invisible(NULL)
    },
    refresh_channel_vars = function() invisible(.lcall(self$edf$refresh_channel_vars())),
    drop = function() invisible(.lcall(self$edf$drop())),

    # --- description -----------------------------------------------------
    stat = function() {
      s <- .lcall(.py(self$edf$stat()))
      data.frame(Value = vapply(s, function(v) paste(as.character(v), collapse = ","), ""),
                 row.names = names(s), stringsAsFactors = FALSE)
    },
    desc = function() {
      d <- .desc_df(list(.lcall(.py(self$edf$desc()))))
      print(d, right = FALSE)
      invisible(d)
    },
    channels = function() {
      x <- .lcall(as.character(.py(self$edf$channels())))
      if (!length(x)) return(data.frame())
      data.frame(Channels = x, stringsAsFactors = FALSE)
    },
    chs = function() self$channels(),
    headers = function() {
      r <- .silently(self$proc("HEADERS"))
      r$get("HEADERS: CH")
    },
    annots = function() {
      x <- .lcall(as.character(.py(self$edf$annots())))
      if (!length(x)) return(data.frame())
      data.frame(Annotations = x, stringsAsFactors = FALSE)
    },
    fetch_annots = function(anns, interp = -1) {
      .lcall(.frame_to_df(.br()$fetch_annots(self$edf, .strs(anns), interp)))
    },
    fetch_fulls_annots = function(anns, add_keys = FALSE) {
      .lcall(.frame_to_df(.br()$fetch_full_annots(self$edf, .strs(anns), isTRUE(add_keys))))
    },
    fetch_full_annots = function(anns, add_keys = FALSE) self$fetch_fulls_annots(anns, add_keys),
    has_staging = function() .lcall(.silently(isTRUE(.py(self$edf$has_staging())))),
    has_annots = function(anns) {
      if (is.null(anns)) return(NULL)
      .lcall(as.logical(.py(self$edf$has_annots(.strs(anns)))))
    },
    has_annot = function(anns) self$has_annots(anns),
    has_channels = function(ch) {
      if (is.null(ch)) return(NULL)
      .lcall(as.logical(.py(self$edf$has_channels(.strs(ch)))))
    },
    has = function(ch) self$has_channels(ch),

    # --- variables -------------------------------------------------------
    clear_vars = function(keys = NULL) {
      if (is.null(keys)) .lcall(self$edf$clear_ivar())
      else .lcall(self$edf$clear_selected_ivar(.str_set(keys)))
      invisible(NULL)
    },
    var = function(key = NULL, value = NULL) self$vars(key, value),
    vars = function(key = NULL, value = NULL) {
      if (is.null(key)) return(.py_dict(self$edf$ivars()))
      if (is.list(key)) {
        for (k in names(key)) self$vars(k, key[[k]])
        return(invisible(NULL))
      }
      if (is.null(value)) {
        v <- .py(self$edf$get_ivar(as.character(key)))
        return(if (is.null(v)) NULL else v)
      }
      .lcall(self$edf$ivar(as.character(key), .coerce_var_value(value)))
      invisible(NULL)
    },

    # --- running commands ------------------------------------------------
    eval = function(cmdstr) {
      .lcall(self$edf$eval(.cmd_text(cmdstr)))
      invisible(self$strata())
    },
    eval_dummy = function(cmdstr) .lcall(as.character(.py(self$edf$eval_dummy(.cmd_text(cmdstr))))),
    proc = function(cmdstr) {
      res <- .lcall(self$edf$proc(.cmd_text(cmdstr)))
      ProcResult$new(reticulate::py_get_item(res, 1L))
    },
    silent_proc = function(cmdstr) .silently(self$proc(cmdstr)),
    empty_result_set = function() .lcall(length(.py(self$edf$strata())) == 0L),
    strata = function() .lcall(.strata_df(self$edf)),
    table = function(cmd, strata = "BL") .store_table(self$edf, cmd, strata),
    variables = function(cmd, strata = "BL") .store_variables(self$edf, cmd, strata),

    # --- epochs, masks, freezes ------------------------------------------
    epoch = function(f = "") invisible(self$eval(paste("EPOCH", f))),
    epochs = function() {
      self$eval("EPOCH table")
      d <- self$table("EPOCH", "E")
      d[, intersect(c("E", "E1", "LABEL", "HMS", "START", "STOP", "DUR"), names(d)), drop = FALSE]
    },
    mask = function(f = NULL) {
      if (is.null(f)) return(invisible(NULL))
      for (x in as.character(f)) self$eval(paste("MASK", x))
      self$eval("RE")
      invisible(NULL)
    },
    segments = function() {
      self$eval("SEGMENTS")
      self$table("SEGMENTS", "SEG")
    },
    freeze = function(f) invisible(self$eval(paste("FREEZE", f))),
    thaw = function(f, remove = FALSE) {
      invisible(self$eval(if (isTRUE(remove)) paste0("THAW tag=", f, " remove") else paste("THAW", f)))
    },
    empty_freezer = function() invisible(self$eval("CLEAN-FREEZER")),
    stages = function() {
      r <- self$silent_proc("STAGE")
      r$get("STAGE: E")
    },

    # --- time and intervals ----------------------------------------------
    e2i = function(epochs) {
      .lcall(.tp_matrix_to_r(.br()$e2i(self$edf, reticulate::r_to_py(as.list(as.integer(epochs))))))
    },
    s2i = function(secs) {
      .lcall(.tp_matrix_to_r(.br()$s2i(self$edf, .np_matrix(.as_intervals(secs)))))
    },

    # --- signals ---------------------------------------------------------
    data = function(chs, annots = NULL, time = FALSE) {
      chs <- .chk_chs(self$edf, chs)
      annots <- .chk_annots(self$edf, annots)
      .lcall(.signal_to_df(.br()$data(self$edf, .strs(chs), .strs(annots), isTRUE(time))))
    },
    slice = function(intervals, chs, annots = NULL, time = FALSE) {
      chs <- .chk_chs(self$edf, chs)
      annots <- .chk_annots(self$edf, annots)
      m <- .np_matrix(.as_intervals(intervals))
      .lcall(.signal_to_df(.br()$slice(self$edf, m, .strs(chs), .strs(annots), isTRUE(time))))
    },
    slices = function(intervals, chs, annots = NULL, time = FALSE) {
      chs <- .chk_chs(self$edf, chs)
      annots <- .chk_annots(self$edf, annots)
      m <- .np_matrix(.as_intervals(intervals))
      .lcall({
        r <- .br()$slices(self$edf, m, .strs(chs), .strs(annots), isTRUE(time))
        cols <- as.character(.py(r[["cols"]]))
        lapply(.py(r[["data"]]), .mat_to_df, cols = cols)
      })
    },
    insert_signal = function(label, data, sr) {
      data <- as.numeric(data)
      if (anyNA(data)) .luna_abort("signal contains missing values")
      if (label %in% as.character(.py(self$edf$channels()))) {
        .luna_abort(sprintf("channel %s already exists; use update_signal()", label))
      }
      invisible(.lcall(.br()$insert_signal(self$edf, label, reticulate::np_array(data), sr),
                       context = paste("inserting", label)))
    },
    update_signal = function(label, data) {
      .chk_chs(self$edf, label)
      data <- as.numeric(data)
      if (anyNA(data)) .luna_abort("signal contains missing values")
      invisible(.lcall(.br()$update_signal(self$edf, label, reticulate::np_array(data)),
                       context = paste("updating", label)))
    },
    insert_annot = function(label, intervals, durcol2 = FALSE) {
      m <- .as_intervals(intervals)
      invisible(.lcall(.br()$insert_annot(self$edf, label, .np_matrix(m), isTRUE(durcol2))))
    },

    # --- plots -----------------------------------------------------------
    hypno = function() {
      if (!self$has_staging()) .luna_abort("no staging annotations attached")
      lp$hypno(self$stages()$STAGE)
    },
    psd = function(ch, var = "PSD", minf = NULL, maxf = 25, minp = NULL, maxp = NULL,
                   xlines = NULL, ylines = NULL) {
      if (is.null(ch)) return(invisible(NULL))
      ch <- .chk_chs(self$edf, ch)
      cmd <- if (var == "PSD") "PSD spectrum dB" else "MTM tw=15 dB"
      r <- self$silent_proc(sprintf("%s max=%s sig=%s", cmd, maxf, paste(ch, collapse = ",")))
      df <- r$table(if (var == "PSD") "PSD" else "MTM", "CH_F")
      lp$psd(df = df, ch = ch, var = var, minf = minf, maxf = maxf, minp = minp,
             maxp = maxp, xlines = xlines, ylines = ylines)
    },
    spec = function(ch, mine = NULL, maxe = NULL, minf = NULL, maxf = NULL, w = 0.025) {
      if (!is.character(ch) || length(ch) != 1L) return(invisible(NULL))
      ch <- .chk_chs(self$edf, ch)
      if (is.null(minf)) minf <- 0.5
      if (is.null(maxf)) maxf <- 25
      r <- self$silent_proc(sprintf("PSD epoch-spectrum dB sig=%s min=%s max=%s", ch, minf, maxf))
      lp$spec(r$table("PSD", "CH_E_F"), ch = ch, var = "PSD", mine = mine, maxe = maxe,
              minf = minf, maxf = maxf, w = w)
    }
  )
)
