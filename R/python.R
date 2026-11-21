# Python / lunapi plumbing: lazy initialization, platform checks, the
# error wrapper, and user-facing configuration helpers.

# lunapi releases and Python versions this package supports.  The upper
# bounds keep reticulate from choosing a Python for which lunapi has no
# wheel (lunapi ships no source distribution).
.lunapi_requirement <- "lunapi>=1.7.1,<2"
.lunapi_min_version <- "1.7.1"
.python_requirement <- ">=3.9,<3.15"

.auto_install <- function() {
  env <- tolower(Sys.getenv("LUNA_AUTO_INSTALL", ""))
  if (env %in% c("false", "0", "no", "off")) return(FALSE)
  isTRUE(getOption("luna.auto_install", TRUE))
}

# Platforms with lunapi wheels on PyPI.  Returns NULL if supported, else a
# short description of the platform.
.unsupported_platform <- function() {
  sys <- Sys.info()[["sysname"]]
  arch <- tolower(R.version$arch)
  arch <- if (arch %in% c("x86_64", "amd64", "x64")) "x86_64"
          else if (arch %in% c("aarch64", "arm64")) "arm64" else arch
  if (sys == "Darwin" && arch %in% c("x86_64", "arm64")) return(NULL)
  if (sys == "Windows" && arch == "x86_64") return(NULL)
  if (sys == "Linux" && arch == "x86_64") {
    glibc <- tryCatch(
      suppressWarnings(system2("getconf", "GNU_LIBC_VERSION", stdout = TRUE, stderr = FALSE)),
      error = function(e) character(0))
    v <- if (length(glibc)) sub("^glibc\\s+", "", glibc[1]) else NA_character_
    if (!is.na(v) && utils::compareVersion(v, "2.28") >= 0) return(NULL)
    return(if (!is.na(v)) paste0("linux-x86_64 with glibc ", v)
           else "linux-x86_64 without glibc (e.g. musl/Alpine)")
  }
  paste(tolower(sys), arch, sep = "-")
}

.supported_platforms <- paste(
  "macOS (x86_64, arm64), Linux x86_64 with glibc >= 2.28, Windows x86_64;",
  "Python", .python_requirement)

# Initialize Python, lunapi and the bridge on first use.  Idempotent.
.luna_init <- function() {
  if (!is.null(.luna$eng)) return(invisible(TRUE))

  was_running <- reticulate::py_available(initialize = FALSE)
  user_python <- nzchar(Sys.getenv("RETICULATE_PYTHON"))

  # Fail early, before reticulate tries to download anything, if lunapi
  # cannot be installed on this platform.
  if (!was_running && !user_python && isTRUE(.luna$required)) {
    bad <- .unsupported_platform()
    if (!is.null(bad)) {
      .luna_abort(c(
        sprintf("lunapi has no prebuilt package for this platform (%s).", bad),
        paste("Supported:", .supported_platforms),
        "If you have built lunapi yourself, set RETICULATE_PYTHON to that Python."
      ), class = "luna_unsupported_platform")
    }
  }

  lp <- tryCatch(reticulate::import("lunapi", convert = FALSE),
                 error = function(e) e)
  if (inherits(lp, "error")) {
    msg <- .clean_py_message(conditionMessage(lp))
    hint <- if (was_running) {
      c(sprintf("Python (%s) was already running without lunapi, probably started by another package.",
                reticulate::py_config()$python),
        "Restart R and use luna before other Python-based packages, or run install_lunapi() to add lunapi to that Python.")
    } else if (!.auto_install()) {
      "Automatic installation is off (luna.auto_install / LUNA_AUTO_INSTALL): run install_lunapi(), or point RETICULATE_PYTHON at a Python with lunapi."
    } else {
      "Run luna_config() for details, or install_lunapi() if you manage your own Python."
    }
    .luna_abort(c("could not load the 'lunapi' Python package.",
                  paste0("Python error: ", msg), hint), class = "luna_no_lunapi")
  }

  ver <- as.character(reticulate::py_to_r(lp$`__version__`))
  if (utils::compareVersion(ver, .lunapi_min_version) < 0) {
    .luna_abort(sprintf(
      "lunapi %s is too old: luna needs %s. Upgrade with install_lunapi().",
      ver, .lunapi_requirement))
  }

  .luna$bridge <- reticulate::import_from_path(
    "lunar_bridge", path = system.file("python", package = "luna"),
    convert = FALSE)
  .luna$lp <- lp
  .luna$eng <- lp$proj$eng
  .luna$version <- ver
  invisible(TRUE)
}

.clean_py_message <- function(msg) {
  msg <- sub("\nRun `reticulate::py_last_error\\(\\)` for details\\.?\\s*$", "", msg)
  msg <- sub("^\\s*[A-Za-z_.]*(Error|Exception):\\s*", "", msg)
  trimws(msg)
}

.luna_abort <- function(msg, call = NULL, parent = NULL, class = NULL) {
  msg <- paste(msg, collapse = "\n  ")
  cond <- structure(
    class = c(class, "luna_error", "error", "condition"),
    list(message = msg, call = call, parent = parent))
  stop(cond)
}

# Every call into lunapi goes through here: Python exceptions become
# 'luna_error' conditions with clean messages, and Luna's problem flag is
# reset so that the next command runs normally.
.lcall <- function(expr, context = NULL) {
  .luna_init()
  tryCatch(expr, error = function(e) {
    if (inherits(e, "luna_error")) stop(e)
    if (!is.null(.luna$eng)) try(.luna$eng$reset(), silent = TRUE)
    msg <- .clean_py_message(conditionMessage(e))
    if (!is.null(context)) msg <- paste0(context, ": ", msg)
    .luna_abort(msg, parent = e)
  })
}

.py <- function(x) reticulate::py_to_r(x)
.br <- function() .luna$bridge

#' Is lunapi available?
#'
#' By default this never starts Python or installs anything, so it is safe
#' in examples, tests and package code.
#'
#' @param initialize if `FALSE` (default), only report what is known
#'   without starting Python: `TRUE` once luna has loaded lunapi, or if
#'   Python is already running and has lunapi installed; `FALSE` if Python
#'   has not started yet (because that cannot be checked without starting
#'   it). If `TRUE`, start Python and load lunapi if needed, which may
#'   install them on first use.
#' @return `TRUE` or `FALSE`
#' @export
#' @examples
#' lunapi_available()
lunapi_available <- function(initialize = FALSE) {
  if (!is.null(.luna$eng)) return(TRUE)
  if (isTRUE(initialize)) {
    return(isTRUE(tryCatch({ .luna_init(); TRUE }, error = function(e) FALSE)))
  }
  if (!reticulate::py_available(initialize = FALSE)) return(FALSE)
  isTRUE(tryCatch(reticulate::py_module_available("lunapi"), error = function(e) FALSE))
}

#' Report the Python / lunapi configuration
#'
#' Starts Python if needed and reports which interpreter is in use, the
#' lunapi and Luna versions, and how the environment was chosen. Include
#' this output in bug reports.
#'
#' @return invisibly, a list with elements `available`, `error`, `python`,
#'   `lunapi`, `luna` and `auto_install`
#' @export
#' @examplesIf interactive()
#' luna_config()
luna_config <- function() {
  err <- tryCatch({ .luna_init(); NULL }, error = function(e) conditionMessage(e))
  cfg <- list(available = is.null(err), error = err,
              python = NA_character_, lunapi = NA_character_,
              luna = NA_character_, auto_install = .auto_install())
  if (reticulate::py_available(initialize = FALSE)) {
    pc <- reticulate::py_config()
    cfg$python <- pc$python
    cfg$python_version <- as.character(pc$version)
    cfg$forced <- if (is.null(pc$forced)) NA_character_ else pc$forced
  }
  if (is.null(err)) {
    cfg$lunapi <- .luna$version
    cfg$luna <- as.character(.py(.luna$lp$lunapi0$version()))
  }
  bad <- .unsupported_platform()
  cat("package :", as.character(utils::packageVersion("luna")), "\n")
  cat("platform:", if (is.null(bad)) "supported" else paste(bad, "(no lunapi wheels)"), "\n")
  cat("auto    :", if (cfg$auto_install) paste("on, requires", .lunapi_requirement)
                   else "off (luna.auto_install / LUNA_AUTO_INSTALL)", "\n")
  cat("python  :", cfg$python, "\n")
  if (!is.null(cfg$forced) && !is.na(cfg$forced)) cat("  chosen by:", cfg$forced, "\n")
  cat("lunapi  :", cfg$lunapi, "\n")
  cat("luna    :", cfg$luna, "\n")
  if (!cfg$available) cat("status  : NOT AVAILABLE\n  ", err, "\n") else cat("status  : ok\n")
  invisible(cfg)
}

#' Install lunapi into a user-managed Python environment
#'
#' Not needed for most users: by default luna asks reticulate to provide a
#' Python with lunapi automatically. Use this if you manage Python yourself
#' (`RETICULATE_PYTHON`, a virtualenv or conda environment, or
#' `options(luna.auto_install = FALSE)`).
#'
#' @param envname name of, or path to, the Python environment to install
#'   into; passed to [reticulate::py_install()]
#' @param ... further arguments to [reticulate::py_install()]
#' @return invisibly, the result of [reticulate::py_install()]
#' @export
install_lunapi <- function(envname = NULL, ...) {
  invisible(reticulate::py_install(.lunapi_requirement, envname = envname,
                                   pip = TRUE, ...))
}
