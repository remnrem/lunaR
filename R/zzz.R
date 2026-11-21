#' luna: R interface to the Luna sleep signal toolset
#'
#' An R mirror of the [lunapi](https://pypi.org/project/lunapi/) Python
#' API, implemented in pure R through
#' [reticulate][reticulate::reticulate]. The package exports one object,
#' [lp], which plays the role of `import lunapi as lp`: Python code such as
#' `p = lp.proj(); i = p.inst(1); i.eval("PSD sig=C3"); i.table("PSD", "CH")`
#' becomes `p <- lp$proj(); i <- p$inst(1); i$eval("PSD sig=C3");
#' i$table("PSD", "CH")`.
#'
#' No compiler or system libraries are needed: the Luna engine ships inside
#' the lunapi wheel. Python starts on the first call that needs Luna.
#'
#' @section Setup:
#' By default, loading luna asks reticulate to provide a Python with
#' lunapi (`reticulate::py_require()`); nothing is downloaded until Luna
#' is first used. To manage Python yourself, set
#' `options(luna.auto_install = FALSE)` or the environment variable
#' `LUNA_AUTO_INSTALL=false` *before* loading luna, and see
#' [install_lunapi()]. [luna_config()] reports what is in use.
#'
#' @keywords internal
#' @aliases luna-package
#' @importFrom R6 R6Class
#' @importFrom graphics plot
"_PACKAGE"

# session state
.luna <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  .luna$lp <- NULL        # lunapi module
  .luna$proj <- NULL      # the single R proj object (wraps lunapi's engine)
  .luna$eng <- NULL       # lunapi0 engine
  .luna$bridge <- NULL    # inst/python/lunar_bridge.py
  .luna$version <- NULL
  .luna$required <- FALSE

  # lp$py: the lunapi module itself, for anything not wrapped here
  if (!exists("py", envir = lp, inherits = FALSE)) {
    makeActiveBinding("py", function() { .luna_init(); .luna$lp }, lp)
  }
  lockEnvironment(lp, bindings = TRUE)

  # Declare the Python requirement.  This does not start Python or install
  # anything: reticulate resolves it when Python is first initialized.  It
  # is ignored if the user has chosen an interpreter (RETICULATE_PYTHON,
  # use_python(), ...).
  if (.auto_install()) {
    .luna$required <- !inherits(try(reticulate::py_require(
      .lunapi_requirement, python_version = .python_requirement),
      silent = TRUE), "try-error")
  }
}
