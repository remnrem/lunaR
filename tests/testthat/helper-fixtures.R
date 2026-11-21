# Shared fixtures: a synthetic 2-minute EDF written to a temp folder, with
# an annotation file and a two-record sample list.  No external data.

skip_if_no_lunapi <- function() {
  skip_on_cran()
  if (!lunapi_available(initialize = TRUE)) skip("lunapi (Python) not available")
}

SR <- 128
DUR <- 120

fixture_signals <- function() {
  t <- seq(0, DUR - 1 / SR, by = 1 / SR)
  set.seed(1)
  list(EEG = 50 * sin(2 * pi * 10 * t) + rnorm(length(t)),
       EMG = rnorm(length(t)))
}

.fixture <- new.env()

# a quiet project handle
quiet_proj <- function() {
  p <- suppressMessages(lp$proj(verbose = FALSE))
  p$silence(TRUE)
  p
}

# Builds (once per session) and returns paths: dir, edf, annot, lst
luna_fixture <- function() {
  if (!is.null(.fixture$paths)) return(.fixture$paths)
  dir <- file.path(tempdir(), "luna-fixture")
  dir.create(dir, showWarnings = FALSE)
  p <- quiet_proj()
  i <- p$empty_inst("subj1", nr = DUR, rs = 1, startdate = "01.01.20", starttime = "22.00.00")
  s <- fixture_signals()
  i$insert_signal("EEG", s$EEG, SR)
  i$insert_signal("EMG", s$EMG, SR)
  i$eval(paste0("WRITE edf-dir=", dir, "/ edf-tag=fx"))
  i$drop()
  edf <- file.path(dir, "subj1-fx.edf")
  annot <- file.path(dir, "subj1.annot")
  writeLines(c("W\t0\t30", "N2\t30\t90", "W\t90\t120",
               "arousal\t40\t45", "arousal\t95\t97"), annot)
  lst <- file.path(dir, "s.lst")
  writeLines(c(paste("subj1", edf, annot, sep = "\t"),
               paste("subj2", edf, annot, sep = "\t")), lst)
  .fixture$paths <- list(dir = dir, edf = edf, annot = annot, lst = lst)
  .fixture$paths
}

# project with the fixture sample list loaded
fixture_proj <- function() {
  f <- luna_fixture()
  p <- quiet_proj()
  suppressMessages(p$sample_list(f$lst))
  p
}

# instance for subject 1 of the fixture
fixture_inst <- function() fixture_proj()$inst(1)
