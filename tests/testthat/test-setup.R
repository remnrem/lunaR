# Pure-R behaviour: no Python needed.

test_that("lp is the only exported object besides setup helpers", {
  ex <- getNamespaceExports("luna")
  expect_setequal(ex, c("lp", "lunapi_available", "luna_config", "install_lunapi"))
  expect_true(is.environment(lp))
  expect_true(bindingIsLocked("proj", lp))
  expect_output(print(lp), "lunapi interface")
})

test_that("auto-install switch reads the option and environment variable", {
  withr::local_envvar(LUNA_AUTO_INSTALL = "")
  withr::local_options(luna.auto_install = NULL)
  expect_true(luna:::.auto_install())
  withr::with_options(list(luna.auto_install = FALSE), expect_false(luna:::.auto_install()))
  for (v in c("false", "0", "FALSE", "off")) {
    withr::with_envvar(c(LUNA_AUTO_INSTALL = v), expect_false(luna:::.auto_install()))
  }
})

test_that("platform check accepts this machine's platform when wheels exist", {
  bad <- luna:::.unsupported_platform()
  if (Sys.info()[["sysname"]] %in% c("Darwin", "Windows")) expect_null(bad)
  expect_true(is.null(bad) || is.character(bad))
})

test_that("table lookup resolves strata and explains failures", {
  pairs <- data.frame(Command = c("PSD", "PSD", "PSD", "DUMP_MASK"),
                      Strata = c("CH", "CH_F", "B_CH", "E"))
  r <- luna:::.resolve_table
  expect_equal(r(pairs, "PSD", "CH_F"), c("PSD", "CH_F"))
  expect_equal(r(pairs, "PSD", c("F", "CH")), c("PSD", "CH_F"))
  expect_equal(r(pairs, "PSD", "F_CH"), c("PSD", "CH_F"))
  expect_equal(r(pairs, "DUMP-MASK", "E"), c("DUMP_MASK", "E"))
  expect_error(r(pairs, "PSD", "CH_E"), "available for PSD: CH, CH_F, B_CH", class = "luna_no_table")
  expect_error(r(pairs, "PSD"), "stratified by BL", class = "luna_no_table")
  expect_error(r(pairs, "SPINDLES", "CH"), "commands with output: PSD, DUMP_MASK")
  expect_error(r(NULL, "PSD", "CH"), "run eval\\(\\) or proc\\(\\) first")
})

test_that("intervals are validated", {
  a <- luna:::.as_intervals
  expect_equal(a(list(c(1, 2), c(3, 4))), matrix(c(1, 3, 2, 4), 2))
  expect_equal(a(c(1, 2)), matrix(c(1, 2), 1))
  expect_equal(a(data.frame(x = 1, y = 5)), matrix(c(1, 5), 1, dimnames = list(NULL, c("x", "y"))))
  expect_error(a(list(c(3, 1))), "stop before a start", class = "luna_error")
  expect_error(a(list(1:3)), "pairs", class = "luna_error")
  expect_error(a(matrix(1:6, 2)), "two columns", class = "luna_error")
})

test_that("stage helpers match lunapi's mappings", {
  expect_equal(lp$stgn(c("N1", "N2", "N3", "R", "W", "?", "L", NA)), c(-1, -2, -3, 0, 1, 2, 2, 2))
  expect_equal(lp$stgcol(c("N2", "W", NA, "blue")), c("#0050C8FF", "#31AD52FF", "#00000000", "blue"))
  xy <- lp$default_xy()
  expect_equal(names(xy), c("CH", "X", "Y"))
  expect_equal(nrow(xy), 64)
})

test_that("results helpers work on plain lists of tables", {
  ts <- list("PSD: CH" = data.frame(ID = c("a", "b"), CH = "C3", X = 1:2))
  expect_equal(lp$strata(ts)$Strata, "CH")
  expect_equal(lp$table(ts, "PSD", "CH")$X, 1:2)
  d <- data.frame(ID = c("a", "b", "c"), F = c(1, 5, 20), P = 1:3)
  expect_equal(lp$subset(d, ids = c("a", "c"))$F, c(1, 20))
  expect_equal(lp$subset(d, qry = "F < 10")$ID, c("a", "b"))
  expect_equal(names(lp$subset(d, vars = "P")), c("ID", "P"))
  res <- list(s1 = ts, s2 = ts)
  cc <- lp$concat(res, "PSD: CH", add_index = "SET")
  expect_equal(nrow(cc), 4)
  expect_equal(cc$SET, c("s1", "s1", "s2", "s2"))
  f <- withr::local_tempfile()
  writeLines(c("EPOCH", "PSD sig=C3"), f)
  expect_equal(lp$cmdfile(f), "EPOCH\nPSD sig=C3")
})

test_that("plots draw on a null device", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_null(lp$hypno(c("W", "N1", "N2", "N2", "R", "W")))
  expect_null(lp$hypno_density(data.frame(PP_N1 = 0.2, PP_N2 = 0.2, PP_N3 = 0.2, PP_R = 0.2, PP_W = c(0.2, 0.2))))
  psd <- data.frame(CH = rep(c("C3", "C4"), each = 5), F = rep(1:5, 2), PSD = runif(10))
  expect_null(lp$psd(psd, c("C3", "C4"), xlines = 2, ylines = 0.5))
  sp <- expand.grid(F = 1:4, E = 1:6)
  sp$CH <- "C3"; sp$PSD <- rnorm(nrow(sp))
  expect_null(lp$spec(sp, "C3"))
  expect_null(lp$topo_heat(c("C3", "c4", "Cz"), c(1, -1, 0), ths = c(0.01, 0.5, 0.5)))
  expect_error(lp$topo_heat(c("XX"), 1), "no matching channels")
  expect_error(lp$topo_heat("C3", 5, lmts = c(0, 1)), "out of specified limits")
})
