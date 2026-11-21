# luna

R interface to [Luna](https://zzz-luna.org/luna/), the toolset for sleep
and physiological signal analysis. It mirrors the API of the
[lunapi](https://pypi.org/project/lunapi/) Python package, which it uses
via [reticulate](https://rstudio.github.io/reticulate/).

**Pure R: no compiler, FFTW or system libraries needed.** The Luna engine
ships inside the lunapi wheel, which reticulate installs into an isolated
environment the first time you use luna.

```r
# install.packages("remotes")
remotes::install_github("remnrem/lunaR")
```

## Same API as lunapi

The package exports one object, `lp`, standing in for `import lunapi as lp`.
Translating Python is mechanical: `.` becomes `$`, `True`/`None` become
`TRUE`/`NULL`, and argument names stay the same.

```r
library(luna)

p <- lp$proj()                        # p = lp.proj()
p$sample_list("s.lst")                # p.sample_list('s.lst')
i <- p$inst(1)                        # i = p.inst(1)

i$eval("EPOCH & MASK ifnot=N2 & RE & PSD sig=C3 spectrum")
i$strata()
psd <- i$table("PSD", "CH_F")         # a typed R data frame

d <- i$data(c("C3", "C4"), annots = "arousal", time = TRUE)
i$insert_signal("X", 2 * d$C3, 128)

r <- p$proc("HEADERS & STAGE")        # whole sample list -> ProcResult
r[["HEADERS: CH"]]
r$table("STAGE", "E")

i$hypno(); i$psd("C3"); lp$topo_heat(chs, z)
```

See `?lp`, `?proj`, `?inst` and `?ProcResult` for every method. Luna
commands and their options are documented in the
[command reference](https://zzz-luna.org/luna/ref/), and the methods
themselves in the [lunapi reference](https://zzz-luna.org/luna/lunapi/ref/);
the [lunapi tutorial](https://zzz-luna.org/luna/lunapi/) translates line by
line.

Everything else in lunapi (`moonbeam`, `proc_parallel()`, `pops()`, ...)
is reachable as `lp$py` (the Python module) and `i$edf`.

### Differences from lunapi

* Tables are R data frames with typed columns; signals are data frames;
  intervals from `e2i()`/`s2i()` are two-column matrices of time-points.
* Errors are R conditions of class `luna_error`, and Luna recovers so the
  next command works. Asking for a table that doesn't exist is an error
  naming the tables that do (lunapi returns an empty table). Commands may
  be written with `-` or `_` (`DUMP-MASK`, `DUMP_MASK`).
* A `ProcResult` keeps its own copy of the tables, so later commands don't
  change it.
* `get_n()`, `get_id()`, `get_edf()` and `get_annots()` are 1-based, like
  `inst(n)` (lunapi's are 0-based).
* Plots use base R graphics; `xsize`/`ysize` are ignored.
* Methods that in lunapi turn console output back on as a side effect
  (`refresh()`, `headers()`, `has_staging()`, `silent_proc()`) don't.

## Installing Python and lunapi

By default, loading luna declares the requirement with
`reticulate::py_require()`; nothing is downloaded until Luna is first
used. `luna_config()` shows what is in use. lunapi has wheels for macOS
(x86_64, arm64), Linux x86_64 (glibc >= 2.28) and Windows x86_64; on other
platforms luna stops with a message saying so.

To manage Python yourself, set `options(luna.auto_install = FALSE)` or
`LUNA_AUTO_INSTALL=false` before loading luna (or set `RETICULATE_PYTHON`),
and run `install_lunapi()` to add lunapi to that environment.
