# Plots under lunapi's names (lunapi/viz.py), drawn with base R graphics.

.stgcols <- c(N1 = "#00BEFAFF", N2 = "#0050C8FF", N3 = "#000050FF",
              NREM4 = "#000032FF", R = "#FA1432FF", W = "#31AD52FF",
              L = "#F6F32AFF", "?" = "#64646464")

lp$stgcol <- function(ss) {
  ss <- as.character(ss)
  out <- unname(.stgcols[ss])
  out[is.na(ss)] <- "#00000000"
  unk <- is.na(out)
  out[unk] <- ss[unk]
  out
}

.stgns <- c(N1 = -1, N2 = -2, N3 = -3, NREM4 = -4, R = 0, W = 1, L = 2, "?" = 2)

lp$stgn <- function(ss) {
  ss <- as.character(ss)
  out <- unname(.stgns[ss])
  out[is.na(ss)] <- 2
  out
}

lp$default_xy <- function() {
  ch <- c("FP1", "AF7", "AF3", "F1", "F3", "F5", "F7", "FT7", "FC5", "FC3", "FC1",
          "C1", "C3", "C5", "T7", "TP7", "CP5", "CP3", "CP1", "P1", "P3", "P5", "P7",
          "P9", "PO7", "PO3", "O1", "IZ", "OZ", "POZ", "PZ", "CPZ", "FPZ", "FP2",
          "AF8", "AF4", "AFZ", "FZ", "F2", "F4", "F6", "F8", "FT8", "FC6", "FC4",
          "FC2", "FCZ", "CZ", "C2", "C4", "C6", "T8", "TP8", "CP6", "CP4", "CP2",
          "P2", "P4", "P6", "P8", "P10", "PO8", "PO4", "O2")
  x <- c(-0.139058, -0.264503, -0.152969, -0.091616, -0.184692, -0.276864,
         -0.364058, -0.427975, -0.328783, -0.215938, -0.110678, -0.1125, -0.225,
         -0.3375, -0.45, -0.427975, -0.328783, -0.215938, -0.110678, -0.091616,
         -0.184692, -0.276864, -0.364058, -0.4309, -0.264503, -0.152969, -0.139058,
         0, 0, 0, 0, 0, 0, 0.139058, 0.264503, 0.152969, 0, 0, 0.091616, 0.184692,
         0.276864, 0.364058, 0.427975, 0.328783, 0.215938, 0.110678, 0, 0, 0.1125,
         0.225, 0.3375, 0.45, 0.427975, 0.328783, 0.215938, 0.110678, 0.091616,
         0.184692, 0.276864, 0.364058, 0.4309, 0.264503, 0.152969, 0.139058)
  y <- c(0.430423, 0.373607, 0.341595, 0.251562, 0.252734, 0.263932, 0.285114,
         0.173607, 0.162185, 0.152059, 0.14838, 0.05, 0.05, 0.05, 0.05, -0.073607,
         -0.062185, -0.052059, -0.04838, -0.151562, -0.152734, -0.163932, -0.185114,
         -0.271394, -0.273607, -0.241595, -0.330422, -0.45, -0.35, -0.25, -0.15,
         -0.05, 0.45, 0.430423, 0.373607, 0.341595, 0.35, 0.25, 0.251562, 0.252734,
         0.263932, 0.285114, 0.173607, 0.162185, 0.152059, 0.14838, 0.15, 0.05,
         0.05, 0.05, 0.05, 0.05, -0.073607, -0.062185, -0.052059, -0.04838,
         -0.151562, -0.152734, -0.163932, -0.185114, -0.271394, -0.273607,
         -0.241595, -0.330422)
  data.frame(CH = ch, X = x, Y = y, stringsAsFactors = FALSE)
}

lp$hypno <- function(ss, e = NULL, xsize = 20, ysize = 2, title = NULL) {
  ss <- as.character(ss)
  y <- lp$stgn(ss)
  n <- length(y)
  if (n == 0L) return(invisible(NULL))
  if (is.null(e)) e <- seq(0, by = 1, length.out = n)
  x <- as.numeric(e) / 120
  dur <- if (n > 1L) x[2] - x[1] else 1 / 120
  cols <- lp$stgcol(ss)
  graphics::plot(NA, xlim = c(0, max(x) + dur), ylim = c(-3.5, 2.5), xaxs = "i",
                 xlab = "Time (hrs)", ylab = "Sleep stage", yaxt = "n", main = title)
  graphics::abline(h = -3:1, col = "#d0d0d0", lwd = 0.5)
  ok <- is.finite(y)
  graphics::segments(x[ok], y[ok], x[ok] + dur, y[ok], col = "#b8b8b8", lwd = 9, lend = 1)
  graphics::segments(x[ok], y[ok], x[ok] + dur, y[ok], col = cols[ok], lwd = 5.5, lend = 1)
  if (n > 1L) {
    j <- which(is.finite(y[-n]) & is.finite(y[-1]) & y[-n] != y[-1])
    graphics::segments(x[j] + dur, y[j], x[j] + dur, y[j + 1], col = "#aaaaaa", lwd = 1)
  }
  graphics::axis(2, at = -3:2, labels = c("N3", "N2", "N1", "R", "W", "?"), las = 1)
  invisible(NULL)
}

lp$hypno_density <- function(probs, e = NULL, xsize = 20, ysize = 2, title = NULL) {
  if (is.null(probs) || nrow(probs) == 0L) return(invisible(NULL))
  cols <- c("PP_N1", "PP_N2", "PP_N3", "PP_R", "PP_W")
  miss <- setdiff(cols, names(probs))
  if (length(miss)) .luna_abort(paste("missing columns:", paste(miss, collapse = ", ")))
  m <- as.matrix(probs[, cols])
  m[is.na(m)] <- 0
  ne <- nrow(m)
  x <- seq_len(ne)
  graphics::plot(NA, xlim = c(1, ne), ylim = c(0, 1), xaxs = "i", yaxs = "i",
                 xlab = "Epoch", ylab = "Prob(stage)", main = title)
  pal <- lp$stgcol(c("N1", "N2", "N3", "R", "W"))
  lower <- rep(0, ne)
  for (k in seq_along(cols)) {
    upper <- lower + m[, k]
    graphics::polygon(c(x, rev(x)), c(upper, rev(lower)), col = pal[k], border = NA)
    lower <- upper
  }
  invisible(NULL)
}

lp$psd <- function(df, ch, var = "PSD", minf = NULL, maxf = NULL, minp = NULL,
                   maxp = NULL, xlines = NULL, ylines = NULL, dB = FALSE) {
  if (is.null(ch)) return(invisible(NULL))
  ch <- as.character(ch)
  df <- df[df$CH %in% ch, , drop = FALSE]
  if (nrow(df) == 0L) return(invisible(NULL))
  f <- as.numeric(df$F)
  p <- as.numeric(df[[var]])
  if (isTRUE(dB)) p <- 10 * log10(p)
  if (is.null(minp)) minp <- min(p, na.rm = TRUE)
  if (is.null(maxp)) maxp <- max(p, na.rm = TRUE)
  if (is.null(minf)) minf <- min(f)
  if (is.null(maxf)) maxf <- max(f)
  keep <- f >= minf & f <= maxf
  f <- f[keep]; p <- pmin(pmax(p[keep], minp), maxp); cx <- df$CH[keep]
  pal <- grDevices::palette.colors(max(length(ch), 1L), "Tableau 10", recycle = TRUE)
  graphics::plot(NA, xlim = range(f), ylim = c(minp, maxp),
                 xlab = "Frequency (Hz)", ylab = "Power (dB)")
  if (!is.null(xlines)) graphics::abline(v = xlines, col = "gray", lwd = 1)
  if (!is.null(ylines)) graphics::abline(h = ylines, col = "gray", lwd = 1)
  for (k in seq_along(ch)) {
    s <- cx == ch[k]
    graphics::lines(f[s][order(f[s])], p[s][order(f[s])], col = pal[k], lwd = 1.5)
  }
  graphics::legend("topright", legend = ch, col = pal[seq_along(ch)], lwd = 1.5, bty = "n")
  invisible(NULL)
}

.winsorize <- function(z, w) {
  if (is.null(w) || w <= 0) return(z)
  q <- stats::quantile(z, c(w, 1 - w), na.rm = TRUE, names = FALSE, type = 1)
  pmin(pmax(z, q[1]), q[2])
}

.turbo <- function(n) {
  # Google's Turbo colour map (Apache 2.0), as used by matplotlib's 'turbo'
  k <- c("#30123B", "#4145AB", "#4675ED", "#39A2FC", "#1BCFD4", "#24ECA6",
         "#61FC6C", "#A4FC3B", "#D1E834", "#F3C63A", "#FE9B2D", "#F36315",
         "#D93806", "#B11901", "#7A0403")
  grDevices::colorRampPalette(k)(n)
}

lp$spec <- function(df, ch = NULL, var = "PSD", mine = NULL, maxe = NULL,
                    minf = NULL, maxf = NULL, w = 0.025) {
  if (!is.null(ch)) df <- df[df$CH == ch, , drop = FALSE]
  if (nrow(df) == 0L) return(invisible(NULL))
  x <- as.integer(df$E); y <- as.numeric(df$F); z <- as.numeric(df[[var]])
  if (is.null(mine)) mine <- min(x)
  if (is.null(maxe)) maxe <- max(x)
  if (is.null(minf)) minf <- min(y)
  if (is.null(maxf)) maxf <- max(y)
  keep <- x >= mine & x <= maxe & y >= minf & y <= maxf
  x <- x[keep]; y <- y[keep]; z <- .winsorize(z[keep], w)
  if (!length(z)) return(invisible(NULL))
  ex <- seq(min(x), max(x)); fy <- sort(unique(y))
  m <- tapply(z, list(factor(x, levels = ex), factor(y, levels = fy)), mean)
  graphics::image(ex, fy, m, col = .turbo(100), xlab = "Epoch", ylab = "Frequency (Hz)",
                  xlim = c(mine, maxe) + c(-0.5, 0.5), useRaster = TRUE)
  invisible(NULL)
}

.colormap <- function(name, n = 101) {
  if (length(name) > 1L) return(grDevices::colorRampPalette(name)(n))
  switch(name,
    bwr = grDevices::colorRampPalette(c("blue", "white", "red"))(n),
    turbo = .turbo(n),
    tryCatch(grDevices::hcl.colors(n, name),
             error = function(e) .luna_abort(sprintf("unknown colormap '%s': use 'bwr', 'turbo', an hcl.colors() palette name, or a vector of colours", name))))
}

lp$topo_heat <- function(chs, z, ths = NULL, th = 0.05, topo = NULL, lmts = NULL,
                         sz = 70, colormap = "bwr", title = "", rimcolor = "black",
                         lab = "dB") {
  chs <- toupper(as.character(chs))
  z <- as.numeric(z)
  if (length(chs) != length(z)) .luna_abort("chs and z must have the same length")
  if (!is.null(ths) && length(ths) != length(z)) .luna_abort("ths and z must have the same length")
  if (is.null(topo)) topo <- lp$default_xy()
  topo$CH <- toupper(topo$CH)
  rng <- range(z, na.rm = TRUE)
  if (is.null(lmts)) lmts <- rng
  else if (rng[1] < lmts[1] || rng[2] > lmts[2]) .luna_abort("channel values are out of specified limits")
  topo <- topo[topo$CH %in% chs, , drop = FALSE]
  if (nrow(topo) == 0L) .luna_abort("no matching channels")
  idx <- match(topo$CH, chs)
  vals <- z[idx]
  rims <- if (is.null(ths)) rep(0.5, length(idx)) else ifelse(ths[idx] < th, 1.5, 0.5)
  pal <- .colormap(colormap)
  k <- if (diff(lmts) > 0) 1 + round(100 * (vals - lmts[1]) / diff(lmts)) else rep(51, length(vals))
  op <- graphics::par(mar = c(0, 0, 0, 0))
  on.exit(graphics::par(op))
  graphics::plot(NA, xlim = c(-0.6, 0.6), ylim = c(-0.6, 0.6), axes = FALSE,
                 xlab = "", ylab = "", asp = 1)
  graphics::points(topo$X, topo$Y, pch = 21, bg = pal[k], col = rimcolor,
                   lwd = rims * 2, cex = sqrt(sz) / 4)
  graphics::text(-0.4, 0.5, title, cex = 0.9)
  # horizontal colour bar
  xb <- seq(0.15, 0.53, length.out = length(pal) + 1)
  graphics::rect(xb[-length(xb)], -0.56, xb[-1], -0.53, col = pal, border = NA)
  graphics::text(0.15, -0.5, signif(lmts[1], 2), cex = 0.7)
  graphics::text(0.53, -0.5, signif(lmts[2], 2), cex = 0.7)
  graphics::text(0.34, -0.48, lab, cex = 0.8)
  invisible(NULL)
}
