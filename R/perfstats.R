#' Performance statistics in the mseviz format
#'
#' Computes performance statistics of one or more projections (named list of
#' [ms_simulate()] runs, one per management procedure or rule) and returns
#' them as a long `data.table` in the format of the
#' [mseviz](https://github.com/flr/mseviz) package: one row per run (`mp`),
#' unit (`biol`: a stock code, a segment, or `"All"` for the whole system),
#' statistic and iteration, with the value in `data`. The table can be passed
#' to [plot_bps()], [plot_tradeoffs()] and [plot_quilt()], or to
#' `mseviz::plotBPs()`, `mseviz::plotTOs()` and `mseviz::perfTable()`.
#'
#' Statistics are computed per iteration over the evaluation `years`:
#'
#' | statistic | unit | definition |
#' |---|---|---|
#' | `BBmsy`  | stock | median B/Bmsy |
#' | `FFmsy`  | stock | median F/Fmsy |
#' | `CMSY`   | stock | mean catch / MSY (MSY = rK/4) |
#' | `PBlim`  | stock | share of years with B < Blim |
#' | `PBpa`   | stock | share of years with B < Bpa |
#' | `PB80`   | stock | share of years with B > 0.8 Bmsy |
#' | `green`  | stock | share of years in the Kobe green zone (B > Bmsy, F < Fmsy) |
#' | `Days`   | segment, All | mean days fished |
#' | `C`      | All | mean total catch |
#' | `Rev`    | All | mean revenue |
#' | `Profit` | All | mean profit |
#' | `CMMSY`  | All | mean total catch / sum of single-stock MSYs |
#' | `AAVC`   | All | mean annual absolute change of total catch (relative) |
#' | `AAVE`   | All | mean annual absolute change of total days (relative) |
#' | `NBpa`   | All | mean number of stocks below Bpa |
#' | `PanyBlim` | All | share of years with any stock below Blim |
#' | `minBBmsy` | All | lowest B/Bmsy of any stock in any year |
#'
#' Probabilities (statistics starting with `P` and `green`) are best shown
#' by their mean over iterations, which is the probability over years and
#' iterations.
#'
#' @param runs named list of [ms_simulate()] results (with `keep = "all"`),
#'   one per management procedure; all from the same system.
#' @param years evaluation years (default the last 20).
#' @param Blim,Bpa limit and precautionary biomass relative to Bmsy.
#' @return A `data.table` with columns `mp`, `biol`, `statistic`, `name`
#'   (plotmath label), `desc`, `type` (`"value"` or `"prob"`), `year`
#'   (the evaluation period), `iter` and `data`.
#' @seealso [ms_perf_table()], [plot_bps()], [plot_tradeoffs()], [plot_quilt()]
#' @export
ms_perf <- function(runs, years = NULL, Blim = 0.25, Bpa = 0.5) {
  if (inherits(runs, "ms_sim")) runs <- list(run = runs)
  if (is.null(names(runs))) names(runs) <- paste0("MP", seq_along(runs))
  sys <- runs[[1]]$sys
  ny <- dim(runs[[1]]$B)[1]
  yr <- years %||% seq(max(1, ny - 19), ny)
  period <- if (length(yr) > 1) paste0(min(yr), "-", max(yr)) else as.character(yr)
  msy <- sys$r * sys$K / 4
  meta <- ms_statistics()
  out <- lapply(names(runs), function(nm) {
    x <- runs[[nm]]
    n <- dim(x$B)[2]
    bb <- sweep(x$B[yr, , , drop = FALSE], 3, sys$K / 2, "/")             # years x iter x stocks
    ff <- ffmsy(x$F[yr, , , drop = FALSE], sys$r)
    cc <- x$C[yr, , , drop = FALSE]
    by_stock <- list(
      BBmsy = apply(bb, c(2, 3), stats::median),
      FFmsy = apply(ff, c(2, 3), stats::median),
      CMSY  = sweep(apply(cc, c(2, 3), mean), 2, msy, "/"),
      PBlim = apply(bb < Blim, c(2, 3), mean),
      PBpa  = apply(bb < Bpa, c(2, 3), mean),
      PB80  = apply(bb > 0.8, c(2, 3), mean),
      green = apply(bb > 1 & ff < 1, c(2, 3), mean))
    d1 <- do.call(rbind, lapply(names(by_stock), function(s)
      data.frame(biol = rep(sys$code, each = n), statistic = s, iter = rep(seq_len(n), sys$S),
                 data = as.vector(by_stock[[s]]))))
    ey <- x$E_seg[yr, , , drop = FALSE]
    d2 <- data.frame(biol = rep(names(sys$segments), each = n), statistic = "Days",
                     iter = rep(seq_len(n), sys$G), data = as.vector(apply(ey, c(2, 3), mean)))
    ctot <- apply(cc, 1:2, sum); etot <- apply(ey, 1:2, sum)
    aav <- function(m) if (nrow(m) < 2) rep(NA_real_, ncol(m)) else
      colMeans(abs(diff(m)) / pmax(m[-nrow(m), , drop = FALSE], 1e-9))
    sysw <- list(
      Days = colMeans(etot), C = colMeans(ctot),
      Rev = colMeans(x$revenue[yr, , drop = FALSE]), Profit = colMeans(x$profit[yr, , drop = FALSE]),
      CMMSY = colMeans(ctot) / sum(msy), AAVC = aav(ctot), AAVE = aav(etot),
      NBpa = colMeans(apply(bb < Bpa, 1:2, sum)),
      PanyBlim = colMeans(apply(bb < Blim, 1:2, any)),
      minBBmsy = apply(bb, 2, min))
    d3 <- do.call(rbind, lapply(names(sysw), function(s)
      data.frame(biol = "All", statistic = s, iter = seq_len(n), data = as.vector(sysw[[s]]))))
    d <- rbind(d1, d2, d3); d$mp <- nm; d
  })
  d <- do.call(rbind, out)
  d <- merge(d, meta, by = "statistic", sort = FALSE)
  d$year <- period
  d <- d[, c("mp", "biol", "statistic", "name", "desc", "type", "year", "iter", "data")]
  d$mp <- factor(d$mp, names(runs))
  d$biol <- factor(d$biol, unique(c("All", names(sys$segments), sys$code)))
  d <- d[order(d$mp, d$biol, match(d$statistic, meta$statistic), d$iter), ]
  rownames(d) <- NULL
  data.table::as.data.table(d)
}

#' Definitions of the performance statistics
#'
#' @return A data frame with `statistic`, `name` (plotmath), `desc`, `type`
#'   (`"value"` or `"prob"`) and `higher_better` (direction used to colour
#'   decision tables; `NA` = no preferred direction).
#' @export
ms_statistics <- function() {
  data.frame(
    statistic = c("BBmsy", "FFmsy", "CMSY", "PBlim", "PBpa", "PB80", "green",
                  "Days", "C", "Rev", "Profit", "CMMSY", "AAVC", "AAVE", "NBpa", "PanyBlim", "minBBmsy"),
    name = c("B/B[MSY]", "F/F[MSY]", "Catch/MSY", "P(B<B[lim])", "P(B<B[pa])", "P(B>0.8*B[MSY])", "P(Green)",
             "Days", "Catch", "Revenue", "Profit", "Catch/Sigma*MSY", "AAV[catch]", "AAV[effort]",
             "N(B<B[pa])", "P(any~B<B[lim])", "min~B/B[MSY]"),
    desc = c("Median biomass relative to Bmsy", "Median F relative to Fmsy", "Mean catch relative to MSY",
             "Probability that B is below Blim", "Probability that B is below Bpa",
             "Probability that B is above 0.8 Bmsy", "Probability of being in the Kobe green zone",
             "Mean days fished", "Mean total catch", "Mean revenue", "Mean profit",
             "Mean total catch relative to the sum of single-stock MSYs",
             "Mean annual variation of total catch", "Mean annual variation of total days",
             "Mean number of stocks below Bpa", "Probability that any stock is below Blim",
             "Lowest B/Bmsy of any stock in any year"),
    type = c("value", "value", "value", "prob", "prob", "prob", "prob",
             "value", "value", "value", "value", "value", "value", "value", "value", "prob", "value"),
    higher_better = c(TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE,
                      NA, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE),
    stringsAsFactors = FALSE)
}

#' Summary (decision) table of performance statistics
#'
#' Medians (or means, for probabilities) of each statistic by management
#' procedure, with the 10th and 90th percentiles.
#'
#' @param perf output of [ms_perf()].
#' @param statistics statistics to include (default all in `perf` for `biol`).
#' @param biol unit: `"All"` (system), a segment or a stock code; several
#'   values give one block per unit.
#' @param probs lower and upper quantiles.
#' @param digits significant digits.
#' @return A data frame: one row per `mp` (and `biol`), one column per
#'   statistic, cells "median (lower-upper)".
#' @export
ms_perf_table <- function(perf, statistics = NULL, biol = "All", probs = c(0.1, 0.9), digits = 3) {
  s <- summ_perf(perf, statistics, biol, probs)
  f <- function(x) formatC(signif(x, digits), format = "fg", digits = digits, big.mark = ",")
  s$cell <- ifelse(s$type == "prob", f(s$mid), paste0(f(s$mid), " (", f(s$lo), "-", f(s$hi), ")"))
  w <- stats::reshape(s[, c("mp", "biol", "statistic", "cell")], idvar = c("mp", "biol"),
                      timevar = "statistic", direction = "wide")
  names(w) <- sub("^cell\\.", "", names(w))
  if (length(unique(w$biol)) == 1) w$biol <- NULL
  rownames(w) <- NULL
  w
}

summ_perf <- function(perf, statistics, biol, probs = c(0.1, 0.9)) {
  d <- as.data.frame(perf)
  d <- d[as.character(d$biol) %in% biol, , drop = FALSE]
  if (!is.null(statistics)) d <- d[d$statistic %in% statistics, , drop = FALSE]
  if (!nrow(d)) stop("no statistics for this selection", call. = FALSE)
  key <- interaction(d$mp, d$biol, d$statistic, drop = TRUE, lex.order = TRUE)
  s <- do.call(rbind, lapply(split(d, key), function(x) {
    p <- x$type[1] == "prob"
    data.frame(mp = x$mp[1], biol = x$biol[1], statistic = x$statistic[1], name = x$name[1], type = x$type[1],
               mid = if (p) mean(x$data, na.rm = TRUE) else stats::median(x$data, na.rm = TRUE),
               lo = stats::quantile(x$data, probs[1], na.rm = TRUE, names = FALSE),
               hi = stats::quantile(x$data, probs[2], na.rm = TRUE, names = FALSE))
  }))
  lev <- if (is.null(statistics)) unique(d$statistic) else statistics[statistics %in% d$statistic]
  s$statistic <- factor(s$statistic, lev)
  s <- s[order(s$mp, s$biol, s$statistic), ]
  s$statistic <- as.character(s$statistic)
  rownames(s) <- NULL
  s
}

#' Boxplots of performance statistics by management procedure
#'
#' The style of `mseviz::plotBPs()`: one panel per statistic, a box per
#' management procedure (10th, 25th, 50th, 75th and 90th percentiles over
#' iterations) with a point at the median; probabilities are shown as a
#' point at their mean.
#'
#' @inheritParams ms_perf_table
#' @param biol a single unit (`"All"`, a segment or a stock code) or several
#'   stock codes (one row of panels per stock).
#' @param target,limit named vectors of reference lines by statistic (green
#'   and red dashed lines).
#' @param ncol columns of panels (single unit).
#' @return A ggplot.
#' @export
plot_bps <- function(perf, statistics = NULL, biol = "All", target = NULL, limit = NULL, ncol = 3) {
  d <- as.data.frame(perf)
  d <- d[as.character(d$biol) %in% biol, , drop = FALSE]
  if (!is.null(statistics)) d <- d[d$statistic %in% statistics, , drop = FALSE]
  key <- interaction(d$mp, d$biol, d$statistic, drop = TRUE, lex.order = TRUE)
  q <- do.call(rbind, lapply(split(d, key), function(x) {
    qq <- stats::quantile(x$data, c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = TRUE, names = FALSE)
    p <- x$type[1] == "prob"
    if (p) qq <- rep(mean(x$data, na.rm = TRUE), 5)
    data.frame(mp = x$mp[1], biol = x$biol[1], statistic = x$statistic[1], name = x$name[1], prob = p,
               ymin = qq[1], lower = qq[2], middle = qq[3], upper = qq[4], ymax = qq[5])
  }))
  lev <- unique(d$name[order(match(d$statistic, if (is.null(statistics)) unique(d$statistic) else statistics))])
  q$name <- factor(q$name, lev)
  q$mp <- factor(q$mp, levels(perf$mp) %||% unique(q$mp))
  qb <- q[!q$prob, , drop = FALSE]
  p <- ggplot(q, aes(x = mp, ymin = ymin, lower = lower, middle = middle, upper = upper, ymax = ymax, fill = mp)) +
    (if (nrow(qb)) geom_boxplot(data = qb, stat = "identity", width = 0.7, linewidth = 0.3)) +
    geom_point(aes(y = middle), shape = 21, size = 2.6, stroke = 0.4, colour = "black") +
    scale_fill_manual(values = rep(ms_cols, length.out = nlevels(q$mp)), name = NULL) +
    labs(x = NULL, y = "Performance statistics") + theme_ms() +
    theme(axis.text.x = element_blank(), panel.grid.major.x = element_blank(), legend.position = "right",
          strip.background = element_rect(fill = "grey88", colour = NA))
  ref <- function(v, col) {
    if (is.null(v)) return(NULL)
    r <- unique(q[q$statistic %in% names(v), c("statistic", "name", "biol")])
    if (!nrow(r)) return(NULL)
    r$y <- v[r$statistic]
    geom_hline(data = r, aes(yintercept = y), colour = col, linetype = "longdash", linewidth = 0.5)
  }
  p <- p + ref(target, "#2e9e44") + ref(limit, "#d62d20")
  if (length(unique(q$biol)) > 1)
    p + facet_grid(biol ~ name, scales = "free_y", labeller = labeller(name = label_parsed))
  else p + facet_wrap(~ name, scales = "free_y", ncol = ncol, labeller = label_parsed)
}

#' Trade-off plots of performance statistics
#'
#' The style of `mseviz::plotTOs()`: medians (means for probabilities) of
#' each statistic in `y` against statistic `x`, with 10th-90th percentile
#' ranges, one point per management procedure.
#'
#' @inheritParams plot_bps
#' @param x statistic on the x axis (default total catch).
#' @param y statistics on the y axes.
#' @param biol_x,biol_y units of `x` and `y` (default the system, `"All"`).
#' @return A ggplot.
#' @export
plot_tradeoffs <- function(perf, x = "C", y = c("NBpa", "PanyBlim", "Profit", "AAVC"),
                           biol_x = "All", biol_y = "All", ncol = 2) {
  sx <- summ_perf(perf, x, biol_x); sy <- summ_perf(perf, y, biol_y)
  names(sx)[match(c("mid", "lo", "hi"), names(sx))] <- c("x", "xlo", "xhi")
  d <- merge(sy, sx[, c("mp", "x", "xlo", "xhi")], by = "mp")
  d$name <- factor(d$name, unique(sy$name))
  d$mp <- factor(d$mp, levels(perf$mp) %||% unique(d$mp))
  xl <- ms_statistics()$name[match(x, ms_statistics()$statistic)]
  ggplot(d, aes(x, mid)) +
    geom_linerange(aes(ymin = lo, ymax = hi), linewidth = 0.4, alpha = 0.7) +
    geom_linerange(aes(xmin = xlo, xmax = xhi), linewidth = 0.4, alpha = 0.7) +
    geom_point(aes(fill = mp), shape = 21, size = 3.4, colour = "black") +
    facet_wrap(~ name, scales = "free_y", ncol = ncol, labeller = label_parsed) +
    scale_fill_manual(values = rep(ms_cols, length.out = nlevels(d$mp)), name = NULL) +
    labs(x = parse(text = xl), y = NULL) + theme_ms() +
    theme(strip.background = element_rect(fill = "grey88", colour = NA))
}

#' Quilt plot: a colour-coded decision table
#'
#' A table of medians (means for probabilities) with each cell coloured by
#' how well the management procedure does on that statistic relative to the
#' others: green = best, red = worst within a column (or, with
#' `layout = "stocks"`, within a stock), using the direction in
#' [ms_statistics()]; statistics without a preferred direction (days) are
#' grey. With `risk`, probabilities of being below a limit are coloured by
#' absolute classes instead.
#'
#' @inheritParams ms_perf_table
#' @param layout `"statistics"` (rows = management procedures, columns =
#'   statistics, for one unit) or `"stocks"` (rows = stocks, columns =
#'   management procedures, for one statistic).
#' @param statistic the statistic for `layout = "stocks"` (e.g. `"BBmsy"`,
#'   `"PBpa"`).
#' @param risk optional risk limit for probabilities of being below a limit
#'   (`PBlim`, `PBpa`, `PanyBlim`), e.g. 0.05: cells at or below the limit
#'   are green, up to twice the limit amber, above red, regardless of rank.
#' @param palette colours from worst to best.
#' @return A ggplot.
#' @export
plot_quilt <- function(perf, statistics = NULL, biol = "All", layout = c("statistics", "stocks"),
                       statistic = "BBmsy", risk = NULL, digits = 2,
                       palette = c("#e8857c", "#f6d38a", "#a9d99a")) {
  layout <- match.arg(layout)
  meta <- ms_statistics()
  f <- function(v) ifelse(abs(v) >= 1000, formatC(v, format = "d", big.mark = ","),
                          formatC(signif(v, digits + 1), format = "fg", digits = digits + 1))
  if (layout == "statistics") {
    s <- summ_perf(perf, statistics, biol)
    s$col <- s$statistic
  } else {
    sys_codes <- levels(perf$biol)[!levels(perf$biol) %in% c("All", unique(as.character(perf$biol[perf$statistic == "Days"])))]
    s <- summ_perf(perf, statistic, sys_codes)
    s$col <- s$mp
  }
  hb <- meta$higher_better[match(s$statistic, meta$statistic)]
  grp <- as.character(if (layout == "statistics") s$statistic else s$biol)
  s$score <- stats::ave(s$mid, grp, FUN = function(v) {
    r <- range(v, na.rm = TRUE); if (diff(r) <= 0) rep(0.5, length(v)) else (v - r[1]) / diff(r) })
  s$score <- ifelse(is.na(hb), NA, ifelse(hb, s$score, 1 - s$score))
  if (!is.null(risk)) {                         # absolute classes for risks of being below a limit
    pr <- s$statistic %in% c("PBlim", "PBpa", "PanyBlim")
    s$score[pr] <- ifelse(s$mid[pr] <= risk, 1, ifelse(s$mid[pr] <= 2 * risk, 0.5, 0))
  }
  s$label <- f(s$mid)
  if (layout == "statistics") {
    s$x <- factor(meta$name[match(s$statistic, meta$statistic)],
                  meta$name[match(unique(s$statistic), meta$statistic)])
    s$y <- factor(s$mp, rev(levels(perf$mp) %||% unique(s$mp)))
    xl <- NULL; yl <- NULL
  } else {
    s$x <- factor(s$mp, levels(perf$mp) %||% unique(s$mp))
    s$y <- factor(s$biol, rev(sys_codes))
    xl <- NULL; yl <- NULL
  }
  ttl <- if (layout == "stocks") meta$name[match(statistic, meta$statistic)] else NULL
  ggplot(s, aes(x, y)) +
    geom_tile(aes(fill = score), colour = "white", linewidth = 0.8) +
    geom_text(aes(label = label), size = 3.2, colour = ms_ink[["primary"]]) +
    scale_fill_gradientn(colours = palette, limits = c(0, 1), na.value = "grey92", guide = "none") +
    scale_x_discrete(position = "top", labels = function(v) if (layout == "statistics") parse(text = v) else v) +
    labs(x = xl, y = yl, title = if (!is.null(ttl)) parse(text = ttl)) + theme_ms() +
    theme(panel.grid = element_blank(), axis.text.x = element_text(size = rel(0.95)),
          axis.ticks = element_blank())
}
