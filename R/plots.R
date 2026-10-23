# Plots: stock status, fishing mortality, effort and yield.
# Colours: categorical slots validated for colour-vision deficiency (slots 1-3
# for any pair, 1-6 for adjacent pairs).

#' Colours and theme of the package plots
#'
#' `ms_cols` are the categorical colours, `ms_ink` the text and line
#' colours, and `theme_ms()` the ggplot2 theme.
#' @param base_size base font size.
#' @export
theme_ms <- function(base_size = 10) {
  theme_minimal(base_size = base_size) %+replace% theme(
    text = element_text(colour = ms_ink[["primary"]]),
    axis.text = element_text(colour = ms_ink[["secondary"]], size = rel(0.85)),
    axis.title = element_text(colour = ms_ink[["secondary"]], size = rel(0.9)),
    panel.grid.major = element_line(colour = ms_ink[["grid"]], linewidth = 0.3),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", hjust = 0, size = rel(0.9), margin = margin(2, 0, 4, 0)),
    legend.position = "top", legend.justification = "left",
    legend.title = element_text(colour = ms_ink[["secondary"]], size = rel(0.9)),
    plot.title = element_text(face = "bold", hjust = 0, size = rel(1.15), margin = margin(0, 0, 4, 0)),
    plot.subtitle = element_text(colour = ms_ink[["secondary"]], hjust = 0, margin = margin(0, 0, 8, 0)),
    plot.title.position = "plot", panel.spacing = unit(10, "pt"))
}

#' @rdname theme_ms
#' @export
ms_cols <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300", "#6250d6", "#e34948")

#' @rdname theme_ms
#' @export
ms_ink <- c(primary = "#0b0b0b", secondary = "#52514e", muted = "#8a8984", grid = "#e8e7e3", ref = "#8a8984")

ref_B <- function(Bpa = 0.5, Blim = 0.25) list(
  geom_hline(yintercept = 1, colour = ms_ink[["ref"]], linewidth = 0.3),
  geom_hline(yintercept = Bpa, colour = ms_ink[["ref"]], linewidth = 0.3, linetype = "dashed"),
  geom_hline(yintercept = Blim, colour = ms_ink[["ref"]], linewidth = 0.3, linetype = "dotted"))
ref_F <- function() geom_hline(yintercept = 1, colour = ms_ink[["ref"]], linewidth = 0.3)

#' Kobe plot of equilibrium states
#'
#' One point per stock, one panel per scenario. With `zones = "hcr"` the
#' background follows the WKREF/GFCM convention of `FLRef::plotWKREF()`,
#' with zones cut by the harvest control rule: below Bthresh, red above the
#' HCR line and yellow below it; above Bthresh, orange above and green
#' below; a dark overlay below Blim. The HCR line is `Fmin` up to `Bclose`,
#' a linear ramp to `Ftgt` at `Btrigger`, and `Ftgt` above (as
#' `FLRef::plotGFCM(fmin = 0.1, bclose = 0.25, btrigger = 0.8)`).
#'
#' @param states named list of states with `BBmsy` and `FFmsy` (e.g. from
#'   [ms_status_quo()] or [ms_optimise()]).
#' @param sys an `ms_system`.
#' @param Bpa,Blim reference points relative to Bmsy (`Bpa` is Bthresh).
#' @param Btrigger,Ftgt,Bclose,Fmin HCR trigger, target F (relative to Fmsy),
#'   biomass at and below which F is at its minimum (default `Blim`, as in
#'   [hcr_effort()]) and that minimum F (default 0.1, the minimum effort
#'   `Emin` of [hcr_effort()]).
#' @param zones `"hcr"` (coloured zones and the HCR line) or `"simple"`.
#' @param palette zone colours: `"pastel"` (default) or `"flref"` (the
#'   saturated colours of `FLRef::plotWKREF()`).
#' @param alpha opacity of the zones.
#' @return A ggplot.
#' @export
#' @examples
#' sys <- atlantic_system()
#' \donttest{
#' sq <- ms_status_quo(sys, capacity = 1000, gamma = 1.5, years = 200)
#' plot_kobe(list("Status quo" = sq), sys)
#' }
plot_kobe <- function(states, sys, Bpa = 0.5, Blim = 0.25, Btrigger = 0.8, Ftgt = 1,
                      Bclose = Blim, Fmin = 0.1, zones = c("hcr", "simple"), palette = c("pastel", "flref"),
                      alpha = 1) {
  zones <- match.arg(zones); palette <- match.arg(palette)
  zc <- if (palette == "pastel")
    c(green = "#d4ecd0", yellow = "#fbf1b8", orange = "#fbd9b4", red = "#f6c6c0", crit = "#8b1a1a", crit_a = 0.22)
  else c(green = "green", yellow = "yellow", orange = "orange", red = "red", crit = "red4", crit_a = 0.7)
  d <- do.call(rbind, lapply(names(states), function(nm)
    data.frame(scenario = nm, code = sys$code, group = sys$group,
               BBmsy = unname(states[[nm]]$BBmsy), FFmsy = unname(states[[nm]]$FFmsy))))
  d$scenario <- factor(d$scenario, names(states))
  xmax <- max(2.1, max(d$BBmsy, na.rm = TRUE) * 1.05)
  ymax <- max(1.5 * Ftgt, max(d$FFmsy, na.rm = TRUE) * 1.1)
  if (zones == "simple") {
    p <- ggplot(d, aes(BBmsy, FFmsy)) +
      annotate("rect", xmin = 1, xmax = Inf, ymin = -Inf, ymax = 1, fill = "#0ca30c", alpha = 0.07) +
      annotate("rect", xmin = -Inf, xmax = 1, ymin = 1, ymax = Inf, fill = "#d03b3b", alpha = 0.07) +
      geom_vline(xintercept = 1, colour = ms_ink[["ref"]], linewidth = 0.3) +
      geom_vline(xintercept = Bpa, colour = ms_ink[["ref"]], linewidth = 0.3, linetype = "dashed") +
      geom_vline(xintercept = Blim, colour = ms_ink[["ref"]], linewidth = 0.3, linetype = "dotted") +
      geom_hline(yintercept = 1, colour = ms_ink[["ref"]], linewidth = 0.3) +
      geom_point(aes(colour = group), size = 2.6, stroke = 0) +
      scale_colour_manual(values = ms_cols, name = NULL)
  } else {
    hcr <- function(b) ifelse(b <= Bclose, Fmin, ifelse(b < Btrigger,
      Fmin + (Ftgt - Fmin) * (b - Bclose) / (Btrigger - Bclose), Ftgt))
    zone <- function(x0, x1, lower, fill) {
      b <- sort(unique(c(seq(x0, x1, length.out = 200), Bclose, Btrigger)))
      b <- b[b >= x0 & b <= x1]
      f <- pmin(hcr(b), ymax)
      geom_ribbon(data = data.frame(b = b, lo = if (lower) 0 else f, hi = if (lower) f else ymax),
                  aes(x = b, ymin = lo, ymax = hi), inherit.aes = FALSE, fill = fill, alpha = alpha)
    }
    bl <- seq(0, xmax, length.out = 400)
    lab_ref <- function(x, y, l, ...) annotate("text", x = x, y = y, label = l, parse = TRUE, size = 3.3, ...)
    p <- ggplot(d, aes(BBmsy, FFmsy)) +
      zone(0, Bpa, TRUE, zc[["yellow"]]) + zone(0, Bpa, FALSE, zc[["red"]]) +
      zone(Bpa, xmax, TRUE, zc[["green"]]) + zone(Bpa, xmax, FALSE, zc[["orange"]]) +
      annotate("rect", xmin = 0, xmax = Blim, ymin = 0, ymax = ymax, fill = zc[["crit"]],
               alpha = as.numeric(zc[["crit_a"]])) +
      annotate("line", x = bl, y = hcr(bl), colour = "black", linewidth = 0.4) +
      annotate("segment", x = 0, xend = Btrigger, y = Ftgt, yend = Ftgt, linetype = "dashed", linewidth = 0.3) +
      annotate("segment", x = Btrigger, xend = Btrigger, y = 0, yend = Ftgt, linetype = "dashed", linewidth = 0.3) +
      annotate("segment", x = Blim, xend = Blim, y = 0, yend = Ftgt, linewidth = 0.8) +
      annotate("segment", x = Bpa, xend = Bpa, y = 0, yend = hcr(Bpa), linewidth = 0.3) +
      annotate("segment", x = 1, xend = 1, y = 0, yend = Ftgt, colour = if (palette == "pastel") "#2a5db0" else "blue",
               linewidth = 0.8) +
      lab_ref(Blim, Ftgt * 1.05, "B[lim]", hjust = 0, vjust = 0) +
      lab_ref(Bpa, Ftgt * 0.05, "B[thresh]", hjust = 1.05, vjust = 0) +
      lab_ref(Btrigger, Ftgt * 1.05, "B[trigger]", hjust = 1, vjust = 0) +
      lab_ref(1.02, Ftgt * 0.05, "B[MSY]", hjust = 0, vjust = 0) +
      lab_ref(xmax * 0.98, Ftgt * 1.03, "F[MSY]", hjust = 1, vjust = 0) +
      geom_point(aes(fill = group), shape = 21, size = 2.8, stroke = 0.5, colour = "black") +
      scale_fill_manual(values = ms_cols, name = NULL)
  }
  lab <- if (requireNamespace("ggrepel", quietly = TRUE))
    ggrepel::geom_text_repel(aes(label = code), size = 2.7, colour = ms_ink[["primary"]],
                             bg.colour = if (zones == "hcr") "white" else NA, bg.r = 0.12,
                             min.segment.length = 0.2, segment.colour = ms_ink[["primary"]],
                             segment.size = 0.25, box.padding = 0.25, max.overlaps = Inf, seed = 1)
    else geom_text(aes(label = code), size = 2.7, vjust = -0.9, colour = ms_ink[["primary"]])
  p + lab + facet_wrap(~ scenario) +
    scale_x_continuous(limits = c(0, xmax), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, ymax), expand = c(0, 0)) +
    labs(x = "B / Bmsy", y = "F / Fmsy") + theme_ms() +
    (if (zones == "hcr") theme(panel.grid = element_blank(), panel.spacing = unit(1, "lines")) else NULL)
}

#' Equilibrium yield curves
#'
#' @param curves named list of curves from [ms_yield_curve()] or
#'   [ms_shared_q_curve()].
#' @param ref optional reference state (e.g. the status quo) to mark.
#' @param sys optional `ms_system`, to draw the sum of single-stock MSY.
#' @param Bpa safeguard relative to Bmsy.
#' @return A ggplot (yield over days; filled points: MMSY, open points:
#'   highest yield with every stock >= Bpa).
#' @export
plot_yield_curves <- function(curves, ref = NULL, sys = NULL, Bpa = 0.5) {
  d <- do.call(rbind, lapply(names(curves), function(nm) cbind(treatment = nm, curves[[nm]][, c("E", "yield", "min_BBmsy")])))
  d$treatment <- factor(d$treatment, names(curves))
  pts <- do.call(rbind, lapply(split(d, d$treatment), function(x) {
    i <- which.max(x$yield); ok <- which(x$min_BBmsy >= Bpa & x$E > 0)
    rbind(data.frame(x[i, ], point = "MMSY"),
          if (length(ok)) data.frame(x[ok[which.max(x$yield[ok])], ], point = "Safeguarded (all >= Bpa)")) }))
  p <- ggplot(d, aes(E, yield, colour = treatment))
  if (!is.null(sys)) p <- p + geom_hline(yintercept = sum(sys$r * sys$K / 4), colour = ms_ink[["ref"]],
                                         linewidth = 0.3, linetype = "dashed")
  p <- p + geom_line(linewidth = 0.8) +
    geom_point(data = pts, aes(shape = point), size = 2.8, stroke = 1) +
    scale_shape_manual(values = c(MMSY = 16, `Safeguarded (all >= Bpa)` = 1), name = NULL) +
    scale_colour_manual(values = ms_cols, name = NULL) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(x = "Total days fished", y = "Equilibrium yield") + theme_ms() + theme(legend.position = "right")
  if (!is.null(ref)) p <- p + annotate("point", x = sum(ref$E_seg), y = ref$summary$yield, shape = 23,
                                       size = 3.2, fill = ms_ink[["primary"]])
  p
}

q90 <- function(a, f) apply(a, f, quantile, c(0.05, 0.5, 0.95), na.rm = TRUE)

#' Projections by stock: B/Bmsy, F/Fmsy or catch over time
#'
#' Median and 90% band by stock, for a named list of runs.
#'
#' @param runs named list of `ms_sim` runs with `keep = "all"`.
#' @param what "BBmsy", "FFmsy" or "yield".
#' @param Bpa,Blim reference points relative to Bmsy.
#' @param ncol columns of the panel grid.
#' @return A ggplot.
#' @export
plot_projection <- function(runs, what = c("BBmsy", "FFmsy", "yield"), Bpa = 0.5, Blim = 0.25, ncol = 5) {
  what <- match.arg(what); sys <- runs[[1]]$sys
  d <- do.call(rbind, lapply(names(runs), function(nm) {
    x <- runs[[nm]]; ny <- dim(x$B)[1]
    a <- switch(what, BBmsy = sweep(x$B, 3, sys$K / 2, "/"), FFmsy = ffmsy(x$F, sys$r), yield = x$C)
    q <- q90(a, c(1, 3))
    d <- data.frame(run = nm, year = rep(seq_len(ny), sys$S), species = rep(sys$code, each = ny),
                    lo = as.vector(q[1, , ]), med = as.vector(q[2, , ]), hi = as.vector(q[3, , ]))
    if (what == "BBmsy" && !is.null(x$init)) {                       # year 0: the common starting point
      q0 <- apply(sweep(matrix(x$init$B, ncol = sys$S), 2, sys$K / 2, "/"), 2, stats::quantile, c(0.05, 0.5, 0.95))
      d <- rbind(data.frame(run = nm, year = 0, species = sys$code, lo = q0[1, ], med = q0[2, ], hi = q0[3, ]), d)
    }
    d
  }))
  d$species <- factor(d$species, sys$code); d$run <- factor(d$run, names(runs))
  ylab <- c(BBmsy = "B / Bmsy", FFmsy = "F / Fmsy", yield = "Catch")[[what]]
  p <- ggplot(d, aes(year, med, colour = run, fill = run)) +
    (if (what == "BBmsy") ref_B(Bpa, Blim) else if (what == "FFmsy") ref_F() else NULL) +
    geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.18, colour = NA) +
    geom_line(linewidth = 0.7) +
    facet_wrap(~ species, ncol = ncol, scales = if (what == "yield") "free_y" else "fixed") +
    scale_colour_manual(values = ms_cols, name = NULL) + scale_fill_manual(values = ms_cols, name = NULL) +
    labs(x = "Year", y = ylab) + theme_ms()
  if (what == "BBmsy") p <- p + coord_cartesian(ylim = c(0, 2.2))
  if (what == "FFmsy") p <- p + coord_cartesian(ylim = c(0, 5))
  p
}

#' Projections of days by segment and total yield
#'
#' @inheritParams plot_projection
#' @return A ggplot.
#' @export
plot_fleet <- function(runs) {
  sys <- runs[[1]]$sys
  d <- do.call(rbind, lapply(names(runs), function(nm) {
    x <- runs[[nm]]; ny <- dim(x$B)[1]
    qe <- q90(x$E_seg, c(1, 3)); qy <- q90(apply(x$C, 1:2, sum), 1)
    e0 <- if (is.null(x$init)) NULL else {                           # year 0: the common starting point
      q0 <- apply(matrix(x$init$E_seg, ncol = sys$G), 2, stats::quantile, c(0.05, 0.5, 0.95))
      data.frame(run = nm, panel = paste("Days:", names(sys$segments)), year = 0,
                 lo = q0[1, ], med = q0[2, ], hi = q0[3, ])
    }
    rbind(e0, data.frame(run = nm, panel = paste("Days:", rep(names(sys$segments), each = ny)),
                     year = rep(seq_len(ny), sys$G), lo = as.vector(qe[1, , ]),
                     med = as.vector(qe[2, , ]), hi = as.vector(qe[3, , ])),
          data.frame(run = nm, panel = "Total yield", year = seq_len(ny), lo = qy[1, ], med = qy[2, ], hi = qy[3, ]))
  }))
  d$panel <- factor(d$panel, c(paste("Days:", names(sys$segments)), "Total yield"))
  d$run <- factor(d$run, names(runs))
  ggplot(d, aes(year, med, colour = run, fill = run)) +
    geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.18, colour = NA) +
    geom_line(linewidth = 0.7) +
    facet_wrap(~ panel, nrow = 1, scales = "free_y") +
    scale_y_continuous(limits = c(0, NA)) +
    scale_colour_manual(values = ms_cols, name = NULL) + scale_fill_manual(values = ms_cols, name = NULL) +
    labs(x = "Year", y = NULL) + theme_ms()
}

#' Catchability heatmap
#'
#' Catchability of each stock in each métier, relative to the stock's
#' highest catchability.
#'
#' @param sys an `ms_system`.
#' @return A ggplot.
#' @export
plot_catchability <- function(sys) {
  rel <- sweep(sys$Q, 2, apply(sys$Q, 2, max), "/")
  d <- data.frame(metier = factor(rep(rownames(rel), sys$S), rev(rownames(rel))),
                  code = factor(rep(sys$code, each = sys$M), sys$code), value = as.vector(rel))
  ggplot(d, aes(code, metier, fill = value)) +
    geom_tile(colour = "#fcfcfb", linewidth = 0.6) +
    scale_fill_gradient(low = "#f0efec", high = "#104281", limits = c(0, 1), name = "q / max q") +
    labs(x = NULL, y = NULL) + theme_ms() +
    theme(panel.grid.major = element_blank(), legend.position = "right",
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
}
