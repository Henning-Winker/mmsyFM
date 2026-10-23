#' Build a multispecies fleet system from tables
#'
#' Defines the stocks, métiers, catchability, fleet segments and costs that
#' all other functions use.
#'
#' @param species data frame, one row per stock, with columns `species`,
#'   `r`, `K` and `price`; optional `code` (short label), `group`,
#'   `season_amp` (0 = resident, 1 = absent at the low point),
#'   `season_peak` (month of highest availability), `F_cap` (cap on
#'   realised F in units of Fmsy, see [ms_simulate()]) and `dep_init`
#'   (starting B/K of simulations, default 1; set by [ms_condition()]).
#' @param metiers data frame, one row per métier, with column `metier`;
#'   optional `tactic`, `ground` and `cost` (cost per day of a vessel with
#'   efficiency 1). Without `cost`, costs are `cost_share` times each
#'   métier's unfished revenue rate.
#' @param catchability catchability of each stock in each métier: a matrix
#'   (métiers x stocks, with dimnames) or a data frame with a `metier`
#'   column and one column per stock.
#' @param segments data frame, one row per fleet segment, with columns
#'   `segment`, `loyalty` (share of days on home métiers) and `home`
#'   (home métiers separated by `;`, empty = all); optional `efficiency`
#'   (on home métiers, default 1), `efficiency_away` (default `efficiency`),
#'   `cost_multiplier` (daily cost relative to a vessel of efficiency 1,
#'   default `efficiency`), `vessel_cv` (spread of vessel efficiency within
#'   the segment, default 0.25), `share_target` (target share of days,
#'   used by [ms_tune()]) and `capacity` (registered days, the default fleet
#'   capacity of the segment, e.g. for the app).
#' @param cost_share used when `metiers` has no `cost` column.
#' @param effort_season_amp seasonal amplitude of effort (0 = equal months;
#'   effort peaks in July).
#' @param F_cap cap on realised F (units of Fmsy) for stocks without an
#'   `F_cap` column value; `Inf` = no cap, 1.95 = floor at 0.05 Bmsy.
#' @return An object of class `ms_system`.
#' @seealso [atlantic_system()] for a ready-made example.
#' @export
#' @examples
#' sys <- atlantic_system()
#' sys
ms_system <- function(species, metiers, catchability, segments,
                      cost_share = 0.4, effort_season_amp = 0, F_cap = Inf) {
  species <- as.data.frame(species); metiers <- as.data.frame(metiers)
  segments <- as.data.frame(segments)
  need <- function(df, cols, what) {
    miss <- setdiff(cols, names(df))
    if (length(miss)) stop("`", what, "` needs column(s): ", paste(miss, collapse = ", "), call. = FALSE)
  }
  need(species, c("species", "r", "K", "price"), "species")
  need(metiers, "metier", "metiers")
  need(segments, c("segment", "loyalty", "home"), "segments")
  sp <- as.character(species$species); S <- length(sp)
  met <- as.character(metiers$metier); M <- length(met)
  if (anyDuplicated(sp) || anyDuplicated(met)) stop("stock and metier names must be unique", call. = FALSE)
  if (any(species$r <= 0) || any(species$K <= 0)) stop("`r` and `K` must be positive", call. = FALSE)

  # catchability: métiers x stocks, in the order of the tables
  Q <- if (is.data.frame(catchability)) {
    need(catchability, c("metier", sp), "catchability")
    q <- as.matrix(catchability[, sp, drop = FALSE]); rownames(q) <- catchability$metier; q
  } else as.matrix(catchability)
  if (!all(met %in% rownames(Q)) || !all(sp %in% colnames(Q)))
    stop("`catchability` needs rows for all metiers and columns for all stocks", call. = FALSE)
  Q <- Q[met, sp, drop = FALSE]
  storage.mode(Q) <- "double"
  if (any(Q < 0)) stop("catchability must be non-negative", call. = FALSE)

  # seasonal availability (months x stocks) and effort by month
  amp <- species$season_amp %||% rep(0, S); peak <- species$season_peak %||% rep(1, S)
  amp[is.na(amp)] <- 0; peak[is.na(peak)] <- 1
  avail <- t(vapply(1:12, function(mo)
    (1 - amp) + amp * (1 + cos(2 * pi * (mo - peak) / 12)) / 2, numeric(S)))
  colnames(avail) <- sp
  month_w <- 1 + effort_season_amp * cos(2 * pi * (1:12 - 7) / 12)
  month_w <- month_w / sum(month_w)

  # segments: home métiers, loyalty, efficiency, cost, vessel spread
  seg <- as.character(segments$segment); G <- length(seg)
  home <- matrix(0, G, M, dimnames = list(seg, met))
  for (g in seq_len(G)) {
    h <- trimws(strsplit(as.character(segments$home[g] %||% ""), ";")[[1]])
    h <- h[nzchar(h)]
    if (length(h) && !all(h %in% met))
      stop("unknown home metier(s) for segment ", seg[g], ": ", paste(setdiff(h, met), collapse = ", "), call. = FALSE)
    home[g, if (length(h)) h else met] <- 1
  }
  eff_home <- segments$efficiency %||% rep(1, G)
  eff_away <- segments$efficiency_away %||% eff_home
  eff <- home * eff_home + (1 - home) * eff_away
  cost_seg <- setNames(segments$cost_multiplier %||% eff_home, seg)
  vessel_cv <- segments$vessel_cv %||% rep(0.25, G)
  shares <- setNames(segments$share_target %||% rep(1 / G, G), seg)
  capacity <- if (is.null(segments$capacity) || all(is.na(segments$capacity))) NULL else
    setNames(as.numeric(segments$capacity), seg)

  rev0 <- as.vector(Q %*% (species$price * species$K * colMeans(avail)))
  cost <- metiers$cost %||% (cost_share * rev0)

  fcap <- species$F_cap %||% rep(F_cap, S); fcap[is.na(fcap)] <- F_cap
  dep0 <- species$dep_init %||% rep(1, S); dep0[is.na(dep0)] <- 1
  if (any(dep0 <= 0 | dep0 > 1.5)) stop("`dep_init` must be in (0, 1.5]", call. = FALSE)
  if (any(fcap <= 0)) stop("`F_cap` must be positive", call. = FALSE)

  mt <- data.frame(name = met, tactic = metiers$tactic %||% met, ground = metiers$ground %||% NA,
                   stringsAsFactors = FALSE)
  structure(list(
    species = sp, code = as.character(species$code %||% abbreviate(sp, 3)),
    group = as.character(species$group %||% rep("stock", S)),
    r = species$r, K = species$K, price = species$price, F_cap = fcap, dep_init = dep0,
    metiers = mt, Q = Q, avail = avail, month_w = month_w,
    segments = shares, capacity = capacity, home = home, loyalty = setNames(segments$loyalty, seg), eff = eff,
    cost_seg = cost_seg, vessel_cv = mean(vessel_cv),
    cost = setNames(cost, met), rev0 = setNames(rev0, met),
    S = S, M = M, G = G), class = "ms_system")
}

#' @export
print.ms_system <- function(x, ...) {
  cat("<ms_system>", x$S, "stocks,", x$M, "metiers,", x$G, "segments\n")
  cat("  stocks:  ", paste(x$code, collapse = ", "), "\n")
  cat("  metiers: ", paste(x$metiers$name, collapse = ", "), "\n")
  for (g in seq_len(x$G))
    cat(sprintf("  segment %-10s loyalty %.2f, efficiency %.2f, daily cost x%.3f, home: %s\n",
                names(x$segments)[g], x$loyalty[g], max(x$eff[g, ]), x$cost_seg[g],
                paste(colnames(x$home)[x$home[g, ] > 0], collapse = ", ")))
  if (any(is.finite(x$F_cap %||% Inf)))
    cat("  F cap:    ", paste(unique(format(x$F_cap)), collapse = ", "), "x Fmsy\n")
  if (!is.null(attr(x, "status_quo"))) cat("  (tuned; status quo attached)\n")
  invisible(x)
}

#' Read a system from CSV tables
#'
#' Reads `<prefix>_species.csv`, `<prefix>_metiers.csv`,
#' `<prefix>_catchability.csv` and `<prefix>_segments.csv` from a folder and
#' passes them to [ms_system()].
#'
#' @param dir folder with the tables.
#' @param prefix file prefix.
#' @param ... passed to [ms_system()].
#' @export
read_ms_system <- function(dir, prefix, ...) {
  rd <- function(x) utils::read.csv(file.path(dir, paste0(prefix, "_", x, ".csv")),
                                    check.names = FALSE, stringsAsFactors = FALSE)
  ms_system(rd("species"), rd("metiers"), rd("catchability"), rd("segments"), ...)
}

#' Write a system to CSV tables
#'
#' The inverse of [read_ms_system()]: writes the four input tables.
#'
#' @param sys an `ms_system`.
#' @inheritParams read_ms_system
#' @export
write_ms_system <- function(sys, dir, prefix) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  wr <- function(d, x) utils::write.csv(d, file.path(dir, paste0(prefix, "_", x, ".csv")), row.names = FALSE)
  amp <- 1 - apply(sys$avail, 2, min)
  wr(data.frame(species = sys$species, code = sys$code, group = sys$group, r = sys$r, K = sys$K,
                price = sys$price, season_amp = round(amp, 6),
                season_peak = apply(sys$avail, 2, which.max), F_cap = sys$F_cap %||% Inf, dep_init = sys$dep_init %||% 1), "species")
  wr(data.frame(metier = sys$metiers$name, tactic = sys$metiers$tactic, ground = sys$metiers$ground,
                cost = sys$cost), "metiers")
  wr(data.frame(metier = rownames(sys$Q), sys$Q, check.names = FALSE), "catchability")
  seg <- names(sys$segments)
  wr(data.frame(segment = seg, share_target = sys$segments, loyalty = sys$loyalty,
                efficiency = apply(sys$eff * sys$home, 1, max),
                efficiency_away = vapply(seq_len(sys$G), function(g) {
                  a <- sys$eff[g, sys$home[g, ] == 0]; if (length(a)) max(a) else max(sys$eff[g, ]) }, 0),
                cost_multiplier = sys$cost_seg, vessel_cv = sys$vessel_cv,
                capacity = if (is.null(sys$capacity)) NA else sys$capacity,
                home = vapply(seg, function(g) if (all(sys$home[g, ] > 0)) "" else
                  paste(colnames(sys$home)[sys$home[g, ] > 0], collapse = ";"), "")), "segments")
  invisible(dir)
}
