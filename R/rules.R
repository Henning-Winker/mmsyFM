# Ported from the dev-mmsy prototype (cpueTargeting, branch dev-mmsy, dev/mmsy/R/msfleet.R).

#' Segment effort rule with an optional choke-stock closure
#'
#' @param sys an `ms_system`.
#' @param Btrig,Blim trigger and limit relative to Bmsy (effort rule).
#' @param Btrig_close trigger of the closure.
#' @param assess_cv,assess_rho assessment error CV and its AR(1).
#' @param own_q a segment answers for a stock it can catch with >= `own_q` of the stock's highest catchability.
#' @param close stocks or complexes protected by a closure.
#' @param close_q métiers with >= `close_q` of the stock's highest catchability are closed.
#' @param Emin minimum effort: the lowest fraction of its reference days a
#'   segment is set to (at and below Blim), default 0.1. F scales with days,
#'   so the floor puts about `Emin` x the reference F on each stock; 0 lets the
#'   rule close a segment completely.
#' @param max_change limit on the annual change of each segment's fraction of
#'   its reference days, in absolute terms (0.2 = at most 20% of the
#'   reference days up or down per year), so a closed segment can reopen.
#'   The first year starts from the reference days (fraction 1).
#' @param assessed,assign,complexes,complex_index see Details.
#' @details
#'
#' Each year the assessed B/Bmsy of every stock is the true value times a
#' lognormal assessment error (CV `assess_cv`, AR(1) `assess_rho`). Each
#' segment's days are a fraction of its reference days (the caps passed to
#' ms_simulate(), e.g. the status-quo days): the fraction ramps from `Emin`
#' at Blim (and below) to 1 at Btrig (both relative to Bmsy) of the most depleted stock the
#' segment can catch substantially (any usable metier has >= `own_q` of the
#' stock's highest catchability).
#'
#' Stocks in `close` are protected by a closure instead: the open share of
#' the metiers that drive them (catchability >= `close_q` of the stock's
#' highest) follows the same ramp, and they are left out of the effort rule.
#' `max_change` limits the annual change of each segment's fraction (in
#' units of the reference days).
#'
#' `Btrig` (effort rule) and `Btrig_close` (closure) may be vectors with one
#' value per row of the simulation, so a grid of triggers runs in one call.
#'
#' `assign`: nominal assignment of stocks to segments for management, a named
#' list segment -> stock names (as in practice: a species is "managed" through
#' one fleet although others catch it). It replaces the catchability-based
#' responsibility; segments without stocks are not constrained by the rule.
#'
#' `complexes`: named list of stock complexes assessed as one unit (pooled
#' taxa, e.g. reef fish), the remaining stocks are single units. The complex
#' index is pooled B/Bmsy (`complex_index = "biomass"`: sum B / sum Bmsy, as a
#' production model fitted to pooled catch and biomass) or a pooled catch rate
#' (`"cpue"`: taxa weighted by their catchability). `close`, `assign` and
#' `assessed` then refer to unit names (complexes or stocks).
#'
#' `assessed`: names of the stocks with an assessment (default all). The rule
#' sees only these; a stock in `close` must be assessed.
#'
#' Create a new rule for every simulation run (it keeps the last fraction and
#' the assessment error between years).
#' @return A rule function for the `hcr` argument of [ms_simulate()], with attributes `own`, `drive` and `units`.
#' @export
hcr_effort <- function(sys, Btrig = 0.8, Blim = 0.25, assess_cv = 0.2, assess_rho = 0,
                       own_q = 0.2, close = NULL, close_q = 0.5, max_change = Inf, Emin = 0.1,
                       Btrig_close = Btrig, assessed = NULL, assign = NULL,
                       complexes = NULL, complex_index = c("biomass", "cpue")) {
  complex_index <- match.arg(complex_index)
  S <- sys$S; G <- sys$G; M <- sys$M; seg <- names(sys$segments)
  qmax <- apply(sys$Q, 2, max)
  # assessment units: complexes (pooled taxa) and the remaining single stocks
  units <- c(complexes, as.list(setNames(setdiff(sys$species, unlist(complexes)),
                                         setdiff(sys$species, unlist(complexes)))))
  U <- length(units); un <- names(units)
  w <- if (complex_index == "cpue") qmax else rep(1, S)       # cpue: pooled catch rates weight taxa by catchability
  W <- sapply(units, function(m) w * (sys$species %in% m))     # S x U membership weights
  own_sp <- sapply(seq_len(G), function(g) {
    acc <- if (sys$loyalty[g] > 0) sys$home[g, ] > 0 else rep(TRUE, M)
    apply(sys$Q[acc, , drop = FALSE], 2, max) >= own_q * qmax })   # S x G
  own <- (t(W > 0) %*% own_sp) > 0                                 # U x G: a segment answers for a unit
  if (!is.null(assign)) {                                          # if it catches any member
    own[] <- FALSE
    for (g in names(assign)) own[un %in% assign[[g]], match(g, seg)] <- TRUE
  }
  if (!is.null(assessed)) {
    if (!all(close %in% assessed)) stop("a unit in `close` must be assessed")
    own[!un %in% assessed, ] <- FALSE
  }
  ic <- match(close, un)
  if (anyNA(ic)) stop("`close` must name stocks or complexes")
  if (length(ic)) own[ic, ] <- FALSE
  drive_sp <- sys$Q >= close_q * matrix(qmax, M, S, byrow = TRUE)  # M x S
  drive <- (drive_sp %*% (W > 0)) > 0                              # M x U
  Bmsy_w <- colSums(W * sys$K / 2)
  sig <- if (assess_cv > 0) sqrt(log(1 + assess_cv^2)) else 0
  ramp <- function(d, bt) pmin(1, pmax(0, (d - Blim) / (bt - Blim)))
  prev <- NULL; eps <- NULL
  rule <- function(st) {
    n <- nrow(st$B)
    if (st$year == 1) { prev <<- NULL; eps <<- NULL }
    nd <- st$nd %||% n; idx <- st$idx %||% seq_len(n)
    z <- matrix(stats::rnorm(nd * U), nd, U)[idx, , drop = FALSE]
    bt <- rep_len(Btrig, n); btc <- rep_len(Btrig_close, n)
    eps <<- if (is.null(eps) || assess_rho == 0) z else assess_rho * eps + sqrt(1 - assess_rho^2) * z
    true_u <- (st$B %*% W) / matrix(Bmsy_w, n, U, byrow = TRUE)   # pooled B/Bmsy by unit
    bb <- true_u * exp(sig * eps - sig^2 / 2)                       # assessed
    mult <- matrix(sapply(seq_len(G), function(g)
      if (any(own[, g])) Emin + (1 - Emin) * ramp(apply(bb[, own[, g], drop = FALSE], 1, min), bt)
      else rep(1, n)), n, G)
    if (is.finite(max_change)) {
      if (is.null(prev)) prev <- matrix(1, n, G)   # start from the reference (status-quo) days
      mult <- pmin(pmax(mult, prev - max_change), prev + max_change)   # absolute: zero is not a trap
    }
    prev <<- mult
    if (!length(ic)) return(mult)
    opm <- matrix(1, n, M)
    for (u in ic) for (m in which(drive[, u])) opm[, m] <- pmin(opm[, m], ramp(bb[, u], btc))
    list(mult = mult, open = opm)
  }
  attr(rule, "own") <- own; attr(rule, "drive") <- drive; attr(rule, "units") <- units
  rule
}


#' Process SD by productivity: a power law in r through two anchors
#'
#' sd(r) = sd1 * (r / r1)^p with p = log(sd2 / sd1) / log(r2 / r1).
#' Default anchors: sharks (r = 0.057, sd = 0.05) and anchovy (r = 1.2, sd = 0.4),
#' giving sd = 0.355 * r^0.68.
#' @param r intrinsic growth rates.
#' @param r1,sd1,r2,sd2 the two anchors.
#' @return Process SD (log scale) for each r.
#' @export
proc_sd_from_r <- function(r, r1 = 0.057, sd1 = 0.05, r2 = 1.2, sd2 = 0.4) {
  p <- log(sd2 / sd1) / log(r2 / r1)
  sd1 * (r / r1)^p
}


#' JABBA `proc.dyn` relative process SD
#'
#' relsig(x) = raw(x) / raw(1), raw(x) = a + (1 - a) / (1 + (x / c)^d),
#' with x = B/Bmsy (empirical ICES a = 0.128, c = 0.223, d = 1.326), as in
#' JABBA's `jabba2jags.R`. relsig = 1 at Bmsy.
#' @param BBmsy B/Bmsy.
#' @param a,c,d parameters.
#' @return relsig for each B/Bmsy.
#' @export
relsig_jabba <- function(BBmsy, a = 0.128, c = 0.223, d = 1.326) {
  raw <- function(x) a + (1 - a) / (1 + (x / c)^d)
  raw(BBmsy) / raw(1)
}

