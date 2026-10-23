# Ported from the dev-mmsy prototype (cpueTargeting, branch dev-mmsy, dev/mmsy/R/msfleet.R).

#' Segment effort rule with an optional choke-stock closure
#'
#' @param sys an `ms_system`.
#' @param Btrig,Blim trigger and limit relative to Bmsy (effort rule). The
#'   default trigger is 0.5 Bmsy, i.e. at Bpa, as is common in ICES and GFCM
#'   advice. `Btrig` may be named by segment to set triggers by segment (e.g.
#'   `c(trawl = 0.5, purse_seine = 0.8, small_scale = 0.5)`; unnamed segments
#'   get 0.5), or be a vector with one value per simulation row (a grid of
#'   triggers, as in [ms_tune_hcr()]).
#' @param Btrig_close trigger of the closure.
#' @param assess_cv,assess_rho assessment error CV and its AR(1).
#' @param own_q a segment answers for a stock it catches as a main fishery: one of
#'   its metiers has >= `own_q` (default 0.5) of the stock's highest
#'   catchability. Lower values (e.g. 0.2) also count by-catch.
#' @param seed seed of the rule's own random stream (assessment errors), kept
#'   apart from the simulation's, so rules compared with the same simulation
#'   seed see the same process and implementation errors.
#' @param aggregate how a segment's status is formed from the assessed B/Bmsy
#'   of the units it answers for: `"weighted"` (default), the mean weighted by
#'   each unit's potential revenue for the segment (catchability on its
#'   metiers x price x K), so the stocks that make up its fishery drive its
#'   effort; or `"min"`, the weakest link (the most depleted unit sets the
#'   segment's effort), a special, most precautionary case.
#' @param close stocks or complexes protected by a closure.
#' @param close_q métiers with >= `close_q` of the stock's highest catchability are closed.
#' @param Emin minimum effort: the lowest fraction of its reference days a
#'   segment is set to (at and below Blim), default 0.1. F scales with days,
#'   so the floor puts about `Emin` x the reference F on each stock; 0 lets the
#'   rule close a segment completely.
#' @param max_change limit on the annual change of each segment's days,
#'   relative to the previous year (0.2 = at most 20% up or down), as for TAC
#'   changes. The first year starts from the reference days (fraction 1); a
#'   segment at zero (with `Emin = 0`) can reopen from 5% of its reference
#'   days.
#' @param assessed,assign,complexes,complex_index see Details.
#' @details
#'
#' Each year the assessed B/Bmsy of every stock is the true value times a
#' lognormal assessment error (CV `assess_cv`, AR(1) `assess_rho`). Each
#' segment's days are a fraction of its reference days, the caps passed to
#' ms_simulate(): normally the safeguarded-MMSY effort from [ms_optimise()]
#' (the "full" effort above the trigger), or the status-quo days. The
#' fraction ramps from `Emin` at Blim (and below) to 1 at Btrig (both
#' relative to Bmsy) of the segment's status: the value-weighted mean
#' (`aggregate = "weighted"`) or the minimum (`"min"`, weakest link) of the
#' assessed B/Bmsy of the units it catches as a main fishery (`own_q`). With
#' `max_change`, the first year starts from the current days relative to the
#' reference days, so the move from the status quo to the reference effort
#' is phased.
#'
#' Stocks in `close` are protected by a closure instead: the open share of
#' the metiers that drive them (catchability >= `close_q` of the stock's
#' highest) follows the same ramp, and they are left out of the effort rule.
#' `max_change` limits the annual change of each segment's days relative to
#' the previous year.
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
hcr_effort <- function(sys, Btrig = 0.5, Blim = 0.25, assess_cv = 0.2, assess_rho = 0,
                       own_q = 0.5, close = NULL, close_q = 0.5, max_change = Inf, Emin = 0.1,
                       Btrig_close = Btrig, assessed = NULL, assign = NULL,
                       complexes = NULL, complex_index = c("biomass", "cpue"),
                       aggregate = c("weighted", "min"), seed = 1) {
  complex_index <- match.arg(complex_index); aggregate <- match.arg(aggregate)
  S <- sys$S; G <- sys$G; M <- sys$M; seg <- names(sys$segments)
  qmax <- apply(sys$Q, 2, max)
  # assessment units: complexes (pooled taxa) and the remaining single stocks
  units <- c(complexes, as.list(setNames(setdiff(sys$species, unlist(complexes)),
                                         setdiff(sys$species, unlist(complexes)))))
  U <- length(units); un <- names(units)
  w <- if (complex_index == "cpue") qmax else rep(1, S)       # cpue: pooled catch rates weight taxa by catchability
  W <- sapply(units, function(m) w * (sys$species %in% m))     # S x U membership weights
  acc_g <- lapply(seq_len(G), function(g) if (sys$loyalty[g] > 0) sys$home[g, ] > 0 else rep(TRUE, M))
  own_sp <- sapply(seq_len(G), function(g)
    apply(sys$Q[acc_g[[g]], , drop = FALSE], 2, max) >= own_q * qmax)   # S x G
  # value weights: each stock's potential revenue for the segment (catchability on its
  # metiers x price x K), pooled by unit
  val_sp <- sapply(seq_len(G), function(g)
    colSums(sys$Q[acc_g[[g]], , drop = FALSE]) * sys$price * sys$K)   # S x G
  val_u <- t(W > 0) %*% val_sp                                     # U x G
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
    # assessment errors from the rule's own random stream (seed + year), so the process
    # errors of the simulation are the same whatever the rule (common random numbers)
    has_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
    old <- if (has_seed) get(".Random.seed", envir = globalenv())
    set.seed(seed * 1000 + st$year)
    z <- matrix(stats::rnorm(nd * U), nd, U)[idx, , drop = FALSE]
    if (has_seed) assign(".Random.seed", old, envir = globalenv()) else rm(".Random.seed", envir = globalenv())
    # trigger: by segment (named by segment) or by simulation row (a grid, see ms_tune_hcr())
    btm <- if (!is.null(names(Btrig)) && all(names(Btrig) %in% seg))
      matrix(c(Btrig, setNames(rep(0.5, G), seg))[seg], n, G, byrow = TRUE)
      else matrix(rep_len(Btrig, n), n, G)
    btc <- rep_len(Btrig_close, n)
    eps <<- if (is.null(eps) || assess_rho == 0) z else assess_rho * eps + sqrt(1 - assess_rho^2) * z
    true_u <- (st$B %*% W) / matrix(Bmsy_w, n, U, byrow = TRUE)   # pooled B/Bmsy by unit
    bb <- true_u * exp(sig * eps - sig^2 / 2)                       # assessed
    status_g <- function(g) {                                       # the status that drives segment g
      b <- bb[, own[, g], drop = FALSE]
      if (aggregate == "min") return(apply(b, 1, min))             # weakest link
      w <- val_u[own[, g], g]; w <- if (sum(w) > 0) w / sum(w) else rep(1 / length(w), length(w))
      as.vector(b %*% w)                                           # value-weighted mean
    }
    mult <- matrix(sapply(seq_len(G), function(g)
      if (any(own[, g])) Emin + (1 - Emin) * ramp(status_g(g), btm[, g]) else rep(1, n)), n, G)
    if (is.finite(max_change)) {
      # start from the current days relative to the reference days (e.g. status quo
      # relative to the MMSY effort), so the transition is phased
      if (is.null(prev)) prev <- if (!is.null(st$E) && !is.null(st$caps))
        matrix(pmin(st$E / pmax(st$caps, 1e-9), 10), n, G) else matrix(1, n, G)
      # relative to last year's fraction; the upper bound starts from at least 0.05 so a
      # segment at zero (Emin = 0) can reopen
      mult <- pmin(pmax(mult, prev * (1 - max_change)), pmax(prev, 0.05) * (1 + max_change))
    }
    prev <<- mult
    if (!length(ic)) return(mult)
    opm <- matrix(1, n, M)
    for (u in ic) for (m in which(drive[, u])) opm[, m] <- pmin(opm[, m], ramp(bb[, u], btc))
    list(mult = mult, open = opm)
  }
  attr(rule, "own") <- own; attr(rule, "drive") <- drive; attr(rule, "units") <- units
  attr(rule, "weights") <- val_u * own
  rule
}


#' Default assessment strategies: indicator species, complexes and assignment
#'
#' Starting points for the `assessed`, `complexes` and `assign` arguments of
#' [hcr_effort()], derived from the system:
#'
#' * `indicators`: one indicator species per segment, the stock of highest
#'   potential value to that segment (catchability on its metiers x price x
#'   K) among the stocks it catches as a main fishery, without repeats;
#' * `complexes`: the species `group`s of the system with two or more
#'   stocks (stocks alone in their group stay single units);
#' * `assign`: each stock assigned to the segment for whose fishery it
#'   matters most (its largest share of the segment's potential value), its
#'   "main" fleet.
#'
#' @param sys an `ms_system`.
#' @param own_q as in [hcr_effort()].
#' @return A list with `indicators` (stock names), `complexes` (named list of
#'   stock names) and `assign` (named list segment -> stock names).
#' @export
#' @examples
#' ms_assessment_presets(atlantic_system())
ms_assessment_presets <- function(sys, own_q = 0.5) {
  seg <- names(sys$segments); M <- sys$M
  qmax <- apply(sys$Q, 2, max)
  acc <- lapply(seq_len(sys$G), function(g) if (sys$loyalty[g] > 0) sys$home[g, ] > 0 else rep(TRUE, M))
  val <- sapply(seq_len(sys$G), function(g) colSums(sys$Q[acc[[g]], , drop = FALSE]) * sys$price * sys$K)
  own <- sapply(seq_len(sys$G), function(g) apply(sys$Q[acc[[g]], , drop = FALSE], 2, max) >= own_q * qmax)
  val <- matrix(val, sys$S, sys$G); own <- matrix(own, sys$S, sys$G)
  ind <- character(0)
  for (g in order(-colSums(val))) {                       # most valuable segment picks first
    v <- val[, g]; v[!own[, g] | sys$species %in% ind] <- -Inf
    if (any(is.finite(v))) ind <- c(ind, sys$species[which.max(v)])
  }
  grp <- split(sys$species, factor(sys$group, unique(sys$group)))
  main <- seg[apply(sweep(val, 2, pmax(colSums(val), 1e-12), "/"), 1, which.max)]
  list(indicators = sys$species[sys$species %in% ind],
       complexes = grp[lengths(grp) > 1],
       assign = lapply(stats::setNames(seg, seg), function(g) sys$species[main == g]))
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

