# Ported from the dev-mmsy prototype (cpueTargeting, branch dev-mmsy, dev/mmsy/R/msfleet.R).

#' Maximise an equilibrium objective over segment effort caps
#'
#' Finds the effort caps that maximise total yield (or revenue, or profit)
#' at equilibrium subject to every constrained stock staying at or above
#' Bpa: B_i / Bmsy_i >= Bpa for all i in `species`.
#'
#' The search is a vectorised zooming grid in log1p(effort) space: a grid of
#' n^d candidates is run in one call, the best feasible one is kept, and the
#' box shrinks around it. This is robust to the kinks and small cycles of a
#' behavioural fleet, which defeat gradient-based optimisers.
#'
#' @param sys     an `ms_system`.
#' @param objective what is maximised: `"yield"` (total catch), `"revenue"`,
#'   `"profit"`, or the catch of one or more stocks given by code or name
#'   (e.g. `"ANC"` to maximise the anchovy catch), always subject to the
#'   safeguard for the stocks in `species`.
#' @param Bpa     safeguard relative to Bmsy.
#' @param n,zoom  grid points per dimension and number of zoom steps.
#' @param years   years to equilibrium.
#' @param levers  segments whose caps are optimised, or "total" for one global
#'   cap split in proportion `ratio` across all segments
#' @param fixed   named caps for segments not in `levers` (e.g. the free
#'   agent: mixed = 40)
#' @param ratio   split of the global cap when levers = "total"
#' @param upper   upper bound of each cap
#' @param species names of the stocks under the safeguard (default all)
#' @param min_share viability floor: every segment's cap must be at least this
#'   share of the total cap (e.g. 0.2). Applies to caps by segment only (with
#'   `levers = "total"` the split is fixed by `ratio`).
#' @param ...     passed to ms_simulate() (gamma, behaviour, phi, open, ...)
#' @return list: caps, summary row, B/Bmsy, F/Fmsy and catch by stock, the value
#'   of the objective, effort by segment and
#'   metier, the binding stock(s), and the search history
#' @export
ms_optimise <- function(sys, objective = "yield",
                        levers = names(sys$segments), fixed = NULL,
                        ratio = rep(1 / sys$G, sys$G), upper = 3000,
                        Bpa = 0.5, species = sys$species, min_share = 0, n = NULL, zoom = 5,
                        years = 150, ...) {
  # objective: total yield, revenue or profit, or the catch of one or more stocks
  obj_stock <- NULL
  if (!(length(objective) == 1 && objective %in% c("yield", "revenue", "profit"))) {
    obj_stock <- match(objective, sys$code); obj_stock[is.na(obj_stock)] <- match(objective[is.na(obj_stock)], sys$species)
    if (anyNA(obj_stock)) stop("`objective` must be \"yield\", \"revenue\", \"profit\" or stock code(s)/name(s)", call. = FALSE)
  }
  seg <- names(sys$segments)
  # with one global cap the split across segments is fixed (ratio): a viability floor
  # cannot be met by optimising and would only admit zero effort, so it does not apply
  if (identical(levers, "total")) min_share <- 0
  d <- if (identical(levers, "total")) 1 else length(levers)
  n <- n %||% c(41, 19, 11)[min(d, 3)]
  lo <- rep(0, d); hi <- rep(log1p(upper), d)
  to_caps <- function(X) {                      # X: k x d in log1p space
    E <- expm1(X)
    if (identical(levers, "total")) return(outer(E[, 1], ratio))
    caps <- matrix(0, nrow(E), sys$G, dimnames = list(NULL, seg))
    caps[, levers] <- E
    for (g in names(fixed)) caps[, g] <- fixed[[g]]
    caps
  }
  isp <- match(species, sys$species)
  best <- NULL; hist <- list()
  for (z in seq_len(zoom)) {
    axes <- lapply(seq_len(d), function(k) seq(lo[k], hi[k], length.out = n))
    X <- as.matrix(expand.grid(axes))
    caps <- to_caps(X)
    sm <- ms_summary(ms_simulate(sys, caps, years = years, ...), Bpa = Bpa)
    BB <- attr(sm, "BBmsy")
    feas <- apply(BB[, isp, drop = FALSE], 1, min) >= Bpa &
      apply(caps, 1, min) >= min_share * rowSums(caps)
    raw <- if (is.null(obj_stock)) sm[[objective]] else rowSums(attr(sm, "catch")[, obj_stock, drop = FALSE])
    val <- raw; val[!feas] <- -Inf
    i <- which.max(val)
    hist[[z]] <- data.frame(zoom = z, caps, value = raw, feasible = feas)
    if (!is.finite(val[i])) break             # nothing feasible (e.g. free agent too large)
    best <- list(X = X[i, ], caps = caps[i, ], summary = sm[i, ], BBmsy = BB[i, ],
                 FFmsy = attr(sm, "FFmsy")[i, ], catch = attr(sm, "catch")[i, ], value = raw[i])
    step <- (hi - lo) / (n - 1)
    lo <- pmax(0, X[i, ] - 2 * step); hi <- pmin(log1p(upper), X[i, ] + 2 * step)
  }
  if (is.null(best)) return(list(feasible = FALSE, history = do.call(rbind, hist),
                                 reason = "no caps keep every constrained stock at or above Bpa"))
  if (sum(best$caps) <= 0)
    return(list(feasible = FALSE, history = do.call(rbind, hist),
                reason = "only zero effort meets the constraints (e.g. a viability floor above a segment's share)"))
  # rerun the optimum to keep effort by segment and metier
  x <- ms_simulate(sys, matrix(best$caps, 1), years = years, ...)
  best$E_seg <- setNames(colMeans(x$E_seg[, 1, , drop = FALSE][, 1, ]), seg)
  best$E_seg_metier <- matrix(apply(x$E_seg_metier[, 1, , , drop = FALSE], c(3, 4), mean),
                              sys$G, sys$M, dimnames = list(seg, sys$metiers$name))
  best$binding <- sys$species[isp][abs(best$BBmsy[isp] - Bpa) < 0.05]
  best$feasible <- TRUE
  best$objective <- if (is.null(obj_stock)) objective else paste("catch of", paste(sys$code[obj_stock], collapse = " + "))
  best$objective_stocks <- if (is.null(obj_stock)) NULL else sys$code[obj_stock]
  best$history <- do.call(rbind, hist)
  best
}

