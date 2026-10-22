# Ported from the dev-mmsy prototype (cpueTargeting, branch dev-mmsy, dev/mmsy/R/msfleet.R).

#' Allocate segment effort to metiers
#'
#' share_gm = loyalty_g * P(m | home_g) + (1 - loyalty_g) * P(m | all)
#' P(m | set) proportional to open_m * pmax(profit_gm, 0)^gamma, with the
#' segment's own profit per unit (nominal) effort
#'   profit_gm = eff_gm * value_m - cost_m
#' gamma = 0: profit-blind (equal shares); larger gamma: stronger preference
#' for the most profitable metier. Returns an n x M matrix of nominal effort
#' by metier, with attributes "by_seg" (n x G x M nominal effort) and
#' "effective" (n x M effort weighted by efficiency, which drives F).
#' @param sys    an `ms_system`.
#' @param gamma  preference exponent.
#' @param by_seg kept for compatibility (effort by segment is always returned).
#' @param cseg   n x G daily cost multiplier by segment (default `sys$cost_seg`).
#' @param value  n x M expected revenue per unit of effective effort
#' @param cost   n x M cost per unit of nominal effort
#' @param emult  n x G mean efficiency of the active vessels relative to the segment mean
#' @param E_seg  n x G effort by segment
#' @param open   n x M or length-M 0/1 (closures)
#' @return An n x M matrix of nominal effort by métier, with attributes `by_seg` and `effective`.
#' @keywords internal
#' @export
allocate_effort <- function(sys, value, E_seg, gamma, open = 1, cost = 0, by_seg = TRUE, emult = NULL,
                            cseg = NULL) {
  n <- nrow(value); M <- sys$M
  cost <- if (is.matrix(cost)) cost else matrix(cost, n, M, byrow = TRUE)
  op <- if (is.matrix(open)) open else matrix(open, n, M, byrow = TRUE)
  E_m <- matrix(0, n, M); E_eff <- E_m
  E_gm <- array(0, c(n, sys$G, M))
  if (is.null(cseg)) cseg <- matrix(sys$cost_seg, n, sys$G, byrow = TRUE)
  all_set <- rep(1, M)
  for (g in seq_len(sys$G)) {
    effg <- matrix(sys$eff[g, ], n, M, byrow = TRUE)
    if (!is.null(emult)) effg <- effg * emult[, g]     # mean efficiency of the active vessels
    util <- if (gamma == 0) matrix(1, n, M) else pmax(value * effg - cost * cseg[, g], 0)^gamma
    util <- util * op
    for (part in c("home", "free")) {
      lam <- if (part == "home") sys$loyalty[g] else 1 - sys$loyalty[g]
      if (lam == 0) next
      acc <- if (part == "home") sys$home[g, ] else all_set
      u <- util * matrix(acc, n, M, byrow = TRUE)
      tot <- rowSums(u)
      p <- u / ifelse(tot > 0, tot, 1)          # rows with no profitable option: no effort
      E_add <- p * (lam * E_seg[, g])
      E_m <- E_m + E_add; E_eff <- E_eff + E_add * effg
      E_gm[, g, ] <- E_gm[, g, ] + E_add
    }
  }
  attr(E_m, "by_seg") <- E_gm; attr(E_m, "effective") <- E_eff
  E_m
}

