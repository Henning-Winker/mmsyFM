# Ported from the dev-mmsy prototype (cpueTargeting, branch dev-mmsy, dev/mmsy/R/msfleet.R).

#' Open-access and status-quo equilibria
#'
#' `ms_open_access()` runs the system to equilibrium with the caps set to the
#' fleet capacity, so the days fished are set by profit (the vessels that
#' break even). `ms_status_quo()` is the same with `capacity` days per
#' segment.
#' @param sys an `ms_system`.
#' @param years years to equilibrium.
#' @param cap capacity (days) by segment.
#' @param ... passed to [ms_simulate()].
#' @return A list with `E_seg` (days by segment), `B`, `summary`, `BBmsy` and `FFmsy`.
#' @export
ms_open_access <- function(sys, years = 500, cap = 1000, ...) {
  x <- ms_simulate(sys, matrix(rep_len(cap, sys$G), 1, sys$G), years = years, ...)
  s <- ms_summary(x)
  s$E_seg <- NULL
  list(E_seg = setNames(colMeans(x$E_seg[, 1, , drop = FALSE][, 1, ]), names(sys$segments)),
       B = colMeans(x$B[, 1, ]), summary = s, BBmsy = attr(s, "BBmsy")[1, ], FFmsy = attr(s, "FFmsy")[1, ])
}


#' @rdname ms_open_access
#' @param capacity days by segment (scalar or vector).
#' @export
ms_status_quo <- function(sys, capacity = 1000, ...) ms_open_access(sys, cap = capacity, ...)

# -----------------------------------------------------------------
# 6. Tuning: fleet composition as an open-access equilibrium
# -----------------------------------------------------------------

#' Tune daily costs so that a target fleet composition is an open-access equilibrium
#'
#' At open access each segment's active days are the share of its vessels
#' that break even (heterogeneous vessel efficiency), so the composition of
#' the active fleet is set by the daily costs. The G log cost multipliers are
#' solved so that G residuals vanish: the shares of days of the first G - 1
#' segments and one status target for the stocks,
#'   r = (log share_g / target_g, g < G;  log status / target).
#' Newton steps with a forward-difference Jacobian; the base point and the G
#' perturbed points run in one vectorised call (warm-started).
#'
#' @param shares  target share of active days by segment (default the
#'   `share_target` of the segment table)
#' @param status  c(median = x): median B/Bmsy; c(min = x): the weakest stock;
#'   c(E_total = x): total active days
#' @param cap     fleet capacity (registered days) by segment
#' @param ...     passed to ms_simulate() (gamma, ...)
#' @return the tuned system (cost_seg replaced), with attributes "tuning"
#'   (history), "status_quo" (the open-access equilibrium) and "converged"
ms_tune <- function(sys, shares = sys$segments,
                    status = c(median = 0.9), cap = 1000, max_iter = 25, tol = 0.02,
                    h = 0.05, max_step = 0.5, years_first = 400, years = 250, avg_years = 30,
                    verbose = TRUE, ...) {
  G <- sys$G; seg <- names(sys$segments)
  shares <- shares[seg] / sum(shares)
  stat_fun <- switch(names(status),
    median  = function(BB, E) apply(BB, 1, stats::median),
    min     = function(BB, E) apply(BB, 1, min),
    E_total = function(BB, E) rowSums(E),
    stop("status must be named median, min or E_total"))
  resid <- function(x) {
    E <- apply(x$E_seg, c(2, 3), mean); BB <- attr(ms_summary(x), "BBmsy")
    sh <- E / rowSums(E)
    cbind(log(pmax(sh[, -G, drop = FALSE], 1e-4) / matrix(shares[-G], nrow(E), G - 1, byrow = TRUE)),
          log(pmax(stat_fun(BB, E), 1e-4) / status))
  }
  lc <- log(sys$cost_seg); B0 <- NULL; E0 <- NULL; hist <- list(); conv <- FALSE
  best <- list(err = Inf, lc = lc)
  for (it in seq_len(max_iter)) {
    LC <- rbind(lc, t(lc + diag(h, G)))                    # base + G perturbations
    nr <- nrow(LC)
    x <- ms_simulate(sys, matrix(cap, nr, G, byrow = TRUE), years = if (it == 1) years_first else years,
                     cost_seg = exp(LC), capacity = matrix(cap, nr, G, byrow = TRUE),
                     B_init = if (is.null(B0)) NULL else B0[rep(1, nr), , drop = FALSE],
                     E_init = if (is.null(E0)) NULL else E0[rep(1, nr), , drop = FALSE],
                     avg_years = avg_years, ...)
    R <- resid(x); r0 <- R[1, ]
    E <- apply(x$E_seg, c(2, 3), mean)
    hist[[it]] <- data.frame(iter = it, t(setNames(round(exp(lc), 4), paste0("cost_", seg))),
                             t(setNames(round(E[1, ] / sum(E[1, ]), 3), paste0("share_", seg))),
                             status = round(exp(r0[G]) * status, 3), check.names = FALSE)
    if (verbose) cat(sprintf("iter %2d | costs %s | shares %s | %s %.3f\n", it,
                             paste(sprintf("%.3f", exp(lc)), collapse = "/"),
                             paste(sprintf("%.3f", E[1, ] / sum(E[1, ])), collapse = "/"),
                             names(status), exp(r0[G]) * status))
    if (max(abs(r0)) < best$err) best <- list(err = max(abs(r0)), lc = lc)
    if (max(abs(r0)) < tol) { conv <- TRUE; break }
    J <- t((R[-1, , drop = FALSE] - matrix(r0, G, G, byrow = TRUE)) / h)   # d r / d log c
    step <- tryCatch(-solve(J, r0), error = function(e) -0.3 * sign(r0))
    if (max(abs(step)) > max_step) step <- step * max_step / max(abs(step))
    lc <- lc + step
    B0 <- matrix(apply(x$B[, 1, , drop = FALSE], 3, mean), 1); E0 <- matrix(pmax(E[1, ], 1), 1)
  }
  sys$cost_seg <- setNames(exp(best$lc), seg)                 # best iterate
  attr(sys, "tuning") <- do.call(rbind, hist)
  attr(sys, "max_error") <- best$err
  attr(sys, "status_quo") <- ms_open_access(sys, cap = cap, avg_years = avg_years, ...)
  attr(sys, "converged") <- conv
  sys
}


#' Tune daily costs so that a target fleet composition is an open-access equilibrium
#'
#' At open access each segment's active days are the share of its vessels
#' that break even (heterogeneous vessel efficiency), so the composition of
#' the active fleet is set by the daily costs. The G log cost multipliers are
#' solved so that G residuals vanish: the shares of days of the first G - 1
#' segments and one status target for the stocks,
#'   r = (log share_g / target_g, g < G;  log status / target).
#' Newton steps with a forward-difference Jacobian; the base point and the G
#' perturbed points run in one vectorised call (warm-started).
#'
#' @param sys     an `ms_system`.
#' @param max_iter,tol,h,max_step Newton settings.
#' @param years_first,years,avg_years years of the first and later runs, and years averaged.
#' @param verbose print progress.
#' @param shares  target share of active days by segment (default the
#'   `share_target` of the segment table)
#' @param status  c(median = x): median B/Bmsy; c(min = x): the weakest stock;
#'   c(E_total = x): total active days
#' @param cap     fleet capacity (registered days) by segment
#' @param ...     passed to ms_simulate() (gamma, ...)
#' @return the tuned system (cost_seg replaced), with attributes "tuning"
#'   (history), "status_quo" (the open-access equilibrium) and "converged"
#' @export
ms_tune <- function(sys, shares = sys$segments,
                    status = c(median = 0.9), cap = 1000, max_iter = 25, tol = 0.02,
                    h = 0.05, max_step = 0.5, years_first = 400, years = 250, avg_years = 30,
                    verbose = TRUE, ...) {
  G <- sys$G; seg <- names(sys$segments)
  shares <- shares[seg] / sum(shares)
  stat_fun <- switch(names(status),
    median  = function(BB, E) apply(BB, 1, stats::median),
    min     = function(BB, E) apply(BB, 1, min),
    E_total = function(BB, E) rowSums(E),
    stop("status must be named median, min or E_total"))
  resid <- function(x) {
    E <- apply(x$E_seg, c(2, 3), mean); BB <- attr(ms_summary(x), "BBmsy")
    sh <- E / rowSums(E)
    cbind(log(pmax(sh[, -G, drop = FALSE], 1e-4) / matrix(shares[-G], nrow(E), G - 1, byrow = TRUE)),
          log(pmax(stat_fun(BB, E), 1e-4) / status))
  }
  lc <- log(sys$cost_seg); B0 <- NULL; E0 <- NULL; hist <- list(); conv <- FALSE
  best <- list(err = Inf, lc = lc)
  for (it in seq_len(max_iter)) {
    LC <- rbind(lc, t(lc + diag(h, G)))                    # base + G perturbations
    nr <- nrow(LC)
    x <- ms_simulate(sys, matrix(cap, nr, G, byrow = TRUE), years = if (it == 1) years_first else years,
                     cost_seg = exp(LC), capacity = matrix(cap, nr, G, byrow = TRUE),
                     B_init = if (is.null(B0)) NULL else B0[rep(1, nr), , drop = FALSE],
                     E_init = if (is.null(E0)) NULL else E0[rep(1, nr), , drop = FALSE],
                     avg_years = avg_years, ...)
    R <- resid(x); r0 <- R[1, ]
    E <- apply(x$E_seg, c(2, 3), mean)
    hist[[it]] <- data.frame(iter = it, t(setNames(round(exp(lc), 4), paste0("cost_", seg))),
                             t(setNames(round(E[1, ] / sum(E[1, ]), 3), paste0("share_", seg))),
                             status = round(exp(r0[G]) * status, 3), check.names = FALSE)
    if (verbose) cat(sprintf("iter %2d | costs %s | shares %s | %s %.3f\n", it,
                             paste(sprintf("%.3f", exp(lc)), collapse = "/"),
                             paste(sprintf("%.3f", E[1, ] / sum(E[1, ])), collapse = "/"),
                             names(status), exp(r0[G]) * status))
    if (max(abs(r0)) < best$err) best <- list(err = max(abs(r0)), lc = lc)
    if (max(abs(r0)) < tol) { conv <- TRUE; break }
    J <- t((R[-1, , drop = FALSE] - matrix(r0, G, G, byrow = TRUE)) / h)   # d r / d log c
    step <- tryCatch(-solve(J, r0), error = function(e) -0.3 * sign(r0))
    if (max(abs(step)) > max_step) step <- step * max_step / max(abs(step))
    lc <- lc + step
    B0 <- matrix(apply(x$B[, 1, , drop = FALSE], 3, mean), 1); E0 <- matrix(pmax(E[1, ], 1), 1)
  }
  sys$cost_seg <- setNames(exp(best$lc), seg)                 # best iterate
  attr(sys, "tuning") <- do.call(rbind, hist)
  attr(sys, "max_error") <- best$err
  attr(sys, "status_quo") <- ms_open_access(sys, cap = cap, avg_years = avg_years, ...)
  attr(sys, "converged") <- conv
  sys
}



#' Equilibrium yield curve over effort
#'
#' Scales a reference effort vector (days by segment) by `mult` and runs each
#' scaled vector to equilibrium. The scaled days are the caps (registered
#' days); the days fished are those of the vessels that break even, so the
#' curve ends where effort is limited by profit.
#'
#' @param sys an `ms_system`.
#' @param E_ref reference days by segment (e.g. the status quo).
#' @param mult multipliers of `E_ref`.
#' @param years years to equilibrium.
#' @param Bpa safeguard relative to Bmsy.
#' @param ... passed to [ms_simulate()] (gamma, open, ...).
#' @return A data frame with `E` (days fished), `yield`, `profit`, `min_BBmsy`
#'   and `n_below_Bpa`, with attribute `BBmsy`.
#' @export
ms_yield_curve <- function(sys, E_ref, mult = c(0, 10^seq(-1.5, log10(8), length.out = 60)),
                           years = 300, Bpa = 0.5, ...) {
  caps <- outer(mult, E_ref)
  s <- ms_summary(ms_simulate(sys, caps, years, capacity = caps, ...), Bpa = Bpa)
  BB <- attr(s, "BBmsy")
  out <- data.frame(mult = mult, E = s$E_total, yield = s$yield, profit = s$profit,
                    min_BBmsy = apply(BB, 1, min), n_below_Bpa = rowSums(BB < Bpa))
  attr(out, "BBmsy") <- BB
  out
}

#' Shared-q yield curve (the mse.SEA model)
#'
#' The classical multispecies model with one effort and a fixed catchability
#' vector, calibrated so that it reproduces the fishing mortality of a
#' reference state: q_i = F_i / E, with B_i/K_i = 1 - q_i E / r_i at
#' equilibrium.
#'
#' @param sys an `ms_system`.
#' @param ref a reference state from [ms_open_access()] or
#'   [ms_status_quo()] (uses its `FFmsy` and `E_seg`).
#' @param mult multipliers of the reference total days.
#' @param Bpa safeguard relative to Bmsy.
#' @return A data frame as [ms_yield_curve()].
#' @export
ms_shared_q_curve <- function(sys, ref, mult = 10^seq(-1.5, log10(4), length.out = 60), Bpa = 0.5) {
  E0 <- sum(ref$E_seg)
  q <- ref$FFmsy * sys$r / 2 / E0
  E <- c(0, mult) * E0
  BK <- pmax(outer(E, q / sys$r, function(e, a) 1 - e * a), 0)
  out <- data.frame(mult = c(0, mult), E = E, yield = rowSums(outer(E, q * sys$K) * BK), profit = NA,
                    min_BBmsy = apply(2 * BK, 1, min), n_below_Bpa = rowSums(2 * BK < Bpa))
  attr(out, "BBmsy") <- 2 * BK
  out
}
