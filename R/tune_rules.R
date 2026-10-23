# Ported from the dev-mmsy prototype (cpueTargeting, branch dev-mmsy, dev/mmsy/R/msfleet.R).

#' Tune the triggers of hcr_effort() to a risk criterion
#'
#' Runs a grid of effort-rule triggers (and closure triggers, if `close` is
#' given) in one vectorised call with common random numbers: every grid point
#' sees the same process and assessment errors. The tuned rule maximises the
#' objective among the grid points that meet the risk limit.
#'
#' @param sys an `ms_system`.
#' @param B_init,E_init initial biomass and days (the status quo).
#' @param nsim,years,eval_years,seed simulations, years, years evaluated, seed.
#' @param Blim,assess_cv,max_change,Emin,complexes,complex_index,assign,aggregate,own_q passed to [hcr_effort()].
#' @param triggers       effort-rule triggers (B/Bmsy)
#' @param close          stock or complex protected by a closure (see [hcr_effort()]).
#' @param close_triggers closure triggers; NULL = same as the effort trigger
#' @param risk           named limit on a perf_rows() column, e.g.
#'   c(worst_P_below_Bpa = 0.05) (every stock below Bpa at most 5% of the time)
#'   or c(worst_P_assessed = 0.05) (only the assessed stocks)
#' @param assessed       stocks with an assessment (default all)
#' @param objective      perf_rows() column to maximise ("yield" or "profit")
#' @param E_ref          reference days by segment (the caps the rule scales), e.g.
#'   the safeguarded-MMSY effort from ms_optimise() or the status-quo days
#' @param ...            passed to ms_simulate() (gamma, capacity, proc_cv, ...)
#' @return A list with `table` (performance by grid point), `best` (the tuned triggers) and the criteria.
#' @export
ms_tune_hcr <- function(sys, triggers = seq(0.5, 1.2, by = 0.05), close = NULL,
                        close_triggers = NULL, risk = c(worst_P_below_Bpa = 0.05),
                        objective = "yield", E_ref, B_init, E_init = E_ref,
                        nsim = 200, years = 50, eval_years = 31:50, seed = 1,
                        Blim = 0.25, assess_cv = 0.2, max_change = Inf, Emin = 0.1, assessed = NULL,
                        assign = NULL, complexes = NULL, complex_index = "biomass",
                        aggregate = "weighted", own_q = 0.5, ...) {
  grid <- if (is.null(close) || is.null(close_triggers)) data.frame(Btrig = triggers, Btrig_close = triggers)
          else expand.grid(Btrig = triggers, Btrig_close = close_triggers)
  k <- nrow(grid); n <- k * nsim
  rep_rows <- function(v, m) matrix(v, n, m, byrow = TRUE)
  rule <- hcr_effort(sys, Btrig = rep(grid$Btrig, each = nsim), Btrig_close = rep(grid$Btrig_close, each = nsim),
                     Blim = Blim, close = close, assess_cv = assess_cv, max_change = max_change, Emin = Emin,
                     assessed = assessed, assign = assign, complexes = complexes, complex_index = complex_index,
                     aggregate = aggregate, own_q = own_q)
  x <- ms_simulate(sys, rep_rows(E_ref, sys$G), years, B_init = rep_rows(B_init, sys$S),
                   E_init = rep_rows(E_init, sys$G), hcr = rule, crn = nsim, seed = seed, keep = "all", ...)
  tab <- cbind(grid, do.call(rbind, lapply(seq_len(k), function(j)
    perf_rows(x, rows = (j - 1) * nsim + seq_len(nsim), years = eval_years, assessed = assessed))))
  if (is.null(close)) tab$Btrig_close <- NULL
  ok <- tab[[names(risk)]] <= risk
  best <- if (any(ok)) tab[ok, ][which.max(tab[ok, objective]), ] else NULL
  list(table = tab, best = best, risk = risk, objective = objective)
}

