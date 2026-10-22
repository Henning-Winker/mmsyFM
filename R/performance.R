# Ported from the dev-mmsy prototype (cpueTargeting, branch dev-mmsy, dev/mmsy/R/msfleet.R).

#' Performance statistics of projection runs
#'
#' `perf_rows()` summarises rows (simulations) of one run over `years`;
#' `ms_performance()` does this for a named list of runs.
#' @param x an `ms_sim` run with `keep = "all"`.
#' @param rows simulation rows (default all).
#' @param years years to summarise (default the last 20).
#' @param Bpa,Blim reference points relative to Bmsy.
#' @param assessed stocks for `worst_P_assessed`.
#' @return A one-row data frame of performance statistics.
#' @export
perf_rows <- function(x, rows = NULL, years = NULL, Bpa = 0.5, Blim = 0.25, assessed = NULL) {
  sys <- x$sys; ny <- dim(x$B)[1]; rows <- rows %||% seq_len(dim(x$B)[2])
  yr <- years %||% seq(max(1, ny - 19), ny)
  bb <- sweep(x$B[yr, rows, , drop = FALSE], 3, sys$K / 2, "/")
  Ey <- apply(x$E_seg[yr, rows, , drop = FALSE], 1:2, sum)              # years x sims
  aav <- colMeans(abs(diff(Ey)) / pmax(Ey[-nrow(Ey), , drop = FALSE], 1e-9))
  pst <- apply(bb < Bpa, 3, mean)
  data.frame(
    yield = round(mean(apply(x$C[yr, rows, , drop = FALSE], 1:2, sum))),
    profit = round(mean(x$profit[yr, rows])),
    days = round(mean(Ey)),
    P_any_below_Blim = round(mean(apply(bb < Blim, 1:2, any)), 3),
    P_any_below_Bpa = round(mean(apply(bb < Bpa, 1:2, any)), 3),
    worst_P_below_Bpa = round(max(pst), 3),
    worst_stock = sys$species[which.max(pst)],
    worst_P_assessed = round(max(pst[if (is.null(assessed)) seq_along(pst) else sys$species %in% assessed]), 3),
    median_min_BBmsy = round(stats::median(apply(bb, 1:2, min)), 2),
    AAV_effort = round(stats::median(aav), 3))
}


#' @rdname perf_rows
#' @param runs named list of `ms_sim` runs.
#' @export
ms_performance <- function(runs, years = NULL, Bpa = 0.5, Blim = 0.25) {
  do.call(rbind, lapply(names(runs), function(nm)
    cbind(run = nm, perf_rows(runs[[nm]], years = years, Bpa = Bpa, Blim = Blim))))
}

