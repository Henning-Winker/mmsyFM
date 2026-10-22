#' mmsyFM: multispecies MSY fleet model
#'
#' Simulates mixed fisheries in which fleet segments allocate their effort
#' across métiers by expected profit, and uses the simulations to find
#' safeguarded multispecies MSY and to tune and test effort- and
#' closure-based harvest strategies under uncertainty.
#'
#' The model has four layers:
#' * **stocks**: one Schaefer surplus-production pool per stock, with
#'   seasonal availability and JABBA-type process error;
#' * **fleet**: segments (e.g. demersal, pelagic and mixed/small-scale) with
#'   home métiers, loyalty, efficiency, daily cost and a lognormal spread of
#'   vessel efficiency;
#' * **behaviour**: each segment allocates its days over métiers with
#'   probability proportional to expected profit^gamma, and its days follow
#'   the share of vessels that break even;
#' * **management**: effort caps by segment, métier closures, and feedback
#'   rules with assessment error, implementation error and a data lag.
#'
#' Main functions: [ms_system()], [atlantic_system()], [ms_simulate()],
#' [ms_open_access()], [ms_tune()], [ms_optimise()], [ms_yield_curve()],
#' [hcr_effort()], [ms_tune_hcr()], [ms_performance()].
#'
#' @keywords internal
"_PACKAGE"

#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_hline geom_vline geom_ribbon
#'   geom_area geom_step geom_text annotate facet_wrap scale_colour_manual scale_fill_manual
#'   scale_x_log10 scale_y_continuous scale_x_continuous scale_shape_manual coord_cartesian
#'   labs theme theme_minimal element_text element_line element_blank margin rel unit
#'   expansion %+replace% geom_tile scale_fill_gradient scale_linetype_identity
NULL

#' @importFrom stats rnorm pnorm median quantile sd setNames
NULL

`%||%` <- function(x, y) if (is.null(x)) y else x

utils::globalVariables(c(
  "E", "value", "scenario", "yield", "BBmsy", "FFmsy", "code", "group", "year", "med",
  "lo", "hi", "run", "share", "metier", "treatment", "point", "min_BBmsy",
  "n_below_Bpa", "species", "lt", "y", "panel", "b", "depth"))
