#' Atlantic example system
#'
#' Ten stocks (six demersal, four pelagic), six métiers (reef handline,
#' sediment longline and pelagic troll, each on a shallow and a deep ground)
#' and three segments (demersal and pelagic specialists, and a mixed
#' generalist fishing at 0.75 efficiency). The catchability matrix was
#' derived from the habitat landscape of `cpueTargeting::simulate_logbook()`
#' (seed 1). Daily costs are tuned so that the open-access equilibrium has
#' 35/35/30% of days by segment and the weakest stock at 0.4 Bmsy (see
#' [ms_tune()]).
#'
#' Dentex (narrow reef habitat, high catchability relative to its growth
#' rate) is the choke stock.
#'
#' @return An `ms_system`.
#' @export
#' @examples
#' sys <- atlantic_system()
#' sq <- ms_status_quo(sys, capacity = 1000, gamma = 1.5, years = 200)
#' round(sq$BBmsy, 2)
atlantic_system <- function() {
  read_ms_system(system.file("extdata", package = "mmsyFM"), "atlantic", effort_season_amp = 0.3)
}

#' Gulf of Thailand species groups (mse.SEA input)
#'
#' The 18 species groups of the FAO RAP mse.SEA application for the Gulf of
#' Thailand: K, r, 2024 depletion (`Dep_ini`, B/K), a placeholder
#' catchability `q` and a group label. This is the starting point for a
#' Gulf of Thailand system (trawl, purse seine and small-scale segments);
#' métiers and catchability are still to be defined.
#'
#' @return A data frame.
#' @export
#' @examples
#' head(got_species())
got_species <- function() {
  d <- utils::read.csv(system.file("extdata", "got_species_mseSEA.csv", package = "mmsyFM"),
                       fileEncoding = "UTF-8-BOM", check.names = FALSE)
  d <- d[, c("Species", "K", "r", "Dep_ini", "q", "Group")]
  d$Species <- sub("_2024$", "", sub("^[0-9]+\\.\\s*", "", d$Species))
  names(d) <- c("species", "K", "r", "dep_2024", "q", "group")
  d
}
