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

#' Gulf of Thailand system (draft)
#'
#' **Draft, indicative only.** Twenty stocks (the 18 mse.SEA groups plus
#' anchovies and sardines), ten métiers and three segments:
#' * trawl: inner (15-30 m, mud), mid (30-50 m) and outer shelf (50-80 m,
#'   sand) grounds;
#' * purse seine: mackerel/scad, and light-luring anchovy/small pelagics;
#' * small-scale: five coastal métiers (crab, shrimp, pelagic gillnet,
#'   hook and trap, cephalopod), confined to the coastal zone. Small-scale
#'   is the generalist: no loyalty, low efficiency (0.075 of an industrial
#'   vessel-day) and a matching low daily cost, and the target share of days
#'   is 82%.
#'
#' Catchability follows an expert overlap matrix (main fishery 1, by-catch
#' 0.2) scaled by r (q = 0.006 r x overlap), so a day has the same F/Fmsy
#' effect on every stock and the spread in status comes from the overlaps; squid is shared by small-scale, trawl and the
#' light-luring seine, and scads by both seines and trawl. Prices are
#' relative and indicative. `dep_2024` (mse.SEA) is carried in the species
#' table but the stocks are not yet conditioned on it: daily costs were
#' tuned ([ms_tune()], `cap = c(450, 225, 3000)`, `gamma = 1.5`) to 12/6/82%
#' of days and a median status of 0.5 Bmsy, close to the median 2024
#' depletion. The outer-shelf trawl stocks collapse at this status quo.
#'
#' @return An `ms_system`.
#' @export
#' @examples
#' sys <- got_system_draft()
#' sys
#' \donttest{
#' sq <- ms_status_quo(sys, capacity = c(450, 225, 3000), gamma = 1.5, years = 300)
#' round(sort(sq$BBmsy), 2)
#' }
got_system_draft <- function() {
  read_ms_system(system.file("extdata", package = "mmsyFM"), "got")
}
