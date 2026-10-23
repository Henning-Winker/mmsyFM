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

#' Gulf of Thailand species groups (mmsyapp input)
#'
#' The 18 species groups of the multispecies MSY app (mmsyapp; Amoroso, Pons and Sharma) for the Gulf of
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
  d <- utils::read.csv(system.file("extdata", "got_species_mmsyapp.csv", package = "mmsyFM"),
                       fileEncoding = "UTF-8-BOM", check.names = FALSE)
  d <- d[, c("Species", "K", "r", "Dep_ini", "q", "Group")]
  d$Species <- sub("_2024$", "", sub("^[0-9]+\\.\\s*", "", d$Species))
  names(d) <- c("species", "K", "r", "dep_2024", "q", "group")
  d
}

#' Gulf of Thailand system (draft)
#'
#' **Draft, indicative only.** Twenty stocks (the 18 mmsyapp groups plus
#' anchovies and sardines), ten métiers and three segments:
#' * trawl: inner (15-30 m, mud), mid (30-50 m) and outer shelf (50-80 m,
#'   sand) grounds;
#' * purse seine: mackerel/scad, and light-luring anchovy/small pelagics;
#' * small-scale: five coastal métiers (crab, shrimp, pelagic gillnet,
#'   hook and trap, cephalopod), confined to the coastal zone. Small-scale
#'   is the generalist, switching freely between its five tactics, with low
#'   efficiency (0.075 of an industrial vessel-day) and a matching low daily
#'   cost.
#'
#' Every segment has loyalty 1 (zoning): it switches only among its own
#' métiers.
#'
#' Catchability starts from an expert overlap matrix (main fishery 1,
#' by-catch 0.2, scaled by r) and was then conditioned with [ms_condition()]
#' so that each stock's status quo matches its mmsyapp 2024 depletion
#' (`dep_2024`), with B/Bmsy limited to 0.05-1.5: shrimp is set to 1.5
#' (mmsyapp: 0.97 B/K), croakers end at 0.07 (target 0.12, at the F cap),
#' anchovies and sardines are free (about 0.75 Bmsy). Daily costs keep the
#' days at 12/6/82% (trawl / purse seine / small-scale; capacity 450 / 225 /
#' 3000 days, `gamma = 1.5`). The median status is 0.46 Bmsy. All stocks
#' have `F_cap = 1.95` (a floor at 0.05 Bmsy) and simulations start from the
#' conditioned depletions (`dep_init`). Squid is shared by small-scale, trawl
#' and the light-luring seine, and scads by both seines and trawl. Prices are
#' relative and indicative.
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
