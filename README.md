# mmsyFM

<!-- badges: start -->
[![R-CMD-check](https://github.com/Henning-Winker/mmsyFM/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Henning-Winker/mmsyFM/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

**Multispecies MSY fleet model.** Simulates mixed fisheries in which fleet segments allocate their fishing days across métiers by expected profit. It finds safeguarded multispecies MSY, and tunes and tests effort- and closure-based harvest strategies under uncertainty. A Shiny app for participatory workshops is in development (branch `dev-shiny`).

`mmsyFM` grew out of the logbook simulator of [cpueTargeting](https://github.com/Henning-Winker/cpueTargeting) and replaces the shared-q model of the FAO RAP multispecies app (mse.SEA). In a shared-q model every effort rule moves along one line, because F_i / F_j = q_i / q_j is fixed. Here fishers choose where to fish.

## Installation

```r
# install.packages("remotes")
remotes::install_github("Henning-Winker/mmsyFM")
```

## Quick start

```r
library(mmsyFM)
sys <- atlantic_system()                                    # 10 stocks, 6 métiers, 3 segments
sq  <- ms_status_quo(sys, capacity = 1000, gamma = 1.5)      # open-access status quo
opt <- ms_optimise(sys, "yield", levers = "total", gamma = 1.5)  # MMSY with every stock >= Bpa
plot_kobe(list("Status quo" = sq, "Safeguarded MMSY" = opt), sys)
```

See `vignette("mmsyFM")` for yield curves and feedback harvest strategies.

## Shiny app

```r
mmsyFM::run_app()
```

The app has six tabs:

1. **Set-up:** system, status quo, tuning of the daily costs.
2. **Equilibrium explorer:** days by segment, closures.
3. **Yield curves:** conceptual treatments compared.
4. **Safeguarded MMSY.**
5. **Harvest strategy:** rule, uncertainty, projections.
6. **Trigger tuning.**

## Model

| Layer | Content |
|---|---|
| Stocks | One Schaefer pool per stock; monthly catches; seasonal availability; JABBA-type process error, scaled by r (`proc_sd_from_r()`) and depletion (JABBA `proc.dyn`, `relsig_jabba()`) |
| Métiers | Tactic × ground; catchability matrix by stock |
| Fleet | Segments with home métiers, loyalty, efficiency, daily cost and lognormal vessel efficiency; days follow the share of vessels that break even |
| Behaviour | P(métier) ∝ max(efficiency × expected revenue − cost, 0)^γ |
| Management | Effort caps by segment; métier closures (fractional, e.g. seasonal or area share); feedback rules (`hcr_effort()`) with assessment and implementation error, effort noise, a data lag, complexes, assessed subsets and nominal assignment |
| Tuning | `ms_tune()` (fleet composition at open access), `ms_tune_hcr()` (triggers to a risk limit, common random numbers) |

## Inputs

A system is four tables (see `read_ms_system()`; the Atlantic and Gulf of Thailand examples are in `inst/extdata`):

* `species`: `species`, `code`, `group`, `r`, `K`, `price`; optional `season_amp`, `season_peak`, `F_cap` (persistence floor, in Fmsy) and `dep_init` (starting B/K);
* `metiers`: `metier`, `tactic`, `ground`; optional `cost`;
* `catchability`: `metier` plus one column per stock, written directly or derived from a habitat landscape with `ms_landscape()`;
* `segments`: `segment`, `share_target`, `loyalty`, `efficiency`, `cost_multiplier`, `vessel_cv`, `home`; optional `capacity`.

See `vignette("scenario-setup")` for a worked example (Gulf of Thailand) and `vignette("how-it-works")` for the model equations.

## Roadmap

- [x] Shiny app (`run_app()`): set-up, equilibrium, yield curves, safeguarded MMSY, harvest strategies with rule comparison, trigger tuning, guide.
- [x] Gulf of Thailand draft (`got_system_draft()`): 20 stock groups; trawl, purse seine and small-scale fisheries with coastal zoning; conditioned on the 2024 depletions (`ms_condition()`).
- [x] Monthly (seasonal) closures; habitat landscape for catchability (`ms_landscape()`).
- [x] Performance statistics in the mseviz format (`ms_perf()`), boxplots, trade-offs and colour-coded decision tables (`plot_quilt()`).
- [ ] CPUE-based rules (observation model); SSCOM-type effort dynamics (Thorson et al. 2013); real prices and costs for the Gulf of Thailand.

## Authors and origins

**Authors:** Henning Winker (maintainer), Ricardo Amoroso, Maite Pons and Rishi Sharma.

mmsyFM combines two lines of work, and we want to be transparent about where its parts come from.

**1. The multispecies MSY app (mmsyapp).** The starting point for the management questions is the multispecies MSY Shiny application by Ricardo Amoroso, Maite Pons and Rishi Sharma ([mmsyapp](https://maitepons.shinyapps.io/mmsyapp/); referred to in this package as *mse.SEA*). It represents a mixed fishery as Schaefer surplus-production stocks fished by one pool of effort with a fixed catchability per stock (shared q), and was applied to species groups of the Gulf of Thailand. mmsyFM keeps:

* the multispecies MSY framing: yield over effort with a safeguard for every stock;
* the shared-q model, as one of the treatments compared in the yield curves (`ms_shared_q_curve()`);
* the Gulf of Thailand species groups and their r, K and 2024 depletions (`got_species()`, `got_system_draft()`).

**2. The CPUE targeting simulator (cpueTargeting).** Fleet behaviour comes from the multispecies logbook simulator of the cpueTargeting package (H. Winker), which re-implements the spatial simulator of Thorson et al. (2017), itself generalising Ono et al. (2015), with fisher targeting as in Winker et al. (2013, 2014). mmsyFM takes from it:

* métiers as fishing tactic x ground, and catchability from the overlap of where stocks live and where metiers fish (the Atlantic example was derived from its habitat landscape; `ms_landscape()` generalises this);
* targeting by expected profit, and vessel heterogeneity in efficiency (with a lower efficiency for generalist, mixed or small-scale vessels);
* the Schaefer stocks with seasonal availability that drive its catch rates.

**New in mmsyFM:** fleet segments with loyalty, costs and entry and exit; calibration of the fleet (`ms_tune()`) and of stock status (`ms_condition()`); the safeguarded MMSY optimiser; effort and closure rules in closed loop with assessment and implementation error, data lags, assessed subsets, complexes and nominal assignment; JABBA-type process error scaled by r and depletion; the F cap; and the performance statistics, whose presentation follows the FLR package mseviz (Mosqueira) and Winker et al. (2025).

### References

Ono, K., Punt, A.E. and Hilborn, R. (2015) Think outside the grids: an objective approach to define spatial strata for catch and effort analysis. *Fisheries Research* 170, 89-101.

Thorson, J.T., Fonner, R., Haltuch, M.A., Ono, K. and Winker, H. (2017) Accounting for spatiotemporal variation and fisher targeting when estimating abundance from multispecies fishery data. *Canadian Journal of Fisheries and Aquatic Sciences* 74, 1794-1807.

Winker, H., Kerwath, S.E. and Attwood, C.G. (2013) Comparison of two approaches to standardize catch-per-unit-effort for targeting behaviour in a multispecies hand-line fishery. *Fisheries Research* 139, 118-131.

Winker, H., Kerwath, S.E. and Attwood, C.G. (2014) Proof of concept for a novel procedure to standardize multispecies catch and effort data. *Fisheries Research* 155, 149-159.

Winker, H., Cardinale, M., Gerritsen, H., Herrera, J.G., Castellanos, P., Farias, I., Sampedro, P., Moura, T., Mosqueira, I. and Kell, L. (2025) The ICES MSY approach to reference point estimation is not precautionary. *ICES Journal of Marine Science* 82, fsaf204.

Contact: Henning Winker, FAO Fisheries and Aquaculture Division (NFIFM), henning.winker@fao.org
