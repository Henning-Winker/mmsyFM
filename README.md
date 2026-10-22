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

A system is four tables (see `read_ms_system()`; the Atlantic example is in `inst/extdata`):

* `species`: `species`, `code`, `group`, `r`, `K`, `price`, `season_amp`, `season_peak`;
* `metiers`: `metier`, `tactic`, `ground`, `cost`;
* `catchability`: `metier` plus one column per stock;
* `segments`: `segment`, `share_target`, `loyalty`, `efficiency`, `cost_multiplier`, `vessel_cv`, `home`.

## Roadmap

- [x] Shiny app skeleton (`inst/shiny/mmsyFM`, `run_app()`). Next: Gulf of Thailand preset in the app, upload checks, download of results, trade-off plots across runs.
- [ ] Gulf of Thailand preset: 18 + 2 groups (`got_species()`); trawl (3 grounds), purse seine (mackerel/scad, anchovy/small pelagics) and small-scale (5 coastal tactics); conditioning on the 2024 depletions.
- [ ] Monthly closures, CPUE-based rules, SSCOM-type effort dynamics (Thorson et al. 2013).

## Author

Henning Winker, FAO Fisheries and Aquaculture Division (NFIFM), henning.winker@fao.org
