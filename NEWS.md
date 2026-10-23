# mmsyFM 0.1.0

* Adriatic demersal draft (`adriatic_system_draft()`, preset in the app):
  GFCM GSAs 17-18, ten stocks incl. four Norway lobster functional units,
  five fleets on five areas; built in `data-raw/adriatic_build.R`.
* `ms_condition(on = "F")` conditions on assessed F/Fmsy; species column
  `dep_now` and `ms_start()` start projections from the assessed biomass
  (option "Start from" in the app).
* Management plans: species columns `plan` and `unit`, segment column
  `plan`; `hcr_effort(segments =)`; app rules "HCR plan stocks" and "HCR
  plan stocks, pooled units" (default when a system defines a plan).
* `ms_landscape(distribution =)`: stock distribution by cell, e.g. from
  species distribution models.

* Assessment strategies in the app: rules with indicator species, species
  complexes (pooled biomass or CPUE) and nominal assignment of stocks to
  fleets, compared with the all-stocks rule; defaults from
  `ms_assessment_presets()`.

* First version, ported from the dev-mmsy prototype in cpueTargeting and
  combined with the multispecies MSY app (mmsyapp).
* Systems: `ms_system()` from four tables; habitat landscape for
  catchability (`ms_landscape()`); Atlantic example and Gulf of Thailand
  draft (`atlantic_system()`, `got_system_draft()`).
* Calibration: `ms_tune()` (fleet composition at open access) and
  `ms_condition()` (stock status, e.g. 2024 depletions).
* Stocks: discrete Schaefer with exact F/Fmsy (harvest-rate ratio), F cap
  (persistence floor), JABBA-type process error scaled by r and depletion,
  and JABBA's floor in the process equation.
* Equilibrium analyses: yield curves, shared-q comparison, safeguarded MMSY
  (`ms_optimise()`; total yield, revenue, profit or the catch of one stock).
* Harvest strategies: `hcr_effort()` with full effort at the MMSY days,
  value-weighted or weakest-link status, triggers by segment, minimum
  effort, phased annual changes, stock-triggered closures, indicator
  species, complexes and nominal assignment; fixed metier closures
  (`ms_closure()`); trigger tuning (`ms_tune_hcr()`).
* Performance statistics in the mseviz format (`ms_perf()`), boxplots,
  trade-offs and colour-coded decision tables (`plot_quilt()`).
* Shiny app (`run_app()`) with a guide and a management-options tab.
