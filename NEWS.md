# mmsyFM 0.1.0

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
