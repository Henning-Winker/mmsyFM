## 1. What mmsyFM does

mmsyFM simulates a mixed fishery in which several fleet segments share many
stocks, and fishers decide **where** and **whether** to fish by expected
profit. It is used to

* find the **safeguarded multispecies MSY**: the effort limits that give the
  highest long-term yield (or revenue, or profit) while every stock stays at
  or above a safeguard such as Bpa = 0.5 Bmsy;
* compare **management schemes**: effort caps by segment, closures of
  metiers (all year or by month), and feedback harvest control rules (HCRs)
  under assessment error, implementation error, process error and data lags.

It replaces the shared-q model of mse.SEA, in which every stock receives a
fixed share of a single effort pool, with a model of fleet behaviour.

The model has four layers: **stocks**, **fleet**, **behaviour** and
**management**. The sections below describe each, then how a system is
calibrated ("tuned"), and how the analyses work.

## 2. Stocks

Each stock is a Schaefer surplus-production pool with intrinsic growth `r`
and carrying capacity `K`. The year is fished in 12 monthly steps and growth
is added once a year on the start-of-year biomass:

    B[t+1] = ( B[t] + r B[t] (1 - B[t]/K) - C[t] ) x exp(eta[t])
    C[t]   = sum over months of  B[m] (1 - exp(-F[m]))

`eta` is process error (section 8; zero in deterministic runs). A stock's
monthly F is the sum over metiers of catchability x effective days x the
stock's seasonal availability in that month.

**F/Fmsy is measured as a harvest rate.** Over a year a stock loses the share
`h = 1 - exp(-F)` of its biomass to fishing. In this discrete model the
equilibrium is `B/K = 1 - h/r`, so `hmsy = r/2`, and mmsyFM reports
`F/Fmsy = h / hmsy`. With this definition the identity

    B/Bmsy + F/Fmsy = 2

holds exactly at equilibrium (the Kobe plot shows every stock on that line).
Using `r/2` for the instantaneous F would overstate F/Fmsy for fast stocks:
for r = 1.4 the instantaneous Fmsy is 1.2, not 0.7 (see `ffmsy()`).

**F cap: persistence at low abundance.** A Schaefer stock goes extinct when
F/Fmsy >= 2, but heavily fished stocks rarely do: the last fish are scattered,
in refuges or not worth the trip. `F_cap` (column of the species table, in
units of Fmsy) limits each stock's realised F. With `F_cap = 1.95` no stock
can settle below `2 - 1.95 = 0.05` Bmsy; extra effort on a capped stock
catches nothing more, so it becomes "economically extinct" and is left to
by-catch. The cap is **hard** in deterministic runs (equilibrium, tuning) and
**smooth** in stochastic runs, `F = (F_nom^-p + F_cap^-p)^(-1/p)` with
p = 8, so it has no kink. For schooling pelagics (purse seine), whose catch
rates hold up as the stock shrinks, a higher cap or none may be more
realistic.

**Starting depletion.** `dep_init` (B/K, species table) is the biomass at
which simulations start; default 1 (unfished). Conditioned systems carry
their calibrated depletion, so projections start from the present state.

## 3. Fleet

* **Metiers** are tactic x ground combinations (e.g. trawl on the inner
  shelf, hook and line on the reef). Each has a daily cost.
* **Catchability** `Q` (metiers x stocks) says how strongly a metier catches
  each stock; it encodes gear selectivity and the overlap between where a
  stock lives and where the metier fishes.
* **Segments** (e.g. trawl, purse seine, small-scale) have:
  * **home metiers** and **loyalty**: the share of their days spent on home
    metiers; loyalty 1 = zoning (a segment fishes only its own metiers),
    loyalty < 1 lets part of the days go to any open metier;
  * **efficiency**: catch per day relative to a specialist (e.g. 0.75 for a
    mixed vessel, 0.075 for a small-scale vessel-day);
  * a **daily cost multiplier**;
  * **vessel heterogeneity**: vessel efficiency varies (lognormal, CV 0.25),
    so some vessels of a segment break even when others do not;
  * **capacity**: registered days, the most the segment can fish in a year.

## 4. Behaviour

**Where to fish.** Each segment spreads its days over the metiers it can use
in proportion to expected profit per day, raised to the power `gamma`
("how strongly fishers chase profit"):

    P(metier m) proportional to  open[m] x max(efficiency x value[m] - cost[m], 0)^gamma

with `value[m]` the expected revenue per day at current biomass. A segment
puts the share `loyalty` of its days on its home metiers and the rest on all
open metiers. `gamma = 0` is profit-blind (days spread evenly); with
`gamma = 1.5`, a metier paying twice the profit of another gets 74% of the
days.

**Whether to fish.** Days fished are those of the vessels that break even:
each year a segment's days move (at rate `phi`, default 0.5) towards
capacity x the share of its vessels whose catch covers their costs. When
stocks are depleted, trips stop paying and effort falls; when they recover,
effort returns. Effort caps (management) can only lower the days further.

## 5. Calibration: getting the starting point right

A system is calibrated in three steps, normally once per case study:

1. **Daily costs (`ms_tune()`).** At open access the fleet composition is set
   by costs. `ms_tune()` solves the daily cost multiplier of each segment so
   that the open-access equilibrium has the target share of days by segment
   (e.g. 12 / 6 / 82% for trawl / purse seine / small-scale) and a target
   status: the weakest stock (e.g. 0.05 Bmsy), the median stock, or the total
   days.
2. **Stock status (`ms_condition()`).** When the present status of each stock
   is known (e.g. 2024 depletions from assessments), `ms_condition()`
   rescales each stock's catchability until its equilibrium B/Bmsy matches
   the target, while `ms_tune()` keeps the shares of days with the total days
   fixed (the overall effort level and the catchability scale cannot both be
   estimated). Targets are limited to a range (default 0.05-1.5 Bmsy);
   stocks without a target are left free. The step uses
   `F/Fmsy = 2 - target`, so it converges in a few iterations; simulations
   start at the targets, because slow stocks fished near their limit would
   need centuries to settle from K.
3. **Status quo (`ms_status_quo()`).** The calibrated open-access equilibrium
   is the reference for everything else: the days by segment it produces are
   the reference effort that yield curves scale, caps are compared with and
   HCRs act on.

## 6. Equilibrium analyses

### Yield curves

`ms_yield_curve()` scales the status-quo days of all segments by a common
factor and runs each to equilibrium. The app compares four treatments:

* **Shared q (mse.SEA):** one effort pool and a fixed catchability per
  stock, calibrated to reproduce the status-quo F of every stock. Effort
  raises F on all stocks in the same proportions; there is no behaviour.
  Solved analytically (`ms_shared_q_curve()`).
* **Profit-blind:** the metier structure and economic exit, but days are
  spread evenly over each segment's metiers (`gamma = 0`).
* **Profit-seeking:** days follow expected profit (`gamma` as set).
* **Profit-seeking + closure:** as above with one metier closed all year;
  its days move to the other metiers (displacement).

All curves pass through the status quo. Shared q gives the classic dome;
behaviour flattens it, because effort leaves depleted stocks. The
safeguarded yield (all stocks >= Bpa) can be far lower with behaviour: when
effort is cut, stocks recover and effort concentrates on the most valuable
ground, often the choke stock's.

### Safeguarded MMSY (`ms_optimise()`)

Finds the effort caps that maximise the **objective** (total yield, revenue,
or profit = MEY) at equilibrium, subject to every stock at or above **Bpa**.
The stock at the limit is the **choke stock**.

* **Levers:** one global cap (split across segments in their status-quo
  proportions) or one cap per segment.
* **Viability floor:** with caps per segment, every segment keeps at least
  this share of the total days, so the optimum cannot shut a segment down.
* **Closures:** metiers kept closed during the optimisation.

The search is a zooming grid over the caps, robust to the kinks of a
behavioural fleet.

## 7. Harvest strategies (feedback control)

`hcr_effort()` builds a rule that sets each segment's days every year;
`ms_simulate(..., hcr = rule)` runs it in a closed loop.

**Each year:**

1. **Assessment.** Each assessed unit (stock or complex) gets an estimated
   B/Bmsy = true value x lognormal error (CV `assess_cv`, optional AR(1)),
   from data `hcr_lag` years old (default 2: data of year t-2, assessed in
   t-1, advice for t).
2. **Responsibility.** A segment answers for a stock if one of the metiers
   it can use (its home metiers when loyalty > 0) catches it substantially:
   catchability at least `own_q` (default 0.2) of the stock's highest
   catchability in any metier.
3. **Weakest link.** A segment's days are a fraction of its status-quo days,
   set by the most depleted stock it answers for: 1 at or above the trigger
   `Btrig`, falling linearly to the **minimum effort** `Emin` (default 0.1)
   at `Blim` (0.25 Bmsy) and below. The rule reduces effort but never closes
   a segment; since F scales with days, the floor puts about 0.1 x the
   status-quo F on each stock (`Emin = 0` allows a full closure).
4. **Smoothing and implementation.** The fraction may change by at most
   `max_change` a year, in units of the status-quo days (0.2 = 20% of the
   status-quo days up or down), so a closed segment reopens gradually once
   its stocks recover; realised days differ from allowed days by
   implementation error (CV `impl_cv`).

**Mixed fleets.** A mixed or generalist segment fishes many metiers, so it
answers for many stocks, including the choke stock, and is cut whenever that
stock is low, together with the specialist that targets it. This is the
central problem of mixed fisheries: one weak stock constrains every fleet
that touches it.

**Alternatives to cutting days** (arguments of `hcr_effort()`):

* `close`: protect a choke stock by closing the metiers that drive it
  (catchability >= `close_q`, default 0.5, of its highest) for a share of the
  time that ramps from 0 at `Btrig_close` to fully closed at Blim; that stock
  is then left out of the effort rule.
* `assessed`: only some stocks are assessed (e.g. 4 of 20); the rule ignores
  the rest, which may then decline unnoticed.
* `assign`: nominal assignment of each stock to one segment for management
  (e.g. Dentex to the demersal fleet), as is common in practice; other
  segments still catch it but are not cut for it.
* `complexes`: stocks assessed as a pooled group (e.g. "reef fish"), from
  pooled biomass or a pooled catch rate. Pooling can hide a weak member.

**Seasonal closures.** The `open` argument of `ms_simulate()` can be a
12 x metier matrix (open share by month), e.g. closing the trawl grounds
from 15 February to 15 May.

**Tuning triggers (`ms_tune_hcr()`).** Runs a grid of effort (and closure)
triggers with common random numbers (every trigger sees the same random
errors) and picks the trigger with the highest yield whose risk meets the
limit, by default: the worst stock below Bpa at most 5% of the time.

## 8. Uncertainty

* **Process error** on the surplus-production equation (JABBA form), with
  AR(1) autocorrelation and optional correlation between stocks:
  * its SD scales with r: `sd = 0.05 (r / 0.057)^0.68`, i.e. about 0.05 for
    sharks (r = 0.057) and 0.4 for anchovy (r = 1.2) (`proc_sd_from_r()`);
  * it increases at low biomass, following JABBA's proc.dyn function
    (`relsig_jabba()`): about 1.5x at 0.5 Bmsy and 4x at 0.05 Bmsy, so
    depleted stocks are more volatile;
  * centring: `"median"` (as JABBA's lognormal) has no drift; `"mean"`
    is mean-unbiased, so the median drifts down when the SD is large
    (stochastic depensation).
* **Assessment error** and **data lag** (section 7).
* **Implementation error** on the allowed days by segment.
* **Effort noise**: year-to-year variation of effort and catch rates by
  segment, in every run, regulated or not.

## 9. Performance statistics

`ms_performance()` summarises stochastic runs over the evaluation years
(default the last 20): mean yield, profit and days; the probability that any
stock is below Blim or Bpa in a year; the worst stock's probability of being
below Bpa (and which stock it is); the median of the lowest B/Bmsy; and the
median average annual variation of total days.

## 10. Function reference

| Step | Functions |
|---|---|
| Build a system | `ms_system()`, `read_ms_system()`, `write_ms_system()`, presets `atlantic_system()`, `got_system_draft()` |
| Calibrate | `ms_tune()`, `ms_condition()`, `ms_status_quo()` |
| Simulate | `ms_simulate()`, `ms_summary()` |
| Equilibrium analyses | `ms_yield_curve()`, `ms_shared_q_curve()`, `ms_optimise()` |
| Harvest strategies | `hcr_effort()`, `ms_tune_hcr()`, `ms_performance()` |
| Uncertainty helpers | `proc_sd_from_r()`, `relsig_jabba()`, `ffmsy()` |
| Plots | `plot_kobe()`, `plot_yield_curves()`, `plot_projection()`, `plot_fleet()`, `plot_catchability()` |
| App | `run_app()` |
