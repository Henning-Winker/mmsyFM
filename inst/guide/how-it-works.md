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

It extends the shared-q model of the multispecies MSY app (mmsyapp), in which every stock receives a
fixed share of a single effort pool, with a model of fleet behaviour.

The model has four layers: **stocks**, **fleet**, **behaviour** and
**management**. The sections below describe each, then how a system is
calibrated ("tuned"), and how the analyses work. The equations are in
Appendix A and the search algorithms in Appendix B.

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
  stock lives and where the metier fishes. It can be written directly (e.g.
  an expert overlap matrix) or derived from a habitat landscape with
  `ms_landscape()`: seabed cells (depth, substrate, area), each stock's depth
  and substrate preference, each metier's ground (depth range, substrates)
  and gear selectivity, giving
  `q = q0 x selectivity x (stock density on the ground / average density)`.
  The calibration (section 5) then sets the level of each stock's column.
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

* **Shared q:** one effort pool and a fixed catchability per
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
2. **Responsibility.** A segment answers for the stocks it catches as a
   **main fishery**: one of the metiers it can use (its home metiers when
   loyalty > 0) has at least `own_q` (default 0.5) of the stock's highest
   catchability in any metier. With `own_q = 0.2` by-catch counts too.
3. **Status of the segment.** By default (`aggregate = "weighted"`) it is the
   mean assessed B/Bmsy of those stocks, weighted by each stock's potential
   revenue for the segment (its catchability on the segment's metiers x
   price x K): the stocks that make up a fishery drive its effort. The
   **weakest link** (`aggregate = "min"`, the most depleted stock decides) is
   the special, most precautionary case.
4. **Ramp.** A segment's days are a fraction of its **full effort**, normally
   the safeguarded-MMSY days from `ms_optimise()` (or the status-quo days):
   1 at or above the trigger `Btrig` (default 0.5 Bmsy, i.e. at Bpa, as in
   ICES and GFCM advice; it can differ by segment, e.g. higher for small
   pelagics), falling linearly to the **minimum effort** `Emin` (default
   0.1) at `Blim` (0.25 Bmsy) and below. The rule reduces effort but never
   closes a segment (`Emin = 0` allows a full closure).
5. **Smoothing and implementation.** The days may change by at most
   `max_change` a year relative to the previous year (0.2 = 20% up or down,
   as for TAC changes), starting from the current (status-quo) days, so the
   move to the full effort and later cuts and recoveries are phased;
   realised days differ from allowed days by implementation error (CV
   `impl_cv`).

The weak species are protected in three ways: the full effort itself keeps
every stock at or above Bpa at equilibrium (the safeguard of the MMSY
optimisation); the ramp cuts a segment when the stocks it fishes decline;
and a choke stock can be protected by a closure of the metiers that drive it
(below) instead of cutting whole fleets.

**Mixed fleets.** A mixed or generalist segment fishes many metiers, so it
answers for many stocks. Under the weakest link it is cut whenever any of
them is low, together with the specialist that targets it: one weak stock
constrains every fleet that touches it, and with many depleted, slow stocks
and noisy assessments the fleets stay at the minimum effort for decades.
The value-weighted status avoids this while the full effort and closures
keep the safeguard.

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

`ms_perf()` computes performance statistics of a set of runs (one per rule
or management procedure) over the evaluation years (default the last 20),
per iteration, and returns them as a long `data.table` in the format of the
FLR package **mseviz** (`mp`, `biol`, `statistic`, `name`, `desc`, `year`,
`iter`, `data`), so they can also be passed to `mseviz::plotBPs()`,
`mseviz::plotTOs()` or `mseviz::perfTable()`.

* **By stock** (`biol` = stock code): median B/Bmsy and F/Fmsy, catch/MSY,
  P(B < Blim), P(B < Bpa), P(B > 0.8 Bmsy) and P(Kobe green).
* **By segment**: mean days fished.
* **System** (`biol = "All"`): total catch, revenue, profit, days, catch
  relative to the sum of single-stock MSYs, the average annual variation of
  catch and of days, the mean number of stocks below Bpa, P(any stock below
  Blim) and the lowest B/Bmsy reached.

Probabilities are the share of years (per iteration) and are summarised by
their mean over iterations. Presentation:

* `plot_bps()`: boxplots by rule (10th-90th percentiles), probabilities as
  points, with target and limit lines (the style of `mseviz::plotBPs()`);
  for the system or for selected stocks (one row of panels per stock);
* `plot_tradeoffs()`: medians and 10th-90th percentile ranges of each
  statistic against catch (or revenue, profit, days);
* `plot_quilt()`: colour-coded decision tables, either rules x statistics
  (green = best, red = worst within each statistic, by its preferred
  direction) or stocks x rules for one statistic, with optional absolute
  classes for probabilities (green at or below a risk limit, amber up to
  twice the limit, red above);
* `ms_perf_table()`: medians with 10th-90th percentiles as a table.

The earlier summary `ms_performance()` (one row per rule) is kept.

## 10. Function reference

| Step | Functions |
|---|---|
| Build a system | `ms_system()`, `read_ms_system()`, `write_ms_system()`, `ms_landscape()`, presets `atlantic_system()`, `got_system_draft()` |
| Calibrate | `ms_tune()`, `ms_condition()`, `ms_status_quo()` |
| Simulate | `ms_simulate()`, `ms_summary()` |
| Equilibrium analyses | `ms_yield_curve()`, `ms_shared_q_curve()`, `ms_optimise()` |
| Harvest strategies | `hcr_effort()`, `ms_tune_hcr()`, `ms_performance()` |
| Performance statistics | `ms_perf()`, `ms_statistics()`, `ms_perf_table()`, `plot_bps()`, `plot_tradeoffs()`, `plot_quilt()` |
| Uncertainty helpers | `proc_sd_from_r()`, `relsig_jabba()`, `ffmsy()` |
| Plots | `plot_kobe()`, `plot_yield_curves()`, `plot_projection()`, `plot_fleet()`, `plot_catchability()`, `plot_landscape()` |
| App | `run_app()` |

## Appendix A. Model equations

Notation: stocks $s$, metiers $j$, segments $g$, years $t$, months $m$.

### A1. Stock dynamics (discrete Schaefer with monthly fishing)

Within a year, each month removes catch from the biomass (Baranov, no growth
within the year):

$$F_{s,m} = a_{s,m} \sum_j q_{js} \tilde E_{j,m}, \qquad C_{s,m} = B_{s,m} \left(1 - e^{-F_{s,m}}\right), \qquad B_{s,m+1} = B_{s,m} - C_{s,m}$$

with $a_{s,m}$ the seasonal availability of stock $s$, $q_{js}$ the
catchability of metier $j$ and $\tilde E_{j,m}$ the effective days (days x
efficiency x open share). Growth is added once a year on the start-of-year
biomass, with process error $\eta$:

$$B_{s,t+1} = \max\left( B_{s,t} + r_s B_{s,t} \left(1 - \frac{B_{s,t}}{K_s}\right) - C_{s,t}, \quad 0.005 K_s \right) e^{\eta_{s,t}}, \qquad C_{s,t} = \sum_m C_{s,m}$$

The floor at 0.005 K (as in JABBA) matters for fast stocks: with r > 1, a
large positive shock can take a stock above K, and the logistic update from
there can turn negative (overshoot); without the floor the stock would be
absorbed at zero.

### A2. Reference points and F/Fmsy

A year of fishing removes the share $h_{s,t} = 1 - e^{-F_{s,t}}$ with
$F_{s,t} = \sum_m F_{s,m}$. The deterministic equilibrium is

$$\frac{B_s}{K_s} = 1 - \frac{h_s}{r_s}, \qquad h_{MSY} = \frac{r_s}{2}, \qquad B_{MSY} = \frac{K_s}{2}, \qquad MSY = \frac{r_s K_s}{4}$$

so that, with $F/F_{MSY} \equiv h / h_{MSY}$,

$$\frac{B}{B_{MSY}} + \frac{F}{F_{MSY}} = 2 .$$

### A3. F cap

With cap $\phi_s$ (units of $F_{MSY}$), the largest annual harvest rate is
$h^{max}_s = \min(\phi_s r_s / 2, 1)$, i.e. $F^{max}_s = -\ln(1 - h^{max}_s)$,
applied monthly as $F^{max}_s / 12$. The equilibrium floor is
$B/B_{MSY} \ge 2 - \phi_s$. The realised monthly F is

$$F = \min\left(F^{nom}, F^{max}\right) \quad \text{(hard)}, \qquad F = \left( (F^{nom})^{-p} + (F^{max})^{-p} \right)^{-1/p} \quad \text{(smooth, } p = 8\text{)} .$$

### A4. Process error

$$\eta_{s,t} = \rho \eta_{s,t-1} + \sqrt{1 - \rho^2} \varepsilon_{s,t}, \qquad \varepsilon_{s,t} \sim N\left(0, \sigma_{s,t}^2\right)$$

centred on the median (as above) or on the mean ($\eta - \sigma^2/2$). The SD
depends on r and on depletion (JABBA proc.dyn):

$$\sigma_{s,t} = \sigma_s^0 \frac{g(B_{s,t}/B_{MSY,s})}{g(1)}, \qquad \sigma_s^0 = 0.05 \left(\frac{r_s}{0.057}\right)^{0.68}, \qquad g(x) = a + \frac{1 - a}{1 + (x/c)^d}$$

with $a = 0.128$, $c = 0.223$, $d = 1.326$, capped at 1. Correlation between
stocks mixes a common draw: $\varepsilon_s = \sqrt{1-\omega} z_s + \sqrt{\omega} z_0$.

### A5. Effort allocation: the profit preference

The expected revenue per effective day of metier $j$ in month $m$, at
current biomass, is

$$V_{j,m} = \sum_s p_s q_{js} a_{s,m} B_{s,m}$$

with price $p_s$. A vessel of segment $g$ (efficiency $e_{gj}$, times the
mean efficiency $\bar v_g$ of its active vessels) expects the profit per day
$e_{gj} \bar v_g V_{j,m} - \kappa_g c_j$, with $c_j$ the daily cost of the
metier and $\kappa_g$ the segment's cost multiplier. Its utility is

$$u_{gj} = o_{j} \max\left(e_{gj} \bar v_g V_{j,m} - \kappa_g c_j , 0\right)^{\gamma}$$

($o_j$ the open share, $\gamma$ the strength of the preference; $\gamma = 0$
gives $u = 1$ for every open metier). With home metiers $H_g$ and loyalty
$\lambda_g$, the segment's days $E_{g,m}$ are allocated as

$$E_{gj,m} = E_{g,m} \left( \lambda_g \frac{u_{gj} 1[j \in H_g]}{\sum_{k \in H_g} u_{gk}} + (1 - \lambda_g) \frac{u_{gj}}{\sum_k u_{gk}} \right), \qquad \tilde E_{j,m} = o_j \sum_g e_{gj} \bar v_g E_{gj,m}$$

A segment's days are spread over the months by a fixed seasonal pattern.

### A6. Entry and exit (vessel heterogeneity)

Vessel efficiency is lognormal with mean 1:
$v \sim LN(\mu, \sigma_v^2)$, $\sigma_v^2 = \ln(1 + CV^2)$, $\mu = -\sigma_v^2 / 2$.
From last year's revenue $R_g$ and cost $D_g$ per day of a vessel of mean
efficiency (weighted by its allocation), a vessel breaks even when
$v \ge v^* = D_g / R_g$. The share of vessels that break even, and the mean
efficiency of the active ones, are

$$P_g = 1 - \Phi\left(\frac{\ln v^* - \mu}{\sigma_v}\right), \qquad \bar v_g = \frac{1}{P_g} \Phi\left(\frac{\mu + \sigma_v^2 - \ln v^*}{\sigma_v}\right)$$

and the segment's days move towards the days of those vessels, within the
cap $\bar E_g$ (management) and the capacity $\mathcal{K}_g$:

$$E_{g,t+1} = E_{g,t} + \varphi \left( \min\left(\bar E_g, \mathcal{K}_g P_g\right) - E_{g,t} \right), \qquad \varphi = 0.5 .$$

### A7. Economics

Revenue and cost of a year, and profit:

$$R_t = \sum_s p_s C_{s,t}, \qquad D_t = \sum_g \kappa_g \sum_j c_j E_{gj,t}, \qquad \Pi_t = R_t - D_t .$$

Without a `cost` column, the daily cost of a metier is a share (0.4) of its
unfished revenue rate, $c_j = 0.4 \sum_s p_s q_{js} K_s \bar a_s$.

### A8. Harvest control rule (effort)

The assessed status is $\hat b_{u,t} = b_{u,t-L} e^{\epsilon_{u,t}}$ with
$b = B/B_{MSY}$ of unit $u$ (stock or complex), lag $L$ and lognormal error
$\epsilon$ (CV `assess_cv`, optional AR(1)). Segment $g$ answers for the
units $U_g$ it catches as a main fishery
($\max_{j \in H_g} q_{js} \ge \theta \max_j q_{js}$, $\theta$ = `own_q`). Its
status is the value-weighted mean (or, for the weakest link, the minimum)

$$\hat b_{g,t} = \sum_{u \in U_g} w_{gu} \hat b_{u,t}, \qquad w_{gu} \propto \sum_{s \in u} p_s K_s \sum_{j \in H_g} q_{js} \qquad \left(\text{or } \hat b_{g,t} = \min_{u \in U_g} \hat b_{u,t}\right)$$

and its fraction of the reference (full) days $E^{ref}_g$, the safeguarded-MMSY
days or the status-quo days, is

$$f_{g,t} = E_{min} + (1 - E_{min}) \min\left(1, \max\left(0, \frac{\hat b_{g,t} - B_{lim}}{B_{trig,g} - B_{lim}}\right)\right)$$

limited to $(1 - \Delta) f_{g,t-1} \le f_{g,t} \le (1 + \Delta) \max(f_{g,t-1}, 0.05)$
(with $f_{g,0} = E_{g,0} / E^{ref}_g$, the current days relative to the reference). The cap is
$\bar E_{g,t} = f_{g,t} E^{ref}_g e^{\xi_{g,t}}$ with implementation error
$\xi$. A closure for unit $u$ sets the open share of the metiers that drive it
($q_{js} \ge$ `close_q` $\max_j q_{js}$) to the same ramp, from 0 at
$B_{lim}$ to 1 at its own trigger.

### A9. Calibration

`ms_tune()` solves the log cost multipliers $\log \kappa_g$ so that the
open-access equilibrium meets $G$ conditions: the day shares of the first
$G - 1$ segments and one status target. `ms_condition()` updates each targeted
stock's catchability column by

$$q_{js} \leftarrow q_{js} \left(\frac{2 - b^*_s}{F_s / F_{MSY,s}}\right)^{0.8}$$

with target $b^*_s$ (from $B/B_{MSY} + F/F_{MSY} = 2$), keeping each metier's
cost proportional to its unfished revenue, and re-runs `ms_tune()` with the
total days fixed.

## Appendix B. Algorithms

* **Equilibria** are found by simulation: a run of 200-500 years, averaged
  over its last 10-30 years. Many scenarios (rows) run in one vectorised call.
* **`ms_optimise()`**: a zooming grid search over the caps in
  $\log(1 + E)$. Each round runs $n^d$ candidates ($d$ levers) to
  equilibrium in one call, keeps the best feasible one (every stock at or
  above Bpa) and shrinks the box around it (5 rounds). No bisection or
  gradient method is used: with fleet behaviour the safeguard is not
  monotone in effort (cutting effort can concentrate it on the most valuable
  ground and lower the weakest stock), and the objective has flat stretches
  and small kinks from vessel exit.
* **`ms_tune()`**: Newton's method on $\log \kappa_g$ with a numerical
  Jacobian ($G + 1$ perturbed systems in one call), bounded steps, and the
  best iterate kept.
* **`ms_condition()`**: the fixed-point update of A9, alternated with
  `ms_tune()`; simulations start at the targets so slow stocks need not
  settle from K.
* **`ms_tune_hcr()`**: a grid of triggers run with common random numbers
  (every trigger sees the same process, assessment and implementation
  errors); the highest yield that meets the risk limit is chosen.
