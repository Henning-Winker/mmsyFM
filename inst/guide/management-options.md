## Management options in mmsyFM

This page explains the management options of the app in a real-world
context: what each option means, how the model represents it, where it is
used in practice, and what to watch for. It applies to any system, whether
the Atlantic example, the Gulf of Thailand draft or your own tables.

Mixed fisheries are hard to manage because one fleet, and often one day at
sea, catches many species at once. Every measure below is a different answer
to the same question: **how much fishing, by whom and where, keeps every
stock safe while delivering the most food, income or jobs?**

---

## 1. Reference points: what "safe" and "good" mean

| Reference point | Meaning | Use in the app |
|---|---|---|
| **Bmsy** | Biomass that gives the maximum sustainable yield of a stock | All statistics are relative to it (B/Bmsy) |
| **0.8 Bmsy** | FAO threshold: below it a stock is classified as overfished (SOFIA, SDG 14.4.1) | Green target line; statistic P(B > 0.8 Bmsy) |
| **Bpa = 0.5 Bmsy** | Precautionary biomass: a buffer above the limit | The safeguard of the optimisation; default trigger of the rules |
| **Blim = 0.25 Bmsy** | Limit below which recruitment and productivity may be impaired | Risk P(B < Blim); the rules reach their minimum effort here |

**In practice.** ICES and GFCM advice sets the trigger of harvest control
rules at Bpa (often called MSY Btrigger) and requires that the risk of
falling below Blim is at most 5%. FAO reports stocks below 0.8 Bmsy as
overfished. The defaults of the app follow these conventions; all can be
changed.

---

## 2. Objectives: what to maximise

| Objective | Real-world meaning |
|---|---|
| **Total yield** | Food supply and landings: the multispecies MSY (MMSY) |
| **Revenue** | Value of the landings at first sale |
| **Profit** | Revenue minus fishing costs: maximum economic yield (MEY). Usually reached at much lower effort than MMSY, with higher catch rates and lower costs |
| **Catch of one stock** | Priority to one stock, e.g. a small pelagic that supplies a processing industry, or a high-value or culturally important species |

Whatever the objective, the **safeguard** is applied: no stock may fall
below Bpa at equilibrium. The stock that limits the solution is the
**choke stock**. In mixed fisheries this is often a slow-growing or highly
catchable species (e.g. reef fish, sharks, rays), and protecting it costs
yield of the healthier stocks caught alongside it.

**Single-stock MSYs do not add up.** Because stocks are caught together,
fishing every stock at its own Fmsy is not possible; the multispecies MSY is
lower than the sum of single-stock MSYs.

---

## 3. Effort limits (input control)

Most tropical and many mixed fisheries are managed by limiting fishing
effort (licences, vessel numbers, days at sea, gear) rather than catches,
because catches are hard to monitor across many species and landing sites.

* **One global cap.** A single limit on total days, shared by the fleet
  segments in their current proportions. Simple, but it cuts every fleet by
  the same proportion, including fleets that do not catch the choke stock.
* **Caps by segment.** Separate limits for e.g. trawl, purse seine and
  small-scale fisheries (licences or days by fleet). Cuts can target the
  fleet that drives the choke stock, so they usually give a higher yield for
  the same safeguard.
* **Viability floor.** With caps by segment, no segment is cut below a
  minimum share of the days. This reflects social and political
  constraints: livelihoods of small-scale fishers, food security, or keeping
  a fleet viable. It is an explicit trade-off between yield and equity.
* **Capacity** (set-up tab) is the registered fishing capacity: the most
  days a fleet could fish. Without limits, the days actually fished are
  those of the vessels that make a profit (open access).

**What the model adds.** Fishers respond to limits: when a segment's days
are capped, its vessels still choose *where* to fish by expected profit,
and unprofitable vessels stay in port. An effort limit therefore does not
reduce fishing mortality equally on all stocks; effort can concentrate on
the most valuable grounds, which may be those of the choke stock.

---

## 4. Static and feedback management

* **No control (open access).** The fleet at its capacity, limited only by
  profitability. The baseline to compare against.
* **Fixed MMSY effort.** Each segment fixed at the days of the safeguarded
  optimum (tab 4): a plan set once, like licence caps that are not revised.
  It works at equilibrium, but does not respond if stocks decline because of
  bad years, assessment errors or poor compliance.
* **Harvest control rule (HCR).** Effort is revised every year from the
  assessed status of the stocks: full effort when the stocks are above the
  trigger, reduced effort below it, down to a minimum at Blim. This is
  feedback (adaptive) management, as used by ICES, GFCM and tuna RFMOs, here
  applied to effort.

### Settings of the rule

| Setting | Real-world meaning |
|---|---|
| **Full effort** | Days allowed when stocks are healthy: the safeguarded-MMSY days (recommended), or today's days. With today's days as full effort, a stock that recovers is fished back down if the fishery is currently overfished |
| **Trigger** | Status below which effort is cut. Default 0.5 Bmsy (= Bpa, ICES/GFCM). Can differ by segment, e.g. higher for fleets targeting short-lived small pelagics, whose biomass can drop fast |
| **Minimum effort** | The rule reduces but does not close a fleet (default 10% of full effort): closing a fleet entirely is rarely acceptable, and some fishing keeps data flowing. 0 allows full closure |
| **Maximum annual change** | Limits year-to-year changes (default 20%), as for TAC stability clauses. Gives predictability to fishers, at the cost of a slower response |
| **Value-weighted or weakest link** | How a fleet's status is formed from the stocks it fishes. *Value-weighted* (default): the stocks that make up the fishery drive its effort. *Weakest link*: the most depleted stock decides. This is the "choke species" problem (e.g. the EU landing obligation, ICES mixed-fisheries "min" scenario): the most precautionary option, but with many depleted or slow stocks it can keep fleets at the minimum for decades |
| **By-catch responsibility** | Whether a fleet is cut for stocks it only catches as by-catch. Off by default: a fleet answers for the stocks it targets |

---

## 5. What is assessed

Assessments are expensive, and in most multispecies fisheries only some
stocks are assessed. The rule can only respond to what is assessed.

* **All stocks** (data-rich): every stock has its own assessment.
* **Indicator species**: a few well-monitored stocks stand for the fishery
  (e.g. one demersal, one pelagic, one invertebrate). Cheap, but stocks that
  are not assessed can decline unnoticed.
* **Species complexes**: stocks assessed together as a group (e.g. "reef
  fish", "demersal fish"), from pooled catch and effort or pooled survey
  biomass. Common where species are landed and recorded together. A healthy
  dominant species can hide a depleted member of the group.
* **Nominal assignment**: each stock is managed through one fleet (e.g. a
  pelagic stock "belongs" to the purse seine), although other fleets catch
  it. Simple to administer, but the other fleets are not constrained and the
  burden falls on the assigned fleet.

**In the app** (tab 5), tick the rules "HCR, indicator species", "HCR,
species complexes" or "HCR, nominal assignment" next to the "HCR,
value-weighted" rule (all stocks assessed) to see what each assessment
strategy costs or risks, with the same random draws. Defaults come from the
system and can be edited: one indicator per fleet segment (the stock of
highest value to it), the species groups as complexes (pooled biomass or
pooled CPUE), and each stock assigned to the segment for whose fishery it
matters most. The differences show most with the weakest link, where a
complex or a missing assessment can hide the depleted stock that would
otherwise cut effort. In the package: `hcr_effort(assessed =, complexes =, assign =)`
and `ms_assessment_presets()`.

---

## 6. Closures

Closures act on *where and when* fishing happens, not on how much.

* **Metier closures** (fixed): close one or more metiers (a gear on a
  ground) all year or for some months, fully or for part of the ground.
  Real-world examples: closed seasons during spawning (e.g. the seasonal
  closures for Indo-Pacific mackerel in the Gulf of Thailand), trawl bans in
  the coastal zone reserved for small-scale fisheries, reef or nursery area
  closures and marine protected areas. Often more practical than species
  rules, because they are easy to monitor and enforce.
* **Closure triggered by a stock**: the metiers that drive one choke stock
  are closed in proportion to how depleted it is (fully at Blim, reopened at
  the closure trigger). This protects the choke stock without cutting whole
  fleets, but needs an assessment of that stock.

**Displacement.** Closed days do not disappear: vessels move to the open
metiers by expected profit. A closure can therefore increase pressure on
other stocks; the model shows this directly.

---

## 7. Uncertainty: why rules are tested in simulation

| Source | Real-world meaning |
|---|---|
| **Data lag** | Advice for next year is based on data that are one or two years old (default 2) |
| **Assessment error** | Estimated status differs from the truth (default CV 0.2) |
| **Implementation error** | The days fished differ from the days allowed: compliance, enforcement, illegal and unreported fishing |
| **Effort noise** | Year-to-year variation in fishing behaviour and catch rates |
| **Process error** | Environmental variability in stock productivity; larger for fast, short-lived stocks and larger at low abundance |

A rule that looks good at equilibrium can fail under these uncertainties.
Every rule is therefore run many times with the **same random draws**, so
differences between rules are due to the rules, not to chance.

---

## 8. Reading the results

* **Risk**: P(B < Blim) for each stock and for any stock; a common standard
  is at most 5%.
* **Status**: B/Bmsy and P(B > 0.8 Bmsy) (the FAO "not overfished"
  criterion).
* **Benefits**: catch, revenue, profit.
* **Stability**: average annual variation of catch and of effort.
* **Employment and equity**: days fished by fleet segment.

No rule is best on everything. The decision table and trade-off plots make
the choice explicit: how much catch or profit is given up for how much less
risk, and which fleets bear the cost.
