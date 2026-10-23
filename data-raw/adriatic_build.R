# Build the Adriatic demersal system (GFCM GSAs 17-18) -- draft, indicative.
#
# Sources (reference year 2023 unless stated):
#  * Stock status (B/Btarget, F/Ftarget): GFCM SRC-AS 2025 report, Appendix 4, Table 1
#    (WGSAD December 2024).
#  * Norway lobster functional units (17C Pomo, 17NW, 17NE, 18): Stock Assessment Form
#    NEP GSA 17-18, reference year 2023 (JABBA Fox models, Tables 7 and 13.1-13.4).
#  * Hake: WGSAD benchmark HKE 17-18 (2024), Table 6 (catch by operational unit, 2022).
#  * Red mullet: WGSAD benchmark MUT 17-18 (2024), Figure 4 (landings by fleet, 2022).
#  * Deep-water rose shrimp: WGSAD benchmark DPS 12-20 (2023), SPiCT r prior 1.4.
#  * Cuttlefish, mantis shrimp (GSA 17) and deep-water rose shrimp (GSAs 17-20): Stock
#    Assessment Forms, reference year 2024 (status and landings by fleet, 2024).
#  * Total catches 2023 of hake, red mullet and sole: SoMFI 2025 chapter 4 time series
#    (STAR 2024); sole landings by gear split as rapido 68%, set nets 25%, trawl 4%,
#    Croatian nets 3% (estimate, to be checked).
#  * Spatial distribution of stocks by area: Panzeri et al. (2024) Fish and Fisheries
#    25:187-204, ensemble species distribution models of MEDITS/SOLEMON surveys
#    (hot spots of adults and juveniles, Figures 3-6), read by area.
#  * Fleets and effort: Recommendation GFCM/43/2019/5 (effort regime, Annex 4) and FAO
#    SoMFI 2025 (1 378 authorised trawlers, 103 in the Jabuka/Pomo FRA).
#  * Closures: Jabuka/Pomo Pit FRA (Rec. GFCM/41/2017/3), Otranto Channel FRA
#    (Rec. GFCM/47/2024/6; buffer zone 400-800 m closed to trawl, set nets, longlines and
#    traps in September-November).
#
# Productivity: r = 2 Fmsy for the Nephrops units (Schaefer equivalent of the JABBA Fmsy);
# for the other stocks from the assessment priors and life history (indicative).
# Carrying capacity from the 2023 catch, F/Fmsy and B/Bmsy: in the Schaefer model
# C = F B with F = (F/Fmsy) r/2 and B = (B/Bmsy) K/2, so K = 4 C / (r (F/Fmsy) (B/Bmsy)).
# The assessments use F and B targets (F35/F40/Fmsy, B35/B40/Bmsy); they are used here as
# proxies of Fmsy and Bmsy.

library(mmsyFM)
out <- "inst/extdata"

# ---- areas (cells) ------------------------------------------------------------------
# 17NW: Italian (and Slovenian) side of GSA 17, incl. the Po delta and west of Istria
# 17NE: Croatian side of GSA 17, incl. Kvarner and the channels
# Pomo: Jabuka/Pomo Pits (central GSA 17, 200-270 m)
# 18shelf: GSA 18 shelf (< 200 m; Italy, Montenegro, Albania)
# 18slope: GSA 18 upper slope of the South Adriatic Pit (200-800 m)
cells <- data.frame(cell = c("17NW", "17NE", "Pomo", "18shelf", "18slope"),
                    depth = c(40, 60, 230, 90, 450),
                    substrate = c("17NW", "17NE", "Pomo", "18shelf", "18slope"),
                    area = c(47000, 42000, 3600, 13000, 7000))      # km2, approximate

# ---- stocks -------------------------------------------------------------------------
# catch: tonnes, the most recent year by fleet (see source); FFmsy, BBmsy: F/Ftarget and
# B/Btarget of the latest assessment. Rose shrimp: the Adriatic part (GSAs 17-18) of the
# GSAs 17-20 stock (3 709 of 4 605 t in 2024), with the status of the whole stock.
st <- read.csv(text = "
species,code,group,r,price,FFmsy,BBmsy,plan,unit,source
European hake,HKE,demersal fish,0.45,7,1.11,0.48,1,,SS3 benchmark 2024 (RefY 2023); catch 2022 by operational unit
Red mullet,MUT,demersal fish,0.8,6,0.19,1.71,1,,SS3 benchmark 2024 (RefY 2023); landings 2022 by fleet
Common sole,SOL,flatfish,0.6,14,0.73,1.30,1,,SS3 (RefY 2023); landings by gear estimated
Spottail mantis shrimp,MTS,crustacean,0.8,10,0.96,0.86,0,,SS3 SAF RefY 2024; landings 2024 by gear
Common cuttlefish,CTC,cephalopod,0.9,9,0.69,0.38,0,,JABBA SAF RefY 2024 (low productivity); landings 2024 by fleet
Deep-water rose shrimp,DPS,crustacean,1.6,9,1.24,0.81,1,,SPiCT SAF RefY 2024 (GSAs 17-20; r prior 1.5); landings 2024 GSAs 17-18
Norway lobster 17C (Pomo),NEP17C,Norway lobster,0.29,18,0.21,1.65,1,NEP,JABBA SAF RefY 2023 (FU 17C)
Norway lobster 17NW (ITA),NEP17NW,Norway lobster,0.25,18,0.32,0.40,1,NEP,JABBA SAF RefY 2023 (FU 17NW)
Norway lobster 17NE (HRV),NEP17NE,Norway lobster,0.25,18,1.34,0.59,1,NEP,JABBA SAF RefY 2023 (FU 17NE)
Norway lobster 18,NEP18,Norway lobster,0.28,18,0.50,0.39,1,NEP,JABBA SAF RefY 2023 (FU 18)
", stringsAsFactors = FALSE)

# ---- metiers, fleets and status-quo days ----------------------------------------------
met <- read.csv(text = "
metier,tactic,ground,segment,days
OTB_17NW,Bottom otter trawl - western GSA 17 (ITA SVN),17NW,OTB,62000
OTB_17NE,Bottom otter trawl - eastern GSA 17 (HRV),17NE,OTB,33000
OTB_Pomo,Bottom otter trawl - Jabuka/Pomo Pits outside the FRA core (ITA HRV),Pomo,OTB,10000
OTB_18shelf,Bottom otter trawl - GSA 18 shelf (ITA ALB MNE),18shelf,OTB,30000
OTB_18slope,Bottom otter trawl - GSA 18 upper slope of the South Adriatic Pit (ITA ALB),18slope,OTB,15000
TBB_17,Beam trawl rapido - sole (ITA),17NW;17NE,TBB_sole,7000
GTR_17NW,Trammel nets gillnets pots and fykes - western GSA 17 (ITA),17NW,gillnet,40000
GNS_17NE,Gillnets and trammel nets - eastern GSA 17 (HRV),17NE,gillnet,20000
FPO_17NE,Creels - Norway lobster (HRV),17NE,gillnet,8000
GNS_18,Gillnets and trammel nets - GSA 18 (ITA MNE ALB),18shelf,gillnet,4000
LLS_18deep,Bottom longline - GSA 18 deep (ITA),18slope,LLS_18deep,20000
LLS_17E,Bottom longline - eastern GSA 17 (HRV),17NE,LLS_17E,25000
", stringsAsFactors = FALSE)

# ---- catch by metier and stock (t) ------------------------------------------------------
# Country x GSA x gear catches allocated to the areas with the distributions of Panzeri et
# al. (2024): e.g. Italian GSA 17 trawl catches of hake split between 17NW and the Pomo
# Pits, Croatian trawl catches between 17NE and the Pomo Pits.
C <- matrix(0, nrow(met), nrow(st), dimnames = list(met$metier, st$code))
set <- function(code, x) C[names(x), code] <<- x
set("HKE", c(OTB_17NW = 1700, OTB_Pomo = 1029, OTB_17NE = 1000, OTB_18shelf = 1100, OTB_18slope = 661,
             LLS_17E = 145, LLS_18deep = 113, GNS_17NE = 100, GNS_18 = 25))
set("MUT", c(OTB_17NW = 1400, OTB_17NE = 550, GNS_17NE = 100, OTB_18shelf = 570, GNS_18 = 30, TBB_17 = 20))
set("SOL", c(TBB_17 = 1100, GTR_17NW = 400, OTB_17NW = 60, GNS_17NE = 40))                    # estimate
set("MTS", c(OTB_17NW = 2014, TBB_17 = 180, GTR_17NW = 391))
set("CTC", c(OTB_17NW = 457, TBB_17 = 218, GTR_17NW = 710, OTB_17NE = 11.5, GNS_17NE = 84))
set("DPS", c(OTB_17NW = 500, OTB_Pomo = 703, OTB_17NE = 400, OTB_18shelf = 606, OTB_18slope = 1500))
set("NEP17C", c(OTB_Pomo = 392))
set("NEP17NW", c(OTB_17NW = 59))
set("NEP17NE", c(OTB_17NE = 62, FPO_17NE = 75))
set("NEP18", c(OTB_18slope = 136))
# scale to the 2023 totals of the SoMFI 2025 time series (same year as the status)
C[, "HKE"] <- C[, "HKE"] * 6298 / sum(C[, "HKE"])
C[, "MUT"] <- C[, "MUT"] * 2203 / sum(C[, "MUT"])
C[, "SOL"] <- C[, "SOL"] * 1548 / sum(C[, "SOL"])
st$catch <- colSums(C)

# ---- productivity and carrying capacity -------------------------------------------------
# Schaefer: C = F B, F = (F/Fmsy) r / 2, B = (B/Bmsy) K / 2  =>  K = 4 C / (r (F/Fmsy) (B/Bmsy))
st$K <- round(4 * st$catch / (st$r * st$FFmsy * st$BBmsy))
st$dep_now <- st$BBmsy / 2

# ---- catchability from catch, effort and biomass ---------------------------------------
# q[m, s] = C[m, s] / (days[m] B[s]): the catch per day of each metier relative to the
# stock's biomass. ms_condition() then fine-tunes each stock's column so that the
# status-quo equilibrium reproduces the assessed F/Fmsy.
B <- st$BBmsy * st$K / 2
Q <- sweep(C / met$days, 2, B, "/")

# Spatial check against Panzeri et al. (2024): relative density of each stock across the
# trawl grounds implied by the catch per day, against the distribution of hot spots.
dist <- read.csv(text = "
cell,HKE,MUT,SOL,MTS,CTC,DPS,NEP17C,NEP17NW,NEP17NE,NEP18
17NW,0.22,0.38,0.70,0.85,0.55,0.12,0,1,0,0
17NE,0.30,0.27,0.30,0.15,0.45,0.13,0,0,1,0
Pomo,0.13,0,0,0,0,0.18,1,0,0,0
18shelf,0.20,0.35,0,0,0,0.17,0,0,0,0.1
18slope,0.15,0,0,0,0,0.40,0,0,0,0.9
", stringsAsFactors = FALSE)

# ---- write the tables ------------------------------------------------------------------
seg <- data.frame(segment = c("OTB", "TBB_sole", "gillnet", "LLS_18deep", "LLS_17E"),
                  plan = c(1, 1, 0, 0, 0), loyalty = 1, efficiency = 1,
                  cost_multiplier = c(1, 1, 0.15, 0.2, 0.2), vessel_cv = 0.25)
seg$capacity <- tapply(met$days, met$segment, sum)[seg$segment]
seg$share_target <- seg$capacity / sum(seg$capacity)
seg$home <- vapply(seg$segment, function(g) paste(met$metier[met$segment == g], collapse = ";"), "")

sp <- data.frame(species = st$species, code = st$code, group = st$group, r = st$r, K = st$K,
                 price = st$price, F_cap = 1.95, dep_now = st$dep_now, plan = st$plan,
                 unit = ifelse(is.na(st$unit) | st$unit == "", st$code, st$unit),
                 FFmsy_now = st$FFmsy, BBmsy_now = st$BBmsy, catch = st$catch, source = st$source)
colnames(Q) <- st$species
sys0 <- ms_system(sp, met[, c("metier", "tactic", "ground")], data.frame(metier = rownames(Q), Q, check.names = FALSE),
                  seg[, c("segment", "share_target", "loyalty", "efficiency", "cost_multiplier", "vessel_cv",
                          "capacity", "plan", "home")])
cap <- sys0$capacity

# ---- calibration ------------------------------------------------------------------------
# Status quo = open access within capacity (the effort regime caps trawl days). Iterate:
# (1) each metier's daily cost so that the days by metier match the observed days;
# (2) each stock's catchability column so that its F/Fmsy matches the assessment.
calibrate <- function(sys, iter = 45, gamma = 1.5, years = 200, damp = 0.35, verbose = TRUE) {
  for (i in seq_len(iter)) {
    x <- ms_simulate(sys, matrix(cap, 1), years, capacity = matrix(cap, 1), gamma = gamma, avg_years = 20)
    Em <- colMeans(x$E_metier[, 1, ])
    sq <- ms_status_quo(sys, capacity = cap, gamma = gamma, years = years)
    eF <- sq$FFmsy / st$FFmsy; eE <- (Em + 1) / met$days
    if (verbose) message(sprintf("%2d | max |F/Fmsy ratio - 1| %.3f | max |days ratio - 1| %.3f", i,
                                 max(abs(eF - 1)), max(abs(eE - 1))))
    if (max(abs(eF - 1)) < 0.02 && max(abs(eE - 1)) < 0.05) break
    sys$cost <- sys$cost * pmin(pmax(eE, 0.5), 2)^damp
    f <- pmin(pmax(1 / eF, 0.2), 5)^damp
    sys$Q <- sweep(sys$Q, 2, f, "*")
  }
  sys
}
sys1 <- calibrate(sys0)
sq <- ms_status_quo(sys1, capacity = cap, gamma = 1.5, years = 300)
x <- ms_simulate(sys1, matrix(cap, 1), 300, capacity = matrix(cap, 1), gamma = 1.5, avg_years = 20)
check_days <- data.frame(metier = met$metier, observed = met$days, model = round(colMeans(x$E_metier[, 1, ])))
check_F <- data.frame(stock = st$code, F_assessed = st$FFmsy, F_model = round(sq$FFmsy, 2),
                      B_assessed = st$BBmsy, B_equilibrium = round(sq$BBmsy, 2))
print(check_days); print(check_F)

# ---- write ------------------------------------------------------------------------------
sys1$dep_init <- pmin(pmax(sq$BBmsy / 2, 1e-3), 1.5)          # status-quo equilibrium (fast start)
write_ms_system(sys1, out, "adriatic")
spf <- file.path(out, "adriatic_species.csv")
spx <- read.csv(spf, check.names = FALSE)
spx$catch <- st$catch; spx$FFmsy_now <- st$FFmsy; spx$BBmsy_now <- st$BBmsy; spx$source <- st$source
write.csv(spx, spf, row.names = FALSE)
write.csv(data.frame(metier = met$metier, segment = met$segment, days = met$days, round(C, 1), check.names = FALSE),
          file.path(out, "adriatic_catch.csv"), row.names = FALSE)
write.csv(cbind(cells[, c("cell", "area")], dist[, -1]), file.path(out, "adriatic_distribution.csv"), row.names = FALSE)
