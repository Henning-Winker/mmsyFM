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
#  * Sole, mantis shrimp, cuttlefish, rose shrimp catches by gear: GFCM WGSAD 2024 report.
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
st <- read.csv(text = "
species,code,group,r,price,catch,FFmsy,BBmsy,source
European hake,HKE,demersal fish,0.45,7,5873,1.11,0.48,SS3 benchmark 2024; catch 2022 (HKE benchmark Table 6)
Red mullet,MUT,demersal fish,0.8,6,2650,0.19,1.71,SS3 benchmark 2024; landings 2022 (MUT benchmark Fig. 4)
Common sole,SOL,flatfish,0.6,14,NA,0.73,1.30,SS3 2023
Spottail mantis shrimp,MTS,crustacean,0.8,10,NA,0.86,1.06,SS3 2023
Common cuttlefish,CTC,cephalopod,0.9,9,NA,0.73,0.33,JABBA 2023 (low productivity regime)
Deep-water rose shrimp,DPS,crustacean,1.4,9,NA,1.13,0.93,SPiCT 2023 (GSAs 17-20; Adriatic part)
Norway lobster 17C (Pomo),NEP17C,Norway lobster,0.29,18,391,0.21,1.65,JABBA 2023 (FU 17C)
Norway lobster 17NW (ITA),NEP17NW,Norway lobster,0.25,18,59,0.32,0.40,JABBA 2023 (FU 17NW)
Norway lobster 17NE (HRV),NEP17NE,Norway lobster,0.25,18,137,1.34,0.59,JABBA 2023 (FU 17NE)
Norway lobster 18,NEP18,Norway lobster,0.28,18,136,0.50,0.39,JABBA 2023 (FU 18)
", stringsAsFactors = FALSE)
