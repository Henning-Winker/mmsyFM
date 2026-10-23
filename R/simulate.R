# Ported from the dev-mmsy prototype (cpueTargeting, branch dev-mmsy, dev/mmsy/R/msfleet.R).

#' Simulate the system forward
#'
#' Annual Schaefer growth on start-of-year biomass; catches removed
#' monthly (Baranov-type, C = B (1 - exp(-F))). Each month every segment
#' allocates its effort among metiers by expected profit computed from
#' the biomass it perceives (`info`: "current" = true current biomass,
#' "lagged" = last year's realised CPUE by metier).
#'
#' @param sys     an `ms_system`.
#' @param years   number of years.
#' @param info    expected value from the current biomass or last year's catch rates.
#' @param seed    random seed.
#' @param avg_years years kept when `keep = "final"`.
#' @param proc_dep_pars parameters a, c, d of the JABBA `proc.dyn` function.
#' @param caps    n x G matrix of annual effort caps by segment
#' @param gamma   preference exponent (0 = profit-blind)
#' @param behaviour "dynamic" (reallocate every month) or "fixed"
#'   (shares frozen at the unfished allocation: the classical shared-q model)
#' @param phi     entry/exit rate of each segment: E_t+1 = min(cap, E_t * exp(phi * margin_g)),
#'   margin_g = segment profit / segment revenue; 0 = effort always at the cap
#' @param open    métier access, 0-1 (the open share of each métier's ground or
#'   season): a vector of length M (all months), a 12 x M matrix (by month, e.g.
#'   a seasonal closure), or a function of the year returning either.
#' @param hcr     optional function(state) -> n x G multiplier on caps
#' @param hcr_lag years between the data a rule sees and the year its advice applies
#'   (default 2: data of year y - 2, assessed in y - 1, advice for y). Before the
#'   projection the data years are taken to be the initial state.
#' @param B_init  initial biomass (n x S, or one value per stock); default
#'   `sys$dep_init * K` (the system's starting depletion, 1 if not set)
#' @param E_init  initial effort by segment (vector of length G or n x G; default the caps)
#' @param proc_cv process error CV at the median r (JABBA form: the whole
#'   process equation is multiplied by exp(eta), eta ~ N(-sigma^2/2, sigma^2))
#' @param rho     AR(1) autocorrelation of the process error in time
#' @param proc_r_power the process CV of species i is proc_cv * (r_i / median r)^power:
#'   more productive species are more variable (0 = same CV for all)
#' @param proc_rho_sp correlation of the process error across species (shared environment)
#' @param proc_sd log-scale process SD by species (length S), overrides proc_cv and
#'   proc_r_power; e.g. proc_sd_from_r(sys$r) (0.05 for sharks, 0.4 for anchovy)
#' @param proc_dep depletion-dependent process error: "jabba" = JABBA proc.dyn
#'   (jabba2jags.R): sd_t = sd_i * relsig(B/Bmsy), relsig(x) = raw(x) / raw(1),
#'   raw(x) = a + (1 - a) / (1 + (x / c)^d), empirical ICES a = 0.128, c = 0.223,
#'   d = 1.326 (`proc_dep_pars`); relsig = 1 at Bmsy, ~1.5 at 0.5 Bmsy, -> 4.3 at 0.
#'   "power": see proc_dep_power. Capped at proc_sd_max.
#' @param proc_center "mean": exp(sd eps - sd^2/2) (mean-unbiased, the median drifts
#'   down as sd grows: stochastic depensation); "median": exp(sd eps), as in
#'   JABBA's dlnorm(Pmean, iPV) process equation
#' @param proc_dep_power,proc_dep_ref,proc_sd_max power form (proc_dep = "power"):
#'   sd_i,t = sd_i * ((B_i,t / K_i) / proc_dep_ref)^(-proc_dep_power), capped at
#'   proc_sd_max; sd_i applies at B/K = proc_dep_ref (Bmsy). proc_dep_power > 0:
#'   stocks become more volatile at low abundance; < 0: less volatile
#' @param effort_cv,effort_rho effort/catchability noise by segment, in every run
#'   (regulated or not): an annual lognormal multiplier on effective effort
#'   (weather, fuel, markets, catchability), AR(1) effort_rho, mean 1. Unlike the
#'   implementation error it acts when no cap binds.
#' @param impl_cv,impl_bias implementation error on the effort caps by segment:
#'   realised cap = cap * exp(bias + N(-s^2/2, s^2)), s from impl_cv
#' @param crn     common random numbers: rows i and i + crn share the same process
#'   (and rule) error draws, e.g. crn = nsim when the rows are nsim simulations
#'   repeated for each of several control settings
#' @param cost_seg daily cost by segment (length G or n x G), overrides sys$cost_seg
#' @param capacity fleet days by segment (registered vessels x days); default the caps.
#'   With vessel heterogeneity (sys$vessel_cv > 0) and phi > 0, the days used move
#'   towards capacity x the share of vessels that break even (lognormal vessel
#'   efficiency, mean 1), and the active vessels' mean efficiency is the mean of
#'   that upper tail. phi is then the annual adjustment rate (0-1).
#' @param F_cap   cap on each stock's realised F, in units of Fmsy (scalar or
#'   one per stock; default `sys$F_cap`, else `Inf`). F/Fmsy is measured as
#'   the harvest-rate ratio h/hmsy (see [ffmsy()]); the cap on h is converted
#'   to instantaneous F and applied monthly as one twelfth. Extra effort on a capped stock catches nothing more, so
#'   its catch per day falls (economic extinction: the stock is left to rare
#'   by-catch). In Schaefer equilibrium B/Bmsy = 2 - F/Fmsy, so a cap below 2
#'   sets a floor: `F_cap = 1.95` keeps every stock at or above 0.05 Bmsy.
#' @param F_cap_p shape of the cap: `Inf` = hard cap, min(F, F_cap); a finite
#'   p = smooth cap, (F^-p + F_cap^-p)^(-1/p), which approaches F_cap
#'   without a kink (p = 8 lowers F by about 1.5% at 1.5 Fmsy when F_cap = 1.95).
#'   Default `NULL`: hard without process error (equilibrium and tuning),
#'   p = 8 with process error (stochastic runs).
#' @param keep    "final" (equilibrium summaries) or "all" (trajectories)
#' @return An object of class `ms_sim`: arrays `B`, `C`, `F` (years x rows x stocks), `E_metier`, `E_seg_metier`, `E_seg`, matrices `revenue`, `profit`, the system, and `init` (the year-0 biomass and days, rows x stocks / segments).
#' @export
ms_simulate <- function(sys, caps, years = 100, gamma = 1.5,
                        behaviour = c("dynamic", "fixed"),
                        info = c("current", "lagged"),
                        phi = 0.5, open = 1, hcr = NULL, hcr_lag = 2, B_init = NULL, E_init = NULL, capacity = NULL,
                        cost_seg = NULL,
                        proc_cv = 0, rho = 0, proc_r_power = 1, proc_rho_sp = 0,
                        proc_sd = NULL, proc_dep = c("none", "jabba", "power"),
                        proc_dep_pars = c(a = 0.128, c = 0.223, d = 1.326),
                        proc_dep_power = 0, proc_dep_ref = 0.5, proc_sd_max = 1,
                        proc_center = c("mean", "median"),
                        impl_cv = 0, impl_bias = 0, effort_cv = 0, effort_rho = 0,
                        seed = NULL, crn = NULL, keep = c("final", "all"),
                        avg_years = 10, F_cap = sys$F_cap %||% Inf, F_cap_p = NULL) {
  behaviour <- match.arg(behaviour); info <- match.arg(info); keep <- match.arg(keep)
  proc_dep <- match.arg(proc_dep); proc_center <- match.arg(proc_center)
  if (proc_dep == "none" && proc_dep_power != 0) proc_dep <- "power"
  raw_pe <- function(x) proc_dep_pars[["a"]] + (1 - proc_dep_pars[["a"]]) / (1 + (x / proc_dep_pars[["c"]])^proc_dep_pars[["d"]])
  if (!is.null(seed)) set.seed(seed)
  caps <- as.matrix(caps); n <- nrow(caps); S <- sys$S; M <- sys$M; G <- sys$G
  K <- matrix(sys$K, n, S, byrow = TRUE); r <- matrix(sys$r, n, S, byrow = TRUE)
  price <- sys$price; Q <- sys$Q; tQ <- t(Q)
  cost_m <- matrix(sys$cost, n, M, byrow = TRUE)
  B <- if (is.null(B_init)) K * matrix(sys$dep_init %||% 1, n, S, byrow = TRUE)
       else matrix(B_init, n, S, byrow = is.null(dim(B_init)))
  # with a rule, the starting days are not clipped to the caps: the rule phases the move
  # from the current days to its reference (e.g. status quo -> MMSY effort)
  E_seg <- if (is.null(E_init)) caps else if (!is.null(hcr)) matrix(E_init, n, G, byrow = is.null(dim(E_init)))
           else pmin(caps, matrix(E_init, n, G, byrow = is.null(dim(E_init))))
  init <- list(B = B, E_seg = E_seg)                 # year-0 state (for plots of projections)
  open_fun <- if (is.function(open)) open else function(y) open
  F_cap <- rep_len(F_cap, sys$S)
  h_cap <- F_cap * sys$r / 2                  # cap on the annual harvest rate h = 1 - exp(-F)
  Fcap_mo <- matrix(ifelse(h_cap < 1, -log1p(-pmin(h_cap, 1)), Inf) / 12, n, sys$S, byrow = TRUE)
  Bbuf <- replicate(hcr_lag + 1, B, simplify = FALSE)      # start-of-year biomass, most recent first
  capacity <- if (is.null(capacity)) caps else matrix(capacity, n, G, byrow = is.null(dim(capacity)))
  emult <- matrix(1, n, G)
  idx <- if (is.null(crn)) seq_len(n) else (seq_len(n) - 1) %% crn + 1
  nd <- if (is.null(crn)) n else crn
  cseg <- if (is.null(cost_seg)) matrix(sys$cost_seg, n, G, byrow = TRUE) else
    matrix(cost_seg, n, G, byrow = is.null(dim(cost_seg)))
  sv <- sys$vessel_cv; sdv <- if (sv > 0) sqrt(log(1 + sv^2)) else 0; muv <- -sdv^2 / 2
  w_fixed <- NULL
  if (behaviour == "fixed") {
    # shares frozen at the unfished allocation (per unit segment effort)
    w_fixed <- (K * matrix(price, n, S, byrow = TRUE)) %*% tQ   # unfished revenue rate, frozen
  }
  cpue_last <- (B * matrix(price, n, S, byrow = TRUE)) %*% tQ   # value per effort, last year
  eps <- 0
  if (is.null(proc_sd)) {
    cv_i <- proc_cv * (sys$r / stats::median(sys$r))^proc_r_power
    proc_sd <- sqrt(log(1 + cv_i^2))
  }
  sig_i <- matrix(proc_sd, n, S, byrow = TRUE)                       # by species, at proc_dep_ref
  sig <- max(proc_sd)
  if (is.null(F_cap_p)) F_cap_p <- if (sig > 0) 8 else Inf   # hard cap deterministic, smooth stochastic
  sig_impl <- if (impl_cv > 0) sqrt(log(1 + impl_cv^2)) else 0
  sig_eff <- if (effort_cv > 0) sqrt(log(1 + effort_cv^2)) else 0
  qdev <- matrix(1, n, G); edev <- NULL

  nkeep <- if (keep == "all") years else min(avg_years, years)
  Bk <- array(NA_real_, c(nkeep, n, S)); Ck <- Bk; Fk <- Bk
  Ek <- array(NA_real_, c(nkeep, n, M)); Rk <- matrix(NA_real_, nkeep, n); Pk <- Rk
  Esk <- array(NA_real_, c(nkeep, n, G)); Egk <- array(NA_real_, c(nkeep, n, G, M))

  for (y in seq_len(years)) {
    op <- open_fun(y)
    monthly <- is.matrix(op) && nrow(op) == 12 && ncol(op) == M
    op_mo <- if (monthly) op else matrix(1, 12, M)              # month-by-metier open share
    opm <- if (monthly) matrix(1, n, M) else
      matrix(if (is.null(dim(op))) rep_len(op, M) else op, n, M, byrow = is.null(dim(op)))
    E_cap_y <- caps
    if (!is.null(hcr)) {
      # a rule returns multipliers on the caps (n x G), or list(mult, open) where
      # open (n x M, 0-1) is the share of each metier's ground/season left open
      Bbuf <- c(list(B), Bbuf)[seq_len(hcr_lag + 1)]
      ctl <- hcr(list(B = Bbuf[[hcr_lag + 1]], K = K, year = y, sys = sys, idx = idx, nd = nd, lag = hcr_lag,
                      E = E_seg, caps = caps))
      if (is.list(ctl)) {
        if (!is.null(ctl$mult)) E_cap_y <- caps * ctl$mult
        if (!is.null(ctl$open)) opm <- opm * ctl$open
      } else E_cap_y <- caps * ctl
    }
    if (sig_eff > 0) {                                        # effort / catchability noise
      ze <- matrix(rnorm(nd * G), nd, G)[idx, , drop = FALSE]
      edev <- if (is.null(edev) || effort_rho == 0) ze else effort_rho * edev + sqrt(1 - effort_rho^2) * ze
      qdev <- exp(sig_eff * edev - sig_eff^2 / 2)
    }
    if (sig_impl > 0 || impl_bias != 0) {                    # implementation error
      zi <- matrix(rnorm(nd * G), nd, G)[idx, , drop = FALSE]
      E_cap_y <- E_cap_y * exp(impl_bias + sig_impl * zi - sig_impl^2 / 2)
    }
    E_seg <- pmin(E_seg, E_cap_y); if (phi == 0) E_seg <- E_cap_y
    Bm <- B
    C_y <- matrix(0, n, S); F_y <- C_y; Em_y <- matrix(0, n, M); rev_m <- matrix(0, n, M); Eg_y <- array(0, c(n, G, M)); Eeff_y <- Em_y
    opm_y <- opm
    for (mo in 1:12) {
      opm <- opm_y * matrix(op_mo[mo, ], n, M, byrow = TRUE)     # this month's open share
      av <- matrix(sys$avail[mo, ], n, S, byrow = TRUE)
      Bav <- Bm * av
      if (behaviour == "fixed") {
        E_m <- allocate_effort(sys, w_fixed * opm, E_seg * sys$month_w[mo], gamma, opm > 0, cost = cost_m, emult = emult * qdev, cseg = cseg)
      } else {
        exp_val <- if (info == "current") (Bav * matrix(price, n, S, byrow = TRUE)) %*% tQ
                   else cpue_last
        # a partly closed metier fishes only its open share: value per day scales with it
        E_m <- allocate_effort(sys, exp_val * (if (info == "current") opm else 1), E_seg * sys$month_w[mo],
                               gamma, opm > 0, cost = cost_m, emult = emult * qdev, cseg = cseg)
      }
      E_eff <- attr(E_m, "effective") * opm         # effective effort on the open share
      Fm <- (E_eff %*% Q) * av                     # n x S nominal fishing mortality this month
      Fs <- pmax(Fm, 1e-12)
      if (any(is.finite(F_cap)))                    # saturation: realised F at most F_cap Fmsy
        Fm <- if (is.finite(F_cap_p)) (Fm^-F_cap_p + Fcap_mo^-F_cap_p)^(-1 / F_cap_p) else pmin(Fm, Fcap_mo)
      Cm <- Bm * (1 - exp(-Fm))
      # catch by metier (proportional split of each species' catch by F share)
      share_m <- array(0, c(n, M))
      for (m in seq_len(M)) {
        Fim <- outer(E_eff[, m], Q[m, ]) * av
        rev_m[, m] <- rev_m[, m] + rowSums((Cm * Fim / Fs) %*% diag(price, S))
      }
      Bm <- Bm - Cm
      C_y <- C_y + Cm; F_y <- F_y + Fm; Em_y <- Em_y + E_m; Eg_y <- Eg_y + attr(E_m, "by_seg"); Eeff_y <- Eeff_y + E_eff
    }
    opm <- opm_y * matrix(colMeans(op_mo), n, M, byrow = TRUE)  # annual open share (exit decisions)
    cpue_last <- ifelse(Eeff_y > 0, rev_m / pmax(Eeff_y, 1e-12), cpue_last)   # per effective effort
    revenue <- as.vector(C_y %*% price)
    costs <- rowSums(matrix(sapply(seq_len(G), function(g)
      rowSums(matrix(Eg_y[, g, ], n, M) * cost_m) * cseg[, g]), n, G))
    # growth on start-of-year biomass with process error
    if (sig > 0) {
      z <- matrix(rnorm(nd * S), nd, S)
      if (proc_rho_sp > 0) z <- sqrt(1 - proc_rho_sp) * z + sqrt(proc_rho_sp) * stats::rnorm(nd)
      z <- z[idx, , drop = FALSE]
      eps <- if (y == 1 || rho == 0) z else rho * eps + sqrt(1 - rho^2) * z
      sd_t <- switch(proc_dep,
        none  = sig_i,
        power = pmin(sig_i * ((B / K) / proc_dep_ref)^(-proc_dep_power), proc_sd_max),
        jabba = pmin(sig_i * raw_pe(2 * B / K) / raw_pe(1), proc_sd_max))
      pe <- if (proc_center == "mean") exp(sd_t * eps - sd_t^2 / 2) else exp(sd_t * eps)
    } else pe <- 1
    keep_idx <- if (keep == "all") y else y - (years - nkeep)
    if (keep_idx >= 1) {
      Bk[keep_idx, , ] <- B; Ck[keep_idx, , ] <- C_y; Fk[keep_idx, , ] <- F_y; Ek[keep_idx, , ] <- Em_y; Egk[keep_idx, , , ] <- Eg_y
      Rk[keep_idx, ] <- revenue; Pk[keep_idx, ] <- revenue - costs; Esk[keep_idx, , ] <- E_seg
    }
    # JABBA-type process error, with JABBA's floor on the surplus-production update (0.005 K):
    # for fast stocks (r > 1) a shock above K makes the logistic update negative (overshoot);
    # the floor keeps the stock from being absorbed at zero
    B <- pmax(B + r * B * (1 - B / K) - C_y, 0.005 * K) * pe
    # entry / exit below the cap
    if (phi > 0) {
      for (g in seq_len(G)) {
        Egm <- matrix(Eg_y[, g, ], n, M); tot <- rowSums(Egm)
        acc <- if (sys$loyalty[g] < 1) rep(TRUE, M) else sys$home[g, ] > 0
        # revenue and cost per day of a vessel of mean efficiency: allocation-weighted
        # when the segment fishes, otherwise its best open metier
        val <- cpue_last * matrix(sys$eff[g, ], n, M, byrow = TRUE) * (opm > 0)   # realised: already net of closure
        cst <- cost_m * cseg[, g]
        net <- val - cst; net[, !acc] <- -Inf
        best <- max.col(net, ties.method = "first")
        rev_day <- ifelse(tot > 0, rowSums(Egm * val) / pmax(tot, 1e-12), val[cbind(seq_len(n), best)])
        cost_day <- ifelse(tot > 0, rowSums(Egm * cst) / pmax(tot, 1e-12), cst[cbind(seq_len(n), best)])
        if (sdv > 0) {
          # heterogeneous vessels: those with efficiency e > e* = cost / revenue break even
          lest <- log(pmax(cost_day, 1e-12) / pmax(rev_day, 1e-12))
          pg <- 1 - stats::pnorm((lest - muv) / sdv)
          emult[, g] <- pmin(stats::pnorm((muv + sdv^2 - lest) / sdv) / pmax(pg, 1e-9), exp(3 * sdv))
          target <- pmin(E_cap_y[, g], capacity[, g] * pg)
          E_seg[, g] <- E_seg[, g] + min(phi, 1) * (target - E_seg[, g])
        } else {
          margin <- ifelse(rev_day > 0, (rev_day - cost_day) / rev_day, -1)
          E_seg[, g] <- pmin(E_cap_y[, g], pmax(E_seg[, g], 1e-4 * E_cap_y[, g]) * exp(phi * margin))
        }
      }
    }
  }
  out <- list(B = Bk, C = Ck, F = Fk, E_metier = Ek, E_seg_metier = Egk, E_seg = Esk, revenue = Rk, profit = Pk,
              caps = caps, sys = sys, init = init,
              settings = list(gamma = gamma, behaviour = behaviour, info = info, phi = phi,
                              years = years, proc_cv = proc_cv))
  class(out) <- "ms_sim"
  out
}


#' Summaries over the kept years
#'
#' Means over the kept years of an [ms_simulate()] run (the equilibrium when
#' `keep = "final"` and the run is long enough).
#' @param x an `ms_sim` object.
#' @param Bpa,Blim reference points relative to Bmsy.
#' @return A data frame with one row per simulation row (yield, revenue, profit, days, lowest B/Bmsy, ...), with attributes `BBmsy`, `FFmsy` and `catch` (rows x stocks).
#' @export
ms_summary <- function(x, Bpa = 0.5, Blim = 0.25) {
  sys <- x$sys
  Bmsy <- sys$K / 2
  Bbar <- apply(x$B, c(2, 3), mean)               # n x S
  Cbar <- apply(x$C, c(2, 3), mean)
  BBmsy <- sweep(Bbar, 2, Bmsy, "/")
  colnames(BBmsy) <- colnames(Cbar) <- sys$species
  data.frame(
    yield = rowSums(Cbar), revenue = colMeans(x$revenue), profit = colMeans(x$profit),
    E_total = rowSums(apply(x$E_seg, c(2, 3), mean)),
    min_BBmsy = apply(BBmsy, 1, min),
    worst = sys$species[apply(BBmsy, 1, which.min)],
    max_FFmsy = apply(ffmsy(apply(x$F, c(2, 3), function(f) mean(1 - exp(-f))), sys$r, h = TRUE), 1, max),
    n_below_Bpa = rowSums(BBmsy < Bpa), n_below_Blim = rowSums(BBmsy < Blim),
    check.names = FALSE) -> out
  FFmsy <- ffmsy(apply(x$F, c(2, 3), function(f) mean(1 - exp(-f))), sys$r, h = TRUE)
  colnames(FFmsy) <- sys$species
  attr(out, "BBmsy") <- BBmsy; attr(out, "FFmsy") <- FFmsy; attr(out, "catch") <- Cbar
  out
}


#' F relative to Fmsy in the discrete Schaefer model
#'
#' Growth is computed on start-of-year biomass and a year of fishing removes
#' B (1 - exp(-F)), so equilibrium is B/K = 1 - h/r with harvest rate
#' h = 1 - exp(-F), hmsy = r/2 and B/Bmsy = 2 - h/hmsy exactly. `ffmsy()`
#' returns h/hmsy. (Using r/2 for instantaneous F overstates F/Fmsy for fast
#' stocks: for r = 1.4, Fmsy = -log(1 - r/2) = 1.2, not 0.7.)
#'
#' @param F instantaneous annual F (vector, or matrix with stocks in columns).
#' @param r intrinsic growth rate(s), one per stock.
#' @param h if `TRUE`, `F` is already a harvest rate.
#' @return h / hmsy, same shape as `F`.
#' @export
#' @examples
#' ffmsy(-log(1 - 0.7), r = 1.4)   # 1: fished at MSY
ffmsy <- function(F, r, h = FALSE) {
  hr <- if (h) F else 1 - exp(-F)
  if (is.null(dim(hr))) hr / (r / 2) else sweep(hr, length(dim(hr)), r / 2, "/")
}
