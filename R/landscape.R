#' Catchability from a habitat landscape
#'
#' Builds the catchability matrix from where stocks live and where (and how)
#' metiers fish, instead of writing it by hand:
#'
#' * the seabed is a set of `cells` with a depth, a substrate and an area;
#' * each stock's habitat suitability in a cell is a normal depth preference
#'   times a substrate weight; its distribution over the cells is
#'   suitability x area, normalised to 1;
#' * each metier fishes a ground (a depth range and a set of substrates) with
#'   a gear selectivity for each stock.
#'
#' The catchability of metier j for stock s is
#'
#'     q[j, s] = q0 x sel[j, s] x (share of s on ground j / area share of ground j)
#'
#' i.e. q0 x selectivity x the stock's density on the ground relative to its
#' average density. A stock spread evenly over the whole seabed has
#' q = q0 x sel on every ground. The level of each stock's column is usually
#' set afterwards by [ms_condition()]; the landscape gives its shape (which
#' metiers catch the stock, and how strongly relative to each other).
#'
#' @param cells data frame with `depth`, `substrate` and `area` (any units).
#' @param species data frame with `species`, `depth_mean`, `depth_sd` and one
#'   column per substrate level (weights, e.g. `mud`, `sand`, `hard`; missing
#'   levels get weight 0, all missing = 1).
#' @param metiers data frame with `metier`, `depth_min`, `depth_max` and
#'   optional `substrate` (levels separated by `;`, empty = all).
#' @param selectivity gear selectivity: a matrix (metiers x species, with
#'   dimnames) or a data frame with a `metier` column and one column per
#'   stock. Values 0-1.
#' @param q0 catchability at average density and selectivity 1.
#' @param distribution optional: each stock's share of its biomass in each
#'   cell (cells x stocks, columns named by stock), e.g. from species
#'   distribution models or survey hot spots. It replaces the depth and
#'   substrate preferences of `species` (which then only needs the column
#'   `species`); columns are normalised to sum to 1.
#' @return A catchability matrix (metiers x stocks) with attributes
#'   `distribution` (cells x stocks), `ground` (cells x metiers, 0/1) and
#'   `overlap` (share of each stock on each ground).
#' @seealso [plot_landscape()]
#' @export
#' @examples
#' cells <- expand.grid(depth = seq(2.5, 77.5, 5), substrate = c("mud", "sand"))
#' cells$area <- 1
#' sp <- data.frame(species = c("A", "B"), depth_mean = c(15, 60), depth_sd = 10,
#'                  mud = c(1, 0.2), sand = c(0.2, 1))
#' mt <- data.frame(metier = c("inner", "outer"), depth_min = c(0, 40), depth_max = c(40, 80))
#' sel <- matrix(1, 2, 2, dimnames = list(mt$metier, sp$species))
#' round(ms_landscape(cells, sp, mt, sel), 5)
ms_landscape <- function(cells, species, metiers, selectivity, q0 = 1e-3, distribution = NULL) {
  cells <- as.data.frame(cells); species <- as.data.frame(species); metiers <- as.data.frame(metiers)
  for (v in c("depth", "substrate", "area"))
    if (is.null(cells[[v]])) stop("`cells` needs column `", v, "`", call. = FALSE)
  for (v in c("species", if (is.null(distribution)) c("depth_mean", "depth_sd")))
    if (is.null(species[[v]])) stop("`species` needs column `", v, "`", call. = FALSE)
  for (v in c("metier", "depth_min", "depth_max"))
    if (is.null(metiers[[v]])) stop("`metiers` needs column `", v, "`", call. = FALSE)
  sp <- as.character(species$species); met <- as.character(metiers$metier)
  sub <- as.character(cells$substrate); lev <- unique(sub)
  # substrate weights by stock (cells x stocks)
  if (!is.null(distribution)) {
    dist <- as.matrix(distribution)
    if (nrow(dist) != nrow(cells) || !all(sp %in% colnames(dist)))
      stop("`distribution` needs one row per cell and a column for every stock", call. = FALSE)
    dist <- dist[, sp, drop = FALSE] * 1
  } else {
  wsub <- vapply(seq_along(sp), function(i) {
    if (!any(lev %in% names(species))) return(rep(1, nrow(cells)))
    w <- vapply(lev, function(l) if (is.null(species[[l]])) 0 else as.numeric(species[[l]][i]), 0)
    unname(w[sub])
  }, numeric(nrow(cells)))
  dens <- vapply(seq_along(sp), function(i)
    stats::dnorm(cells$depth, species$depth_mean[i], species$depth_sd[i]), numeric(nrow(cells)))
  dist <- dens * wsub * cells$area
  }
  if (any(colSums(dist) <= 0)) stop("stock(s) with no habitat: ", paste(sp[colSums(dist) <= 0], collapse = ", "), call. = FALSE)
  dist <- sweep(dist, 2, colSums(dist), "/"); colnames(dist) <- sp
  # grounds (cells x metiers)
  ground <- vapply(seq_along(met), function(j) {
    s <- trimws(strsplit(as.character(metiers$substrate[j] %||% ""), ";")[[1]]); s <- s[nzchar(s)]
    as.numeric(cells$depth >= metiers$depth_min[j] & cells$depth < metiers$depth_max[j] &
                 (if (length(s)) sub %in% s else TRUE))
  }, numeric(nrow(cells)))
  ground <- matrix(ground, nrow(cells), length(met), dimnames = list(NULL, met))
  if (any(colSums(ground) == 0)) stop("metier(s) with an empty ground: ", paste(met[colSums(ground) == 0], collapse = ", "), call. = FALSE)
  overlap <- t(ground) %*% dist                                     # share of each stock on each ground
  area_share <- as.vector(t(ground) %*% cells$area) / sum(cells$area)
  sel <- if (is.data.frame(selectivity)) {
    m <- as.matrix(selectivity[, sp, drop = FALSE]); rownames(m) <- selectivity$metier; m
  } else as.matrix(selectivity)
  if (!all(met %in% rownames(sel)) || !all(sp %in% colnames(sel)))
    stop("`selectivity` needs rows for all metiers and columns for all stocks", call. = FALSE)
  sel <- sel[met, sp, drop = FALSE]
  Q <- q0 * sel * sweep(overlap, 1, area_share, "/")
  dimnames(Q) <- list(met, sp)
  structure(Q, distribution = dist, ground = ground, overlap = overlap)
}

#' Plot a habitat landscape
#'
#' Each stock's distribution over depth (summed over substrates).
#'
#' @param Q a catchability matrix from [ms_landscape()].
#' @param cells the `cells` passed to [ms_landscape()].
#' @param ncol columns of panels.
#' @return A ggplot.
#' @export
plot_landscape <- function(Q, cells, ncol = 5) {
  dist <- attr(Q, "distribution")
  if (is.null(dist)) stop("`Q` must come from ms_landscape()", call. = FALSE)
  d <- do.call(rbind, lapply(colnames(dist), function(s) {
    a <- tapply(dist[, s], cells$depth, sum)
    data.frame(species = s, depth = as.numeric(names(a)), share = as.vector(a))
  }))
  d$species <- factor(d$species, colnames(dist))
  ggplot(d, aes(depth, share)) +
    geom_area(fill = ms_cols[[1]], alpha = 0.35) + geom_line(colour = ms_cols[[1]], linewidth = 0.5) +
    facet_wrap(~ species, ncol = ncol) +
    labs(x = "Depth (m)", y = "Share of the stock") + theme_ms()
}
