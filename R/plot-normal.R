# Normal distribution helpers for STAT 100 (lecture 8B and later)
#
#   plot_normal()        value -> area: draws the normal curve, shades the
#                        area between `lower` and `upper`, and writes it under
#                        the curve.
#   calc_prob_normal()   value -> area: same arguments as plot_normal(), but
#                        returns the area (probability) as a number, without a
#                        plot, so it can be printed, saved, and used inline.
#   find_normal_value()  area -> value: finds the value with a given area
#                        below or above it, draws it, and returns the value.
#
# Both take the random variable ("x", "x_bar", "p", "mu", or "pi"), the mean,
# and either the standard deviation (sd) or the variance.
#
# Colors follow the Hello Data Science book theme (Okabe-Ito).
# Requires ggplot2.

hds_colors <- c(green  = "#009E73",
                yellow = "#F0E442",
                pink   = "#CC79A7")

# ---- internal helpers ------------------------------------------------------

.rv_symbol <- function(random_variable) {
  switch(random_variable,
         x     = quote(X),
         x_bar = quote(bar(X)),
         p     = quote(p),
         mu    = quote(mu),
         pi    = quote(pi),
         stop('`random_variable` must be one of "x", "x_bar", "p", "mu", or "pi".',
              call. = FALSE))
}

.resolve_sd <- function(sd, variance) {
  if (!is.null(sd) && !is.null(variance)) {
    stop("Give either `sd` or `variance`, not both.", call. = FALSE)
  }
  if (!is.null(variance)) return(sqrt(variance))
  if (!is.null(sd)) return(sd)
  1
}

.round_value <- function(x) round(x, 3)

.area_text <- function(area) {
  if (area < 0.00005) "Area < 0.0001" else sprintf("Area = %.4f", area)
}

.normal_title <- function(sym, lower, upper) {
  lo <- .round_value(lower)
  up <- .round_value(upper)
  if (is.finite(lower) && is.finite(upper)) {
    bquote(P(.(lo) < .(sym) ~ "<" ~ .(up)))
  } else if (is.finite(upper)) {
    bquote(P(.(sym) < .(up)))
  } else if (is.finite(lower)) {
    bquote(P(.(sym) > .(lo)))
  } else {
    NULL
  }
}

.draw_normal <- function(sym, mean, sd, lower, upper, area) {
  xlim <- c(mean - 4 * sd, mean + 4 * sd)
  peak <- stats::dnorm(mean, mean, sd)

  curve <- data.frame(x = seq(xlim[1], xlim[2], length.out = 1000))
  curve$y <- stats::dnorm(curve$x, mean, sd)

  # shaded region, with the exact endpoints added so the shading starts and
  # stops exactly at `lower` and `upper`
  ends <- c(lower, upper)
  ends <- ends[is.finite(ends) & ends > xlim[1] & ends < xlim[2]]
  shade <- rbind(curve[curve$x >= lower & curve$x <= upper, ],
                 data.frame(x = ends, y = stats::dnorm(ends, mean, sd)))
  shade <- shade[order(shade$x), ]

  title <- .normal_title(sym, lower, upper)
  base_size <- ggplot2::theme_get()$text$size
  if (is.null(base_size)) base_size <- 11

  p <- ggplot2::ggplot(curve, ggplot2::aes(x = x, y = y))
  if (nrow(shade) > 0) {
    p <- p + ggplot2::geom_area(data = shade, fill = hds_colors[["pink"]], alpha = 0.9)
  }
  p +
    ggplot2::geom_line(color = hds_colors[["green"]], linewidth = 1.3) +
    ggplot2::annotate("label", x = mean, y = -0.13 * peak,
                      label = .area_text(area),
                      fill = hds_colors[["yellow"]], color = "black",
                      size = 0.8 * base_size / ggplot2::.pt) +
    ggplot2::scale_y_continuous(limits = c(-0.22 * peak, 1.05 * peak)) +
    ggplot2::labs(x = as.expression(sym), y = NULL,
                  title = if (is.null(title)) NULL else as.expression(title)) +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5))
}

# ---- student-facing functions ----------------------------------------------

#' Area under a normal curve (value -> area)
#'
#' plot_normal("x", mean = 165, sd = 6.5, upper = 155)              # P(X < 155)
#' plot_normal("x", mean = 165, sd = 6.5, lower = 175)              # P(X > 175)
#' plot_normal("x", mean = 165, sd = 6.5, lower = 155, upper = 175) # in between
#' plot_normal("x", mean = 165, variance = 42.25, upper = 155)      # same as the first
plot_normal <- function(random_variable = "x", mean = 0, sd = NULL,
                        variance = NULL, lower = -Inf, upper = Inf) {
  sd <- .resolve_sd(sd, variance)
  if (sd <= 0) stop("The standard deviation must be positive.", call. = FALSE)
  if (lower >= upper) stop("`lower` must be smaller than `upper`.", call. = FALSE)
  sym <- .rv_symbol(random_variable)

  area <- stats::pnorm(upper, mean, sd) - stats::pnorm(lower, mean, sd)
  print(.draw_normal(sym, mean, sd, lower, upper, area))
  invisible(area)
}

#' Probability (area under a normal curve) as a number (value -> area)
#'
#' calc_prob_normal("x", mean = 1029, sd = 235, upper = 800)               # P(X < 800)
#' calc_prob_normal("x", mean = 1029, sd = 235, lower = 1400)              # P(X > 1400)
#' calc_prob_normal("x", mean = 1029, sd = 235, lower = 800, upper = 1400) # in between
calc_prob_normal <- function(random_variable = "x", mean = 0, sd = NULL,
                             variance = NULL, lower = -Inf, upper = Inf) {
  sd <- .resolve_sd(sd, variance)
  if (sd <= 0) stop("The standard deviation must be positive.", call. = FALSE)
  if (lower >= upper) stop("`lower` must be smaller than `upper`.", call. = FALSE)
  .rv_symbol(random_variable)  # checks that random_variable is valid
  stats::pnorm(upper, mean, sd) - stats::pnorm(lower, mean, sd)
}

#' Value with a given area below or above it (area -> value)
#'
#' find_normal_value("x", mean = 165, sd = 6.5, area = 0.10, side = "above")
#' find_normal_value("x", mean = 178, sd = 7.2, area = 0.25, side = "below")
find_normal_value <- function(random_variable = "x", mean = 0, sd = NULL,
                              variance = NULL, area, side = c("below", "above")) {
  side <- match.arg(side)
  sd <- .resolve_sd(sd, variance)
  if (sd <= 0) stop("The standard deviation must be positive.", call. = FALSE)
  if (missing(area) || area <= 0 || area >= 1) {
    stop("`area` must be a number between 0 and 1.", call. = FALSE)
  }
  sym <- .rv_symbol(random_variable)

  if (side == "below") {
    value <- stats::qnorm(area, mean, sd)
    lower <- -Inf
    upper <- value
  } else {
    value <- stats::qnorm(area, mean, sd, lower.tail = FALSE)
    lower <- value
    upper <- Inf
  }
  print(.draw_normal(sym, mean, sd, lower, upper, area))
  value
}
