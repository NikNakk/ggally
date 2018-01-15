#' Number at risk table for survival graph
#'
#' @param s an object of class \code{\link[survival]{survfit}}
#' @param xlim limits of time parameter (defaults to range of \code{s$time})
#' @param at.risk.interval interval at which to display number at risk (defaults to \code{diff(xlim) / 10})
#' @param surv.col colours for
#'
#' @return custom annotation for adding to a \code{\link[ggplot2]{ggplot}}
#'
#' @details This function produces a number at risk table from a \code{\link[survival]{survfit}} object. It can
#'   be placed in the margin of a \code{\link{ggsurv}} plot using a customAnnotation (although
#'   alignment may be a little tricky) or combined into a composite plot using, for example,
#'   \code{\link[cowplot]{plot_grid}}.
#'
#' @export
#'
#' @examples
#' library(survival)
#' fit <- survfit(Surv(time, status) ~ x, aml)
#' surv_at_risk_table(fit, at.risk.interval = 10)
#'
#' if (require(cowplot)) {
#'   plot_grid(
#'     ggsurv(fit),
#'     surv_at_risk_table(fit, at.risk.interval = 10),
#'     nrow = 2,
#'     rel_heights = c(10, 2),
#'     align = "v"
#'   )
#'
#'   # Note that positioning of legends will need to be applied both to the survival
#'   # plot and the number at risk table to ensure alignment still works
#'   legend_theme <- theme(legend.position = "none")
#'   plot_grid(
#'     ggsurv(fit) + legend_theme,
#'     surv_at_risk_table(fit, at.risk.interval = 10, coloured = TRUE) + legend_theme,
#'     nrow = 2,
#'     rel_heights = c(10, 2),
#'     align = "v"
#'   )
#' }
surv_at_risk_table <- function(
  s,
  xlim = c(0, max(s$time)),
  at.risk.interval = diff(xlim) / 10,
  surv.col = NULL
  ) {
  if (!inherits(fit, "survfit")) {
    stop("`s` parameter should be a `survfit` object")
  }
  if (length(s$strata) > 1L) {
    strata_names <- sub("[^=]+=", "", names(s$strata))
    strata_names_by_row <- rep(strata_names, s$strata)
  } else {
    strata_names <- "Number at risk"
    strata_names_by_row <- rep(strata_names, length(s$time))
  }
  fit_risk <- data.frame(
    stratum = strata_names_by_row,
    time = s$time,
    at_risk = s$n.risk,
    stringsAsFactors = FALSE
  )
  fit_risk <- fit_risk[order(fit_risk$stratum, fit_risk$time), ]
  fit_risk_simple <-
    expand.grid(time = seq(0, xlim[2], at.risk.interval), stratum = unique(fit_risk$stratum))
  fit_risk_simple$at_risk <- mapply(
    function(stratum, time) {
      which_at_risk <- which(fit_risk$stratum == stratum &
                               fit_risk$time > time)
      if (length(which_at_risk) > 0L) {
        fit_risk$at_risk[which_at_risk[1]]
      } else {
        0
      }
    },
    fit_risk_simple$stratum,
    fit_risk_simple$time
  )
  # fit_risk_simple$y <- -as.numeric(fit_risk_simple$stratum)

  if (length(s$strata) > 1L) {
    plot_ylim <- rev(unique(fit_risk$stratum))
    plot_ylabels <- rev(unique(fit_risk$stratum))
  } else {
    plot_ylim <- character(0)
    plot_ylabels <- vector("expression", 0)
  }
  plot_ylim <- c(plot_ylim, "Number at risk")
  plot_ylabels <- c(plot_ylabels, expression(bold("Number at risk")))

  aesthetic <- aes(y = stratum, x = time, label = at_risk)
  if (!is.null(surv.col)) {
    aesthetic$colour <- as.name("stratum")
    if (surv.col[1] == "gg.def") {
      if (length(strata_names) > 1L) {
        surv.col <- scales::hue_pal()(length(strata_names))
      } else {
        surv.col <- "black"
      }
    } else if (length(surv.col) != length(strata_names)) {
      stop("If `surv.col`` is provided it must either be \"gg.def\" or a vector ",
           "of the same length as the number of strata")
    }
  }

  pl <- ggplot() +
    geom_text(aesthetic, data = fit_risk_simple) +
    theme_void() +
    theme(axis.ticks.length = unit(0, "pt")) +
    theme(
      axis.text = element_text(colour = "black", size = rel(0.8)),
      axis.text.x = element_blank()
    ) +
    scale_x_continuous(NULL, limits = xlim) +
    scale_y_discrete(limits = plot_ylim, labels = plot_ylabels)
  if (!is.null(surv.col)) {
    pl <- pl +
      scale_colour_manual(values = surv.col, breaks = strata_names, guide = FALSE) +
      theme(axis.text.y = element_text(colour = rev(c("black", surv.col))))
  }
  if (length(strata_names) > 1) {
    # Dummy legend so that plot can be aligned with ggsurv
    pl <- pl + geom_point(aes(x = xlim[1], y = 1, alpha = " ")) +
    scale_alpha_manual("", values = c(" " = 0))
  }
  pl
}
