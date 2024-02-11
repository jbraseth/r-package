# Import helpers from "R" directory in package directory
source(file.path(getwd(), "R", "helpers.R"))

#' Create a violin plot with consistent formatting
#'
#' This function creates a violin plot with consistent formatting, including filling the plot by the x argument (based on the levels of the factor) and defaulting x and y labels to respective variable names.
#'
#' @param data The dataframe containing the data to plot.
#' @param x The variable to use for the x-axis.
#' @param y The variable to use for the y-axis.
#' @param title The title of the plot.
#' @param xlab (Optional) The label for the x-axis. Defaults to the variable name of \code{x}.
#' @param ylab (Optional) The label for the y-axis. Defaults to the variable name of \code{y}.
#' @return A ggplot object representing the violin plot.
#' @import ggplot2
#' @importFrom stats lm
#' @export
#' @examples
#' violin_plot(mtcars, gear, mpg, "MPG Distribution by Gear")
violin_plot <- function(data, x, y, title, xlab = NULL, ylab = NULL) {
  xlab <- null_check(xlab, x)
  ylab <- null_check(ylab, y)
  ggplot(data, aes(x = {{ x }}, y = {{ y }}, fill = {{ x }})) +
    geom_violin(trim = FALSE) +
    geom_boxplot(width = 0.1, fill = "white") +
    labs(title = title, x = xlab, y = ylab) +
    theme_minimal()
}

#' Create a scatter plot with consistent formatting
#'
#' This function creates a scatter plot with consistent formatting, including defaulting x and y labels to respective variable names.
#'
#' @param data The dataframe containing the data to plot.
#' @param x The variable to use for the x-axis.
#' @param y The variable to use for the y-axis.
#' @param title The title of the plot.
#' @param xlab (Optional) The label for the x-axis. Defaults to the variable name of \code{x}.
#' @param ylab (Optional) The label for the y-axis. Defaults to the variable name of \code{y}.
#' @return A ggplot object representing the scatter plot.
#' @import ggplot2
#' @importFrom stats lm
#' @export
#' @examples
#' scatter_plot(mtcars, mpg, disp, "MPG vs. Displacement")
scatter_plot <- function(data, x, y, title, xlab = NULL, ylab = NULL) {
  xlab <- null_check(xlab, x)
  ylab <- null_check(ylab, y)
  ggplot(data, aes(x = {{ x }}, y = {{ y }})) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    labs(title = title, x = xlab, y = ylab) +
    theme_minimal()
}
