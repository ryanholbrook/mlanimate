#' Make a sample layer
#'
#' @param data data.frame: a sample with continuous features `x` and
#'     `y` grouped by factor `class`
#' @param classes (optional) a vector of which levels of `class` to
#'     plot; default is to plot data from all classes
#'
#' @return a `ggplot2::LayerInstance` object
#'
#' @export
layer_sample <- function(data, classes = NULL, alpha = 0.5, size = 3, ...) {
    if (is.null(classes)) {
        subdata <- data
    } else {
        subdata <- filter(data, class %in% classes)
    }
    list(geom_point(data = subdata,
                    aes(x, y,
                        color = factor(class),
                        shape = factor(class)),
                    size = size,
                    alpha = alpha,
                    ...),
         scale_colour_discrete(drop = TRUE,
                               limits = levels(factor(data$class))))
}

#' Make a density layer
#'
#' @param data data.frame: a data grid of features `x` and `y` with
#'     contours `z`
#' @param z character: the name of the contour column
#'
#' @return a `ggplot2::LayerInstance` object for use with `ggplot2::gg_plot()`
#'
#' @export
layer_density <- function(data, z, size = 1, color = "black", alpha = 1, ...) {
    z <- ensym(z)
    geom_contour(data = data,
                 aes(x, y, z = !!z),
                 size = size,
                 color = color,
                 alpha = alpha,
                 ...)
}

#' Make an optimal boundary layer
#'
#' @param data data.frame: a data grid of features `x` and `y` with a
#'     column with the `optimal` boundary contours
#' @param breaks numeric: which contour levels of `optimal` to plot
#'
#' @export
layer_optimal <- function(data, breaks = c(0), ...) {
    layer_density(data, z = "optimal", breaks = breaks, ...)
}

#' Make a layer of predicted probabilities
#'
#' @export
layer_predicted <- function(sample, density, predicted) {
    layer_density(data = density, z = fitted, breaks = c(0.5))
}


#' Set theme to a clean default
#'
#' @export
set_theme <- function() {
    theme_set(theme_linedraw() +
              theme(plot.title = element_text(size = 20),
                    legend.position = "none",
                    axis.text.x = element_blank(),
                    axis.text.y = element_blank(),
                    axis.title.x = element_blank(),
                    axis.title.y = element_blank(),
                    aspect.ratio = 1))
}
