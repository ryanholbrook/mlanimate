## Graphs and Animations for Machine Learning Concepts

#' Set theme to a clean default
#' 
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

#' Make a sample layer
#'
#' @param data data.frame: a sample with continuous features `x` and `y`
#' grouped by factor `class`
#' @param classes (optional) a vector of which levels of `class` to
#' plot; default is to plot data from all classes
gg_sample <- function(data, classes = NULL, size = 3, alpha = 0.5, ...) {
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
#' @param data data.frame: a data grid of features `x` and `y` with contours `z`
#' @param data character: the name of the contour column 
gg_density <- function(data, z, size = 1, color = "black", alpha = 1, ...) {
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
#' @param data data.frame: a data grid of features `x` and `y` with a column with
#' the `optimal` boundary contours
#' @param breaks numeric: which contour levels of `optimal` to plot
gg_optimal <- function(data, breaks = c(0), ...) {
    gg_density(data, z = optimal, breaks = breaks, ...)
}

#' Make a layer of component labels for a mixture distribution with two classes
#'
#' @param mus list(data.frame): the means for components of each class; every row
#' is a mean, each column is a coordinate
#' @param classes (optional) a vector of which levels of class to plot
gg_mix_label <- function(mus, classes = NULL, size = 10, ...) {
    ns <- map_int(mus, nrow)
    component <- do.call(c, map(ns, seq_len))
    class <- do.call(c, map2(0:(length(ns) - 1), ns, rep.int))
    mu_all <- do.call(rbind, mus)
    data <- cbind(mu_all, component, class) %>%
        set_colnames(c("x", "y", "component", "class")) %>%
        as_tibble()
    if (is.null(classes)) {
        subdata <- data
    } else {
        subdata <- filter(data, class %in% classes)
    }    
    list(shadowtext::geom_shadowtext(data = subdata,
                                     mapping = aes(x, y,
                                                   label = component,
                                                   color = factor(class)),
                                     size = size,
                                     ...),
         scale_colour_discrete(drop = TRUE,
                               limits = levels(factor(data$class))))
}

#' Plot an optimal boundary together with density contours and a sample
#' 
gg_plot_boundary <- function(sample, density, title = "") {
    ggplot() +
    ## gg_sample(data = density, size = 1.5, alpha = 0.1, shape = 15) +
    gg_sample(data = sample) +
    gg_density(data = density, z = optimal, breaks = c(0), linetype = 2) +
    gg_density(data = density, z = fitted, breaks = c(0.5)) +
    coord_fixed(expand = FALSE) +
    xlim(min(density$x), max(density$y)) +
    ylim(min(density$y), max(density$y))
}
