##' Create a data frame sequenced for animation of a sample
##'
##' A function used internally by `animate_boundary`.
##' 
##' @param data `data.frame`: the data to sequence
##' @param delta `int`: how many rows to increase by at each iteration
##' @param start `int`: how many rows for the first sample
##' 
##' @return a data.frame with successive groups of `data` of size `delta`;
##' `data` is randomized before sequencing
sequence_data_sample <- function(data, start = 1L) {
    rows <- nrow(data)
    n <- (rows - start) / delta
    data <- data[sample(nrow(data)), ]
    go <- function(i) {
        ## the size of the sample; make sure we stay within bounds
        h <- min(i * delta + start, rows)
        bind_cols(
            data[1:h, ],
            group = rep.int(i, h))
    }
    sequenced <- bind_rows(lapply(1:n, go))
    sequenced
}

##' Animate the evolution of a decision boundary as the sample size grows
##'
##' @param sample `data.frame`: the complete sample data; should have
##'     columns `x`, `y`, and `class`
##' @param density `data.frame`: the density distribution of `x` and
##'     `y`; should have columns `x`, `y`, and `optimal`, the contours
##'     of the optimal decision distribution
##' @param fit_and_predict `function(sample, density, ...)`: fits a learner
##'     to the sample data and returns its predictions on the density; `...`
##'     are parameters passed to the learner
##' @param delta `integer`: how many points to add at each step of the
##'     animation
##' @param start `integer`: how many rows for the first sample
animate_boundary <- function(sample, density, fit_and_predict, delta, start = 1L) {
    ## Sequence the sample data
    sample_sequenced <- sequence_data_sample(sample, delta)
    ## Sequence the density data and attach predictions from the sample
    density_sequenced <- sample_sequenced %>%
        dplyr::group_by(group) %>%
        dplyr::group_modify(~ fit_and_predict(.x, density)) %>%
        dplyr::ungroup()
    ## Define the animation
    anim <- ggplot2::ggplot() +
        gg_plot_boundary(sample_sequenced, density_sequenced) +
        ## Animate the sample and the fitted boundary
        gganimate::transition_manual(group)
    anim <- gganimate::animate(anim, renderer = gganimate::gifski_renderer(),
                               width = 800, height = 800)
    anim
}

##' Create a data frame sequenced for animation of a model parameter
##'
##' A function unsed internally by animate_model_parameter
##' 
##' @param data `data.frame`: the data to sequence
##' @param fit_and_predict `function(sample, density, param)`: fits a
##'     learner to the sample data and returns its predictions on the
##'     density as a data frame; `param` is the parameter value to
##'     vary over
##' @param params `list`: a named list of parameters with each entry a
##'     vector of values for that parameter
##' 
##' @return a data frame with `n` successive groups of `data`
sequence_data_parameter <- function(data, fit_and_predict, params){
    n <- length(params)
    go <- function(i) {
        fitted <- fit_and_predict(sample, density, param_seq[[i]])
        dplyr::mutate(fitted, "group" = i)
    }
    density_sequenced <- dplyr::bind_rows(lapply(1:n, go))
    ## function to be applied by lapply
    go <- function(i) {
        dplyr::mutate(data,
                      group = i)
    }
    ## * TODO Iterate over combinations of named parameters and mutate onto data
    sequenced <- dplyr::bind_rows(lapply(1:n, go))
    sequenced
}

##' Animate changes in a decision boundary as a parameter in a model
##' changes
##'
##' @param sample `data.frame`: the complete sample data; should have
##'     columns `x`, `y`, and `class`
##' @param density `data.frame`: the density distribution of `x` and
##'     `y`; should have columns `x`, `y`, and `optimal`, the contours
##'     of the optimal decision distribution
##' @param fit_and_predict `function(sample, density, param)`: fits a
##'     learner to the sample data and returns its predictions on the
##'     density as a data frame; `param` is the parameter value to
##'     vary over
##' @param params `list`: a named list of parameters with each
##'     entry a vector of values for that parameter
##' @param ... : arguments to pass on to `gganimate::animate`
##' 
##' @return a rendered `gganimate` animation object
animate_model_parameter <- function(sample, density,
                                    fit_and_predict,
                                    params,
                                    ...) {
    n <- length(param_seq)
    go <- function(i) {
        fitted <- fit_and_predict(sample, density, param_seq[[i]])
        dplyr::mutate(fitted, "group" = i)
    }
    density_sequenced <- dplyr::bind_rows(lapply(1:n, go))
    ## Define the animation
    anim <- ggplot2::ggplot() +
        gg_plot_boundary(sample, density_sequenced) +
        ## Animate the sample and the fitted boundary
        gganimate::transition_manual(group)
    anim <- gganimate::animate(anim,
                               renderer = gganimate::gifski_renderer(),
                               width = 800, height = 800)
    anim
}
