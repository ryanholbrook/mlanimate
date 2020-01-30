##' Create a data frame sequenced for animation of a sample
##'
##' @param data `data.frame`: the data to sequence
##' @param delta `int`: how many rows to increase by at each iteration
##' 
##' @return a data.frame with successive groups of `data` of size `delta`;
##' `data` is randomized before sequencing
sequence_data_sample <- function(data, delta = 1L) {
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
##' @param delta `integer`: how many points to add at each step of the
##'     animation
##' @param fit_and_predict `function(sample, density, ...)`: fits a learner
##'     to the sample data and returns its predictions on the density; `...`
##'     are parameters passed to the learner
animate_boundary <- function(sample, density, delta, fit_and_predict, start = 0) {
    ## Sequence the sample data
    sample_sequenced <- sequence_data_sample(sample, delta)
    ## Sequence the density data and attach predictions from the sample
    density_sequenced <- sample_sequenced %>%
        group_by(group) %>%
        group_modify(~ fit_and_predict(.x, density)) %>%
        ungroup()
    ## Define the animation
    anim <- ggplot() +
        gg_plot_boundary(sample_sequenced, density_sequenced) +
        ## Animate the sample and the fitted boundary
        transition_manual(group)
    anim <- animate(anim, renderer = gifski_renderer(),
                    width = 800, height = 800)
    anim
}

##' Create a data frame sequenced for animation of a model parameter
##' @param data `data.frame`: the data to sequence
##' @param params `list`: a named list of parameters with each entry a
##'     vector of values for that parameter
##'
##' @return a data frame with `n` successive groups of `data`
sequence_data_parameter <- function(data, params){
    n <- length(params)
    ## function to be applied by lapply
    go <- function(i) {
        dplyr::mutate(data,
                      group = i)
    }
    ## * TODO Iterate over combinations of named parameters and mutate onto data
    sequenced <- dplyr::bind_rows(lapply(rep(data, times = n),
                                         go))
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
##' @param param_name `string`: the name of the parameter to vary over
##' @param param_seq `vector`: a vector of values for the parameter on
##'     which to animate the model
animate_model_parameter <- function(sample, density,
                                    fit_and_predict,
                                    param_seq) {
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
