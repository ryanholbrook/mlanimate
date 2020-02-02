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
##'
##' @noRd
##' @keywords internal
sequence_data_sample <- function(data, delta, start = 1L) {
    rows <- nrow(data)
    n <- (rows - start) / delta
    data <- data[sample(nrow(data)), ]
    go <- function(i) {
        ## the size of the sample; make sure we stay within bounds
        h <- min(i * delta + start, rows)
        dplyr::bind_cols(
                   data[1:h, ],
                   group = rep.int(i, h))
    }
    sequenced <- dplyr::bind_rows(lapply(1:n, go))
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
##'
##' @return a rendered `gganimate` animation object
animate_boundary <- function(sample, density, fit_and_predict, delta,
                             start = 1L, ...) {
    ## Sequence the sample data to be animated
    sample_sequenced <- sequence_data_sample(sample, delta)
    ## Sequence the density with the fitted predictions to be animated
    density_sequenced <- sample_sequenced %>%
        ## Each group is a frame
        dplyr::group_by(group) %>%
        dplyr::group_modify(~ fit_and_predict(.x, density)) %>%
        dplyr::ungroup()
    ## Define the animation
    anim <- ggplot2::ggplot() +
        gg_plot_boundary(sample_sequenced, density_sequenced) +
        ## Animate the sample and the fitted boundary
        gganimate::transition_manual(group)
    anim <- gganimate::animate(anim,
                               renderer = gganimate::gifski_renderer(),
                               ...)
    anim
}

##' Create a data frame sequenced for animation of a model parameter
##'
##' A function used internally by animate_model_parameter
##' 
##' @param params `list`: a named list of parameters with each entry a
##'     vector of values for that parameter
##'
##' @return a data frame with the parameters to be sequenced over,
##'     enumerated by a `group` column
##'
##' @noRd
##' @keywords internal
sequence_parameters <- function(params){
    sequenced <-
        do.call(expand.grid, params) %>%
        ## Create a grouping column by string concatenation. This
        ## makes it easier to display the parameters in the animation.
        tidyr::unite(!!!names(.),
                     col = "group",
                     sep = " ",
                     remove = FALSE) %>%
        ## gganimate::transition_manual orders the frames by factor
        ## order, so make sure these are right
        dplyr::mutate(group = forcats::fct_inorder(factor(group)))
    sequenced
}

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
##' @return a frame containing data suitible for animating
##'
##' @noRd
##' @keywords internal
make_parameter_animation_frame <- function(sample, density,
                                    fit_and_predict,
                                    params,
                                    ...) {
    ## Function to pass to `group_modify` below
    ## Builds fitted density for one set of parameters
    go <- function(x) {
        f <- function(...) fit_and_predict(sample, density, ...)
        do.call(f, x)
    }
    ## Build the parameter sequence to be animated
    parameters_sequenced <-
        sequence_parameters(params)
    ## Build the sequence of densities with fitted predictions to be
    ## animated
    density_sequenced <- parameters_sequenced %>%
        ## Each group is a frame
        dplyr::group_by(group) %>%
        ## Call `fit_and_predict` on the row of parameters `.x` in
        ## this group
        dplyr::group_modify(~ go(.x),
                            keep = FALSE) %>%
        dplyr::ungroup()
}

##' Animate changes in a decision boundary as a parameter in a model
##' changes
##'
##' @param parameter_animation_frame data.frame: a frame containing data
##'     suitible to be animated
##'
##' @return a `gganimate` animation object
##'
##' @noRd
##' @keywords internal
animate_model_parameter <- function(parameter_animation_frame) {
    anim <- ggplot2::ggplot() +
        ## Create the optimal boundary layers
        gg_plot_boundary(sample, parameter_animation_frame) +
        ## Animate the sample and the fitted boundary
        gganimate::transition_manual(group) +
        ## Show the parameters for the current frame in the title
    ggplot2::ggtitle('Parameters: {current_frame}  /  Frame: {progress}')
    anim <- gganimate::animate(anim,
                               renderer = gganimate::gifski_renderer(),
                               ...)
    anim
}

plotly_model_parameter <- function(animation_frame) {
    ggplot(animation_frame, aes(frame = 
}
    
##' Animate changes in a decision boundary as a parameter in a
##' probability distribution changes
##'
##' @param sample `data.frame`: the complete sample data; should have
##'     columns `x`, `y`, and `class`
##' @param make_density `function(...)`: a function returning a data
##'     frame with columns `x`, `y`, `p_0_xy`, `p_1_xy`, `optimal`,
##'     and `class`; the parameters of the function will be drawn
##'     from `params`
##' @param fit_and_predict `function(sample, density)`: fits a
##'     learner to the sample data and returns its predictions on the
##'     density as a data frame
##' @param params `list`: a named list of parameters with each entry a
##'     vector of values for that parameter
##' @param ... : arguments to pass on to `gganimate::animate`
##' 
##' @return a rendered `gganimate` animation object
animate_distribution_parameter <- function(sample, make_density,
                                    fit_and_predict, params, ...) {
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
