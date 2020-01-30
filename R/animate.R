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
    ## a data.frame with successive groups of `data` of size `delta`;
    ## `data` is randomized before sequencing
    sequence_data <- function(data, delta) {
        rows <- nrow(data)
        n <- (rows - start) / delta
        data <- data[sample(nrow(data)), ]
        go <- function(i) {
            h <- min(i * delta + start, rows)
            bind_cols(
                head(data, h),
                group = rep.int(i, h))
        }
        sequenced <- bind_rows(lapply(1:n, go))
        sequenced
    }
    ## Sequence the sample data
    sample_sequenced <- sequence_data(sample, delta)
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
        mutate(fitted, "group" = i)
    }
    density_sequenced <- bind_rows(lapply(1:n, go))
     ## Define the animation
    anim <- ggplot() +
        gg_plot_boundary(sample, density_sequenced) +
        ## Animate the sample and the fitted boundary
        transition_manual(group)
    anim <- animate(anim, renderer = gifski_renderer(),
                    width = 800, height = 800)
    anim
}
