#' Create an AnimationFrame object
#'
#'
new_AnimationFrame <- function() {
    data <- tibble::tibble()
    call <- list()
    layers <- list()
    structure(data,
              call = call,
              layers = layers,
              class = "AnimationFrame")
}


#' Make an animation frame for a "fitting" animation
#'
#' @param sample_frame a sample frame
#' @param density_frame a density frame
#' @param fit_and_predict prediction function
#' @param ...
#'
#' @return a FittedFrameSequence
#'
#' @export
new_FittedFrameSequence <- function(sample_frame, density_frame,
                                    fit_and_predict,
                                    delta = 20L, start = 1L,
                                    ...) {
    call <- c(as.list(environment()), list())
    ## Sequence the sample data to be animated
    sample_sequenced <- sequence_sample(sample_frame, delta)
    ## Sequence the density with the fitted predictions to be animated
    density_sequenced <- sample_sequenced %>%
        ## Each group is a frame
        dplyr::group_by(frame) %>%
        dplyr::group_modify(~ fit_and_predict(.x, density_frame)) %>%
        dplyr::ungroup()
    fitted_frame_sequenced <-
        list(sample = sample_sequenced,
             density = density_sequenced)
    structure(fitted_frame_sequenced,
              call = call,
              layer_options = list(...),
              class = c("FittedFrameSequence",
                        class(fitted_frame_sequenced))
              )
}

#' Make an animation frame for a "sample" animation
#'
#' @export
new_sample_frame <- function(sample_fun, ...) {
    sample_fun(...)
}

#' Create a frame for animating a distribution parameter
#'
#' An internal function.
#'
#' @param density_fun `function(...)`: a function returning a
#'     DensityFrame object. The parameters of the function will be
#'     drawn from `params`
#' @param params `list`: a named list of parameters with each entry a
#'     list of values for that parameter
#' @param method `function`
#' @param ... : options to be passed to `layer_density` when plotting
#'
#' @return a DensityFrameSequence object
#'
#' @export
new_DensityFrameSequence <- function(density_fun, params,
                                     method = expand.grid) {
    call <- c(as.list(environment()), list())
    ## Build the parameter sequence to be animated
    parameters_sequenced <-
        sequence_parameters(params, method)
    ## Build the sequence of densities with fitted predictions to be
    ## animated
    density_sequenced <- parameters_sequenced %>%
        ## Each group is a frame
        dplyr::group_by(frame) %>%
        dplyr::group_modify(~ density_fun(.x[[1, 1]]),
                            keep = FALSE) %>%
        dplyr::ungroup()
    structure(density_sequenced,
              class = c("DensityFrameSequence",
                        class(density_sequenced)),
              layer_options = list(...),
              call = call)
}

#' Make an animation frame for a "model" animation
#'
#' @export
new_model_frame <- function(sample_frame, density_frame, fit_and_predict, ...) {
    fit_and_predict(sample_frame, density_frame, fit_and_predict, ...)
}
