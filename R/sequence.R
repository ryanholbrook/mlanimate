##' Create a data frame sequenced for animation over an accumulating
##' sample
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
sequence_sample <- function(data, delta, start = 1L) {
    rows <- nrow(data)
    n <- (rows - start) / delta
    data <- data[sample(nrow(data)), ]
    go <- function(i) {
        ## the size of the sample; make sure we stay within bounds
        h <- min(i * delta + start, rows)
        dplyr::bind_cols(
                   data[1:h, ],
                   frame = rep.int(i, h))
    }
    sequenced <- dplyr::bind_rows(lapply(1:n, go))
    sequenced
}

##' Create a data frame sequenced for animation over a set of
##' parameters
##'
##' @param params `list`: a named list of parameters with each entry a
##'     vector of values for that parameter
##' @param method `function`: a function to create the sequencing from
##'     the parameter list. Use `identity` to keep as-is. For every
##'     method, the resulting object will be passed to
##'     `tibble::as_tibble`. (default `expand.grid`)
##'
##' @return a data frame with the parameters to be sequenced over,
##'     enumerated by a `group` column
##'
##' @noRd
##' @keywords internal
sequence_parameters <- function(params, method = expand.grid){
    sequenced <- params %>%
        method() %>%
        tibble::as_tibble() %>%
        ## Create a grouping column by string concatenation. This
        ## makes it easier to display the parameters in the animation.
        tidyr::unite(!!!names(.),
                     col = "frame",
                     sep = " ",
                     remove = FALSE) %>%
        ## gganimate::transition_manual orders the frames by factor
        ## order, so make sure these are right
        dplyr::mutate(group = forcats::fct_inorder(factor(frame)))
    sequenced
}
