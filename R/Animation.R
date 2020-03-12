
#' Create an Animation Object
#'
#' @export
new_Animation <- function(frame_sequence, ...) {
    UseMethod("new_Animation")
}

#'
#' @param zs list: names of the variables whose contours to plot
#' 
#' @export
new_Animation.DensityFrameSequence <- function(frame_sequence,
                                               distribution = "posterior",
                                               optimal = TRUE,
                                               ...) {
    alpha <- ifelse(optimal, 1, 1)
    colors <- scales::hue_pal()(2)
    layers <- list()
    if (distribution == "posterior") {
        layers <- c(layers,
                    layer_density(frame_sequence,
                                  z = "p_0_xy",
                                  alpha = alpha,
                                  color = colors[[1]]),
                    layer_density(frame_sequence,
                                  z = "p_1_xy",
                                  alpha = alpha,
                                  color = colors[[2]]))

    } else if (distribution == "likelihood") {
        layers <- c(layers,
                    layer_density(frame_sequence,
                                  z = "p_xy_0",
                                  alpha = alpha),
                    layer_density(frame_sequence,
                                  z = "p_xy_1",
                                  alpha = alpha))
    }
    if (optimal) {
        layers <- c(layers,
                    layer_optimal(frame_sequence))
    }
    structure(layers,
              class = c("Animation", class(layers)))
}

new_Animation.FittedFrameSequence <- function(frame_sequence,
                                              ...) {
    layer_options <- attr(frame_sequence, "layer_options")
    sample <- frame_sequence$sample
    density <- frame_sequence$density
    layers <- list(layer_sample(data = sample),
                   layer_density(data = density,
                                 z = optimal,
                                 breaks = c(0),
                                 linetype = 2),
                   layer_density(data = density,
                                 z = fitted,
                                 breaks = c(0.5)),
                   xlim(min(density$x), max(density$y)),
                   ylim(min(density$y), max(density$y))
                   )
    structure(layers,
              class = c("Animation", class(layers)))
}

#' Render an Animation object
#'
#' @export
animate <- function(object, ...) {
    UseMethod("animate")
}

#' @export
animate.Animation <- function(mlanimation,
                              frame = "frame",
                              width = 800,
                              height = 800,
                              ...) {
    anim <- ggplot2::ggplot() +
        mlanimation +
        ## Animate the sample and the fitted boundary
        gganimate::transition_manual(frame)
    anim <- gganimate::animate(anim,
                               renderer = gganimate::gifski_renderer(),
                               width = width,
                               height = height,
                               ...)
    anim

}
