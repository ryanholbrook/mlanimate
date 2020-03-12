#' Generate normally distributed feature samples for a binary
#' classification problem
#'
#' @param n integer: the size of the sample
#' @param mean_0 vector: the mean vector of the first class
#' @param sigma_0 matrix: the 2x2 covariance matrix of the first class
#' @param mean_1 vector: the mean vector of the second class
#' @param sigma_1 matrix: the 2x2 covariance matrix of the second class
#' @param p_0 double: the prior probability of class 0
#'
#' @export
new_sample_mvn <- function(n, mu_0, sigma_0, mu_1, sigma_1, p_0) {
    call <- c(as.list(environment()), list())
    ## Class Frequency
    n_0 <- rbinom(1, n, p_0)
    n_1 <- n - n_0
    ## Sample the Features
    sample_mvn <- dplyr::as_tibble(
        rbind(mvnfast::rmvn(n_0,
                            mu = mu_0,
                            sigma = sigma_0),
              mvnfast::rmvn(n_1,
                            mu = mu_1,
                            sigma = sigma_1)))
    ## Define Classes
    sample_mvn[1:n_0, 3] <- 0
    sample_mvn[(n_0 + 1):(n_0 + n_1), 3] <- 1
    ## Randomize
    sample_mvn <- sample_mvn[sample(nrow(sample_mvn)), ]
    colnames(sample_mvn) <- c("x", "y", "class")
    structure(sample_mvn,
              density = "mvnfast::rmvn",
              call = call,
              class = c("SampleFrame", class(sample_mvn)))
}

#' Make an optimal prediction at a point from two class distributions
#'
#' @param xy vector: 2D coordinate
#' @param p_0 double: prior probability of class 0
#' @param dfun_0 function(x): density of features of class 0
#' @param dfun_1 function(x): density of features of class 1
optimal_predict <- function(xy, p_0, dfun_0, dfun_1) {
    ## Prior probability of class 1
    p_1 <- 1 - p_0
    ## Conditional probability of (x, y) given class 0
    p_xy_0 <- dfun_0(xy)
    ## Conditional probability of (x, y) given class 1
    p_xy_1 <- dfun_1(xy)
    ## Conditional probability of class 0 given (x, y)
    p_0_xy <- p_xy_0 * p_0
    ## Conditional probability of class 1 given (x, y)
    p_1_xy <- p_xy_1 * p_1
    optimal <- p_1_xy - p_0_xy
    class <- ifelse(optimal > 0, 1, 0)
    result <- c(p_xy_0, p_xy_1, p_0_xy, p_1_xy, optimal, class)
    names(result) <- c("p_xy_0", "p_xy_1", "p_0_xy", "p_1_xy",
                       "optimal", "class")
    result
}

#' Construct a data frame with posterior class probabilities and the
#' optimal decision boundary over a grid on the feature space
#' 
#' @param mu_0 vector: the mean vector of the first class
#' @param sigma_0 matrix: the 2x2 covariance matrix of the first class
#' @param mu_1 vector: the mean vector of the second class
#' @param sigma_1 matrix: the 2x2 covariance matrix of the second class
#' @param p_0 double: the prior probability of class 0
#'
#' @return a DensityFrame object
#'
#' @export
new_density_mvn <- function(mu_0, sigma_0, mu_1, sigma_1, p_0,
                             x_min, x_max, y_min, y_max, delta = 0.05) {
    call <- c(as.list(environment()), list())
    x <- seq(x_min, x_max, delta)
    y <- seq(y_min, y_max, delta)
    density_mvn <- expand.grid(x, y)
    names(density_mvn) <- c("x", "y")
    dfun_0 <- function(x) mvnfast::dmvn(matrix(x, nrow = 1),
                                        mu = mu_0,
                                        sigma = sigma_0)
    dfun_1 <- function(x) mvnfast::dmvn(matrix(x, nrow = 1),
                                        mu = mu_1,
                                        sigma = sigma_1)
    optimal_mvn <- function(x, y) optimal_predict(c(x, y), p_0, dfun_0, dfun_1)
    density_mvn <- tibble::as_tibble(
        cbind(density_mvn,
              t(mapply(optimal_mvn,
                       density_mvn$x, density_mvn$y))))
    structure(density_mvn,
              density = "mvnfast::dmvn",
              call = call,
              class = c("DensityFrame", class(density_mvn)))
}

#' Generate normally distributed feature samples for a binary
#' classification problem
#'
#' @param n integer: the size of the sample
#' @param nu_0 numeric: the average mean of the components of the first class
#' @param tau_0 matrix: covariance of components of the first class
#' @param n_0 integer: class frequency of first feature in the sample
#' @param sigma_0 matrix: covariance of observations in first class
#' @param w_0 numeric: vector of weights for components of the first class
#' @param nu_1 numeric: the average mean of the components of the second class
#' @param tau_1 matrix: covariance of components of the second class
#' @param n_1 integer: class frequency of second feature in the sample
#' @param sigma_1 matrix: covariance of observations in second class
#' @param w_1 numeric: vector of weights for components of the second feature
#' @param p_0 double: the prior probability of class 0
#'
#' @export
new_sample_mix <- function(n,
                           nu_0, tau_0, n_0, sigma_0, w_0,
                           nu_1, tau_1, n_1, sigma_1, w_1,
                           p_0) {
    call <- c(as.list(environment()), list())
    ## Number of Components for Each Class
    l_0 <- length(w_0)
    l_1 <- length(w_1)
    ## Sample the Component Means
    mu_0 <- mvnfast::rmvn(n = l_0,
                          mu = nu_0, sigma = tau_0)
    mu_1 <- mvnfast::rmvn(n = l_1,
                          mu = nu_1, sigma = tau_1)
    ## Class Frequency in the Sample
    n_0 <- rbinom(1, n, p_0)
    n_1 <- n - n_0
    ## Sample the Features
    f_0 <- mvnfast::rmixn(n = n_0,
                          mu = mu_0, sigma = sigma_0, w = w_0,
                          retInd = TRUE)
    f_1 <- mvnfast::rmixn(n = n_1,
                          mu = mu_1, sigma = sigma_1, w = w_1,
                          retInd = TRUE)
    
    sample_mix <- as.data.frame(rbind(f_0, f_1))
    ## Store Component Index for each Class
    sample_mix[, 3] <- c(attr(f_0, "index"),
                         attr(f_1, "index"))
    ## Define Classes
    sample_mix[1:n_0, 4] <- 0
    sample_mix[(n_0 + 1):(n_0 + n_1), 4] <- 1
    ## Randomize
    sample_mix <- sample_mix[sample(nrow(sample_mix)), ]
    colnames(sample_mix) <- c("x", "y", "component", "class")
    ## Store Component Means
    attr(sample_mix, "mu_0") <- mu_0
    attr(sample_mix, "mu_1") <- mu_1
    structure(sample_mix,
              "mu_0" = mu_0,
              "mu_1" = mu_1,
              call = call,
              class = c("SampleFrame", class(sample_mvn)))
}

#' Construct a dataframe with posterior class probabilities and the
#' optimal decision boundary over a grid on the feature space
#' 
#' @param mu_0 numeric: the average mean of the components of the first feature
#' @param sigma_0 matrix: covariance of components of the first feature
#' @param w_0 numeric: vector of weights for components of the first feature
#' @param mu_1 numeric: the average mean of the components of the second feature
#' @param sigma_1 matrix: covariance of components of the second feature
#' @param w_1 numeric: vector of weights for components of the second feature
#' @param p_0 double: the prior probability of class 0
#'
#' @export
new_density_mix <- function(mu_0, sigma_0, w_0,
                            mu_1, sigma_1, w_1, p_0,
                            x_min, x_max, y_min, y_max, delta = 0.05) {
    call <- c(as.list(environment()), list())
    x <- seq(x_min, x_max, delta)
    y <- seq(y_min, y_max, delta)
    density_mix <- expand.grid(x, y)
    names(density_mix) <- c("x", "y")
    dfun_0 <- function(x) mvnfast::dmixn(matrix(x, nrow = 1),
                                         mu = mu_0,
                                         sigma = sigma_0,
                                         w = w_0)
    dfun_1 <- function(x) mvnfast::dmixn(matrix(x, nrow = 1),
                                         mu = mu_1,
                                         sigma = sigma_1,
                                         w = w_1)
    optimal_mix <- function(x, y) optimal_predict(c(x, y), p_0, dfun_0, dfun_1)
    density_mix <- tibble::as_tibble(
        cbind(density_mix,
              t(mapply(optimal_mix,
                       density_mix$x, density_mix$y))))
    structure(density_mix,
              density = "mvnfast::dmixn",
              call = call,
              class(c("DensityFrame", class(density_mix))))
}
