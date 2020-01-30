#' Generate normally distributed feature samples for a binary
#' classification problem
#'
#' @param n integer: the size of the sample
#' @param mean_0 vector: the mean vector of the first class
#' @param sigma_0 matrix: the 2x2 covariance matrix of the first class
#' @param mean_1 vector: the mean vector of the second class
#' @param sigma_1 matrix: the 2x2 covariance matrix of the second class
#' @param p_0 double: the prior probability of class 0
make_mvn_sample <- function(n, mu_0, sigma_0, mu_1, sigma_1, p_0) {
    n_0 <- rbinom(1, n, p_0)
    n_1 <- n - n_0
    sample_mvn <- as_tibble(
        rbind(mvnfast::rmvn(n_0,
                            mu = mu_0,
                            sigma = sigma_0),
              mvnfast::rmvn(n_1,
                            mu = mu_1,
                            sigma = sigma_1)))
    sample_mvn[1:n_0, 3] <- 0
    sample_mvn[(n_0 + 1):(n_0 + n_1), 3] <- 1
    sample_mvn <- sample_mvn[sample(nrow(sample_mvn)), ]
    colnames(sample_mvn) <- c("x", "y", "class")
    sample_mvn
}

#' Make an optimal prediction at a point from two class distributions
#'
#' @param x vector: input
#' @param p_0 double: prior probability of class 0
#' @param dfun_0 function(x): density of features of class 0
#' @param dfun_1 function(x): density of features of class 1
optimal_predict <- function(x, p_0, dfun_0, dfun_1) {
    ## Prior probability of class 1
    p_1 <- 1 - p_0
    ## Conditional probability of (x, y) given class 0
    p_x_0 <- dfun_0(x)
    ## Conditional probability of (x, y) given class 1
    p_x_1 <- dfun_1(x)
    ## Conditional probability of class 0 given (x, y)
    p_0_xy <- p_x_0 * p_0
    ## Conditional probability of class 1 given (x, y)
    p_1_xy <- p_x_1 * p_1
    optimal <- p_1_xy - p_0_xy
    class <- ifelse(optimal > 0, 1, 0)
    result <- c(p_0_xy, p_1_xy, optimal, class)
    names(result) <- c("p_0_xy", "p_1_xy", "optimal", "class")
    result
}

#' Construct a dataframe with posterior class probabilities and the
#' optimal decision boundary over a grid on the feature space
#' 
#' @param mean_0 vector: the mean vector of the first class
#' @param sigma_0 matrix: the 2x2 covariance matrix of the first class
#' @param mean_1 vector: the mean vector of the second class
#' @param sigma_1 matrix: the 2x2 covariance matrix of the second class
#' @param p_0 double: the prior probability of class 0
make_density_mvn <- function(mean_0, sigma_0, mean_1, sigma_1, p_0,
                             x_min, x_max, y_min, y_max, delta = 0.05) {
    x <- seq(x_min, x_max, delta)
    y <- seq(y_min, y_max, delta)
    density_mvn <- expand.grid(x, y)
    names(density_mvn) <- c("x", "y")
    dfun_0 <- function(x) mvnfast::dmvn(x, mu_0, sigma_0)
    dfun_1 <- function(x) mvnfast::dmvn(x, mu_1, sigma_1)
    optimal_mvn <- function(x, y) optimal_predict(c(x, y), p_0, dfun_0, dfun_1)
    density_mvn <-as.tibble(
        cbind(density_mvn,
              t(mapply(optimal_mvn,
                       density_mvn$x, density_mvn$y))))
    density_mvn
}

#' Generate normally distributed feature samples for a binary
#' classification problem
#'
#' @param n integer: the size of the sample
#' @param nu_0 numeric: the average mean of the components of the first feature
#' @param sigma_0 matrix: covariance of components of the first feature
#' @param n_0 integer: class frequency of first feature in the sample
#' @param w_0 numeric: vector of weights for components of the first feature
#' @param mean_1 numeric: the average mean of the components of the second feature
#' @param sigma_1 matrix: covariance of components of the second feature
#' @param n_1 integer: class frequency of second feature in the sample
#' @param w_1 numeric: vector of weights for components of the second feature
#' @param p_0 double: the prior probability of class 0
make_mix_sample <- function(n,
                            nu_0, tau_0, n_0, sigma_0, w_0,
                            nu_1, tau_1, n_1, sigma_1, w_1,
                            p_0) {
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
    c_0 <- attr(f_0, "index")
    f_1 <- mvnfast::rmixn(n = n_1,
                          mu = mu_1, sigma = sigma_1, w = w_1,
                          retInd = TRUE)
    c_1 <- attr(f_1, "index")
    sample_mix <- as.data.frame(rbind(f_0, f_1))
    sample_mix[, 3] <- c(c_0, c_1)
    ## Define Classes
    sample_mix[1:n_0, 4] <- 0
    sample_mix[(n_0 + 1):(n_0 + n_1), 4] <- 1
    sample_mix <- sample_mix[sample(nrow(sample_mix)), ]
    names(sample_mix) <- c("x", "y", "component", "class")
    ## Store Component Means
    attr(sample_mix, "mu_0") <- mu_0
    attr(sample_mix, "mu_1") <- mu_1
    sample_mix
}

#' Construct a dataframe with posterior class probabilities and the
#' optimal decision boundary over a grid on the feature space
#' 
#' @param mean_0 numeric: the average mean of the components of the first feature
#' @param sigma_0 matrix: covariance of components of the first feature
#' @param w_0 numeric: vector of weights for components of the first feature
#' @param mean_1 numeric: the average mean of the components of the second feature
#' @param sigma_1 matrix: covariance of components of the second feature
#' @param w_1 numeric: vector of weights for components of the second feature
#' @param p_0 double: the prior probability of class 0
make_density_mix <- function(mean_0, sigma_0, w_0,
                             mean_1, sigma_1, w_1, p_0,
                             x_min, x_max, y_min, y_max, delta = 0.05) {
    x <- seq(x_min, x_max, delta)
    y <- seq(y_min, y_max, delta)
    density_mix <- expand.grid(x, y)
    names(density_mix) <- c("x", "y")
    dfun_0 <- function(x) mvnfast::dmixn(matrix(x, nrow = 1),
                                         mu = mean_0,
                                         sigma = sigma_0,
                                         w = w_0)
    dfun_1 <- function(x) mvnfast::dmixn(matrix(x, nrow = 1),
                                         mu = mean_1,
                                         sigma = sigma_1,
                                         w = w_1)
    optimal_mix <- function(x, y) optimal_predict(c(x, y), p_0, dfun_0, dfun_1)
    density_mix <-as.tibble(
        cbind(density_mix,
              t(mapply(optimal_mix,
                       density_mix$x, density_mix$y))))
    density_mix
}
