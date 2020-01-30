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

fit_and_predict_nb <- function(sample, density, ...) {
    fit_nb <- naivebayes::naive_bayes(factor(class) ~ x + y, data = sample)
    pred_nb <- predict(fit_nb, newdata = density[, c("x", "y")], type = "prob")
    density_nb <- cbind(density, "fitted" = pred_nb[, "1"])
    density_nb
}

fit_and_predict_qda <- function(sample, density, ...) {
    fit_qda <- MASS::qda(class ~ x + y, data = sample)
    pred_qda <- predict(fit_qda, newdata = density)
    density_qda <- cbind(density, "fitted" = pred_qda$posterior[, "1"])
    density_qda
}

mda_opt <- function(data) {
    go <- function(n) {
        fit_mda <- mda::mda(class ~ x + y, data = data, subclasses = n)
        false <- (fit_mda$confusion[1, 2] + fit_mda$confusion[2, 1])
        true <- (fit_mda$confusion[1, 1] + fit_mda$confusion[2, 2])
        false / true
    }
    lapply(1:10, go)
}

fit_and_predict_mda <- function(sample, density, ...) {
    fit_mda <- mda::mda(class ~ x + y, data = sample, subclasses = 10)
    pred_mda <- predict(fit_mda, newdata = density, type = "posterior")
    density_mda <- cbind(density, "fitted" = pred_mda[, "1"])
    density_mda
}

fit_and_predict_glm <- function(sample, density, ...) {
    fit_glm <- glm(class ~ x + y, data = sample, family = binomial)
    pred_glm <- predict(fit_glm, newdata = density_mvn, type = "response")
    density_glm <- cbind(density, fitted = pred_glm)
    density_glm
}

fit_and_predict_gam <- function(sample, density, ...) {
    fit_gam <- mgcv::gam(class ~ s(x, y), class = "bernoulli", data = sample)
    pred_gam <- predict(fit_gam, newdata = density, type = "response")
    density_gam <- cbind(density, "fitted" = as.numeric(pred_gam))
    density_gam
}

fit_and_predict_mars <- function(sample, density, ...) {
    fit_mars <- earth::earth(factor(class) ~ x + y,
                             data = sample,
                             glm = list(family = "binomial"))
    pred_mars <- predict(fit_mars, newdata = density, type = "response")
    density_mars <- cbind(density, "fitted" = as.numeric(pred_mars))
    density_mars
}

fit_and_predict_polymars <- function(sample, density, ...) {
    fit_pmars <- polspline::polymars(sample[["class"]],
                                     as.data.frame(sample[, c("x", "y")]),
                                     classify = TRUE)
    pred_pmars <- predict(fit_pmars,
                          x = as.data.frame(density[, c("x", "y")]))
    density_pmars <- cbind(density, "fitted" = pred_pmars[, 2])
    density_pmars
}

fit_and_predict_knn <- function(sample, density, ...) {
    pred_knn <- class::knn(train = sample[, c("x", "y")],
                           cl = factor(sample$class),
                           test = density[, c("x", "y")],
                           k = 5)
    density_knn <- cbind(density,
                         fitted = as.integer(pred_knn) - 1.5)
    density_knn
}

fit_and_predict_svm <- function(sample, density, ...) {
    fit_svm <- kernlab::ksvm(factor(class) ~ x + y,
                             data = sample,
                             kernel = "rbfdot",
                             prob.model = TRUE)
    pred_svm <- kernlab::predict(fit_svm,
                             newdata = density,
                             type = "probabilities")
    density_svm <- cbind(density, "fitted" = pred_svm[, "1"])
    density_svm
}

fit_and_predict_rpart <- function(sample, density, ...) {
    fit_rpart <- rpart::rpart(class ~ x + y, data = sample,
                              method = "class", ...)
    pred_rpart <- predict(fit_rpart, newdata = density)
    density_rpart <- cbind(density, "fitted" = pred_rpart[, "1"])
    density_rpart
}

fit_and_predict_rf <- function(sample, density, ...) {
    fit_rf <- ranger::ranger(factor(class) ~ x + y,
                             data = sample,
                             probability = TRUE)
    pred_rf <- predict(fit_rf, data = density)
    density_rf <- cbind(density, "fitted" = pred_rf$predictions[, "1"])
    density_rf
}

fit_and_predict_etrees <- function(sample, density, ...) {
    fit_etrees <- extraTrees::extraTrees(x = sample[, c("x", "y")],
                                     y = factor(sample$class))
    pred_etrees <- predict(fit_etrees,
                       newdata = density[, c("x", "y")],
                       probability = TRUE)
    density_etrees <- cbind(density, "fitted" = pred_mda[, "1"])
    density_etrees
}

fit_and_predict_xgboost <- function(sample, density, ...) {
    set.seed(31415)
    sample_xg <- xgboost::xgb.DMatrix(
                              as.matrix(sample[, c("x", "y")]),
                              label = as.numeric(sample$class))
    xgcv <- xgboost::xgb.cv(data = sample_xg,
                            nrounds = 50,
                            early_stopping_rounds = 3,
                            nfold = 5,
                            objective = "binary:logistic",
                            verbose = 0,
                            ...)
    fit_xg <- xgboost::xgboost(data = sample_xg,
                               nrounds = xgcv$best_iteration,
                               objective = "binary:logistic",
                               verbose = 0,
                               ...)
    pred_xg <- predict(fit_xg, newdata = as.matrix(density[, c("x", "y")]))
    density_xg <- cbind(density, "fitted" = pred_xg)
    density_xg
}

fit_and_predict_mb <- function(sample, density, ...) {
    fit_mb <- mboost::mboost(factor(class) ~ bspatial(x, y),
                                 data = sample,
                                 family = Binomial(),
                                 control = boost_control(mstop = 1000,
                                                         nu = 0.1))
    pred_mb <- predict(fit_mb, newdata = density, type = "response")
    density_mb <- cbind(density, "fitted" = as.numeric(pred_mb))
    density_mb
}

fit_and_predict_nn <- function(sample, density, seed = 31415) {
    set.seed(seed)
    fit_nn <- nnet::nnet(factor(class) ~ x + y,
                         data = sample,
                         size = 4,
                         decay = 0.01,
                         rang = 0.3,
                         maxit = 200,
                         trace = FALSE)
    pred_nn <- predict(fit_nn, newdata = density, type = "raw")
    density_nn <- cbind(density, "fitted" = pred_nn)
    density_nn
}

fit_and_predict_elm <- function(sample, density, ...) {
    set.seed(31415)
    fit_elm <- elmNNRcpp::elm_train(x = as.matrix(sample[, c("x", "y")]),
                                    y = elmNNRcpp::onehot_encode(sample[["class"]]),
                                    nhid = 10,
                                    actfun = "sig")
    pred_elm <- elmNNRcpp::elm_predict(fit_elm,
                                       as.matrix(density[, c("x", "y")]))
    density_elm <- cbind(density, "fitted" = pred_elm[, 1])
    density_elm
}

fit_and_predict_gp <- function(sample, density, ...) {
    fit_gp <- kernlab::gausspr(factor(class) ~ x + y,
                                   data = sample,
                                   kernel = "rbfdot")
    pred_gp <- predict(fit_gp,
                           newdata = density,
                           type = "probabilities")
    density_gp <- cbind(density, "fitted" = pred_gp[, "1"])
    density_gp
}

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
