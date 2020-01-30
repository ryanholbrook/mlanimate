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
