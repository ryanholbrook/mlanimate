test_that("layer_sample is a list of LayerInstance", {
    data <- tibble::tribble(
                        ~x, ~y, ~class,
                        0,  1,  1,
                        1,  0,  2)
    expect_is(layer_sample(data), "list")
    expect_is(layer_sample(data)[[1]], "LayerInstance")
})
