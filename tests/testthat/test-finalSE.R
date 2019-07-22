library(PELVIS)

test_that("finalSE gives correct answers", {
    x <- c(0.96, 0.5, 0.04)
    expect_equal(finalSE(x, 0.95), c("M", "I", "F"))
})
