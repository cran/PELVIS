library(PELVIS)

test_that("metavar gives correct answers", {
    x <- c("f", "i", "f")
    y <- c("m", "m", "i")
    z <- c("i", "i", "f")
    a <- c(NA,  "f", "f")
    b <- c(NA, NA, "m")
    expect_equal(metavar(x), "F")
    expect_equal(metavar(y), "M")
    expect_equal(metavar(z), "0")
    expect_equal(metavar(a), "F")
    expect_equal(metavar(b), NA)
})
