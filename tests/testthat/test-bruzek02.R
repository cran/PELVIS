library(PELVIS)

test_that("bruzek02 gives correct answers", {
    x <- c("F", "0", "F", "0", "M")
    y <- c("F", NA, NA, NA, NA)
    z <- c("M", "M", "F", "F", "0")
    a <- c("M", "M", "M", NA, NA)
    expect_equal(bruzek02(x), "F")
    expect_equal(bruzek02(y), "F")
    expect_equal(bruzek02(z), "I")
    expect_equal(bruzek02(a), "M")
})
