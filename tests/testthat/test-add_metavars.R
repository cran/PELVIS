library(PELVIS)
data(CTscanDataBruzek)
extrait <- CTscanDataBruzek[1:2, -c(1:5)]
## Expected answer:
expected <- data.frame(extrait,
                       PrSu = factor(c('M', 'F')),
                       GrSN = factor(c('M', 'F')),
                       InfP = factor(c('M', 'F'))
                       )

test_that("add_metavars gives correct answers", {
    expect_equal(add_metavars(extrait), expected)
})
