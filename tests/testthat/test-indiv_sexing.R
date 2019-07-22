library(PELVIS)
data(refDataBruzek02)
data(CTscanDataBruzek)
indiv2 <- CTscanDataBruzek[1, -c(1:5)]
res_none <- indiv_sexing(ref = refDataBruzek02,
                         new_ind = indiv2,
                         strategy = "None")
res_bic <- indiv_sexing(ref = refDataBruzek02,
                        new_ind = indiv2,
                        strategy = "BIC")

test_that("all traits are kept if no variable selection is done", {
    expect_equal(res_none$VariablesUsed, c("PrSu1, PrSu2, PrSu3, GrSN1, GrSN2, GrSN3, CArc, IsPu, InfP1, InfP2, InfP3"))
})

test_that("A correct sex estimate is given", {
    expect_equal(as.character(res_none$PredictedSex), "M")
    expect_equal(as.character(res_bic$PredictedSex), "M")
})

test_that("A correct post prob is given", {
    expect_equal(as.numeric(round(res_bic$PostProb, 3)), 0.999)
})
