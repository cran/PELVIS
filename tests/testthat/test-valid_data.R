library(PELVIS)
data(CTscanDataBruzek)
altered <- CTscanDataBruzek
colnames(altered) <- paste("Bla", 1:ncol(altered), sep = "_")

test_that("valid_data recognizes a valid dataset", {
    expect_equal(valid_data(CTscanDataBruzek), TRUE)
})
test_that("valid_data recognizes a dataset that has too few columns", {
    expect_warning(valid_data(CTscanDataBruzek[ , -6]))
})
test_that("valid_data recognizes a dataset with invalid row names", {
    expect_warning(valid_data(altered))
})
