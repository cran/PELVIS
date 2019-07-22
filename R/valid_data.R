valid_data <- function(data) {
### Checks if a dataframe "data" is suitable for PELVIS
### output -> boolean, TRUE/FALSE

    ## If there are not enough columns (i.e., less than the 11 required traits):
    if (ncol(data) < 11) {
        warning("The dataset does not have enough columns: at least 11 are required.")
        return(FALSE)
    } else {
        ## If all traits are OK:
        if (all(c("PrSu1", "PrSu2", "PrSu3", "GrSN1", "GrSN2", "GrSN3",
                  "CArc", "InfP1", "InfP2", "InfP3", "IsPu") %in% colnames(data))) {
            return(TRUE)
        } else {
            warning("Invalid column names.")
            return(FALSE)
        }
    }
}
