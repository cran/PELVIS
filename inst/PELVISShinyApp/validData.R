validData <- function(tab) {
# Checks if a dataframe "tab" is suitable for PELVIS
# output -> boolean, TRUE/FALSE
 
  if (ncol(tab) < 11) { # if there are not enough columns (i.e., less than the 11 required traits) 
    return(FALSE)
  } else {
    if (all(c("PrSu1", "PrSu2", "PrSu3", "GrSN1", "GrSN2", "GrSN3", "CArc", "InfP1", "InfP2", "InfP3", "IsPu") %in% colnames(tab))) { # if all traits are OK
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  
}
  
