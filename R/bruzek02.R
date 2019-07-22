bruzek02 <- function(x) {
### Produces sex estimates with the original Bruzek (2002) method from the observed values on the five main characters (PrSu, GrSN, CArc, IsPu, InfP)
### x: character vector (with possible values "F", "0", "M" or NA), is supposed to be of length 5.
### output -> returns a sex estimate ("F", "I" or "M") depending on the input vector, based on a simple majority rule.

    x <- x[!is.na(x)] # remove missing values
    nb_f <- sum(x == "F") # counts of "F" in the input vector
    nb_m <- sum(x == "M") # counts of "M" in the input vector

    ## Return a sex estimate based on a majority rule:
    if (nb_f > nb_m) {
        return("F")  
    } else if (nb_f < nb_m) {
        return("M")
    } else { # nb_f == nb_m
        return("I")
    }
}
