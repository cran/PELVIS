metavar <- function(x, threshold = 2) {
### Internal function
### x: character vector, with possible values "f", "i", "m" or NA.
### threshold: threshold for the majority rule; set to 2 by default since x is supposed to be of length 3 (cf. Bruzek 2002)
### output -> a main character value, "F", "0" or "M", based on a majority rule.

    if (sum(is.na(x)) >= threshold) { # If there are too many missing values,
        return(NA)
    } else if (sum(x == "NA", na.rm = TRUE) >= threshold) {
        return(NA)
    } else if (sum(x == "f", na.rm = TRUE) >= threshold) { # Majority of "f"
        return("F")
    } else if (sum(x == "m", na.rm = TRUE) >= threshold) { # Majority of "m"
        return("M")
    } else { # No clear majority
        return("0")
    }
}

addMetavars <- function(dat) {
    .Deprecated("add_metavars")
    add_metavars(dat)
}

add_metavars <- function(dat) {
### dat: dataframe included the observed values for the eleven trichotomic traits (PrSu1, PrSu2, etc.)
### output -> same dataframe, with three more columns corresponding to the main characters (PrSu, GrSN, InfP)

    dat$PrSu <- factor(apply(dat[ , c("PrSu1", "PrSu2", "PrSu3")], MARGIN = 1, FUN = metavar))
    dat$GrSN <- factor(apply(dat[ , c("GrSN1", "GrSN2", "GrSN3")], MARGIN = 1, FUN = metavar))
    dat$InfP <- factor(apply(dat[ , c("InfP1", "InfP2", "InfP3")], MARGIN = 1, FUN = metavar))

    return(dat)
}

finalSE <- function(x, threshold = 0.95) {
### x: vector of posterior probabilities of being a male individual
### threshold: decision threshold to produce a sex estimate (otherwise indet.)
    det <- ifelse(x >= threshold, "M", ifelse(x <= 1-threshold, "F", "I"))
    return(det)
}
