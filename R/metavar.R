metavar <- function(x, threshold=2) {
# Internal function
# x: character vector, with possible values "f", "i", "m" or NA.
# threshold: threshold for the majority rule; set to 2 by default since x is supposed to be of length 3 (cf. Bruzek 2002)
# output -> a main character value, "F", "0" or "M", based on a majority rule.

  if (sum(is.na(x))>=threshold) { # If there are too many missing values,
    return(NA)
  } else if (sum(x=="NA", na.rm=TRUE)>=threshold) {
    return(NA)
  } else if (sum(x=="f", na.rm=TRUE)>=threshold) { # Majority of "f"
    return("F")
  } else if (sum(x=="m", na.rm=TRUE)>=threshold) { # Majority of "m"
    return("M")
  } else { # No clear majority
    return("0")
  }
  
}
