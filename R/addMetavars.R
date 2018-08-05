addMetavars <- function(dat) {
# dat : dataframe, tableau de données contenant uniquement les variables originelles PrSu1, PrSu2, etc.
# output -> dataframe identique, auquel on ajoute les "méta-variables" PrSu, GrSN, InfP
	dat$PrSu <- factor(apply(dat[ , c("PrSu1", "PrSu2", "PrSu3")], MARGIN=1, FUN=metavar))
	dat$GrSN <- factor(apply(dat[ , c("GrSN1", "GrSN2", "GrSN3")], MARGIN=1, FUN=metavar))
	dat$InfP <- factor(apply(dat[ , c("InfP1", "InfP2", "InfP3")], MARGIN=1, FUN=metavar))
	
	return(dat)
}
