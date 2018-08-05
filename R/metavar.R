metavar <- function(x, seuil=2) {
# Primitive annexe.
# x : vecteur de caractères, contenant des "f", des "i" et des "m". (et éventuellement des NA)
# output -> produit une détermination finale "f", "i" ou "m" en fonction des déterminations du vecteur d'origine, sur la base de la règle de majorité.
# seuil=2 par défaut si x est de longueur 3 (cas de la création de méta-variables PrSu, GrSN et InfP).
# (cf. Bruzek 2002)
	if (sum(is.na(x))>=seuil) {
		return(NA)
	} else if (sum(x=="NA", na.rm=TRUE)>=seuil) {
		return(NA)
	} else if (sum(x=="f", na.rm=TRUE)>=seuil) {
		return("F")
	} else if (sum(x=="m", na.rm=TRUE)>=seuil) {
		return("M")
	} else {
		return("0")
	}
}
