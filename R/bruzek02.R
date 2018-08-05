bruzek02 <- function(x) {
# détermination Bruzek02 à partir des scores sur 5 méta-variables finales.
# x : vecteur de "f", "i", "m" (et éventuellement NA), théoriquement de longueur 5 (même si ce n'est pas obligatoire)
# output -> produit une détermination finale "f", "i" ou "m" en fonction des déterminations du vecteur d'origine, sur la base de la règle de majorité.
	
	x <- x[!is.na(x)] # on commence par retirer les valeurs manquantes
	nb_f <- sum(x=="F") # nb de déterminations féminines
	nb_m <- sum(x=="M") # nb de déterminations masculines	
	
	if (nb_f > nb_m) {
		return("F")	
	} else if (nb_f < nb_m) {
		return("M")
	} else { # nb_f == nb_m
		return("I")
	} 
}
