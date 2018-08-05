validData <- function(tab) {
# verifie si le dataframe "tab" est bien valide pour le package
# output -> booleen, TRUE/FALSE
 
	if (ncol(tab) < 11) { # quel que soit son type, le fichier est invalide s'il n'a pas assez de colonnes (l'utilisateur s'est sans doute tromp\'e de separateur de champ, ou a oubli\'e des variables) 
		return(FALSE)
	} else {
		if (all(c("PrSu1", "PrSu2", "PrSu3", "GrSN1", "GrSN2", "GrSN3", "CArc", "InfP1", "InfP2", "InfP3", "IsPu") %in% colnames(tab))) { # si toutes les variables attendues sont bien presentes...
			return(TRUE) # OK.
		} else {
			return(FALSE)
		}
	}
}
	
