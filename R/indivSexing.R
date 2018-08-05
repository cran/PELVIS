indivSexing <- function(ref, newInd) {
# fonction qui réalise la détermination du sexe de l'individu "newInd" avec un GLM appris sur les données "ref"
# ref : dataframe, données de référence avec les 11 traits originels
# newInd : vecteur ou dataframe à 1 ligne
# output ->

	newInd <- as.data.frame(newInd) # nécessaire pour une utilisation future dans la fonction "predict"
	
	if (ncol(newInd)>0) { # s'il y a des données !
		colOk <- colnames(newInd)[which(! is.na(newInd[1,]) )] # les colonnes portant des valeurs "non-NA"
		newInd <- as.data.frame(newInd[ , !is.na(newInd[1,])])
		colnames(newInd) <- colOk
	
		# 1. Réduire ref aux seuls traits présents chez newInd :
		ref <- na.omit(ref[ , c("Sex", colnames(newInd))])
	
		# 2. GLM complet sur la base d'apprentissage :
		modComp <- glm(Sex ~ ., data=ref, family=binomial)
		
		# 3. Sélection de variables :
		modRed <- stepAIC(modComp, k=log(nrow(ref))) # critère BIC
		predicteurs <- paste(attr(modRed$terms, "term.labels"), collapse=", ") # la liste des prédicteurs, en une seule chaîne de caractères, séparés par des virgules.
	
		# 4. Prédiction du sexe du nouvel individu (posterior probability) :
		postprob <- predict(modRed, newdata=newInd, type="response")
	
		# 5. Calcul des perf du modèle en LOOCV :
		resCV <- tenFoldCV.glm(dat.glm=ref, mod=modRed, seuil=0.95)
		perfCV <- resCV$SuccessRate
		indetCV <- resCV$IndetRate
	
		# 6. Retourner les résultats :
		return(list(PredictedSex=ifelse(postprob>=0.95,"M", ifelse(postprob<=0.05,"F","I")), PostProb=postprob, BestModel=modRed, VariablesUsed=predicteurs, cvRate=perfCV, cvIndet=indetCV))
	} else { # si pas de données
		return(list(PredictedSex=NA, PostProb=NA, BestModel=NULL, VariablesUsed=NULL, cvRate=NA, cvIndet=NA))
	}
}
