tenFoldCV.glm <- function(dat.glm, mod, seuil=0.95) {
# dat.glm : les données à utiliser (sans données manquantes, car elles ont été préalablement soumises à na.omit dans une fonction parente)
# mod : le modèle GLM dont on veut évaluer les perf en CV
# seuil : le seuil classique, à priori 0.95 par défaut.
# output -> les perf en 10-fold CV du modèle.

	N <- nrow(dat.glm) # le nombre d'individus
	dat.glm <- dat.glm[sample(1:N, size=N, replace=FALSE) , ] # on réordonne aléatoirement le jeu de données pour introduire du hasard

	# 1. Mise en place de la structure d'accueil des résultats :
	MatRes <- matrix(nrow=N, ncol=3)
	colnames(MatRes) <- c("True_sex", "Proba_10CV", "Predict_sex_10CV")
	MatRes <- data.frame(MatRes)
	MatRes[,1] <- as.character(dat.glm$Sex) # on renomme la première colonne des données, par précaution.
	
	# 2. Calcul des prédictions en LOOCV (trop chronophage, abandonné... mais pourra être réintégré plus tard si parallélisation efficace) :
	#for (i in 1:nrow(dat.glm)) { # pour chaque individu...
	#	app <- dat.glm[-i, ] # on retire l'individu courant du jeu de données,
	#	modTemp <- glm(mod$call$formula, family=binomial, data=app) # on apprend un modèle sur un jeu de données privé de cet individu,
	#	MatRes[i,2] <- round(predict(modTemp, newdata=dat.glm[i,], type="response"),3) # et on récupère les prédiction & proba.
	#	MatRes[i,3] <- ifelse(MatRes[i,2]>=seuil, "M", ifelse(MatRes[i,2]<=1-seuil, "F", "I"))
	#}
	
	# 2. Calcul des prédictions en 10-fold CV :
	for (k in 1:10) { # pour chacune des 10 itérations ("splits", "folds"),
		pioch <- (1:N <= k*N/10) & (1:N > (k-1)*N/10) # on pioche 10% des individus qui seront utilisés pour la *validation*
		app <- dat.glm[!pioch, ] # et on apprend le modèle sur les 90% restants
		val <- dat.glm[pioch, ]
		modTemp <- glm(mod$call$formula, family=binomial, data=app)
		MatRes[pioch, 2] <- predict(modTemp, newdata=val, type="response")
	}
	MatRes[ , 3] <- ifelse(MatRes[ , 2]>=seuil, "M", ifelse(MatRes[ ,2]<=1-seuil, "F", "I"))
	
	# 3. Calcul des perf synthétiques en 10-fold CV :
	taux_indet <- nrow(MatRes[MatRes$Predict_sex_10CV=="I", ]) / nrow(MatRes)
	if (taux_indet<1) { # s'il n'y a pas que des indéterminés (ce qui peut se produire s'il y a une unique variable prédictrice)
		taux_succes <- nrow(MatRes[(MatRes$Predict_sex_10CV=="F" & MatRes$True_sex=="F") | (MatRes$Predict_sex_10CV=="M" & MatRes$True_sex=="M"), ]) / nrow(MatRes[MatRes$Predict_sex_10CV!="I", ])
	} else { # il n'y a que des indéterminés
		taux_succes <- NA
	}
	
	# 4. Retourner les résultats :
	return(list(ClassifResults=MatRes, IndetRate=taux_indet, SuccessRate=taux_succes))
}
