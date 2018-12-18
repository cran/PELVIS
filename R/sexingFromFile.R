sexingFromFile <- function(dat, ref, updateProgressBar=NULL) {
# dat : dataframe contenant les individus à sexer (avec seulement les traits, pas les caractères ou méta-variables)
# ref : dataframe d'apprentissage contenant les individus de référence
# updateProgressBar : cette fonction pouvant être lente, on se sert d'un indicateur de progression pour l'appli shiny
# (NB : les colonnes doivent contenir les labels adéquats, mais il peut éventuellement y avoir d'autres colonnes illustratives [e.g. âge, ...] dans le jeu de données)

	############################################
	# 1. Ajouter les colonnes PrSu, GrSN, InfP :
	dat <- dat[ , c("PrSu1", "PrSu2", "PrSu3", "GrSN1", "GrSN2", "GrSN3", "CArc", "InfP1", "InfP2", "InfP3", "IsPu")]
	metadat <- addMetavars(dat)

	########################################	
	# 2. Préparation du dataframe résultat :
	MatRes <- matrix(NA, ncol=21, nrow=nrow(dat))
	MatRes <- as.data.frame(MatRes, stringsAsFactors=FALSE)
	colnames(MatRes) <- c("PrSu1", "PrSu2", "PrSu3", "PrSu", "GrSN1", "GrSN2", "GrSN3", "GrSN", "CArc", "InfP1", "InfP2", "InfP3", "InfP", "IsPu", "Sex estimate (Bruzek 2002)", "Statistical sex estimate (2018)", "Prob(F)", "Prob(M)", "Selected predictors in LR model", "10-fold CV accuracy (%)", "Indet. rate in CV (%)")
	rownames(MatRes) <- rownames(dat)
	
	############################################
	# 3. Utilisation de la méthode Bruzek 2002 :
	MatRes[ , "Sex estimate (Bruzek 2002)"] <- factor(apply(metadat[ , c("PrSu", "GrSN", "InfP", "CArc", "IsPu")], MARGIN=1, FUN=bruzek02))
	
	##########################
	# 4. Utilisation des GLM :
	indexModeles <- 1
	tabModeles <- data.frame(InitialVars=character(0), tenCV_perfRate=numeric(0), tenCV_indetRate=numeric(0), stringsAsFactors=FALSE)
	listeModeles <- list()
	for (i in 1:nrow(dat)) { # pour chaque individu,
		variablesInit <- paste(colnames(dat)[which(!is.na(dat[i,]))], collapse=", ") # noter les variables présentes chez lui. # bugfix: paste(colnames(dat[i, !is.na(dat[i,])]), collapse=", ") 
		if ((! variablesInit %in% tabModeles$InitialVars) & (sum(!is.na(dat[i,]))>0)) { # Si ces variables ne correspondent pas à une situation déjà connue/calculée dans le jeu de données,
			res <- indivSexing(ref=ref, newInd=dat[i,]) # on calcule le modèle correspondant grâce à cette fonction annexe,
			MatRes[i, "Statistical sex estimate (2018)"] <- res$PredictedSex # et on note les différents résultats.
			MatRes[i, "Prob(F)"] <- round(1-res$PostProb, 3)
			MatRes[i, "Prob(M)"] <- round(res$PostProb, 3)
			MatRes[i, "Selected predictors in LR model"] <- res$VariablesUsed
			MatRes[i, "10-fold CV accuracy (%)"] <- round(100*res$cvRate, 2)
			MatRes[i, "Indet. rate in CV (%)"] <- round(100*res$cvIndet, 2)
			listeModeles[[indexModeles]] <- res$BestModel
			tabModeles[indexModeles, "InitialVars"] <- variablesInit
			tabModeles[indexModeles, "tenCV_perfRate"] <- round(100*res$cvRate, 2)
			tabModeles[indexModeles, "tenCV_indetRate"] <- round(100*res$cvIndet, 2)
			#tabModeles[i, "tenCV_perfRate"] <- 
			indexModeles <- indexModeles + 1 # on incrémente le compteur de modèles
		} else if (sum(!is.na(dat[i,]))>0) { # Si ces variables correspondent à une situation déjà connue/calculée dans le jeu de données
			where.mod <- which(tabModeles$InitialVars == variablesInit) # situer ce cas dans le tableau des cas déjà traités
			currentMod <- listeModeles[[where.mod]] # récupérer le modèle correspondant...
			postprob <- predict(currentMod, newdata=as.data.frame(dat[i,]), type="response") # et prédire.
			MatRes[i, "Statistical sex estimate (2018)"] <- ifelse(postprob>=0.95,"M", ifelse(postprob<=0.05,"F","I")) 
			MatRes[i, "Prob(F)"] <- round(1-postprob, 3)
			MatRes[i, "Prob(M)"] <- round(postprob, 3)
			MatRes[i, "Selected predictors in LR model"] <- paste(attr(currentMod$terms, "term.labels"), collapse=", ")
			MatRes[i, "10-fold CV accuracy (%)"] <- tabModeles[where.mod, "tenCV_perfRate"]
			MatRes[i, "Indet. rate in CV (%)"] <- tabModeles[where.mod, "tenCV_indetRate"]
		} 
		# Actualiser l'indicateur de progression :
		if (is.function(updateProgressBar)) { # si ce n'est pas NULL !
			text <- paste("Currently processing individual ", rownames(dat)[i], "...", sep="")
			updateProgressBar(detail=text, total=nrow(dat))
		}
	}
	
	# MatRes[ , c("Statistical sex estimate (2018)", "Confidence")] <- apply(dat, MARGIN=1, FUN=function(x) return(unlist(indivSexing(ref=ref, newInd=x))))
	
	######################################
	# 5. Retourner le dataframe résultat :
	MatRes[ , c("PrSu1", "PrSu2", "PrSu3", "GrSN1", "GrSN2", "GrSN3", "CArc", "InfP1", "InfP2", "InfP3", "IsPu")] <- dat
	MatRes[ , c("PrSu", "GrSN", "InfP", "CArc", "IsPu")] <- metadat[ , c("PrSu", "GrSN", "InfP", "CArc", "IsPu")]
	return(MatRes)
}
