shinyServer(function(input, output, session) {

	library(MASS)
	myenvg = new.env() # environnement priv\'e au package ; contiendra le jeu de donnees (vu comme une variable globale)
	#source("addMetavars.R")
	#source("indivSexing.R")
	#source("sexingFromFile.R")
	#source("tenFoldCV.glm.R")
	source("validData.R")
	data(refData, package="PELVIS") # charger la base d'apprentissage (version package)
	#load("data/refData.RData") # charger la base d'apprentissage (version test shinyapp)
	#refData <- readRDS("data/RefHumanOsCoxae.rds") # charger la base d'apprentissage (version test shinyapp)
	
	###########################################################################
	# 1a. INITIALISATION DE LA TABLE DE RÉSULTATS POUR L'ONGLET MANUAL EDITING :
	# (elle est initialisée à une table à 0 ligne et 17 colonnes, correctement nommées.)
	dat <- matrix(NA, ncol=21, nrow=1)
	colnames(dat) <- c("PrSu1", "PrSu2", "PrSu3", "PrSu", "GrSN1", "GrSN2", "GrSN3", "GrSN", "CArc", "InfP1", "InfP2", "InfP3", "InfP", "IsPu", "Sex estimate (Bruzek 2002)", "Statistical sex estimate (2018)", "Prob(F)", "Prob(M)", " Selected predictors in LR model", "10-fold CV accuracy (%)", "Indet. rate in CV (%)")
	dat <- dat[-1,]
	dat <- as.matrix(dat)
	assign("dat", dat, myenvg) # on place cette table dans l'environnement global

	######################################################
	# 1b. LES BOUTONS D'AIDE POUR L'ONGLET MANUAL EDITING :
	observeEvent(input$helpPreauri, {
		showModal(modalDialog(
			title="Preauricular surface",
			img(src='preauricularSurface.png', align="left"),
			helpText("as, auricular surface; sn, sciaticnotch; t, piriform tuubercle; pg, preauricular groove; f, female condition; m, male condition."),
			helpText("F: Specific female shape (f-f-f), showing the deep depression, with closed circumference (true preauricular groove), associated with lack of tubercle."),
			helpText("M: Typical male form (m-m-m), showing the relief as smooth or very slight, with open border, associated with presence of tubercle."),
			helpText("Two examples of other female forms: f-f-i, deep depression, pits or groove with closed border, with intermediate aspects of positive relief; i-f-f, little depression with closed border, associated with lack of tubercle."),
			helpText("Two examples of other male forms: m-m-i, little groove opened laterally, with intermediate aspect of positive relief (paraglenoid groove); m-f-m, smooth relief with closed circumference and very prominent tubercle."),
			easyClose=TRUE,
			size="l"
		))
	})
     	 
	observeEvent(input$helpGSN, {
		showModal(modalDialog(
			title="Greater sciatic notch",
			img(src='greaterSN.png', align="left"),
			helpText("as, auricular surface; sn, sciatic notch; A, top of piriform tubercle, in the case of its absence; A', top of posterior inferior iliac spine; AB and A'B, sciatic notch breadth; CD, sciatic notch depth; AC and A'C, posterior chord of sciatic notch breadth; CB, anterior chord of sciatic notch breadth; AP (A'P), perpendicular at point A (A') to the line formed by the sciatic notch breadth."),
			helpText("F: Extreme female form (f-f-f), showing posterior chord segment AC (or A'C) longer (or close to equality) than the anterior chord CB, symmetry of notch contour associated with no crossing of the line A–P (A'–P) with the contour of the posterior chord."),
			helpText("M: Extreme male form (m-m-m), showing the posterior chord segment AC (A'C) shorter than the anterior chord CB, asymmetry of the outline chords associated with the crossing the line A-P (A'P) with the contour of the posterior chord."),
			helpText("Two examples of other female forms: f-i-f, posterior chord segment AC equal the anterior chord CB, associated with intermediate pattern of the outline chords relative to the deep of the notch, and outline of the posterior chord does not cross the perpendicular line AP. f-f-i, posterior chord segment AC equal to anterior chord CB, presence of relative symmetry of outline chords to the deep of notch and the line AP tangent to the outline of the posterior chord."),
			helpText("Two examples of other male forms: m-m-i, posterior chord segment AC shorter than anterior chord CB, associated with asymmetry of outline chords and line AP tangent to the outline of the posterior chord; m-m-f, posterior chord segment AC shorter than anterior chord CB, associated with asymmetry of outline chords; finally, outline of posterior chord doe not cross perpendicular line AP."),
			easyClose=TRUE,
			size="l"
		))
	})
     	 
	observeEvent(input$helpCompositeArch, {
		showModal(modalDialog(
			title="Composite arch",
			img(src='compositeArch.png', align="left"),
			helpText("Outline of anterior sciatic notch chord (2), relative to outline of anterior segment of auricular surface (1)."),
			helpText("s, auricular surface; sn, sciatic notch."),
			helpText("M: Absence of composite arch (single curve, 1 = 2)."),
			helpText("F: Presence of composite arch (double curve, 1 ≠ 2)."),
			easyClose=TRUE,
			size="l"
		))
	})
     	 
	observeEvent(input$helpInfPelvis, {
		showModal(modalDialog(
			title="Inferior pelvis",
			img(src='inferiorPelvis.png', align="left"),
			helpText("pus, pubic symphysis; itu, ischial tuberosity; cp, phallic ridge. Horizontal lines limit the middle part of the ischiopubic ramus. Broken line represents major axis of this structure."),
			helpText("F: Typical female form (f-f-f), showing external eversion of ischiopubic ramus and absence of the phallic ridge, associated with gracility of the bones."),
			helpText("M: Typical male form (m-m-m), showing direct course of medial part of ischiopubic ramus and presence of phallic ridge, associated with robustness of the bones."),
			helpText("Two examples of other female forms: f-f-i, external eversion of ischiopubic ramus and absence of phallic ridge, associated with intermediate aspect of ischiopubic ramus; f-i-f, external eversion of ischiopubic ramus and doubt about absence of crista phallica, associated with gracility of the bones."),
			helpText("Two examples of other male forms: m-i-m, direct course of medial part of ischiopubic ramus and doubt about presence of phallic ridge, associated with robustness of bones; m-m-i, direct course of medial part of ischiopubic ramus and presence of phallic ridge, associated with intermediate aspect of ramus morphology."),
			easyClose=TRUE,
			size="l"
		))
	})
     	 
	observeEvent(input$helpIschiopubicProp, {
		showModal(modalDialog(
			title="Ischiopubic proportions",
			img(src='ischiopubicProp.png', align="left"),
			helpText("Proportions of length of pubis (pu) and ischium (is)."),
			helpText("M: Male morphology (pu < is)."),
			helpText("F: Female morphology (pu > is)."),
			easyClose=TRUE,
			size="l"
		))
	})
     	 
	#########################################################
	# 1c. LA TABLE DE RESULTATS POUR L'ONGLET MANUAL EDITING :
	dat <- eventReactive(input$calcButton, { # on définit ici le calcul de la table de résultats ; ce calcul n'est actualisé (et effectué pour la première fois) qu'après un clic sur le bouton Apply.
				valeurs <- c(input$preauriSurf1, input$preauriSurf2, input$preauriSurf3, input$greatSN1, input$greatSN2, input$greatSN3, input$compoArch, input$infPelvis1, input$infPelvis2, input$infPelvis3, input$ispubProp) # les valeurs de l'individu actuellement saisies dans l'UI.
				temp <- get("dat", envir=myenvg) # on récupère le contenu actuel de la table de résultats (variable globale)...
				PaS <- metavar(c(input$preauriSurf1, input$preauriSurf2, input$preauriSurf3))
				GSN <- metavar(c(input$greatSN1, input$greatSN2, input$greatSN3))
				InP <- metavar(c(input$infPelvis1, input$infPelvis2, input$infPelvis3))
				
				traitsInd <- data.frame(PrSu1=input$preauriSurf1, PrSu2=input$preauriSurf2, PrSu3=input$preauriSurf3, GrSN1=input$greatSN1, GrSN2=input$greatSN2, GrSN3=input$greatSN3, CArc=input$compoArch, InfP1=input$infPelvis1, InfP2=input$infPelvis2, InfP3=input$infPelvis3, IsPu=input$ispubProp)
				colOk <- colnames(traitsInd)[which(traitsInd[1,]!="NA")] # les colonnes portant des valeurs "non-NA"
				traitsInd <- data.frame(traitsInd[ , traitsInd[1,]!="NA"])
				colnames(traitsInd) <- colOk
				resStatEstimateME <- indivSexing(ref=refData, newInd=traitsInd) # ou traitsInd[1,] ?
				
				temp <- rbind(temp, c(input$preauriSurf1, input$preauriSurf2, input$preauriSurf3, PaS, input$greatSN1, input$greatSN2, input$greatSN3, GSN, input$compoArch, input$infPelvis1, input$infPelvis2, input$infPelvis3, InP, input$ispubProp, bruzek02(c(PaS, GSN, InP, input$compoArch, input$ispubProp)), resStatEstimateME$PredictedSex, round(1-resStatEstimateME$PostProb,3),round(resStatEstimateME$PostProb,3), resStatEstimateME$VariablesUsed, round(100*resStatEstimateME$cvRate,2), round(100*resStatEstimateME$cvIndet,2))) #... et on y ajoute l'individu dont les valeurs sont actuellement saisies dans l'UI.
				rownames(temp)[input$calcButton] <- input$indivName # on ajoute le nom de cet individu...
				assign("dat", temp, envir=myenvg) # et on place cette nouvelle table dans l'environnement global (écrase la précédente valeur)
				return(temp) # on retourne cette table pour la fonction renderTable ci-dessous.
	})
     	 
	output$tabResME <- renderTable(tail(dat(), n=5), align="c", rownames=TRUE, striped=TRUE, hover=TRUE, digits=2) # le rendu de la table de résultats
	
	# SON BOUTON DE TÉLÉCHARGEMENT :
	output$button_download_resultsME <- renderUI({  # ce bouton n'est gener\'e que lorsque l'utilisateur a soumis des donnees
					if (input$calcButton>0) { 
						downloadButton("download_resultsME", "Download the complete table [CSV file]")
					} else { 
						return() 
					}
	})
	output$download_resultsME <- downloadHandler(filename="results_SexingOsCoxae.csv", content=function(file) {
		write.csv(dat(), file)
	}) # la fonction declenchee par le bouton de telechargement	


	###################################################################
	# 2a. CHARGER LE FICHIER DE DONNEES POUR L'ONGLET INPUT FROM FILE :
	observeEvent(input$loadData, { # le fichier est chargé à partir du moment où on clique sur le bouton
		if (! is.null(input$file$name)) { # on verifie que l'utilisateur a bien choisi un fichier !
			datUser <- read.table(input$file$datapath, header=TRUE, sep=input$fieldSep, na.strings=input$charNA)
			if (input$rowNames) { # s'il y a les noms d'individus...
				rownames(datUser) <- datUser[,1] # ... on les ajoute au dataframe à partir de la 1ère colonne...
				datUser[,1] <- NULL # ... que l'on supprime donc ensuite
			} else { # s'il n'y en a pas...
				rownames(datUser) <- 1:nrow(datUser) # ... on en ajoute. (Utile pour l'indicateur de progression.)
			}
			if (validData(datUser)) { # si le fichier d'entrée est bien valide...
				assign("datUser", datUser, envir=myenvg) # ... on place le jeu de donnees dans l'environnement global.
			} else { # sinon, le fichier d'entrée est invalide : on affiche un message d'erreur.
				showModal(modalDialog(title="Error", "Invalid file. Please check the field separator and the spelling of column names.", easyClose = TRUE))
			}
		} else { # l'utilisateur avait oubli\'e de choisir un fichier
			showModal(modalDialog(title = "Error", "Please select a file on your computer.", easyClose = TRUE))
		}	
	})
	
	##################################################################################
	# 2b. REALISER LA DETERMINATION DU SEXE POUR LE FICHIER SOUMIS PAR L'UTILISATEUR :
	# LA TABLE :
	output$tabResFF <- renderTable({
		# Définir et initialiser une barre de progression :
		progressFF <- shiny::Progress$new()
		progressFF$set(message="Computing results...", value=0)
		# Fermer cette barre de progression lorsqu'on quittera cette sortie réactive :
		on.exit(progressFF$close())
		# Fonction liée à la barre de progression :
		updateProgressBarFF <- function(currentValue=NULL, detail=NULL, total=NULL) {
			if (is.null(currentValue)) {
			currentValue <- progressFF$getValue()
			currentValue <- currentValue + 1 / total
		}
		progressFF$set(value=currentValue, detail=detail)
}
		# Retourner le tableau :
		if (input$loadData>0 & exists("datUser", envir=myenvg)) { # si un jeu de donnees a bien ete fourni et qu'il est valide !
			finalResultFF <- sexingFromFile(ref=refData, dat=get("datUser", envir=myenvg), updateProgressBarFF)
			assign("ResultsFF", finalResultFF, envir=myenvg)
			return(head(finalResultFF, 10))
		} else { # sinon on ne retourne rien
			NULL
		}
	}, align="c", rownames=TRUE, striped=TRUE, hover=TRUE, digits=2)
	
	# SON BOUTON DE TÉLÉCHARGEMENT :
	output$button_download_resultsFF <- renderUI({  # ce bouton n'est gener\'e que lorsque l'utilisateur a soumis des donnees
					if (input$loadData>0 & exists("datUser", envir=myenvg)) { 
						downloadButton("download_resultsFF", "Download the complete table [CSV file]")
					} else { 
						return() 
					}
	})
	output$download_resultsFF <- downloadHandler(filename="results_SexingOsCoxae.csv", content=function(file) {
		saveData <- get("ResultsFF", finalResultFF, envir=myenvg)
		write.csv(saveData, file)
	}) # la fonction declenchee par le bouton de telechargement
})

