sexingFromFile <- function(dat, ref, updateProgressBar=NULL, conf_level=0.95) {
# dat: dataframe (individuals to estimate) with at least the eleven trichotomic traits
# ref: dataframe (learning dataset)
# updateProgressBar: function for progress bar in shiny app
# conf_level: 0.95 by default, confidence level needed to produce a sex estimate
# (NB : les colonnes doivent contenir les labels adéquats, mais il peut éventuellement y avoir d'autres colonnes illustratives [e.g. âge, ...] dans le jeu de données)

  ###############################################
  # 1. Add the main characters PrSu, GrSN, InfP #
  ###############################################
  dat <- dat[ , c("PrSu1", "PrSu2", "PrSu3", "GrSN1", "GrSN2", "GrSN3", "CArc", "InfP1", "InfP2", "InfP3", "IsPu")]
  metadat <- addMetavars(dat)

  ######################################################
  # 2. Prepare and initialize the dataframe of results #
  ######################################################
  MatRes <- matrix(NA, ncol=21, nrow=nrow(dat))
  MatRes <- as.data.frame(MatRes, stringsAsFactors=FALSE)
  colnames(MatRes) <- c("PrSu1", "PrSu2", "PrSu3", "PrSu", "GrSN1", "GrSN2", "GrSN3", "GrSN", "CArc", "InfP1", "InfP2", "InfP3", "InfP", "IsPu", "Sex estimate (Bruzek 2002)", "Statistical sex estimate (2019)", "Prob(F)", "Prob(M)", "Selected predictors in LR model", "10-fold CV accuracy (%)", "Indet. rate in CV (%)")
  rownames(MatRes) <- rownames(dat)
  
  ######################################
  # 3. Sex estimation with Bruzek 2002 #
  ######################################
  MatRes[ , "Sex estimate (Bruzek 2002)"] <- factor(apply(metadat[ , c("PrSu", "GrSN", "InfP", "CArc", "IsPu")], MARGIN=1, FUN=bruzek02))
  
  ###############################
  # 4. Sex estimation with GLMs #
  ###############################
  indexModeles <- 1
  tabModeles <- data.frame(InitialVars=character(0), tenCV_perfRate=numeric(0), tenCV_indetRate=numeric(0), stringsAsFactors=FALSE)
  listeModeles <- list()
  for (i in 1:nrow(dat)) { # for each individual
    variablesInit <- paste(colnames(dat)[which(!is.na(dat[i,]))], collapse=", ") # retrieve the observed traits on this individual.
    if ((! variablesInit %in% tabModeles$InitialVars) & (sum(!is.na(dat[i,]))>0)) { # if those variables do not correspond to a GLM that has already been used,
      res <- indivSexing(ref=ref, newInd=dat[i,]) # then we compute this GLM,
      MatRes[i, "Statistical sex estimate (2019)"] <- res$PredictedSex # and we store the results in the dataframe.
      MatRes[i, "Prob(F)"] <- round(1-res$PostProb, 3)
      MatRes[i, "Prob(M)"] <- round(res$PostProb, 3)
      MatRes[i, "Selected predictors in LR model"] <- res$VariablesUsed
      MatRes[i, "10-fold CV accuracy (%)"] <- round(100*res$cvRate, 2)
      MatRes[i, "Indet. rate in CV (%)"] <- round(100*res$cvIndet, 2)
      listeModeles[[indexModeles]] <- res$BestModel
      tabModeles[indexModeles, "InitialVars"] <- variablesInit
      tabModeles[indexModeles, "tenCV_perfRate"] <- round(100*res$cvRate, 2)
      tabModeles[indexModeles, "tenCV_indetRate"] <- round(100*res$cvIndet, 2)
      indexModeles <- indexModeles + 1 # here we increment the index of GLMs that have been used.
    } else if (sum(!is.na(dat[i,]))>0) { # # if those variables do correspond to a GLM that has already been used,
      where.mod <- which(tabModeles$InitialVars == variablesInit) # retrieve this model,
      currentMod <- listeModeles[[where.mod]]
      postprob <- predict(currentMod, newdata=as.data.frame(dat[i,]), type="response") # and use it to return a sex estimate
      MatRes[i, "Statistical sex estimate (2019)"] <- ifelse(postprob>=conf_level,"M", ifelse(postprob<=(1-conf_level),"F","I")) 
      MatRes[i, "Prob(F)"] <- round(1-postprob, 3)
      MatRes[i, "Prob(M)"] <- round(postprob, 3)
      MatRes[i, "Selected predictors in LR model"] <- paste(attr(currentMod$terms, "term.labels"), collapse=", ")
      MatRes[i, "10-fold CV accuracy (%)"] <- tabModeles[where.mod, "tenCV_perfRate"]
      MatRes[i, "Indet. rate in CV (%)"] <- tabModeles[where.mod, "tenCV_indetRate"]
    } 
    # Update progress bar:
    if (is.function(updateProgressBar)) { # i.e., if not-NULL,
      text <- paste("Currently processing individual ", rownames(dat)[i], "...", sep="")
      updateProgressBar(detail=text, total=nrow(dat))
    }
  }
  
  ########################################
  # 5. Return the results in a dataframe #
  ########################################
  MatRes[ , c("PrSu1", "PrSu2", "PrSu3", "GrSN1", "GrSN2", "GrSN3", "CArc", "InfP1", "InfP2", "InfP3", "IsPu")] <- dat
  MatRes[ , c("PrSu", "GrSN", "InfP", "CArc", "IsPu")] <- metadat[ , c("PrSu", "GrSN", "InfP", "CArc", "IsPu")]
  return(MatRes)
}
