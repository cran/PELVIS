indivSexing <- function(ref, newInd) {
# Returns a sex estimate of an individual "newInd" using a GLM learned on the dataframe "ref"
# ref: dataframe, reference dataset with the eleven trichotomic traits used in Bruzek's method
# newInd: vector or 1-row dataframe

  newInd <- as.data.frame(newInd) # mandatory for subsequent use by "predict" function
  
  if (ncol(newInd)>0) { # if we have a valid input,
    colOk <- colnames(newInd)[which(! is.na(newInd[1,]) )] # columns with non-missing values (i.e., observed traits) for the new individual
    newInd <- as.data.frame(newInd[ , !is.na(newInd[1,])])
    colnames(newInd) <- colOk
  
    # 1. In "ref", keep only the traits observed in "newInd":
    ref <- na.omit(ref[ , c("Sex", colnames(newInd))])
  
    # 2. Build a complete GLM on the learning dataset:
    modComp <- glm(Sex ~ ., data=ref, family=binomial)
    
    # 3. Variable selection with BIC:
    modRed <- stepAIC(modComp, k=log(nrow(ref))) # critère BIC
    predicteurs <- paste(attr(modRed$terms, "term.labels"), collapse=", ") # la liste des prédicteurs, en une seule chaîne de caractères, séparés par des virgules.
  
    # 4. Posterior probabilities predicted for "newInd":
    postprob <- predict(modRed, newdata=newInd, type="response")
  
    # 5. Compute indicators of model accuracy in LOOCV:
    resCV <- tenFoldCV.glm(dat.glm=ref, mod=modRed, conf_level=0.95)
    perfCV <- resCV$SuccessRate
    indetCV <- resCV$IndetRate
  
    # 6. Return results:
    return(list(PredictedSex=ifelse(postprob>=0.95,"M", ifelse(postprob<=0.05,"F","I")), PostProb=postprob, BestModel=modRed, VariablesUsed=predicteurs, cvRate=perfCV, cvIndet=indetCV))
  } else { # if no data
    return(list(PredictedSex=NA, PostProb=NA, BestModel=NULL, VariablesUsed=NULL, cvRate=NA, cvIndet=NA))
  }
  
}
