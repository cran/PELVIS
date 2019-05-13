tenFoldCV.glm <- function(dat.glm, mod, conf_level=0.95) {
# dat.glm: dataframe on which to perform a 10-fold CV (with no missing values)
# mod: GLM to evaluate
# conf_level: 0.95 by default, confidence level needed to produce a sex estimate

  # Compute some useful constants:
  N <- nrow(dat.glm) # sample size in the dataset
  dat.glm <- dat.glm[sample(1:N, size=N, replace=FALSE) , ] # reorder the rows to introduce some randomness

  ####################################################
  # 1. Prepare and initialize a dataframe of results #
  ####################################################
  MatRes <- matrix(nrow=N, ncol=3)
  colnames(MatRes) <- c("True_sex", "Proba_10CV", "Predict_sex_10CV")
  MatRes <- data.frame(MatRes)
  MatRes[,1] <- as.character(dat.glm$Sex) 

  ###########################
  # 2. Perform a 10-fold CV #
  ###########################
  for (k in 1:10) { # for each of the ten folds ("splits", "folds"),
    pioch <- (1:N <= k*N/10) & (1:N > (k-1)*N/10) # draw at random 10% of individuals as a test sample
    app <- dat.glm[!pioch, ] # and train a GLM on the remaining 90%.
    val <- dat.glm[pioch, ]
    modTemp <- glm(mod$call$formula, family=binomial, data=app)
    MatRes[pioch, 2] <- predict(modTemp, newdata=val, type="response")
  }
  MatRes[ , 3] <- ifelse(MatRes[ , 2]>=conf_level, "M", ifelse(MatRes[ ,2]<=1-conf_level, "F", "I"))
  
  #####################
  # 3. Model accuracy #
  #####################
  indet_rate <- nrow(MatRes[MatRes$Predict_sex_10CV=="I", ]) / nrow(MatRes)
  if (indet_rate<1) { 
    success_rate <- nrow(MatRes[(MatRes$Predict_sex_10CV=="F" & MatRes$True_sex=="F") | (MatRes$Predict_sex_10CV=="M" & MatRes$True_sex=="M"), ]) / nrow(MatRes[MatRes$Predict_sex_10CV!="I", ])
  } else { # if there is only indeterminate individuals (might happen if one single trait is used as predictor)
    success_rate <- NA
  }
  
  #########################
  # 4. Return the results #
  #########################
  return(list(ClassifResults=MatRes, IndetRate=indet_rate, SuccessRate=success_rate))
  
}
