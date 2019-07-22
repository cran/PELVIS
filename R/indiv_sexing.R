indivSexing <- function(ref, new_ind, strategy = c("BIC", "AIC", "None"),
                        trace = 1, conf_level = 0.95) {
    .Deprecated("indiv_sexing")
    indiv_sexing(ref, new_ind, strategy, trace, conf_level)
}

indiv_sexing <- function(ref, new_ind, strategy = c("BIC", "AIC", "None"),
                         trace = 1, conf_level = 0.95) {
### Returns a sex estimate of an individual "new_ind" using a GLM learned on the dataframe "ref"
### ref: dataframe, reference dataset with the eleven trichotomic traits used in Bruzek's method
### strategy: strategy for variable selection in LR model
### new_ind: vector or 1-row dataframe
### trace: passed to MASS::stepAIC
### conf_level: required posterior probability threshold to produce a sex estimate

#######################################
### 1. Prepare constants and arguments:
    ## to avoid a warning if the user do not specify anything:
    strategy <- match.arg(strategy)
    ## mandatory for subsequent use by "predict" function:
    new_ind <- as.data.frame(new_ind)

######################
### 2. Sex estimation:
    if (ncol(new_ind) > 0) {
        ## if we have a valid input,
        ## Mark columns with non-missing values (i.e., observed traits) for the new individual:
        column_ok <- colnames(new_ind)[which(! is.na(new_ind[1, ]))]
        new_ind <- as.data.frame(new_ind[ , !is.na(new_ind[1, ])])
        colnames(new_ind) <- column_ok

        ## 2.1. In "ref", keep only the traits observed in "new_ind":
        ref <- na.omit(ref[ , c("Sex", colnames(new_ind))])

        ## 2.2. Build a complete GLM on the learning dataset:
        mod.complete <- glm(Sex ~ ., data = ref, family = binomial)

        ## 2.3. Variable selection (optional):
        if (strategy == "None") {
            mod.best <- mod.complete # no variable selection
        } else if (strategy == "AIC") {
            mod.best <- MASS::stepAIC(mod.complete, k = 2, trace = trace)
        } else { # strategy = "BIC"
            mod.best <- MASS::stepAIC(mod.complete, k = log(nrow(ref)),
                                      trace = trace)
        }
        ## Make list of covariates, in one single string, separated by commas:
        predicteurs <- paste(attr(mod.best$terms, "term.labels"),
                             collapse = ", ")

        ## 2.4. Posterior probabilities predicted for "new_ind":
        postprob <- predict(mod.best, newdata = new_ind, type = "response")

        ## 2.5. Compute indicators of model accuracy in LOOCV:
        res_cv <- tenFoldCV.glm(dat.glm = ref, mod = mod.best,
                                conf_level = conf_level)
        perf_cv <- res_cv$SuccessRate
        indet_cv <- res_cv$IndetRate

        ## 2.6. Return results:
        return(list(PredictedSex = finalSE(postprob, conf_level),
                    PostProb = postprob,
                    BestModel = mod.best,
                    VariablesUsed = predicteurs,
                    cvRate = perf_cv,
                    cvIndet = indet_cv))
    } else { # if no data
        return(list(PredictedSex = NA, PostProb = NA, BestModel = NULL,
                    VariablesUsed = NULL, cvRate = NA, cvIndet = NA))
    }
}
