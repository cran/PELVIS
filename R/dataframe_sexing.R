sexingFromFile <- function(data, ref, updateProgressBar = NULL,
                           conf_level = 0.95, strategy = c("BIC", "AIC", "None"),
                           trace = 1){
    .Deprecated("dataframe_sexing")
    dataframe_sexing(data, ref, updateProgressBar,
                     conf_level, strategy, trace)
}

dataframe_sexing <- function(data, ref, updateProgressBar = NULL,
                             conf_level = 0.95, strategy = c("BIC", "AIC", "None"),
                             trace = 1) {
### data: dataframe (individuals to estimate) with at least the eleven trichotomic traits
### ref: dataframe (learning dataset)
### updateProgressBar: function for progress bar in shiny app
### conf_level: 0.95 by default, confidence level needed to produce a sex estimate
### strategy: passed to indivSexing
### trace: passed to indivSexing
### (NB : les colonnes doivent contenir les labels adéquats, mais il peut éventuellement y avoir d'autres colonnes illustratives [e.g. age, ...] dans le jeu de données)

#################################################
### 1. Add the main characters PrSu, GrSN, InfP #
#################################################
    data <- data[ , c("PrSu1", "PrSu2", "PrSu3", "GrSN1", "GrSN2", "GrSN3",
                    "CArc", "InfP1", "InfP2", "InfP3", "IsPu")]
    metadat <- add_metavars(data)

########################################################
### 2. Prepare and initialize the dataframe of results #
########################################################
    df_res <- matrix(NA, ncol = 21, nrow = nrow(data))
    df_res <- as.data.frame(df_res, stringsAsFactors = FALSE)
    colnames(df_res) <- c("PrSu1", "PrSu2", "PrSu3", "PrSu",
                          "GrSN1", "GrSN2", "GrSN3", "GrSN",
                          "CArc", "InfP1", "InfP2", "InfP3", "InfP", "IsPu",
                          "Sex estimate (Bruzek 2002)",
                          "Statistical sex estimate (2019)",
                          "Prob(F)", "Prob(M)",
                          "Selected predictors in LR model",
                          "10-fold CV accuracy (%)", "Indet. rate in CV (%)")
    rownames(df_res) <- rownames(data)

########################################
### 3. Sex estimation with Bruzek 2002 #
########################################
    b02 <- apply(metadat[ , c("PrSu", "GrSN", "InfP", "CArc", "IsPu")],
                 MARGIN = 1, FUN = bruzek02)
    df_res[ , "Sex estimate (Bruzek 2002)"] <- factor(b02)

#################################
### 4. Sex estimation with GLMs #
#################################
    index_models <- 1
    dtf_models <- data.frame(InitialVars = character(0),
                             tenCV_perfRate = numeric(0),
                             tenCV_indetRate = numeric(0),
                             stringsAsFactors = FALSE)
    list_models <- list()

    for (i in 1:nrow(data)) { # for each individual,
        ## retrieve the observed traits on this individual:
        obs_traits <- paste(colnames(data)[which(!is.na(data[i, ]))],
                            collapse = ", ")
        if ((! obs_traits %in% dtf_models$InitialVars) & (sum(!is.na(data[i, ])) > 0)) {
            ## if those variables do not correspond to a GLM that has already been used:
            res <- indiv_sexing(ref = ref, new_ind = data[i, ],
                                strategy = strategy, trace = trace,
                                conf_level = conf_level) # then we compute this GLM,
            df_res[i, "Statistical sex estimate (2019)"] <- res$PredictedSex # and we store the results in the dataframe.
            df_res[i, "Prob(F)"] <- round(1 - res$PostProb, 3)
            df_res[i, "Prob(M)"] <- round(res$PostProb, 3)
            df_res[i, "Selected predictors in LR model"] <- res$VariablesUsed
            df_res[i, "10-fold CV accuracy (%)"] <- round(100 * res$cvRate, 2)
            df_res[i, "Indet. rate in CV (%)"] <- round(100 * res$cvIndet, 2)
            list_models[[index_models]] <- res$BestModel
            dtf_models[index_models, "InitialVars"] <- obs_traits
            dtf_models[index_models, "tenCV_perfRate"] <- round(100 * res$cvRate, 2)
            dtf_models[index_models, "tenCV_indetRate"] <- round(100 * res$cvIndet, 2)
            index_models <- index_models + 1 # here we increment the index of GLMs that have been used.
        } else if (sum(!is.na(data[i,])) > 0) { # # if those variables do correspond to a GLM that has already been used,
            where.mod <- which(dtf_models$InitialVars == obs_traits) # retrieve this model,
            mod_current <- list_models[[where.mod]]
            ## and use this model to return a sex estimate:
            postprob <- predict(mod_current, newdata = as.data.frame(data[i, ]), type = "response")
            df_res[i, "Statistical sex estimate (2019)"] <- finalSE(postprob, conf_level)
            df_res[i, "Prob(F)"] <- round(1 - postprob, 3)
            df_res[i, "Prob(M)"] <- round(postprob, 3)
            df_res[i, "Selected predictors in LR model"] <- paste(attr(mod_current$terms, "term.labels"), collapse = ", ")
            df_res[i, "10-fold CV accuracy (%)"] <- dtf_models[where.mod, "tenCV_perfRate"]
            df_res[i, "Indet. rate in CV (%)"] <- dtf_models[where.mod, "tenCV_indetRate"]
        } 
        ## Update progress bar:
        if (is.function(updateProgressBar)) { # i.e., if not-NULL,
            text <- paste("Currently processing individual ", rownames(data)[i], "...", sep = "")
            updateProgressBar(detail = text, total = nrow(data))
        }
    }
    
##########################################
### 5. Return the results in a dataframe #
##########################################
    df_res[ , c("PrSu1", "PrSu2", "PrSu3", "GrSN1", "GrSN2", "GrSN3", "CArc", "InfP1", "InfP2", "InfP3", "IsPu")] <- data
    df_res[ , c("PrSu", "GrSN", "InfP", "CArc", "IsPu")] <- metadat[ , c("PrSu", "GrSN", "InfP", "CArc", "IsPu")]
    return(df_res)
}
