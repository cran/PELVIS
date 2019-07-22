server <- shinyServer(function(input, output, session) {

    myenvg <- new.env() # private environment for PELVIS; will contain the dataset loaded through the UI
    data(refDataBruzek02, package = "PELVIS", envir = myenvg) # load the learning dataset
    refDataBruzek02 <- get("refDataBruzek02", envir = myenvg) # avoids a NOTE in R CMD check (no visible binding...)
    
###########################
### 1. TAB "MANUAL EDITING"
    ## 1.1. Initialize the dataframe of results that will be returned through the UI:
    ## (initialized to a matrix with 0 row and 21 columns adequately labeled)
    dat <- matrix(NA, ncol = 21, nrow = 1)
    colnames(dat) <- c("PrSu1", "PrSu2", "PrSu3", "PrSu",
                       "GrSN1", "GrSN2", "GrSN3", "GrSN", "CArc",
                       "InfP1", "InfP2", "InfP3", "InfP", "IsPu",
                       "Sex estimate (Bruzek 2002)",
                       "Statistical sex estimate (2019)",
                       "Prob(F)", "Prob(M)", " Selected predictors in LR model",
                       "10-fold CV accuracy (%)", "Indet. rate in CV (%)")
    dat <- dat[-1, ]
    dat <- as.matrix(dat)
    assign("dat", dat, myenvg) # send this matrix to the private environment

    ## 1.2. Help buttons:
    observeEvent(input$helpPreauri, {
        showModal(modalDialog(
            title = "Preauricular surface",
            img(src = 'style/preauricularSurface.png', align = "left"),
            helpText("as, auricular surface; sn, sciaticnotch; t, piriform tuubercle; pg, preauricular groove; f, female condition; m, male condition."),
            helpText("F: Specific female shape (f-f-f), showing the deep depression, with closed circumference (true preauricular groove), associated with lack of tubercle."),
            helpText("M: Typical male form (m-m-m), showing the relief as smooth or very slight, with open border, associated with presence of tubercle."),
            helpText("Two examples of other female forms: f-f-i, deep depression, pits or groove with closed border, with intermediate aspects of positive relief; i-f-f, little depression with closed border, associated with lack of tubercle."),
            helpText("Two examples of other male forms: m-m-i, little groove opened laterally, with intermediate aspect of positive relief (paraglenoid groove); m-f-m, smooth relief with closed circumference and very prominent tubercle."),
            easyClose = TRUE,
            size = "l"
        ))
    })
        
    observeEvent(input$helpGSN, {
        showModal(modalDialog(
            title = "Greater sciatic notch",
            img(src = 'style/greaterSN.png', align = "left"),
            helpText("as, auricular surface; sn, sciatic notch; A, top of piriform tubercle, in the case of its absence; A', top of posterior inferior iliac spine; AB and A'B, sciatic notch breadth; CD, sciatic notch depth; AC and A'C, posterior chord of sciatic notch breadth; CB, anterior chord of sciatic notch breadth; AP (A'P), perpendicular at point A (A') to the line formed by the sciatic notch breadth."),
            helpText("F: Extreme female form (f-f-f), showing posterior chord segment AC (or A'C) longer (or close to equality) than the anterior chord CB, symmetry of notch contour associated with no crossing of the line A-P (A'-P) with the contour of the posterior chord."),
            helpText("M: Extreme male form (m-m-m), showing the posterior chord segment AC (A'C) shorter than the anterior chord CB, asymmetry of the outline chords associated with the crossing the line A-P (A'P) with the contour of the posterior chord."),
            helpText("Two examples of other female forms: f-i-f, posterior chord segment AC equal the anterior chord CB, associated with intermediate pattern of the outline chords relative to the deep of the notch, and outline of the posterior chord does not cross the perpendicular line AP. f-f-i, posterior chord segment AC equal to anterior chord CB, presence of relative symmetry of outline chords to the deep of notch and the line AP tangent to the outline of the posterior chord."),
            helpText("Two examples of other male forms: m-m-i, posterior chord segment AC shorter than anterior chord CB, associated with asymmetry of outline chords and line AP tangent to the outline of the posterior chord; m-m-f, posterior chord segment AC shorter than anterior chord CB, associated with asymmetry of outline chords; finally, outline of posterior chord doe not cross perpendicular line AP."),
            easyClose = TRUE,
            size = "l"
        ))
    })

    observeEvent(input$helpCompositeArch, {
        showModal(modalDialog(
            title = "Composite arch",
            img(src = 'style/compositeArch.png', align = "left"),
            helpText("Outline of anterior sciatic notch chord (2), relative to outline of anterior segment of auricular surface (1)."),
            helpText("s, auricular surface; sn, sciatic notch."),
            helpText("M: Absence of composite arch (single curve, 1 = 2)."),
            helpText("F: Presence of composite arch (double curve, 1 \u2260 2)."),
            easyClose = TRUE,
            size = "l"
        ))
    })

    observeEvent(input$helpInfPelvis, {
        showModal(modalDialog(
            title = "Inferior pelvis",
            img(src = 'style/inferiorPelvis.png', align = "left"),
            helpText("pus, pubic symphysis; itu, ischial tuberosity; cp, phallic ridge. Horizontal lines limit the middle part of the ischiopubic ramus. Broken line represents major axis of this structure."),
            helpText("F: Typical female form (f-f-f), showing external eversion of ischiopubic ramus and absence of the phallic ridge, associated with gracility of the bones."),
            helpText("M: Typical male form (m-m-m), showing direct course of medial part of ischiopubic ramus and presence of phallic ridge, associated with robustness of the bones."),
            helpText("Two examples of other female forms: f-f-i, external eversion of ischiopubic ramus and absence of phallic ridge, associated with intermediate aspect of ischiopubic ramus; f-i-f, external eversion of ischiopubic ramus and doubt about absence of crista phallica, associated with gracility of the bones."),
            helpText("Two examples of other male forms: m-i-m, direct course of medial part of ischiopubic ramus and doubt about presence of phallic ridge, associated with robustness of bones; m-m-i, direct course of medial part of ischiopubic ramus and presence of phallic ridge, associated with intermediate aspect of ramus morphology."),
            easyClose = TRUE,
            size = "l"
        ))
    })
  
    observeEvent(input$helpIschiopubicProp, {
        showModal(modalDialog(
            title = "Ischiopubic proportions",
            img(src = 'style/ischiopubicProp.png', align = "left"),
            helpText("Proportions of length of pubis (pu) and ischium (is)."),
            helpText("M: Male morphology (pu < is)."),
            helpText("F: Female morphology (pu > is)."),
            easyClose = TRUE,
            size = "l"
        ))
    })

    ## 1.3. Compute the table of results
    dat <- eventReactive(input$calcButton, {
        ## We begin here the computation of the table of results;
        ## the computation is done when the user clicks on the button "Apply".
        ## We retrieve here the values entered by the user through the UI:
        valeurs <- c(input$preauriSurf1, input$preauriSurf2, input$preauriSurf3,
                     input$greatSN1, input$greatSN2, input$greatSN3,
                     input$compoArch,
                     input$infPelvis1, input$infPelvis2, input$infPelvis3,
                     input$ispubProp)
        ## Then we get the current state of the table of results:
        temp <- get("dat", envir = myenvg)
        ## We compute the "main characters":
        PaS <- metavar(c(input$preauriSurf1, input$preauriSurf2, input$preauriSurf3))
        GSN <- metavar(c(input$greatSN1, input$greatSN2, input$greatSN3))
        InP <- metavar(c(input$infPelvis1, input$infPelvis2, input$infPelvis3))
        ## Then we can add store the complete values for the current individual:
        traitsInd <- data.frame(PrSu1 = input$preauriSurf1,
                                PrSu2 = input$preauriSurf2,
                                PrSu3 = input$preauriSurf3,
                                GrSN1 = input$greatSN1,
                                GrSN2 = input$greatSN2,
                                GrSN3 = input$greatSN3,
                                CArc = input$compoArch,
                                InfP1 = input$infPelvis1,
                                InfP2 = input$infPelvis2,
                                InfP3 = input$infPelvis3,
                                IsPu = input$ispubProp)
        ## Mark the columns corresponding to nonmissing (i.e., observed) traits:
        colOk <- colnames(traitsInd)[which(traitsInd[1,]!="NA")]
        traitsInd <- data.frame(traitsInd[ , traitsInd[1,]!="NA"])
        colnames(traitsInd) <- colOk
        ## And finally, compute a sex estimate for the current individual:
        resStatEstimateME <- indiv_sexing(ref = refDataBruzek02,
                                          new_ind = traitsInd,
                                          strategy = input$indivSelvar,
                                          conf_level = as.numeric(input$indivConf))
        ## Add all those informations in the table of results:
        temp <- rbind(temp, # i.e., we add a new row below the existing results
                      c(input$preauriSurf1, input$preauriSurf2, input$preauriSurf3, PaS,
                        input$greatSN1, input$greatSN2, input$greatSN3, GSN,
                        input$compoArch,
                        input$infPelvis1, input$infPelvis2, input$infPelvis3, InP,
                        input$ispubProp,
                        bruzek02(c(PaS, GSN, InP, input$compoArch, input$ispubProp)),
                        resStatEstimateME$PredictedSex, # sex estimate with Bruzek 2019
                        round(1 - resStatEstimateME$PostProb, 3), # p(F) with Bruzek 2019
                        round(resStatEstimateME$PostProb, 3), # p(M) with Bruzek 2019
                        resStatEstimateME$VariablesUsed, # traits in LR model
                        round(100 * resStatEstimateME$cvRate, 2), # success rate in CV
                        round(100 * resStatEstimateME$cvIndet,2) # indet rate in CV
                        )
                      )
        rownames(temp)[input$calcButton] <- input$indivName # add a proper row name
        ## We can now update the current state of the table of results, with this new row:
        assign("dat", temp, envir = myenvg)
        return(temp) # pass this new table to renderDT (below) for fisplay in the UI
  })

    ## 1.4. Display the table of results
    output$tabResME <- DT::renderDT(dat(),
                                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")))
                                    )
  
    ## 1.5 Render and display a download button
    ## (rendered only when a first indiv has been submitted)
    output$button_download_resultsME <- renderUI({
        if (input$calcButton > 0) { 
            downloadButton("download_resultsME", "Download the complete table [CSV file]")
        } else { 
            return()
        }
    })
    output$download_resultsME <- downloadHandler(filename = "results_SexingOsCoxae.csv",
                                                 content = function(file) {
                                                     write.csv(dat(), file)
                                                 })


############################
### 2. TAB "INPUT FROM FILE"
    ## 2.1. Load the data file
    ## When the user clicks on the button:
    observeEvent(input$loadData, {
        if (! is.null(input$file$name)) { # is there really a file?
            datUser <- read.table(input$file$datapath, header = TRUE,
                                  sep = input$fieldSep, na.strings = input$charNA)
            ## Handling rownames:
            if (input$rowNames) { # are there row names?
                if (any(duplicated(datUser[ , 1]))) { # if there are duplicates: invalid row names
                    showModal(modalDialog(
                        title = "Invalid row names",
                        helpText("There are duplicates in the first column; it cannot be used as row names. Arbitrary row names have been given."),
                        easyClose = TRUE,
                        size = "l"
                    ))
                    rownames(datUser) <- 1:nrow(datUser)
                } else { # there are no duplicates: valid row names!
                    rownames(datUser) <- datUser[ , 1] # add them to the dataframe
                    datUser[ , 1] <- NULL # and then remove them as a variable
                }
            } else { # no rownames
                rownames(datUser) <- 1:nrow(datUser) # just add an individual number (useful for the progress bar)
            }
            ## Is the dataframe valid?
            if (valid_data(datUser)) { 
                assign("datUser", datUser, envir = myenvg) # store in the private env
            } else {
                showModal(modalDialog(title = "Error",
                                      "Invalid file. Please check the field separator and the spelling of column names.",
                                      easyClose = TRUE)
                          )
            }
        } else { # there is no file!
            showModal(modalDialog(title = "Error",
                                  "Please select a file on your computer.",
                                  easyClose = TRUE)
                      )
        }
    })

    ## 2.2. Compute sex estimates
    output$tabResFF <- DT::renderDT({
        ## 2.2.1. Progress bar
        ## Initialize a progress bar:
        progressFF <- shiny::Progress$new()
        progressFF$set(message = "Computing results...", value = 0)
        ## Close the progress bar when the computation is done:                        
        on.exit(progressFF$close())
        ## Function for the progress bar:
        updateProgressBarFF <- function(currentValue = NULL, detail = NULL, total = NULL) {
            if (is.null(currentValue)) {
                currentValue <- progressFF$getValue()
                currentValue <- currentValue + 1 / total
            }
            progressFF$set(value = currentValue, detail = detail)
        }
        ## 2.2.2. Return the dataframe:
        if (input$loadData > 0 & exists("datUser", envir = myenvg)) { # file OK
            finalResultFF <- dataframe_sexing(ref = refDataBruzek02,
                                              data = get("datUser", envir = myenvg),
                                              updateProgressBarFF,
                                              strategy = input$fileSelvar,
                                              conf_level = as.numeric(input$fileConf))
            assign("ResultsFF", finalResultFF, envir = myenvg)
            return(finalResultFF)
        } else { # file not OK
            NULL # then return nothing
        }
    }, options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")))
    )
  
    ## 2.2.3. Download button
    output$button_download_resultsFF <- renderUI({
        if (input$loadData>0 & exists("datUser", envir = myenvg)) { 
            downloadButton("download_resultsFF", "Download the complete table [CSV file]")
        } else {
            return()
        }
    })
    output$download_resultsFF <- downloadHandler(filename = "results_SexingOsCoxae.csv",
                                                 content = function(file) {
                                                     saveData <- get("ResultsFF", finalResultFF, envir = myenvg)
                                                     write.csv(saveData, file)
                                                 })
})
