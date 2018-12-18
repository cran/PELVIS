shinyUI(fluidPage(theme="kappa.css",

	tags$style(HTML("
		.btn-help.btn {
			display: inline-block;
			padding: 0px 5px 0px 5px;
			border-radius: 20px;
			font-size: 0.8em;
			margin: 0 0 0 0;
			vertical-align: middle;
			color: gray;
			font-weight: bold;
			background-color: white;
			border-color: gray;
		}
		.btn-help.btn:hover {
			color: white;
			background-color: #0098B6;
		}
		.btn-help.active {
			color: white;
			background-color: #0098B6;
			border-color: #0098B6;
		}
		.shiny-notification{
			position: fixed;
			top: 60%;
			left: 20%;
			right: 20%;
			border-color: #E23D22;
			color: #0098B6;
		}
		"
	)),
	
	titlePanel("Visual method for sexing the human os coxae"),
	
	tabsetPanel( # la fenêtre principale est divisée en deux onglets
	
	###################################################
	# 1. L'ONGLET POUR LA SAISIE MANUELLE DES DONNEES :
	tabPanel("Data input: manual editing",
		verticalLayout(
			wellPanel(
				textInput("indivName", label=h4("Name of the unknown individual"), value="Indiv01", width="400px"),
				fluidRow(
        				column(3, # Preauricular surface
        					p(h5("Preauricular surface (PrSu)"), actionButton("helpPreauri", label="?", class="btn-help")),
        					selectInput("preauriSurf1", label="Development of negative relief on preauricular surface  (PrSu1)", choices=c("Could not be observed"="NA", "f, deep depression well-delimited"="f", "i, intermediate form"="i", "m, relief smooth or very slightly negative relief"="m"), selected="NA", multiple=FALSE),
        					selectInput("preauriSurf2", label="Aspects of grooves or pitting (PrSu2)", choices=c("Could not be observed"="NA", "f, pits or groove with closed circumference"="f", "i, intermediate form"="i", "m, depression with open circumference"="m"), selected="NA", multiple=FALSE),
        					selectInput("preauriSurf3", label="Development of positive relief on preauricular surface (PrSu3)", choices=c("Could not be observed"="NA", "f, lack of tubercule"="f", "i, intermediate form"="i", "m, tubercule present or clear protuberance"="m"), selected="NA", multiple=FALSE)
        				),
        				column(3, # Great sciatic notch
        					p(h5("Great sciatic notch (GrSN)"), actionButton("helpGSN", label="?", class="btn-help")),
        					selectInput("greatSN1", label="Proportion of length of sciatic chords  (GrSN1)", choices=c("Could not be observed"="NA", "f, posterior chord segment (AC) longer than or equal to anterior chord (CB)"="f", "i, intermediate form"="i", "m, posterior chord (AC) shorter than anterior chord (CB)"="m"), selected="NA", multiple=FALSE),
        					selectInput("greatSN2", label="Form of contour notch chords (GrSN2)", choices=c("Could not be observed"="NA", "f, symmetry relative to depth in basal portion of sciatic notch"="f", "i, intermediate form"="i", "m, asymmetry relative to depth of sciatic notch"="m"), selected="NA", multiple=FALSE),
        					selectInput("greatSN3", label="Contour of posterior notch chord relative to line from point A to sciatic notch breadth (GrSN3)", choices=c("Could not be observed"="NA", "f, outline of posterior chord doesn't cross perpendicular line"="f", "i, intermediate form"="i", "m, contour of posterior chord crosses perpendicular line"="m"), selected="NA", multiple=FALSE)
        				),
        				column(2, # Composite arch
	        				p(h5("Composite arch (CArc)"), actionButton("helpCompositeArch", label="?", class="btn-help")),
        					radioButtons("compoArch", label="Relation between outline of sciatic notch and outline of auricular surface", choices=c("Could not be observed"="NA", "F, Double curve"="F", "0, Intermediate form"="0", "M, Single curve"="M"), selected="NA")        				
        				),
        				column(2, # Inferior pelvis
        					p(h5("Inferior pelvis (InfP)"), actionButton("helpInfPelvis", label="?", class="btn-help")),
        					selectInput("infPelvis1", label="Characterization of margo inferior ossis coxae (InfP1)", choices=c("Could not be observed"="NA", "f, external eversion"="f", "i, intermediate form"="i", "m, direct course of medial part"="m"), selected="NA", multiple=FALSE),
        					selectInput("infPelvis2", label="Phallic ridge (InfP2)", choices=c("Could not be observed"="NA", "f, lack of the phallic ridge or presence of only little mound"="f", "i, intermediate form"="i", "m, clear presence of the phallic ridge"="m"), selected="NA", multiple=FALSE),
        					selectInput("infPelvis3", label="Ischio-pubic ramus aspect (InfP3)", choices=c("Could not be observed"="NA", "f, gracile aspect"="f", "i, intermediate form"="i", "m, robust aspect"="m"), selected="NA", multiple=FALSE)
        				),
        				column(2, # Ischiopubic proportion
	        				p(h5("Ischiopubic proportion (IsPu)"), actionButton("helpIschiopubicProp", label="?", class="btn-help")),
        					radioButtons("ispubProp", label="Relation between pubis and ischium lengths", choices=c("Could not be observed"="NA", "F, Pubis longer than ischium"="F", "0, Intermediate form"="0", "M, Ischium longer than pubis"="M"), selected="NA")
        				)
        			),
        			actionButton("calcButton", label="Apply")
        		), 
        		
        		wellPanel(
        			h3("Results"),
        			helpText("Please note that posterior probabilities are rounded to the third decimal place."),
			 	div(DT::DTOutput("tabResME")),
			 	uiOutput("button_download_resultsME")
        		)
	 	) # end "verticalLayout"
 	), # end "tabPanel" for manual editing
 	
 	
 	#################################################################
 	# 2. L'ONGLET POUR L'IMPORT DES DONNEES DEPUIS UN FICHIER TEXTE :
	tabPanel("Data input: from text file",
		verticalLayout(
 			wellPanel(
 				fluidRow(
 					column(4,
						fileInput("file", label=strong("File"), accept=c(".csv",".txt")),
						helpText("Only files in plain-text format (such as CSV or TXT files) are accepted.", "Please make sure that the headers are correctly defined.")
					),
					column(4,
						selectInput("fieldSep", label="Field separator", choices=c("Semicolon (;)"=";", "Comma(,)"=",", "Tabulation"="\t", "Space"=" "), selected=";", multiple=FALSE),
						textInput("charNA", label="Indicator for missing values", value="")
					), 
					column(4,
						checkboxInput("rowNames", label=strong("Row names in first column"), value=TRUE),
						helpText("This option should be checked if and only if each bone has a unique attribute or ID in the first column of the data file. Please note that this option is really about bone names, and not individual names: each individual may have two bones in the dataset, so that individual names cannot be seen as unique IDs.")				
					)				
				),
				actionButton("loadData", "Load dataset"), # bouton de chargement de donnees (server.R attend un clic sur ce bouton pour demarrer)
				helpText("Please note that for large input files with many missing values, the calculation may be slow due to the stepwise procedure of variable selection in logistic regression models, and the calculation of 10-fold cross-validation success rate for each model.")
			),
			wellPanel(
				h3("Results"),
				helpText("Please note that posterior probabilities are rounded to the third decimal place."),
				div(DT::DTOutput("tabResFF")),
				uiOutput("button_download_resultsFF")
			)
		)
	)
 	) # end "tabsetPanel"
 ))
