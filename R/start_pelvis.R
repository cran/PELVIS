start_pelvis <- function() {
    ## Define the UI and server files for the app:
    app <- shiny::shinyApp(ui = ui, server = server)
    ## Define a folder that contains a CSS sheet and images:
    shiny::addResourcePath(prefix = "style", directoryPath = system.file("www", package = "PELVIS"))
    ## Run the app:
    shiny::runApp(app)
}

StartPELVIS <- function() {
    .Deprecated("start_pelvis")
    start_pelvis()
}
