# Sys.setenv(PATH=paste(Sys.getenv("PATH"),
                      # file.path(getwd(),"miktex/bin/x64"),
                      # sep = .Platform$path.sep))

# Sys.setenv(RSTUDIO_PANDOC = file.path(getwd(), "Pandoc"))

# app launching code, e.g.:
# shiny::runApp("app/shiny/", launch.browser=TRUE)

source("server.R")
source("ui.R")
shinyApp(ui = ui, server = server)