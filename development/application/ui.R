source("GEO_download.R")
source("Graphs_Generator.R")

library('shiny')
library('shinythemes')
library('shinyFiles')
library('tinytex')
library('DT')

add_path <- function(path) {
  s <- .Platform$path.sep
  paths <- c(path, unlist(strsplit(Sys.getenv('PATH'), s)))              
  Sys.setenv(PATH = paste(paths, collapse = s))
}

add_path(file.path(getwd(),"TinyTex/bin/windows"))
add_path(file.path(getwd(), "Pandoc"))

ui <- fluidPage(theme = shinytheme("readable"),
                navbarPage("TrLab Assistant",
                           tabPanel("Download Assistant",sidebarPanel(
                             textInput("terms",
                                       label = "Search terms",
                                       value = ""), # textInput
                             sliderInput("slider",
                                         label = "Number of searches",
                                         min = 1,
                                         max = 500,
                                         value = 100), # sliderInput
                             numericInput("numeric",
                                          label = "Number of searches",
                                          min = 1,
                                          max = 500,
                                          value = 100), # numericInput
                             actionButton("button_1", 
                                          "Search"), # actionButton
                             actionButton("button_2", 
                                          "Download"), # actionButton
                           ), # sidebarPanel
                           mainPanel(
                             tags$label(h3('Search results')), 
                             # Status/Output Text Box
                             uiOutput('dir_button'),
                             verbatimTextOutput("trlab_stored_dir"),
                             verbatimTextOutput("dwnload_end"),
                             DTOutput('myDT')
                           ) # mainPanel
                           ), # tabPanel "Download Assistant"
                           tabPanel("Report Assistant",sidebarPanel(
                                    shinyDirButton('folder', 
                                                   'Select a folder of solutions',
                                                   'Please select a folder', 
                                                   FALSE),
                                    downloadButton('downloadReport', 
                                                   label = "Download report")
                                      ), # sidebarPanel
                                    mainPanel(
                                      verbatimTextOutput("folderError"),
                                      htmlOutput("html_preview")
                                      ) # mainPanel
                                    ), # tabPanel "Report Assitant"
                           tabPanel("About", 
                                    shinythemes::themeSelector(), # Selects the theme for the shiny app
                                    shinyDirButton('button_3', 
                                                   'Change TrLab installation path', 
                                                   'Please select a folder',
                                                   FALSE), # Cambiar la dirección de TrLab
                                    verbatimTextOutput("resetFolder"),
                                    titlePanel("About"), 
                                    div(includeMarkdown("readme.md"), 
                                        align="justify"),
                                    ) # tabPanel "About"
                ) # navbarPage
) # fluidPage

shinyUI(ui)