
render_report <- function(input, output, params) {
  # Sys.getenv("RSTUDIO_PANDOC")
  # This gets the path of the pandoc enviroment that it´s needed to render the PDF reports

  rmarkdown::render(input,
                    output_file = output,
                    params = params,
                    envir = new.env(parent = globalenv())
  )
}

server <- function(input, output, session) {

  TrLab_dir <- load_Trlab_dir()  # Loads the directory where TrLab is installed
  
  volumes = getVolumes()() # this makes the directory at the base of your computer.
  
  observe({
    shinyDirChoose(input, 'folder', roots=volumes, filetypes=c('', 'txt'))
    folder <- parseDirPath(roots = volumes, input$folder)
    params <- list(path = gsub("/", "//" ,toString(folder)))
    
    if(gsub("/", "//" ,toString(folder)) != "") {
      
      if("coordinates" %in% list.files(folder)){
        id <- showNotification("Rendering preview...", duration = NULL, closeButton = FALSE)
        on.exit(removeNotification(id), add = TRUE)

        rmarkdown::render('report_template.Rmd',
                          output_file = "html_preview.html",
                          output_format = "html_document",
                          params = params,
                          envir = new.env(parent = globalenv()))
        output$html_preview <- renderUI({includeHTML("html_preview.html")})
      } else {
        output$folderError <- renderText({
          paste("The directory: ",
                folder, " is not a TrLab solution folder.", sep = "")})
      }
    }
    
    output$downloadReport <- downloadHandler(
      filename = "my-report.pdf",
      content = function(file) {
        # Creates a notification that shows when a pdf is being rendered
        id <- showNotification("Rendering report...", duration = NULL, closeButton = FALSE)
        on.exit(removeNotification(id), add = TRUE)
        report_path <- tempfile(fileext = ".Rmd")
        file.copy("report_template.Rmd", report_path, overwrite = TRUE)
        out <- callr::r(
          render_report,
          list(input = 'report_template.Rmd', output = file, params = params)
        )
        file.rename(out, file) # Asks the user where he wants to keep the report
      } # content
    ) # downloadHandler
  })
  
  # If it's the first time the app is executed the user needs to select the directory 
  # where TrLab is located
  output$dir_button <- renderUI(
    # We only show the button if the user never chose a directory before
    expr = if (is.null(TrLab_dir)) {
      shinyDirButton("inst_dir", "Specify the TrLab Installation folder" ,
                     title = "Please select a folder:", multiple = FALSE,
                     buttonType = "default", class = NULL)
    }) # renderUI

  observeEvent(input$inst_dir , { 
    shinyDirChoose(input, "inst_dir", roots = volumes)
    folder <- parseDirPath(roots = volumes, input$inst_dir)
    if(gsub("/", "//" ,toString(folder)) != "") {
      save_Trlab_dir(folder)
      output$trlab_stored_dir <- renderText({
        paste("TrLab path indicated is: ",
              folder, ".\n\nRestart the application to save changes.", sep = "")})
    }
  }) # observeEvent
  
  
  # Linking the slider and numeric imput
  observeEvent(input$slider, {
    updateNumericInput(session, "numeric", value = input$slider)
  })
  observeEvent(input$numeric, {
    updateSliderInput(session, "slider", value = input$numeric)
  })
  
  # Render the table of searches
  observeEvent(input$button_1, {
    
    ids <- id_obtainer(term = input$terms,rtmx = input$slider)
    summary <- entrez_summary(db="gds",id=ids)
    info <- create_search_table(summary)
    colnames(info) <- c("Accession", "Description", "Release date","Organism","Samples Number")
    info[["Select"]] <- paste0('<input type="checkbox" name="row_selected" value=',1:nrow(info),' checked>')
    info[["_id"]] <- paste0("row_", seq(nrow(info)))
    
    callback <- c(
      sprintf("table.on('click', 'td:nth-child(%d)', function(){", 
              which(names(info) == "Select")),
      "  var checkbox = $(this).children()[0];",
      "  var $row = $(this).closest('tr');",
      "  if(checkbox.checked){",
      "    $row.removeClass('excluded');",
      "  }else{",
      "    $row.addClass('excluded');",
      "  }",
      "  var excludedRows = [];",
      "  table.$('tr').each(function(i, row){",
      "    if($(this).hasClass('excluded')){",
      "      excludedRows.push(parseInt($(row).attr('id').split('_')[1]));",
      "    }",
      "  });",
      "  Shiny.setInputValue('excludedRows', excludedRows);",
      "});"
    )

    output$myDT <- renderDT({
      
      datatable(
        info, selection = "multiple",
        options = list(pageLength = 15,
                       lengthChange = FALSE,
                       rowId = JS(sprintf("function(data){return data[%d];}", 
                                          ncol(info)-1)),
                       columnDefs = list( # hide the '_id' column
                         list(visible = FALSE, targets = ncol(info)-1)
                       )
        ),
        rownames = FALSE,
        escape = FALSE,
        callback = JS(callback)
      )
    }, server = FALSE)
  }) # observeEvent
  
  # Download button of datasets
  observeEvent(input$button_2, {
    cwd <- getwd()
    ids <- id_obtainer(term = input$terms,rtmx = input$slider)
    if(is.null(input[["excludedRows"]])){
      setwd(TrLab_dir)
      getTrlab_items(ids)
      setwd(cwd)
    } else {
      setwd(TrLab_dir)
      getTrlab_items(ids[input[["excludedRows"]]])
      setwd(cwd)
    }
    setwd(cwd)
    output$dwnload_end <- renderText({
      isolate("The DataSets have been downloaded")
      }) # renderText
  })#observeEvent - button_2
  
  ### FUNCTIONS FOR THE ABOUT TAB ###
  
  observeEvent(input$button_3, {
    shinyDirChoose(input, 'button_3', roots=volumes)
    folder <- parseDirPath(roots = volumes, input$button_3)
    if(gsub("/", "//" ,toString(folder)) != "") {
      save_Trlab_dir(folder)
      output$resetFolder <- renderText({
        paste("TrLab path changed to: ",
              folder, ".\n\nReinicie la aplicación para guardar los cambios.", sep = "")})
    }
  })
  
  # Closes the app when the session is closed
  session$onSessionEnded(function() {
    if("html_preview.html" %in% list.files()){
      file.remove("html_preview.html")
    }
    stopApp()
  })

} # server

shinyServer(server)