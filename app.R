# General parameters
# cssFile <- "www/global.css"
defaultTheme <- "spacelab"
htmlHeaderFile <- "interactive_scatterplot_explorer.html"
labelsFile <- "interactive_scatterplot_explorer.labels.txt"
nRowsDisplayedInFilePreview <- 20
nStepsInScaleSettings <- 10
availableColumnSeparatorsDisplay <- c("tabulation (\\t)", "espace", "virgule (,)" ,"point-virgule (;)")
availableColumnSeparators <- c("\t", " ", ",", ";")
availableEncodingsDisplay <- c("Unicode (UTF-8)","Windows (latin-1)", "Windows (latin-9)", "Mac OS (MacRoman)", "inconnu")
availableEncodings <- c("UTF-8","latin-1", "latin-9", "macintosh", "unknown")
# availableOutputFormats <- c("PNG","PDF", "EPS")
# availableOutputFormatExtensions <- c(".png", ".pdf", ".eps")
availableOutputFormats <- c("PNG","PDF", "EPS", "EMF")
availableOutputFormatExtensions <- c(".png", ".pdf", ".eps", ".emf")
# availableOutputFormats <- c("PNG","PDF", "SVG", "EPS")
# availableOutputFormatExtensions <- c(".png", ".pdf", ".svg", ".eps")
availableDataFileOutputFormats = c("XLSX", "TSV", "CSV")
availableDataFileOutputFormatExtensions = c(".xlsx", ".tsv", ".csv")
availableUnitsForImageExport <- c("cm","in","mm")
# textSize <- 14
# textSizeAxes <- 12
exportedImagesUnit <- "cm"
defaultExportedPlotWidth <- 30
defaultExportedPlotHeight <- 20
figureDisplayRefreshRateMilliseconds <- 200
maxUploadSizeMB <- 200
scatterplotPointsDefaultColor <- "#000000"
scatterplotPointsDefaultShape <- 16
enableExcelCompatibility <- TRUE
enableEMFformatExport <- TRUE
defaultPointsSize <- 2
defaultRegressionLinesSize <- 1
defaultFigureTextSize <- 16

# Load required libraries
library(tools)
library(shiny)
library(shinythemes)
# library(shinyjs)
library(tidyverse)
library(gginnards)
library(rhandsontable)
library(DT)
library(Cairo)
if(enableExcelCompatibility)
  library(readxl)
if(enableEMFformatExport)
  library(devEMF)
library(writexl)

getLabelOrPrompt <- function(code, labelsAndPromptsReference) {
  selectedRow <- labelsAndPromptsReference[labelsAndPromptsReference$entry==code,]
  if(nrow(selectedRow)>0) {
    return(selectedRow$displayedLabel[1])
  } else {
    return("-- unknown label or prompt --")
  }
}
displayedLabelsAndPrompts <- read.table(file = labelsFile, sep = "\t", header = T, encoding = "UTF-8", quote = "", stringsAsFactors = F)

myApp <- shinyApp(
  # UI side of the app: define layout
  ui = fluidPage(
    theme = shinytheme(defaultTheme),
    # useShinyjs(),
    # includeCSS(cssFile),
    # Page title
    titlePanel("", windowTitle = getLabelOrPrompt("windowTitle", displayedLabelsAndPrompts)),
    # General HTML description on top of the page (loaded from an external headerless HTML file)
    htmlOutput("introText"),
    # Data file selection
    fileInput(
      "datafile",
      getLabelOrPrompt("datafileInputLabel", displayedLabelsAndPrompts),
      accept=c(
        'text/csv',
        'text/tsv',
        'text/tab-separated-values,text/plain',
        'application/vnd.ms-excel',
        # 'application/vnd.oasis.opendocument.spreadsheet',
        'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'
      ),
      buttonLabel = getLabelOrPrompt("browseInputFileLabel", displayedLabelsAndPrompts),
      placeholder = getLabelOrPrompt("noFileSelectedPrompt", displayedLabelsAndPrompts)
    ),
    # Panel with controls for variables and filter selection displayed only when a data file is loaded
    conditionalPanel(
      condition="output.fileUploaded",
      p(getLabelOrPrompt("generalInstructionsOnTabDisplay", displayedLabelsAndPrompts)),
      tabsetPanel(
        id = "tabsetPanelID",
        tabPanel(getLabelOrPrompt("variablesChoiceTabLabel", displayedLabelsAndPrompts), 
          uiOutput("nonNumericColsIncludedInDependentVariableChoiceFlag"),
          # These column selectors are dynamically created when the file is loaded
          fluidRow(
            column(3, uiOutput("depVarX"), align="center"),
            column(3, uiOutput("depVarY"), align="center"),
            column(2, uiOutput("exchangeXYvariables", align="center", style = "margin-top: 25px;"))
          ),
          value = "variablesChoiceTabLabel"
        ),
        tabPanel(getLabelOrPrompt("dataFilteringTabLabel", displayedLabelsAndPrompts), 
                 # These column selectors are dynamically created when the file is loaded
                 fluidRow(
                   column(12, DTOutput("filteringDT"))
                   # column(3, uiOutput("filterVar"), align="center"),
                   # column(4, rHandsontableOutput("filterValTable", height = 120)),
                   # column(2, 
                   #        uiOutput("selectAllButton", align="center"),
                   #        uiOutput("unselectAllButton", align="center"),
                   #        uiOutput("revertSelectionButton", align="center")
                   # )
                 ),
                 value = "dataFilteringTabLabel"
        ),
        tabPanel(getLabelOrPrompt("independentVariable1ChoiceTabLabel", displayedLabelsAndPrompts),
          fluidRow(
            column(3, uiOutput("independentVar1"), align="center"),
            column(4, rHandsontableOutput("indepVar1Table", height = 120)),
            column(2,
                   uiOutput("selectAllIndepVar1Button", align="center"),
                   uiOutput("unselectAllIndepVar1Button", align="center"),
                   uiOutput("revertSelectionIndepVar1Button", align="center")
            )
          ),
          value = "independentVariable1ChoiceTabLabel"
        ),
        tabPanel(getLabelOrPrompt("independentVariable2ChoiceTabLabel", displayedLabelsAndPrompts),
                 fluidRow(
                   column(3, uiOutput("independentVar2"), align="center"),
                   column(4, rHandsontableOutput("indepVar2Table", height = 120)),
                   column(2,
                          uiOutput("selectAllIndepVar2Button", align="center"),
                          uiOutput("unselectAllIndepVar2Button", align="center"),
                          uiOutput("revertSelectionIndepVar2Button", align="center")
                   )
                 ),
                 value = "independentVariable2ChoiceTabLabel"
        ),
        tabPanel(getLabelOrPrompt("generalDisplayParametersTabLabel", displayedLabelsAndPrompts),
                 # Panel displayed only when a dependent variable is set, and there is at least 2 data points to be displayed
                 conditionalPanel(
                   condition="output.dependentVariableSelected",
                   # Numeric inputs for limits values, and dropdown menu to set the mode for limits adjustment to auto or manual
                   fluidRow(
                     column(2, numericInput("scatterplotLowLimitX", getLabelOrPrompt("scatterplotLowLimitXLabel", displayedLabelsAndPrompts),NA), align="center"),
                     column(2, numericInput("scatterplotHighLimitX", getLabelOrPrompt("scatterplotHighLimitXLabel", displayedLabelsAndPrompts),NA), align="center"),
                     column(3, uiOutput("scatterplotLimitsMode"), align="center"),
                     column(5, sliderInput(inputId = "pointsSize",
                                 label = getLabelOrPrompt("pointsSizeSliderInputLabel", displayedLabelsAndPrompts),
                                 min = 1,
                                 max = 10,
                                 step = 0.5,
                                 value = defaultPointsSize,
                                 width="100%")
                     )
                   ),
                   fluidRow(
                     column(2, numericInput("scatterplotLowLimitY", getLabelOrPrompt("scatterplotLowLimitYLabel", displayedLabelsAndPrompts),NA), align="center"),
                     column(2, numericInput("scatterplotHighLimitY", getLabelOrPrompt("scatterplotHighLimitYLabel", displayedLabelsAndPrompts),NA), align="center"),
                     column(3, strong(getLabelOrPrompt("reverseAxesLabel", displayedLabelsAndPrompts)), style = "margin-top: 25px;"),
                     column(1, uiOutput("reverseXaxis"), align="center", style = "margin-top: 25px;"),
                     column(1, uiOutput("reverseYaxis"), align="center", style = "margin-top: 25px;")
                   ),
                   fluidRow(
                     column(4, sliderInput(inputId = "figureTextSize",
                                           label = getLabelOrPrompt("figureTextSizeSliderInputLabel", displayedLabelsAndPrompts),
                                           min = 6,
                                           max = 40,
                                           step = 0.5,
                                           value = defaultFigureTextSize,
                                           width="100%")
                     ),
                     column(4, checkboxInput("displayRegressionLinesCheckbox", getLabelOrPrompt("toggleRegressionLinesDisplayLabel", displayedLabelsAndPrompts)), align="center"),
                     column(4, sliderInput(inputId = "regressionLinesSize",
                                           label = getLabelOrPrompt("regressionLinesSizeSliderInputLabel", displayedLabelsAndPrompts),
                                           min = 0.2,
                                           max = 3,
                                           step = 0.2,
                                           value = defaultRegressionLinesSize,
                                           width="100%")
                     )
                   )
                 ),
                 value = "generalDisplayParametersTabLabel"
        ),
        tabPanel(getLabelOrPrompt("descriptiveStatisticsTabLabel", displayedLabelsAndPrompts),
           # Panel displayed only when a dependent variable is set, and there is at least 2 data points to be displayed
           conditionalPanel(
             condition="output.dependentVariableSelected",
             htmlOutput("descriptiveStatisticsInfo"),
             fluidRow(
               # Numeric input: let the user adjust the number of decimals displayed
               # Label display separately to have it on the same line
               column(6, p(strong(getLabelOrPrompt("nDecimalPlacesInputLabel", displayedLabelsAndPrompts)))),
               column(1, p(strong("X"))),
               column(2, numericInput("nDecimalPlacesX", NULL, NA, min = 0, step = 1)),
               column(1, p(strong("Y"))),
               column(2, numericInput("nDecimalPlacesY", NULL, NA, min = 0, step = 1))
             )
           ),
           value = "descriptiveStatisticsTabLabel"
        ),
        tabPanel(getLabelOrPrompt("correlationsTabLabel", displayedLabelsAndPrompts),
           # Panel displayed only when a dependent variable is set, and there is at least 2 data points to be displayed
           conditionalPanel(
             condition="output.dependentVariableSelected",
             fluidRow(
               rHandsontableOutput("correlationsTable", height = 120)
             #   column(10, rHandsontableOutput("correlationsTable", height = 120)),
             #   column(2, downloadButton("downloadCorrelationsTable", label = getLabelOrPrompt("correlationsTabDownloadButtonLabel", displayedLabelsAndPrompts)), align="center", style = "margin-top: 25px;")
             ),
             fluidRow(
               column(12, downloadButton("downloadCorrelationsTable", label = getLabelOrPrompt("correlationsTabDownloadButtonLabel", displayedLabelsAndPrompts)), align="center")
             )
           ),
           value = "correlationsTabLabel"
        ),
        tabPanel(getLabelOrPrompt("figureExportTabLabel", displayedLabelsAndPrompts),
           # Panel displayed only when a dependent variable is set, and there is at least 2 data points to be displayed
           conditionalPanel(
             condition="output.dependentVariableSelected",
             # Controls for figure export as image
             fluidRow(
               column(3, uiOutput("plotDownloadFormat"), align="center"),
               column(2, uiOutput("exportedPlotWidth"), align="center"),
               column(2, uiOutput("exportedPlotHeight"), align="center"),
               column(2, uiOutput("exportedPlotUnits"), align="center") #,
               # column(2, downloadButton("downloadPlot", label = getLabelOrPrompt("figureDownloadButtonLabel", displayedLabelsAndPrompts)), align="center", style = "margin-top: 25px;")
             ),
             fluidRow(
               column(12, downloadButton("downloadPlot", label = getLabelOrPrompt("figureDownloadButtonLabel", displayedLabelsAndPrompts)), align="center")
             )
           ),
           value = "figureExportTabLabel"
        )
      ),
      conditionalPanel(
        condition="output.dependentVariableSelected",
        # Scatterplot display area
        plotOutput("plotScatter", click = "plot_click", brush = "plot_brush"),
        # Checkbox to let the user activate display of selected bin detail in a table
        uiOutput("displaySelectedDataDetailInTableFlag"),
        # Table for the display of selected points detail
        rHandsontableOutput("selectedDataDetailsTable", height = 150)
      ),
      # Conditional panel to let the the user export selected data
      conditionalPanel(
        condition="output.selectedDataDetailsDisplayed",
        fluidRow(
          column(3, htmlOutput("selectedDataDownloadInfo"), align="right", style = "margin-top: 25px;"),
          column(2, uiOutput("selectedDataDownloadFormat"), align="center"),
          column(2, downloadButton("downloadSelectedData"), align="left", style = "margin-top: 25px;")
        )
      )
    )
  ),
  
  # Server side of the app: define behaviour
  server = function(input, output, session) {
    # Set the file upload limit
    base::options(shiny.maxRequestSize = maxUploadSizeMB * 1024^2)
    
    # Disable all tabs except the first one on page load
    hideTab(inputId = "tabsetPanelID", target = "independentVariable1ChoiceTabLabel")
    hideTab(inputId = "tabsetPanelID", target = "independentVariable2ChoiceTabLabel")
    # hideTab(inputId = "tabsetPanelID", target = "dataFilteringTabLabel")
    hideTab(inputId = "tabsetPanelID", target = "generalDisplayParametersTabLabel")
    hideTab(inputId = "tabsetPanelID", target = "descriptiveStatisticsTabLabel")
    hideTab(inputId = "tabsetPanelID", target = "correlationsTabLabel")
    hideTab(inputId = "tabsetPanelID", target = "figureExportTabLabel")
    
    # Define a list of reactive values to store various parameters
    values <- reactiveValues(
       selectedFile = NULL,
       loadedFileType = NULL,
       columnSeparator = "\t",
       selectedFileEncoding = "UTF-8",
       naStringsFileImport = "",
       availableSheetNames = NULL,
       selectedSheetIndex = 1,
       nLinesDisplayedInPreview = nRowsDisplayedInFilePreview,
       associatedMessage = NULL,
       filedata = NULL,
       filedataColnames = NULL,
       filteredFiledata = NULL,
       scatterplotData = NULL,
       currentDatasetScatterplot = NULL,
       clickedSubset = NULL,
       selectedRowInClickedSubset = NULL,
       # filterVariable = NULL,
       # filterValues = NULL,
       # NAinFilterValues = FALSE,
       nonNumericColsIncludedInDependentVariableChoiceFlag = FALSE,
       dependentVariableX = NULL,
       dependentVariableY = NULL,
       independentVariable1 = "\u00A0",
       independentVariable2 = "\u00A0",
       indepVar1Values = NULL,
       indepVar2Values = NULL,
       NAinIndepVar1Values = FALSE,
       NAinIndepVar2Values = FALSE,
       scatterplotLimitsMode = getLabelOrPrompt("scatterplotLimitsModeChoiceAuto", displayedLabelsAndPrompts),
       scatterplotLowLimitX = NA,
       scatterplotHighLimitX = NA,
       scatterplotLowLimitY = NA,
       scatterplotHighLimitY = NA,
       pointsSize = defaultPointsSize,
       displayRegressionLines = FALSE,
       displaySelectedDataDetailInTableFlag = 0,
       figureTextSize = defaultFigureTextSize,
       regressionLinesSize = defaultRegressionLinesSize,
       reverseXaxis = FALSE,
       reverseYaxis = FALSE,
       nDecimalPlacesX = 0,
       nDecimalPlacesY = 0,
       descriptiveStatisticsInfoText = "",
       lastParameterChangeTime = Sys.time()
      )
    
    # Dialog box displayed when a text file is loaded to let the user choose the column separator
    displayLoadedFilePreviewDialog <- function(selectedFile, defaultMainParameterValue) {
      output$inputFilePreview <- renderTable(NULL)
      output$inputFilePreviewMessage <- renderText(NULL)
      output$loadedFilePreviewMainParameterSelector <- renderUI({
        if(values$loadedFileType == "txt")
          selectInput("loadedFilePreviewMainParameterSelector", getLabelOrPrompt("columnSeparatorSelectorLabel", displayedLabelsAndPrompts), availableColumnSeparatorsDisplay, selected = defaultMainParameterValue)
        else {
          if(values$loadedFileType == "xlsx") {
            # wb <- loadWorkbook(selectedFile)
            # values$availableSheetNames <- names(getSheets(wb))
            values$availableSheetNames <- excel_sheets(selectedFile)
          # } else if(values$loadedFileType == "xls") {
          #   values$availableSheetNames <- sheetNames(selectedFile)
          # } else if(values$loadedFileType == "ods") {
          #   values$availableSheetNames <- ods_sheets(selectedFile)
          } else
            values$availableSheetNames <- NULL

          selectInput("loadedFilePreviewMainParameterSelector", getLabelOrPrompt("sheetSelectorLabel", displayedLabelsAndPrompts), values$availableSheetNames)
        }
      })
      output$fileEncoding <- renderUI({
        selectInput("fileEncoding", getLabelOrPrompt("fileEncodingLabel", displayedLabelsAndPrompts),availableEncodingsDisplay)
      })
      output$naStringsFileImport <- renderUI({
        textAreaInput("naStringsFileImport", getLabelOrPrompt("naStringsFileImportLabel", displayedLabelsAndPrompts), "", rows = 3)
      })
      
      # JS code to bind return key (code 13) with validate button and escape (code 27) with cancel button
      jsCodeBindKeysWithPreviewModalButtons <- '
          $(document).keyup(function(event) {
            if (event.keyCode == 13) {
                $("#validateLoadedFilePreviewButton").click();
            } else if (event.keyCode == 27) {
                $("#cancelLoadedFilePreviewButton").click();
            }
          });
          '

      showModal(
        modalDialog(
          tags$script(HTML(jsCodeBindKeysWithPreviewModalButtons)),
          size = "l",
          # includeCSS(cssFile),
          fluidRow(
            column(6, uiOutput("loadedFilePreviewMainParameterSelector")),
            column(6, uiOutput("fileEncoding"))
          ),
          fluidRow(
            column(6, numericInput("nLinesDisplayedInPreview", label = getLabelOrPrompt("nLinesDisplayedInPreviewLabel", displayedLabelsAndPrompts), value = nRowsDisplayedInFilePreview, min = 5, step = 5)),
            column(6, uiOutput("naStringsFileImport"))
          ),
          htmlOutput("inputFilePreviewMessage"),
          tableOutput("inputFilePreview"),
          title = getLabelOrPrompt("loadedFilePreviewDialogLabel", displayedLabelsAndPrompts),
          easyClose = FALSE,
          footer = tagList(
            actionButton("cancelLoadedFilePreviewButton", getLabelOrPrompt("filePreviewCancelButtonLabel", displayedLabelsAndPrompts)),
            actionButton("validateLoadedFilePreviewButton", getLabelOrPrompt("filePreviewValidateButtonLabel", displayedLabelsAndPrompts))
          )
        )
      )
      
      updateLoadedTextFilePreviewDisplay()
    }
    
    # Update the data file preview
    updateLoadedTextFilePreviewDisplay <- function() {
      df = loadDataFile(values$nLinesDisplayedInPreview)
      if(!is.null(df)) {
        output$inputFilePreview <- renderTable(
          df,
          spacing = 'xs',
          bordered = TRUE,
          striped = TRUE,
          align = "c",
          na = ""
        )
      } else {
        output$inputFilePreview <- renderTable(NULL)
      }
      if(!is.null(values$associatedMessage)) {
        output$inputFilePreviewMessage <- renderText(HTML(paste0("<p>", values$associatedMessage, "</p>")))
      } else
        output$inputFilePreviewMessage <- renderText(NULL)
    }
    
    # Load the data file as an Excel spreadsheet or as a text file
    # Return a data structure with the resulting data frame (or NULL in case of failure) and an optional error or warning message
    loadDataFile <- function(nLoadedRows) {
      values$associatedMessage = NULL
      df <- tryCatch({
          withCallingHandlers({
          if(values$loadedFileType == "xlsx") {
            if(is.null(nLoadedRows))
              # extractedRowIndices = NULL
              nMaxRows <- Inf
            else  
              # extractedRowIndices = 1:(nLoadedRows+1)
              nMaxRows <- nLoadedRows+1
            # df = read.xlsx(
            #   values$selectedFile,
            #   values$selectedSheetIndex,
            #   encoding = values$selectedFileEncoding,
            #   check.names = F,
            #   rowIndex = extractedRowIndices
            # )
            df <- read_excel(
              path = values$selectedFile,
              sheet = values$selectedSheetIndex,
              na = values$naStringsFileImport,
              n_max = nMaxRows
            )
          # } else if(values$loadedFileType == "ods") {
          #   # if(is.null(nLoadedRows))
          #   #   extractedRowIndices = NULL
          #   # else  
          #   #   extractedRowIndices =  paste0("R1:R",nLoadedRows)
          #   df = read_ods(
          #     values$selectedFile,
          #     values$selectedSheetIndex
          #     #range = extractedRowIndices
          #     )
          } else if(values$loadedFileType == "txt") {
            if(is.null(nLoadedRows))
              nLoadedRows = -1
              df = read.table(
                values$selectedFile,
                sep=values$columnSeparator,
                header=T,
                fileEncoding = values$selectedFileEncoding,
                na.strings = values$naStringsFileImport,
                check.names = F,
                nrows = nLoadedRows,
                comment.char = ""
              )
          } else {
            df = NULL
          }
        }, warning = function(war) {
          values$associatedMessage = getLabelOrPrompt("loadedFilePreviewWarningMessage", displayedLabelsAndPrompts)
          invokeRestart("muffleWarning")
        })
      }, error = function(err) {
        df = NULL
        values$associatedMessage = getLabelOrPrompt("loadedFilePreviewErrorMessage", displayedLabelsAndPrompts)
      })
      if(!is.data.frame(df))
        df = NULL
      return(df)
    }
    
    # Event observers for the column separator selection dialog
    observeEvent(input$loadedFilePreviewMainParameterSelector, {
      if(!is.null(input$loadedFilePreviewMainParameterSelector)) {
        if(values$loadedFileType == "txt" && (values$columnSeparator != availableColumnSeparators[which(availableColumnSeparatorsDisplay == input$loadedFilePreviewMainParameterSelector)])) {
          values$columnSeparator = availableColumnSeparators[which(availableColumnSeparatorsDisplay == input$loadedFilePreviewMainParameterSelector)]
          updateLoadedTextFilePreviewDisplay()
        }
        else if(values$loadedFileType != "txt" && !is.null(values$availableSheetNames) && (values$selectedSheetIndex != which(input$loadedFilePreviewMainParameterSelector == values$availableSheetNames))) {
          values$selectedSheetIndex = which(input$loadedFilePreviewMainParameterSelector == values$availableSheetNames)
          updateLoadedTextFilePreviewDisplay()
        }
      }
    })
    observeEvent(input$nLinesDisplayedInPreview, {
      if(values$nLinesDisplayedInPreview != input$nLinesDisplayedInPreview) {
        values$nLinesDisplayedInPreview = input$nLinesDisplayedInPreview
        updateLoadedTextFilePreviewDisplay()
      }
    })
    observeEvent(input$fileEncoding, {
      if(!is.null(input$fileEncoding)) {
        if(values$selectedFileEncoding != availableEncodings[which(availableEncodingsDisplay == input$fileEncoding)]) {
          values$selectedFileEncoding = availableEncodings[which(availableEncodingsDisplay == input$fileEncoding)]
          updateLoadedTextFilePreviewDisplay()
        }
      }
    })
    observeEvent(input$naStringsFileImport, {
      if(!is.null(input$naStringsFileImport)) {
        naStringsRaw <- input$naStringsFileImport
        naStringsFileImportTmp <- str_split(naStringsRaw, pattern = "\\n", simplify = T)
        if(!"" %in% naStringsFileImportTmp) {
          naStringsFileImportTmp <- c("", naStringsFileImportTmp)
        }
        values$naStringsFileImport <- naStringsFileImportTmp
        updateLoadedTextFilePreviewDisplay()
      }
    })
    observeEvent(input$validateLoadedFilePreviewButton, {
      removeModal()
      df = loadDataFile(NULL)
      postProcessLoadedFile(df)
    })
    observeEvent(input$cancelLoadedFilePreviewButton, {
      removeModal()
    })
    
    # Load the selected file
    observeEvent(input$datafile, {
      values$filedata <- NULL
      values$filedataColnames <- NULL
      
      infile <- input$datafile
      if (!is.null(infile)) {
        values$selectedFile = infile$datapath
        # Adapt loading method according to the file extension
        inputFileExtension = file_ext(infile$datapath)
        
        if(inputFileExtension == "xlsx" || inputFileExtension == "XLSX" || inputFileExtension == "xls" || inputFileExtension == "XLS") {
          
          values$loadedFileType = "xlsx"
          displayLoadedFilePreviewDialog(values$selectedFile, NULL)

        # } else if(inputFileExtension == "ods" || inputFileExtension == "ODS") {
        #   
        #   values$loadedFileType = "ods"
        #   displayLoadedFilePreviewDialog(values$selectedFile, NULL)

        } else if(inputFileExtension == "csv" || inputFileExtension == "CSV") {

          values$loadedFileType = "txt"
          displayLoadedFilePreviewDialog(values$selectedFile, ",")
          
        } else {

          values$loadedFileType = "txt"
          displayLoadedFilePreviewDialog(values$selectedFile, "\t")
          
        }
      }
    })
    
    postProcessLoadedFile <- function(df) {
      if(!is.null(df)) {
        # If some columns have no variable name, discard them
        dfColnames = colnames(df)
        values$filedataColnames = dfColnames
        emptyColnamesIndices = which(dfColnames == "")
        if(length(emptyColnamesIndices)>0) {
          df <- df[,-emptyColnamesIndices]
        }
        values$filedata <- df
        # Get the column names
        values$filedataColnames = colnames(df)
        
        # Set back most reactive values to default (keep the value of data-independent parameters)
        values$scatterplotData <- NULL
        values$currentDatasetScatterplot <- NULL
        values$clickedSubset <- NULL
        values$selectedRowInClickedSubset <- NULL
        # values$filterVariable <- NULL
        # values$filterValues <- NULL
        # values$NAinFilterValues <- FALSE
        values$dependentVariableX <- NULL
        values$dependentVariableY <- NULL
        values$independentVariable1 <- "\u00A0"
        values$indepVar1Values <- NULL
        values$NAinIndepVar1Values <- FALSE
        values$independentVariable2 <- "\u00A0"
        values$indepVar2Values <- NULL
        values$NAinIndepVar2Values <- FALSE
        values$nDecimalPlacesX = 0
        values$nDecimalPlacesY = 0
        values$descriptiveStatisticsInfoText = ""
        values$filteredFiledata = values$filedata
        values$lastParameterChangeTime = Sys.time()
        
        # Update filtering table
        output$filteringDT <- DT::renderDataTable(
          values$filedata %>% 
            mutate(across(where(is.character), as.factor)),
          filter = "top",
          options = list(
            sDom = '<"top">t<"bottom">lip', # which controls should be shown and where
            pageLength = 5,
            autoWidth = TRUE,
            language = list(
                emptyTable = getLabelOrPrompt("filteringDTdisplay_emptyTable", displayedLabelsAndPrompts),
                info = getLabelOrPrompt("filteringDTdisplay_info", displayedLabelsAndPrompts),
                infoFiltered = getLabelOrPrompt("filteringDTdisplay_infoFiltered", displayedLabelsAndPrompts),
                lengthMenu = getLabelOrPrompt("filteringDTdisplay_lengthMenu", displayedLabelsAndPrompts),
                zeroRecords = getLabelOrPrompt("filteringDTdisplay_zeroRecords", displayedLabelsAndPrompts),
                paginate = list(
                  first = getLabelOrPrompt("filteringDTdisplay_first", displayedLabelsAndPrompts),
                  last = getLabelOrPrompt("filteringDTdisplay_last", displayedLabelsAndPrompts),
                  `next` = getLabelOrPrompt("filteringDTdisplay_next", displayedLabelsAndPrompts),
                  previous = getLabelOrPrompt("filteringDTdisplay_previous", displayedLabelsAndPrompts)
                )
            )
          ),
          rownames = F,
          escape = T,
          selection = 'none'
        )
        
        # Disable all tabs except the first one
        hideTab(inputId = "tabsetPanelID", target = "independentVariable1ChoiceTabLabel")
        hideTab(inputId = "tabsetPanelID", target = "independentVariable2ChoiceTabLabel")
        # hideTab(inputId = "tabsetPanelID", target = "dataFilteringTabLabel")
        hideTab(inputId = "tabsetPanelID", target = "generalDisplayParametersTabLabel")
        hideTab(inputId = "tabsetPanelID", target = "descriptiveStatisticsTabLabel")
        hideTab(inputId = "tabsetPanelID", target = "correlationsTabLabel")
        hideTab(inputId = "tabsetPanelID", target = "figureExportTabLabel")
      }
    }
    
    observeEvent(input$filteringDT_rows_all, {
      if(!is.null(values$filedata)) {
        values$filteredFiledata <- values$filedata %>% 
          slice(input$filteringDT_rows_all)
        # Update data and display
        values$scatterplotData <- getScatterplotData()
        values$currentDatasetScatterplot <- plottedFigure()
        output$plotScatter <- renderPlot({values$currentDatasetScatterplot})
        values$descriptiveStatisticsInfoText <- descriptiveStatText()
        output$descriptiveStatisticsInfo <- renderUI({HTML(values$descriptiveStatisticsInfoText)})
        updateCorrelationsTable()
      } else {
        values$filteredFiledata <- NULL
      }
    })
    
    # Function to apply the current filter (if any) to the full dataset
    filterData <- function(){
      # df <- values$filedata
      df <- values$filteredFiledata
      if (is.null(df)) {
        return(NULL)
      }
      # independent variable 1
      if(!is.null(values$independentVariable1) && values$independentVariable1!="\u00A0") {
        if(!is.null(values$indepVar1Values)) {
          # Apply filter
          if(length(values$indepVar1Values) > 0) {
            df <- df %>%
              filter(!!sym(values$independentVariable1) %in% values$indepVar1Values)
            # If empty cells (NA values when file read as .xlsx or .ods) are part of the filter, make sure to include them
            if(values$NAinIndepVar1Values) {
              df <- df %>% rbind(df %>% filter(is.na(!!sym(values$independentVariable1))))
            }
          } else {
            return(NULL)
          }
        } else {
          return(NULL)
        }
      }
      # independent variable 2
      if(!is.null(values$independentVariable2) && values$independentVariable2!="\u00A0" && (is.null(values$independentVariable1) || values$independentVariable1!=values$independentVariable2)) {
        if(!is.null(values$indepVar2Values)) {
          # Apply filter
          if(length(values$indepVar2Values) > 0) {
            df <- df %>%
              filter(!!sym(values$independentVariable2) %in% values$indepVar2Values)
            # If empty cells (NA values when file read as .xlsx or .ods) are part of the filter, make sure to include them
            if(values$NAinIndepVar2Values) {
              df <- df %>% rbind(df %>% filter(is.na(!!sym(values$independentVariable2))))
            }
          } else {
            return(NULL)
          }
        } else {
          return(NULL)
        }
      }
      # return data frame once all filters have been applied
      return(df)
    }
    
    # Detect when a file has been uploaded (required for conditional display of variables selection)
    output$fileUploaded <- reactive({
      return(!is.null(values$filedata))
    })
    outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
    
    # Detect when a valid dependent variable has been selected and there's either no filter set, or a minimum of 
    # 2 data points remaining in the filter (required for conditional display of statistics and figure)
    output$dependentVariableSelected <- reactive({
      # return(!is.null(input$datafile) && !is.null(values$filedata) && !is.null(values$dependentVariableX) && values$dependentVariableX!="\u00A0" && values$dependentVariableX!=0 && !is.null(values$dependentVariableY) && values$dependentVariableY!="\u00A0" && values$dependentVariableY!=0 && (is.null(values$filterValues) || length(values$filterValues)>0) && !is.null(values$scatterplotData) && length(values$scatterplotData)>1)
      return(!is.null(input$datafile) && !is.null(values$filedata) && !is.null(values$dependentVariableX) && values$dependentVariableX!="\u00A0" && values$dependentVariableX!=0 && !is.null(values$dependentVariableY) && values$dependentVariableY!="\u00A0" && values$dependentVariableY!=0 && !is.null(values$scatterplotData) && length(values$scatterplotData)>1)
    })
    outputOptions(output, 'dependentVariableSelected', suspendWhenHidden=FALSE)
    
    output$selectedDataDetailsDisplayed <- reactive({
      return(values$displaySelectedDataDetailInTableFlag!=0 && !is.null(values$clickedSubset) && nrow(values$clickedSubset)>0)
    })
    outputOptions(output, 'selectedDataDetailsDisplayed', suspendWhenHidden=FALSE)
    
    # HTML displayed on top of the page (contents loaded from file)
    output$introText <- renderUI({
      displayedHTML = readLines(htmlHeaderFile)
      HTML(paste(displayedHTML))
    })
    
    # Checkbox: display the details of the values in the clicked bin in a table
    output$displaySelectedDataDetailInTableFlag <- renderUI({
      df <- values$filedata
      if (is.null(df)) return(NULL)
      checkboxInput("displaySelectedDataDetailInTableFlag", getLabelOrPrompt("displaySelectedDataDetailInTableFlagLabel", displayedLabelsAndPrompts), TRUE, width="100%")
    })
    
    # When the flag for clicked bin detail display is changed, update table display
    observeEvent(input$displaySelectedDataDetailInTableFlag, {
      if(values$displaySelectedDataDetailInTableFlag != input$displaySelectedDataDetailInTableFlag) {
        values$lastParameterChangeTime = Sys.time()
        values$displaySelectedDataDetailInTableFlag <- input$displaySelectedDataDetailInTableFlag
        if(values$displaySelectedDataDetailInTableFlag!=0 && !is.null(values$clickedSubset) && nrow(values$clickedSubset)>0) {
          output$selectedDataDetailsTable <- renderRHandsontable({
            rhandsontable(values$clickedSubset, selectCallback = FALSE, readOnly = TRUE, stretchH = "all", rowHeaderWidth = 0) %>%
              hot_cols(columnSorting = TRUE)
          })
        } else {
          output$selectedDataDetailsTable <- renderRHandsontable({NULL})
        }
      }
    })
    
    # Checkbox to force inclusion of variables not detected as numeric in the dependent variable menu
    output$nonNumericColsIncludedInDependentVariableChoiceFlag <- renderUI({
      df <-values$filedata
      if (is.null(df)) return(NULL)
      checkboxInput("nonNumericColsIncludedInDependentVariableChoiceFlag", getLabelOrPrompt("nonNumericColsIncludedInDependentVariableChoiceLabel", displayedLabelsAndPrompts), FALSE, width="100%")
    })
    
    # Update list of available dependent variables when the state of the checkbox for inclusion of non-numeric variables changes
    observeEvent(input$nonNumericColsIncludedInDependentVariableChoiceFlag, {
      if(values$nonNumericColsIncludedInDependentVariableChoiceFlag != input$nonNumericColsIncludedInDependentVariableChoiceFlag) {
        if(!is.null(values$filedata)) {
          values$nonNumericColsIncludedInDependentVariableChoiceFlag = input$nonNumericColsIncludedInDependentVariableChoiceFlag
            if(values$nonNumericColsIncludedInDependentVariableChoiceFlag!=0) {
              items=c("\u00A0",values$filedataColnames)
              names(items)=items
            } else {
              numericColumnIndices <- sapply(values$filedata, is.numeric)
              if(!is.null(numericColumnIndices) && length(numericColumnIndices[numericColumnIndices]) > 0) {
                items=c("\u00A0",names(numericColumnIndices[numericColumnIndices]))
                names(items)=items
              } else {
                items = 0
                names(items) = getLabelOrPrompt("noNumericVariableWarning", displayedLabelsAndPrompts)
              }
            }
            # if(!is.null(values$dependentVariable) && values$dependentVariable!="" && values$dependentVariable %in% items) {
            #   selectedItem = values$dependentVariable
            # } else {
            #   selectedItem = "\u00A0"
            # }
            updateSelectInput(session, "depVarX", label = NULL, choices = items, selected = values$dependentVariableX)
            updateSelectInput(session, "depVarY", label = NULL, choices = items, selected = values$dependentVariableY)
        }
      }
    })
    
    # Dropdown menus: dependent variables (any numeric variable)
    output$depVarX <- renderUI({
      df <-values$filedata
      if (is.null(df)) return(NULL)
      
      # Show only numeric columns, unless the corresponding options is checked
      if(values$nonNumericColsIncludedInDependentVariableChoiceFlag!=0) {
        items=c("\u00A0",values$filedataColnames)
        names(items)=items
      } else {
        numericColumnIndices <- sapply(df, is.numeric)
        if(!is.null(numericColumnIndices) && length(numericColumnIndices[numericColumnIndices]) > 0) {
          items=c("\u00A0",names(numericColumnIndices[numericColumnIndices]))
          names(items)=items
        } else {
          items = 0
          names(items) = getLabelOrPrompt("noNumericVariableWarning", displayedLabelsAndPrompts)
        }
      }
      selectInput("depVarX", getLabelOrPrompt("dependentVariableInputLabelX", displayedLabelsAndPrompts), items, selected = items[1])
    })
    output$depVarY <- renderUI({
      df <-values$filedata
      if (is.null(df)) return(NULL)
      
      # Show only numeric columns, unless the corresponding options is checked
      if(values$nonNumericColsIncludedInDependentVariableChoiceFlag!=0) {
        items=c("\u00A0",values$filedataColnames)
        names(items)=items
      } else {
        numericColumnIndices <- sapply(df, is.numeric)
        if(!is.null(numericColumnIndices) && length(numericColumnIndices[numericColumnIndices]) > 0) {
          items=c("\u00A0",names(numericColumnIndices[numericColumnIndices]))
          names(items)=items
        } else {
          items = 0
          names(items) = getLabelOrPrompt("noNumericVariableWarning", displayedLabelsAndPrompts)
        }
      }
      selectInput("depVarY", getLabelOrPrompt("dependentVariableInputLabelY", displayedLabelsAndPrompts), items, selected = items[1])
    })
    
    # Update variables, plot and table when the dependent variable has changed
    observeEvent(input$depVarX, {
      if(is.null(values$dependentVariable) || values$dependentVariable != input$depVarX) {
        values$lastParameterChangeTime = Sys.time()
        output$selectedDataDetailsTable <- renderRHandsontable({NULL})
        values$dependentVariableX <- input$depVarX
        # if(is.null(values$filterVariable) || (values$filterVariable!="\u00A0" && (values$dependentVariableX==values$filterVariable || values$dependentVariableY==values$filterVariable))) {
        #   values$filterVariable = "\u00A0"
        #   values$filterValTable = NULL
        # }
        if(!is.null(values$dependentVariableX) && values$dependentVariableX!="\u00A0" && values$dependentVariableX!=0  && values$dependentVariableX!="" && !is.null(values$dependentVariableY) && values$dependentVariableY!="\u00A0" && values$dependentVariableY!=0  && values$dependentVariableY!="") {
        # if(!is.null(values$dependentVariableX) && values$dependentVariableX!="\u00A0" && values$dependentVariableX!=0  && values$dependentVariableX!="" && !is.null(values$dependentVariableY) && values$dependentVariableY!="\u00A0" && values$dependentVariableY!=0  && values$dependentVariableY!="" && !is.null(values$filteredFiledata)) {
          values$scatterplotData = getScatterplotData()
          if(!is.null(values$scatterplotData) && nrow(values$scatterplotData) > 0) {
            # Compute the appropriate number of decimal places for rounding from the range in values and update the corresponding input
            # X
            rangeInCurrentDatasetX = max(values$scatterplotData[,values$dependentVariableX]) - min(values$scatterplotData[,values$dependentVariableX])
            nDecimalPlacesX = -(floor(log10(rangeInCurrentDatasetX)) - 3)
            if(nDecimalPlacesX<0) {
              nDecimalPlacesX = 0
            }
            updateNumericInput(session, "nDecimalPlacesX", value = nDecimalPlacesX)
            values$nDecimalPlacesX = nDecimalPlacesX
            # Y
            rangeInCurrentDatasetY = max(values$scatterplotData[,values$dependentVariableY]) - min(values$scatterplotData[,values$dependentVariableY])
            nDecimalPlacesY = -(floor(log10(rangeInCurrentDatasetY)) - 3)
            if(nDecimalPlacesY<0) {
              nDecimalPlacesY = 0
            }
            updateNumericInput(session, "nDecimalPlacesY", value = nDecimalPlacesY)
            values$nDecimalPlacesY = nDecimalPlacesY
            
            # Update figure and descriptive statistics display
            values$descriptiveStatisticsInfoText <- descriptiveStatText()
            output$descriptiveStatisticsInfo <- renderUI({HTML(values$descriptiveStatisticsInfoText)})
            updateCorrelationsTable()
            values$clickedSubset <- NULL
            values$selectedRowInClickedSubset <- NULL
            values$currentDatasetScatterplot <- plottedFigure()
            output$plotScatter <- renderPlot({values$currentDatasetScatterplot})
            # filterChoices = c("\u00A0",values$filedataColnames[-which(values$filedataColnames == values$dependentVariable)])
            if(!is.null(values$scatterplotData) && nrow(values$scatterplotData)>1) {
              # Show hidden tabs
              showTab(inputId = "tabsetPanelID", target = "independentVariable1ChoiceTabLabel")
              showTab(inputId = "tabsetPanelID", target = "independentVariable2ChoiceTabLabel")
              # showTab(inputId = "tabsetPanelID", target = "dataFilteringTabLabel")
              showTab(inputId = "tabsetPanelID", target = "generalDisplayParametersTabLabel")
              showTab(inputId = "tabsetPanelID", target = "descriptiveStatisticsTabLabel")
              showTab(inputId = "tabsetPanelID", target = "correlationsTabLabel")
              showTab(inputId = "tabsetPanelID", target = "figureExportTabLabel")
            }
          } else {
            # filterChoices = c("\u00A0",values$filedataColnames)
          }
        } else {
          # filterChoices = c("\u00A0",values$filedataColnames)
        }
        # names(filterChoices)=filterChoices
        # updateSelectInput(session, "filterVar", getLabelOrPrompt("filterVariableInputLabel", displayedLabelsAndPrompts), choices = filterChoices, selected = values$filterVariable)
      }
    })
    
    observeEvent(input$depVarY, {
      if(is.null(values$dependentVariable) || values$dependentVariable != input$depVarY) {
        values$lastParameterChangeTime = Sys.time()
        output$selectedDataDetailsTable <- renderRHandsontable({NULL})
        values$dependentVariableY <- input$depVarY
        # if(is.null(values$filterVariable) || (values$filterVariable!="\u00A0" && values$dependentVariable==values$filterVariable)) {
        #   values$filterVariable = "\u00A0"
        #   values$filterValTable = NULL
        # }
        if(!is.null(values$dependentVariableY) && values$dependentVariableY!="\u00A0" && values$dependentVariableY!=0  && values$dependentVariableY!="") {
        # if(!is.null(values$dependentVariableY) && values$dependentVariableY!="\u00A0" && values$dependentVariableY!=0  && values$dependentVariableY!="" && !is.null(values$filteredFiledata)) {
          values$scatterplotData = getScatterplotData()
          if(length(values$scatterplotData) > 0) {
            # Compute the appropriate number of decimal places for rounding from the range in values and update the corresponding input
            # X
            rangeInCurrentDatasetX = max(values$scatterplotData[,values$dependentVariableX]) - min(values$scatterplotData[,values$dependentVariableX])
            nDecimalPlacesX = -(floor(log10(rangeInCurrentDatasetX)) - 3)
            if(nDecimalPlacesX<0) {
              nDecimalPlacesX = 0
            }
            updateNumericInput(session, "nDecimalPlacesX", value = nDecimalPlacesX)
            values$nDecimalPlacesX = nDecimalPlacesX
            # Y
            rangeInCurrentDatasetY = max(values$scatterplotData[,values$dependentVariableY]) - min(values$scatterplotData[,values$dependentVariableY])
            nDecimalPlacesY = -(floor(log10(rangeInCurrentDatasetY)) - 3)
            if(nDecimalPlacesY<0) {
              nDecimalPlacesY = 0
            }
            updateNumericInput(session, "nDecimalPlacesY", value = nDecimalPlacesY)
            values$nDecimalPlacesY = nDecimalPlacesY
            
            # Update figure and descriptive statistics display
            values$descriptiveStatisticsInfoText <- descriptiveStatText()
            output$descriptiveStatisticsInfo <- renderUI({HTML(values$descriptiveStatisticsInfoText)})
            updateCorrelationsTable()
            values$clickedSubset <- NULL
            values$selectedRowInClickedSubset <- NULL
            values$currentDatasetScatterplot <- plottedFigure()
            output$plotScatter <- renderPlot({values$currentDatasetScatterplot})
            # filterChoices = c("\u00A0",values$filedataColnames[-which(values$filedataColnames == values$dependentVariable)])
            if(!is.null(values$scatterplotData) && nrow(values$scatterplotData)>1) {
              # Show hidden tabs
              showTab(inputId = "tabsetPanelID", target = "independentVariable1ChoiceTabLabel")
              showTab(inputId = "tabsetPanelID", target = "independentVariable2ChoiceTabLabel")
              # showTab(inputId = "tabsetPanelID", target = "dataFilteringTabLabel")
              showTab(inputId = "tabsetPanelID", target = "generalDisplayParametersTabLabel")
              showTab(inputId = "tabsetPanelID", target = "descriptiveStatisticsTabLabel")
              showTab(inputId = "tabsetPanelID", target = "correlationsTabLabel")
              showTab(inputId = "tabsetPanelID", target = "figureExportTabLabel")
            }
          } else {
            # filterChoices = c("\u00A0",values$filedataColnames)
          }
        } else {
          # filterChoices = c("\u00A0",values$filedataColnames)
        }
        # names(filterChoices)=filterChoices
        # updateSelectInput(session, "filterVar", getLabelOrPrompt("filterVariableInputLabel", displayedLabelsAndPrompts), choices = filterChoices, selected = values$filterVariable)
      }
    })
    
    # Button to exchange X and Y variable
    output$exchangeXYvariables <- renderUI({
      if(!is.null(values$dependentVariableX) && values$dependentVariableX!="\u00A0" && !is.null(values$dependentVariableY) && values$dependentVariableY!="\u00A0") {
        actionButton("exchangeXYvariables", getLabelOrPrompt("exchangeXYvariablesLabel", displayedLabelsAndPrompts))
      }
    })
    
    # Update dropdown menus and values when the user hits the 'X <-> Y' button
    observeEvent(input$exchangeXYvariables, {
      if(!is.null(values$dependentVariableX) && values$dependentVariableX!="\u00A0" && !is.null(values$dependentVariableY) && values$dependentVariableY!="\u00A0") {
        # Switch dependent variables
        tmpDepVarX = values$dependentVariableX
        values$dependentVariableX = values$dependentVariableY
        values$dependentVariableY = tmpDepVarX
        # Update dropdown menus
        updateSelectInput(session, "depVarX", selected = values$dependentVariableX)
        updateSelectInput(session, "depVarY", selected = values$dependentVariableY)
        # Update data and display
        # values$filteredFiledata <- filterData()
        values$scatterplotData <- getScatterplotData()
        values$currentDatasetScatterplot <- plottedFigure()
        output$plotScatter <- renderPlot({values$currentDatasetScatterplot})
        values$descriptiveStatisticsInfoText <- descriptiveStatText()
        output$descriptiveStatisticsInfo <- renderUI({HTML(values$descriptiveStatisticsInfoText)})
        updateCorrelationsTable()
      }
    })
    
    # Dropdown menu to set limit values in the scatterplot automatically or manually
    output$scatterplotLimitsMode <- renderUI({
      df <-values$filedata
      if (is.null(df)) return(NULL)
      items = c(getLabelOrPrompt("scatterplotLimitsModeChoiceAuto", displayedLabelsAndPrompts), getLabelOrPrompt("scatterplotLimitsModeChoiceFixed", displayedLabelsAndPrompts))
      names(items) = items
      selectInput("scatterplotLimitsMode", getLabelOrPrompt("scatterplotLimitsModeLabel", displayedLabelsAndPrompts), items, selected = items[1])
    })
    
    # When limit values of the scatterplot turn back to automatic mode, update figure display
    observeEvent(input$scatterplotLimitsMode, {
      if(values$scatterplotLimitsMode != input$scatterplotLimitsMode) {
        values$scatterplotLimitsMode = input$scatterplotLimitsMode
        if(values$scatterplotLimitsMode == getLabelOrPrompt("scatterplotLimitsModeChoiceAuto", displayedLabelsAndPrompts)) {
          currentDataset = getScatterplotData()
          
          values$scatterplotLowLimitX = min(currentDataset[values$dependentVariableX])
          values$scatterplotHighLimitX = max(currentDataset[values$dependentVariableX])
          dataRangeX = values$scatterplotHighLimitX - values$scatterplotLowLimitX
          limitsInputStep = 10^round(log10(dataRangeX/nStepsInScaleSettings))
          updateNumericInput(session, "scatterplotLowLimitX", value = values$scatterplotLowLimitX, step = limitsInputStep)
          updateNumericInput(session, "scatterplotHighLimitX", value = values$scatterplotHighLimitX, step = limitsInputStep)
          
          values$scatterplotLowLimitY = min(currentDataset[values$dependentVariableY])
          values$scatterplotHighLimitY = max(currentDataset[values$dependentVariableY])
          dataRangeY = values$scatterplotHighLimitY - values$scatterplotLowLimitY
          limitsInputStep = 10^round(log10(dataRangeY/nStepsInScaleSettings))
          updateNumericInput(session, "scatterplotLowLimitY", value = values$scatterplotLowLimitY, step = limitsInputStep)
          updateNumericInput(session, "scatterplotHighLimitY", value = values$scatterplotHighLimitY, step = limitsInputStep)
          
          values$currentDatasetScatterplot <- plottedFigure()
          output$plotScatter <- renderPlot({values$currentDatasetScatterplot})
        }
      }
    })
    
    # When limit values of the scatterplot are adjusted, update figure display
    observeEvent(input$scatterplotLowLimitX, {
      if(is.na(values$scatterplotLowLimitX) || values$scatterplotLowLimitX != input$scatterplotLowLimitX) {
        values$scatterplotLowLimitX = input$scatterplotLowLimitX
        values$currentDatasetScatterplot <- plottedFigure()
        output$plotScatter <- renderPlot({values$currentDatasetScatterplot})
      }
    })
    observeEvent(input$scatterplotHighLimitX, {
      if(is.na(values$scatterplotHighLimitX) || values$scatterplotHighLimitX != input$scatterplotHighLimitX) {
        values$scatterplotHighLimitX = input$scatterplotHighLimitX
        values$currentDatasetScatterplot <- plottedFigure()
        output$plotScatter <- renderPlot({values$currentDatasetScatterplot})
      }
    })
    observeEvent(input$scatterplotLowLimitY, {
      if(is.na(values$scatterplotLowLimitY) || values$scatterplotLowLimitY != input$scatterplotLowLimitY) {
        values$scatterplotLowLimitY = input$scatterplotLowLimitY
        values$currentDatasetScatterplot <- plottedFigure()
        output$plotScatter <- renderPlot({values$currentDatasetScatterplot})
      }
    })
    observeEvent(input$scatterplotHighLimitY, {
      if(is.na(values$scatterplotHighLimitY) || values$scatterplotHighLimitY != input$scatterplotHighLimitY) {
        values$scatterplotHighLimitY = input$scatterplotHighLimitY
        values$currentDatasetScatterplot <- plottedFigure()
        output$plotScatter <- renderPlot({values$currentDatasetScatterplot})
      }
    })
    
    # When the points size is changed, update figure and text display
    observeEvent(input$pointsSize, {
      if(values$pointsSize != input$pointsSize) {
        values$lastParameterChangeTime = Sys.time()
        values$pointsSize <- input$pointsSize
        values$currentDatasetScatterplot <- plottedFigure()
        output$plotScatter <- renderPlot({values$currentDatasetScatterplot})
      }
    })
    
    # When regression lines display is turned on or off, update figure and text display
    observeEvent(input$displayRegressionLinesCheckbox, {
      if(values$displayRegressionLines != input$displayRegressionLinesCheckbox) {
        values$lastParameterChangeTime = Sys.time()
        values$displayRegressionLines <- input$displayRegressionLinesCheckbox
        values$currentDatasetScatterplot <- plottedFigure()
        output$plotScatter <- renderPlot({values$currentDatasetScatterplot})
      }
    })
    # When the regression lines size is changed, update figure and text display
    observeEvent(input$regressionLinesSize, {
      if(values$regressionLinesSize != input$regressionLinesSize) {
        values$lastParameterChangeTime = Sys.time()
        values$regressionLinesSize <- input$regressionLinesSize
        values$currentDatasetScatterplot <- plottedFigure()
        output$plotScatter <- renderPlot({values$currentDatasetScatterplot})
      }
    })
    # When the figure text size is changed, update figure and text display
    observeEvent(input$figureTextSize, {
      if(values$figureTextSize != input$figureTextSize) {
        values$lastParameterChangeTime = Sys.time()
        values$figureTextSize <- input$figureTextSize
        values$currentDatasetScatterplot <- plottedFigure()
        output$plotScatter <- renderPlot({values$currentDatasetScatterplot})
      }
    })
    
    # Dropdown menu: independent variable 1 (any variable except dependent variable)
    output$independentVar1 <- renderUI({
      df <-values$filedata
      if (is.null(df)) return(NULL)
      
      # Include all columns except the selected dependent variable
      datatsetColnames = colnames(df)
      if (length(datatsetColnames)>0) {
        datatsetColnames = values$filedataColnames
        items = datatsetColnames[-which(datatsetColnames==values$dependentVariable)]
        items=c("\u00A0",datatsetColnames)
        names(items)=items
        selectInput("independentVar1", getLabelOrPrompt("independentVariable1InputLabel", displayedLabelsAndPrompts), choices = items, selected = values$independentVariable1)
      } else {
        return("")
      }
    })
    
    # updateIndepVar1filter <- reactive({
    updateIndepVar1filter <- function() {
        if(!is.null(input$independentVar1) && input$independentVar1!="\u00A0") {
          
          indepVar1previousVal <- values$independentVariable1
          
          values$independentVariable1 = input$independentVar1
          values$indepVar1Values = ""

          if(!is.null(values$independentVariable2) && (values$independentVariable1==values$independentVariable2 || indepVar1previousVal==values$independentVariable2)) {
            # Independent variable 2 identical to independent variable 1: display warning and suspend filtering options for the 2nd variable
            updateIndepVar2filter()
          }
          
          # Get and display independent variable modalities with counts
          indepVar1DF = getIndepVar1Table()
          if(!is.null(indepVar1DF)) {
            output$indepVar1Table <- renderRHandsontable(
              rhandsontable(indepVar1DF, selectCallback = FALSE, stretchH = "all", rowHeaderWidth = 0) %>%
                hot_col(col = 1, readOnly = TRUE) %>%
                hot_col(col = 2, readOnly = TRUE)  %>%
                hot_col(col = 3, type = "checkbox") %>%
                hot_cols(columnSorting = TRUE, colWidths = c(150, 50, 50))
            )
            output$selectAllIndepVar1Button <- renderUI(
              actionButton("selectAllIndepVar1Button", paste0(getLabelOrPrompt("selectAllIndepVar1ButtonLabel", displayedLabelsAndPrompts), " (", nrow(indepVar1DF), ")"))
            )
            output$unselectAllIndepVar1Button <- renderUI(
              actionButton("unselectAllIndepVar1Button", getLabelOrPrompt("unselectAllIndepVar1ButtonLabel", displayedLabelsAndPrompts))
            )
            output$revertSelectionIndepVar1Button <- renderUI(
              actionButton("revertSelectionIndepVar1Button", getLabelOrPrompt("revertSelectionIndepVar1ButtonLabel", displayedLabelsAndPrompts))
            )
          } else {
            output$indepVar1Table <- renderRHandsontable({NULL})
            output$selectAllIndepVar1Button <- renderUI({NULL})
            output$unselectAllIndepVar1Button <- renderUI({NULL})
            output$revertSelectionIndepVar1Button <- renderUI({NULL})
          }
          # Update data and display
          # values$filteredFiledata <- filterData()
          values$scatterplotData <- getScatterplotData()
          values$currentDatasetScatterplot <- plottedFigure()
          output$plotScatter <- renderPlot({values$currentDatasetScatterplot})
          values$descriptiveStatisticsInfoText <- descriptiveStatText()
          output$descriptiveStatisticsInfo <- renderUI({HTML(values$descriptiveStatisticsInfoText)})
          updateCorrelationsTable()
        } else {
          values$independentVariable1 = "\u00A0"
          output$indepVar1Table <- renderRHandsontable({NULL})
          output$selectAllIndepVar1Button <- renderUI({NULL})
          output$unselectAllIndepVar1Button <- renderUI({NULL})
          output$revertSelectionIndepVar1Button <- renderUI({NULL})
          # Update data and display
          # values$filteredFiledata <- filterData()
          values$scatterplotData <- getScatterplotData()
          values$currentDatasetScatterplot <- plottedFigure()
          output$plotScatter <- renderPlot({values$currentDatasetScatterplot})
          values$descriptiveStatisticsInfoText <- descriptiveStatText()
          output$descriptiveStatisticsInfo <- renderUI({HTML(values$descriptiveStatisticsInfoText)})
          updateCorrelationsTable()
        }
    }
    
    # When a new independent variable 1 is set, update text and figure display, and display modalities with counts in a table
    observeEvent(input$independentVar1, {
      if(!is.null(values$independentVariable1) && values$independentVariable1 != input$independentVar1) {
        updateIndepVar1filter()
      }
    })
    
    # Get independent variable 1 modalities with counts, assigning a special value to empty cells if any
    getIndepVar1Table <- function() {
      # df <- values$filedata
      df <- values$filteredFiledata
      if (is.null(df) || is.null(values$independentVariable1) || values$independentVariable1 == "\u00A0") return(NULL)
      
      charVector <- df %>% pull(all_of(values$independentVariable1)) %>% as.character()
      charVector[is.na(charVector)] = getLabelOrPrompt("emptyCellsLabel", displayedLabelsAndPrompts)
      
      # If a independent variable 1 is selected, include all unique values
      independentVar1Counts = table(charVector)
      
      subgroupsDF <- data.frame(
        nValues = independentVar1Counts,
        selected = rep(TRUE, length(independentVar1Counts))
      )
      colnames(subgroupsDF) = c(getLabelOrPrompt("groupColInIndepVar1Table", displayedLabelsAndPrompts), getLabelOrPrompt("countsColInIndepVar1Table", displayedLabelsAndPrompts), getLabelOrPrompt("includeColInIndepVar1Table", displayedLabelsAndPrompts))
      return(subgroupsDF)
    }
    
    # Dropdown menu: value of the independent variable 1 variable to be processed (all unique values of the variable, displayed with corresponding counts)
    output$indepVar1Table <- renderRHandsontable({
      
      subgroupsDF <- getIndepVar1Table()
      if(!is.null(subgroupsDF)) {
        rhandsontable(subgroupsDF, selectCallback = FALSE, stretchH = "all", rowHeaderWidth = 0) %>%
          hot_col(col = 1, readOnly = TRUE) %>%
          hot_col(col = 2, readOnly = TRUE)  %>%
          hot_col(col = 3, type = "checkbox") %>%
          hot_cols(columnSorting = TRUE, colWidths = c(150, 50, 50))
      } else {
        NULL
      }
    })
    
    # When independent variable 1 modalities are checked or uncheck in the table, update data and display (figure and text)
    observeEvent(input$indepVar1Table, {
      # Get the contents of the rhandsontable as data frame
      indepVar1DF = hot_to_r(input$indepVar1Table)
      # Get all checked values
      indepVar1ValuesTmp = indepVar1DF[indepVar1DF[,3]==TRUE,1]
      if(length(indepVar1ValuesTmp)==0)
        indepVar1ValuesTmp = NULL
      # if(is.null(values$indepVar1Values) || (length(values$indepVar1Values)>0 && values$indepVar1Values=="")) {
      #   values$indepVar1Values = indepVar1ValuesTmp
      # } else 
      if(is.null(indepVar1ValuesTmp) || !setequal(values$indepVar1Values, indepVar1ValuesTmp)) {
        values$lastParameterChangeTime = Sys.time()
        values$indepVar1Values = indepVar1ValuesTmp
        # Check if empty cells are included in the filter
        values$NAinIndepVar1Values = getLabelOrPrompt("emptyCellsLabel", displayedLabelsAndPrompts) %in% values$indepVar1Values
        # Update data and display
        # values$filteredFiledata <- filterData()
        values$scatterplotData <- getScatterplotData()
        values$currentDatasetScatterplot <- plottedFigure()
        output$plotScatter <- renderPlot({values$currentDatasetScatterplot})
        values$descriptiveStatisticsInfoText <- descriptiveStatText()
        output$descriptiveStatisticsInfo <- renderUI({HTML(values$descriptiveStatisticsInfoText)})
        updateCorrelationsTable()
      }
    })
    
    # Action buttons for multiple modalities (un)checking in the independent variable 1 table
    output$selectAllIndepVar1Button <- renderUI({
      if(!is.null(values$independentVariable1) && values$independentVariable1!="\u00A0") {
        actionButton("selectAllIndepVar1Button", getLabelOrPrompt("selectAllIndepVar1ButtonLabel", displayedLabelsAndPrompts))
      }
    })
    output$unselectAllIndepVar1Button <- renderUI({
      if(!is.null(values$independentVariable1) && values$independentVariable1!="\u00A0") {
        actionButton("unselectAllIndepVar1Button", getLabelOrPrompt("unselectAllIndepVar1ButtonLabel", displayedLabelsAndPrompts))
      }
    })
    output$revertSelectionIndepVar1Button <- renderUI({
      if(!is.null(values$independentVariable1) && values$independentVariable1!="\u00A0") {
        actionButton("revertSelectionIndepVar1Button", getLabelOrPrompt("revertSelectionIndepVar1ButtonLabel", displayedLabelsAndPrompts))
      }
    })
    
    # Update independent variable 1 table and values when the user hits the 'select all' button
    observeEvent(input$selectAllIndepVar1Button, {
      # Get the contents of the rhandsontable as data frame
      indepVar1DF = hot_to_r(input$indepVar1Table)
      if(!setequal(indepVar1DF[,3], rep(TRUE, nrow(indepVar1DF)))) {
        values$lastParameterChangeTime = Sys.time()
        # Set all modalities as checked
        indepVar1DF[,3] = TRUE
        values$indepVar1Values = indepVar1DF[,1]
        # Update table display
        output$indepVar1Table <- renderRHandsontable(
          rhandsontable(indepVar1DF, selectCallback = FALSE, stretchH = "all", rowHeaderWidth = 0) %>%
            hot_col(col = 1, readOnly = TRUE) %>%
            hot_col(col = 2, readOnly = TRUE)  %>%
            hot_col(col = 3, type = "checkbox") %>%
            hot_cols(columnSorting = TRUE, colWidths = c(150, 50, 50))
        )
        # Update data and display
        # values$filteredFiledata <- filterData()
        values$scatterplotData <- getScatterplotData()
        values$currentDatasetScatterplot <- plottedFigure()
        output$plotScatter <- renderPlot({values$currentDatasetScatterplot})
        values$descriptiveStatisticsInfoText <- descriptiveStatText()
        output$descriptiveStatisticsInfo <- renderUI({HTML(values$descriptiveStatisticsInfoText)})
        updateCorrelationsTable()
      }
    })
    
    # Update independent variable 1 table and values when the user hits the 'unselect all' button
    observeEvent(input$unselectAllIndepVar1Button, {
      # Get the contents of the rhandsontable as data frame
      indepVar1DF = hot_to_r(input$indepVar1Table)
      if(!setequal(indepVar1DF[,3], rep(FALSE, nrow(indepVar1DF)))) {
        values$lastParameterChangeTime = Sys.time()
        # Set all modalities as unchecked
        indepVar1DF[,3] = FALSE
        values$indepVar1Values = NULL
        # Update table display
        output$indepVar1Table <- renderRHandsontable(
          rhandsontable(indepVar1DF, selectCallback = FALSE, stretchH = "all", rowHeaderWidth = 0) %>%
            hot_col(col = 1, readOnly = TRUE) %>%
            hot_col(col = 2, readOnly = TRUE)  %>%
            hot_col(col = 3, type = "checkbox") %>%
            hot_cols(columnSorting = TRUE, colWidths = c(150, 50, 50))
        )
        # Update data and display
        # values$filteredFiledata <- filterData()
        values$scatterplotData <- getScatterplotData()
        values$currentDatasetScatterplot <- plottedFigure()
        output$plotScatter <- renderPlot({values$currentDatasetScatterplot})
        values$descriptiveStatisticsInfoText <- descriptiveStatText()
        output$descriptiveStatisticsInfo <- renderUI({HTML(values$descriptiveStatisticsInfoText)})
        updateCorrelationsTable()
      }
    })
    
    # Update independent variable 1 table and values when the user hits the 'revert selection' button
    observeEvent(input$revertSelectionIndepVar1Button, {
      values$lastParameterChangeTime = Sys.time()
      # Get the contents of the rhandsontable as data frame
      indepVar1DF = hot_to_r(input$indepVar1Table)
      # Set checked modalities as unchecked and conversely
      indepVar1DF[,3] = !as.logical(indepVar1DF[,3])
      # browser()
      values$indepVar1Values = indepVar1DF[indepVar1DF[,3]==TRUE,1]
      # Update table display
      output$indepVar1Table <- renderRHandsontable(
        rhandsontable(indepVar1DF, selectCallback = FALSE, stretchH = "all", rowHeaderWidth = 0) %>%
          hot_col(col = 1, readOnly = TRUE) %>%
          hot_col(col = 2, readOnly = TRUE)  %>%
          hot_col(col = 3, type = "checkbox") %>%
          hot_cols(columnSorting = TRUE, colWidths = c(150, 50, 50))
      )
      # Update data and display
      # values$filteredFiledata <- filterData()
      values$scatterplotData <- getScatterplotData()
      values$currentDatasetScatterplot <- plottedFigure()
      output$plotScatter <- renderPlot({values$currentDatasetScatterplot})
      values$descriptiveStatisticsInfoText <- descriptiveStatText()
      output$descriptiveStatisticsInfo <- renderUI({HTML(values$descriptiveStatisticsInfoText)})
      updateCorrelationsTable()
    })
    
    # Dropdown menu: independent variable 2 (any variable except dependent variable)
    output$independentVar2 <- renderUI({
      df <-values$filedata
      if (is.null(df)) return(NULL)
      
      # Include all columns except the selected dependent variable
      datatsetColnames = colnames(df)
      if (length(datatsetColnames)>0) {
        datatsetColnames = values$filedataColnames
        items = datatsetColnames[-which(datatsetColnames==values$dependentVariable)]
        items=c("\u00A0",datatsetColnames)
        names(items)=items
        selectInput("independentVar2", getLabelOrPrompt("independentVariable2InputLabel", displayedLabelsAndPrompts), choices = items, selected = values$independentVariable2)
      } else {
        return("")
      }
    })
    
    updateIndepVar2filter <- function() {
    # updateIndepVar2filter <- reactive({
      if(!is.null(input$independentVar2) && input$independentVar2!="\u00A0") {
        
        values$independentVariable2 = input$independentVar2
        values$indepVar2Values = ""
        
        # Display filter only if independant variables 1 and 2 are different
        if(!is.null(values$independentVariable1) && values$independentVariable2 != values$independentVariable1) {
          
          # Get and display independent variable modalities with counts
          indepVar2DF = getIndepVar2Table()
          if(!is.null(indepVar2DF)) {
            output$indepVar2Table <- renderRHandsontable(
              rhandsontable(indepVar2DF, selectCallback = FALSE, stretchH = "all", rowHeaderWidth = 0) %>%
                hot_col(col = 1, readOnly = TRUE) %>%
                hot_col(col = 2, readOnly = TRUE)  %>%
                hot_col(col = 3, type = "checkbox") %>%
                hot_cols(
                  columnSorting = TRUE,
                  colWidths = c(150, 50, 50)
                )
            )
            output$selectAllIndepVar2Button <- renderUI(
              actionButton("selectAllIndepVar2Button", paste0(getLabelOrPrompt("selectAllIndepVar2ButtonLabel", displayedLabelsAndPrompts), " (", nrow(indepVar2DF), ")"))
            )
            output$unselectAllIndepVar2Button <- renderUI(
              actionButton("unselectAllIndepVar2Button", getLabelOrPrompt("unselectAllIndepVar2ButtonLabel", displayedLabelsAndPrompts))
            )
            output$revertSelectionIndepVar2Button <- renderUI(
              actionButton("revertSelectionIndepVar2Button", getLabelOrPrompt("revertSelectionIndepVar2ButtonLabel", displayedLabelsAndPrompts))
            )
          } else {
            output$indepVar2Table <- renderRHandsontable({NULL})
            output$selectAllIndepVar2Button <- renderUI({NULL})
            output$unselectAllIndepVar2Button <- renderUI({NULL})
            output$revertSelectionIndepVar2Button <- renderUI({NULL})
          }
        } else {
          # Independent variable 2 identical to independent variable 1: display warning and suspend filtering options for the 2nd variable
          dfTmp <- tibble(warning = str_c("\n", getLabelOrPrompt("identicalIndependentVariablesWarningMessage", displayedLabelsAndPrompts), "\n"), dummyNum = 0, dummyLogical = F)
          colnames(dfTmp) = c(getLabelOrPrompt("groupColInIndepVar2Table", displayedLabelsAndPrompts), getLabelOrPrompt("countsColInIndepVar2Table", displayedLabelsAndPrompts), getLabelOrPrompt("includeColInIndepVar2Table", displayedLabelsAndPrompts))
          output$indepVar2Table <- renderRHandsontable(
            rhandsontable(
              dfTmp,
              colHeaders = NULL,
              stretchH = "all",
              rowHeaderWidth = 0,
              readOnly = TRUE,
              disableVisualSelection = TRUE
            ) %>% 
              hot_cols(colWidths = c(300, .1, .1))
          )
          output$selectAllIndepVar2Button <- renderUI({NULL})
          output$unselectAllIndepVar2Button <- renderUI({NULL})
          output$revertSelectionIndepVar2Button <- renderUI({NULL})
        }
        # Update data and display
        # values$filteredFiledata <- filterData()
        values$scatterplotData <- getScatterplotData()
        values$currentDatasetScatterplot <- plottedFigure()
        output$plotScatter <- renderPlot({values$currentDatasetScatterplot})
        values$descriptiveStatisticsInfoText <- descriptiveStatText()
        output$descriptiveStatisticsInfo <- renderUI({HTML(values$descriptiveStatisticsInfoText)})
        updateCorrelationsTable()
      } else {
        values$independentVariable2 = "\u00A0"
        output$indepVar2Table <- renderRHandsontable({NULL})
        output$selectAllIndepVar2Button <- renderUI({NULL})
        output$unselectAllIndepVar2Button <- renderUI({NULL})
        output$revertSelectionIndepVar2Button <- renderUI({NULL})
        # Update data and display
        # values$filteredFiledata <- filterData()
        values$scatterplotData <- getScatterplotData()
        values$currentDatasetScatterplot <- plottedFigure()
        output$plotScatter <- renderPlot({values$currentDatasetScatterplot})
        values$descriptiveStatisticsInfoText <- descriptiveStatText()
        output$descriptiveStatisticsInfo <- renderUI({HTML(values$descriptiveStatisticsInfoText)})
        updateCorrelationsTable()
      }
    }
    
    # When a new independent variable 2 is set, update text and figure display, and display modalities with counts in a table
    observeEvent(input$independentVar2, {
      if(!is.null(values$independentVariable2) && values$independentVariable2 != input$independentVar2) {
        updateIndepVar2filter()
      }
    })
    
    # Get independent variable 2 modalities with counts, assigning a special value to empty cells if any
    getIndepVar2Table <- function(){
      # df <- values$filedata
      df <- values$filteredFiledata
      if (is.null(df) || is.null(values$independentVariable2) || values$independentVariable2 == "\u00A0") return(NULL)
      
      charVector <- df %>% pull(all_of(values$independentVariable2)) %>% as.character()
      charVector[is.na(charVector)] = getLabelOrPrompt("emptyCellsLabel", displayedLabelsAndPrompts)
      
      # If a independent variable 2 is selected, include all unique values
      independentVar2Counts = table(charVector)
      
      subgroupsDF <- data.frame(
        nValues = independentVar2Counts,
        selected = rep(TRUE, length(independentVar2Counts))
      )
      colnames(subgroupsDF) = c(getLabelOrPrompt("groupColInIndepVar2Table", displayedLabelsAndPrompts), getLabelOrPrompt("countsColInIndepVar2Table", displayedLabelsAndPrompts), getLabelOrPrompt("includeColInIndepVar2Table", displayedLabelsAndPrompts))
      return(subgroupsDF)
    }
    
    # Dropdown menu: value of the independent variable 2 to be processed (all unique values of the variable, displayed with corresponding counts)
    output$indepVar2Table <- renderRHandsontable({
      
      subgroupsDF <- getIndepVar2Table()
      if(!is.null(subgroupsDF)) {
        rhandsontable(subgroupsDF, selectCallback = FALSE, stretchH = "all", rowHeaderWidth = 0) %>%
          hot_col(col = 1, readOnly = TRUE) %>%
          hot_col(col = 2, readOnly = TRUE)  %>%
          hot_col(col = 3, type = "checkbox") %>%
          hot_cols(columnSorting = TRUE, colWidths = c(150, 50, 50))
      } else {
        NULL
      }
    })
    
    # When independent variable 2 modalities are checked or uncheck in the table, update data and display (figure and text)
    observeEvent(input$indepVar2Table, {
      # Get the contents of the rhandsontable as data frame
      indepVar2DF = hot_to_r(input$indepVar2Table)
      # Get all checked values
      indepVar2ValuesTmp = indepVar2DF[indepVar2DF[,3]==TRUE,1]
      if(length(indepVar2ValuesTmp)==0)
        indepVar2ValuesTmp = NULL
      # if(is.null(values$indepVar2Values) || (length(values$indepVar2Values)>0 && values$indepVar2Values=="")) {
      #   values$indepVar2Values = indepVar2ValuesTmp
      # } else 
      if(is.null(indepVar2ValuesTmp) || !setequal(values$indepVar2Values, indepVar2ValuesTmp)) {
        values$lastParameterChangeTime = Sys.time()
        values$indepVar2Values = indepVar2ValuesTmp
        # Check if empty cells are included in the filter
        values$NAinIndepVar2Values = getLabelOrPrompt("emptyCellsLabel", displayedLabelsAndPrompts) %in% values$indepVar2Values
        # Update data and display
        # values$filteredFiledata <- filterData()
        values$scatterplotData <- getScatterplotData()
        values$currentDatasetScatterplot <- plottedFigure()
        output$plotScatter <- renderPlot({values$currentDatasetScatterplot})
        values$descriptiveStatisticsInfoText <- descriptiveStatText()
        output$descriptiveStatisticsInfo <- renderUI({HTML(values$descriptiveStatisticsInfoText)})
        updateCorrelationsTable()
      }
    })
    
    # Action buttons for multiple modalities (un)checking in the independent variable 2 table
    output$selectAllIndepVar2Button <- renderUI({
      if(!is.null(values$independentVariable2) && values$independentVariable2!="\u00A0") {
        actionButton("selectAllIndepVar2Button", getLabelOrPrompt("selectAllIndepVar2ButtonLabel", displayedLabelsAndPrompts))
      }
    })
    output$unselectAllIndepVar2Button <- renderUI({
      if(!is.null(values$independentVariable2) && values$independentVariable2!="\u00A0") {
        actionButton("unselectAllIndepVar2Button", getLabelOrPrompt("unselectAllIndepVar2ButtonLabel", displayedLabelsAndPrompts))
      }
    })
    output$revertSelectionIndepVar2Button <- renderUI({
      if(!is.null(values$independentVariable2) && values$independentVariable2!="\u00A0") {
        actionButton("revertSelectionIndepVar2Button", getLabelOrPrompt("revertSelectionIndepVar2ButtonLabel", displayedLabelsAndPrompts))
      }
    })
    
    # Update independent variable 2 table and values when the user hits the 'select all' button
    observeEvent(input$selectAllIndepVar2Button, {
      # Get the contents of the rhandsontable as data frame
      indepVar2DF = hot_to_r(input$indepVar2Table)
      if(!setequal(indepVar2DF[,3], rep(TRUE, nrow(indepVar2DF)))) {
        values$lastParameterChangeTime = Sys.time()
        # Set all modalities as checked
        indepVar2DF[,3] = TRUE
        values$indepVar2Values = indepVar2DF[,1]
        # Update table display
        output$indepVar2Table <- renderRHandsontable(
          rhandsontable(indepVar2DF, selectCallback = FALSE, stretchH = "all", rowHeaderWidth = 0) %>%
            hot_col(col = 1, readOnly = TRUE) %>%
            hot_col(col = 2, readOnly = TRUE)  %>%
            hot_col(col = 3, type = "checkbox") %>%
            hot_cols(columnSorting = TRUE, colWidths = c(150, 50, 50))
        )
        # Update data and display
        # values$filteredFiledata <- filterData()
        values$scatterplotData <- getScatterplotData()
        values$currentDatasetScatterplot <- plottedFigure()
        output$plotScatter <- renderPlot({values$currentDatasetScatterplot})
        values$descriptiveStatisticsInfoText <- descriptiveStatText()
        output$descriptiveStatisticsInfo <- renderUI({HTML(values$descriptiveStatisticsInfoText)})
        updateCorrelationsTable()
      }
    })
    
    # Update independent variable 2 table and values when the user hits the 'unselect all' button
    observeEvent(input$unselectAllIndepVar2Button, {
      # Get the contents of the rhandsontable as data frame
      indepVar2DF = hot_to_r(input$indepVar2Table)
      if(!setequal(indepVar2DF[,3], rep(FALSE, nrow(indepVar2DF)))) {
        values$lastParameterChangeTime = Sys.time()
        # Set all modalities as unchecked
        indepVar2DF[,3] = FALSE
        values$indepVar2Values = NULL
        # Update table display
        output$indepVar2Table <- renderRHandsontable(
          rhandsontable(indepVar2DF, selectCallback = FALSE, stretchH = "all", rowHeaderWidth = 0) %>%
            hot_col(col = 1, readOnly = TRUE) %>%
            hot_col(col = 2, readOnly = TRUE)  %>%
            hot_col(col = 3, type = "checkbox") %>%
            hot_cols(columnSorting = TRUE, colWidths = c(150, 50, 50))
        )
        # Update data and display
        # values$filteredFiledata <- filterData()
        values$scatterplotData <- getScatterplotData()
        values$currentDatasetScatterplot <- plottedFigure()
        output$plotScatter <- renderPlot({values$currentDatasetScatterplot})
        values$descriptiveStatisticsInfoText <- descriptiveStatText()
        output$descriptiveStatisticsInfo <- renderUI({HTML(values$descriptiveStatisticsInfoText)})
        updateCorrelationsTable()
      }
    })
    
    # Update independent variable 2 table and values when the user hits the 'revert selection' button
    observeEvent(input$revertSelectionIndepVar2Button, {
      values$lastParameterChangeTime = Sys.time()
      # Get the contents of the rhandsontable as data frame
      indepVar2DF = hot_to_r(input$indepVar2Table)
      # Set checked modalities as unchecked and conversely
      indepVar2DF[,3] = !as.logical(indepVar2DF[,3])
      # browser()
      values$indepVar2Values = indepVar2DF[indepVar2DF[,3]==TRUE,1]
      # Update table display
      output$indepVar2Table <- renderRHandsontable(
        rhandsontable(indepVar2DF, selectCallback = FALSE, stretchH = "all", rowHeaderWidth = 0) %>%
          hot_col(col = 1, readOnly = TRUE) %>%
          hot_col(col = 2, readOnly = TRUE)  %>%
          hot_col(col = 3, type = "checkbox") %>%
          hot_cols(columnSorting = TRUE, colWidths = c(150, 50, 50))
      )
      # Update data and display
      # values$filteredFiledata <- filterData()
      values$scatterplotData <- getScatterplotData()
      values$currentDatasetScatterplot <- plottedFigure()
      output$plotScatter <- renderPlot({values$currentDatasetScatterplot})
      values$descriptiveStatisticsInfoText <- descriptiveStatText()
      output$descriptiveStatisticsInfo <- renderUI({HTML(values$descriptiveStatisticsInfoText)})
      updateCorrelationsTable()
    })
    
    output$selectedDataDownloadInfo <- renderUI({
      HTML(str_c("<b>", getLabelOrPrompt("selectedDataDownloadInfo", displayedLabelsAndPrompts), "</b>"))
    })
    
    output$selectedDataDetailsDisplayed <- reactive({
      return(values$displaySelectedDataDetailInTableFlag!=0 && !is.null(values$clickedSubset) && nrow(values$clickedSubset)>0)
    })
    outputOptions(output, 'selectedDataDetailsDisplayed', suspendWhenHidden=FALSE)
    
    # Set the file format for selected data export
    output$selectedDataDownloadFormat <- renderUI({
      selectInput("selectedDataDownloadFormat", getLabelOrPrompt("selectedDataDownloadFormatLabel", displayedLabelsAndPrompts),availableDataFileOutputFormats)
    })
    
    # Allow the user to download selected data
    output$downloadSelectedData <- downloadHandler(
      filename = function() {
        # Get the default image file name using dataset file name and bins number
        str_c(tools::file_path_sans_ext(input$datafile),
              getLabelOrPrompt("defaultSuffixInExportedDataFileName", displayedLabelsAndPrompts),
              availableDataFileOutputFormatExtensions[which(availableDataFileOutputFormats == input$selectedDataDownloadFormat)])
      },
      # Export selected data, using format picked by the user
      content = function(file) {
        if(!is.null(input$selectedDataDownloadFormat) & !is.null(values$clickedSubset) && nrow(values$clickedSubset)>0) {
          if(input$selectedDataDownloadFormat=="XLSX") {
            write_xlsx(values$clickedSubset, path = file, format_headers = F)
          } else if(input$selectedDataDownloadFormat=="TSV") {
            write_tsv(values$clickedSubset, file = file)
          } else if(input$selectedDataDownloadFormat=="CSV") {
            write_csv(values$clickedSubset, file = file)
          }
        }
    })
    
    # # Dropdown menu: filtering variable (any variable except dependent variable)
    # output$filterVar <- renderUI({
    #   df <-values$filedata
    #   if (is.null(df)) return(NULL)
    #   
    #   # Include all columns except the selected dependent variable
    #   datatsetColnames = colnames(df)
    #   if (length(datatsetColnames)>0) {
    #     datatsetColnames = values$filedataColnames
    #     items = datatsetColnames[-which(datatsetColnames==values$dependentVariable)]
    #     items=c("\u00A0",datatsetColnames)
    #     names(items)=items
    #     selectInput("filterVar", getLabelOrPrompt("filterVariableInputLabel", displayedLabelsAndPrompts), choices = items, selected = values$filterVariable)
    #   } else {
    #     return("")
    #   }
    # })
    
    # # When a new filter variable is set, update text and figure display, and display filter modalities with counts in a table
    # observeEvent(input$filterVar, {
    #   if(!is.null(values$filterVariable) && values$filterVariable != input$filterVar) {
    #     if(!is.null(input$filterVar) && input$filterVar!="\u00A0") {
    #       
    #       values$filterVariable = input$filterVar
    #       values$filterValues = ""
    #       
    #       # Get and display filter variable modalities with counts
    #       filterDF = getFilterTable()
    #       if(!is.null(filterDF)) {
    #         output$filterValTable <- renderRHandsontable(
    #           rhandsontable(filterDF, selectCallback = FALSE, stretchH = "all", rowHeaderWidth = 0) %>%
    #             hot_col(col = 1, readOnly = TRUE) %>%
    #             hot_col(col = 2, readOnly = TRUE)  %>%
    #             hot_col(col = 3, type = "checkbox") %>%
    #             hot_cols(columnSorting = TRUE, colWidths = c(150, 50, 50))
    #         )
    #         output$selectAllButton <- renderUI(
    #           actionButton("selectAllButton", paste0(getLabelOrPrompt("selectAllButtonLabel", displayedLabelsAndPrompts), " (", nrow(filterDF), ")"))
    #           )
    #         output$unselectAllButton <- renderUI(
    #           actionButton("unselectAllButton", getLabelOrPrompt("unselectAllButtonLabel", displayedLabelsAndPrompts))
    #           )
    #         output$revertSelectionButton <- renderUI(
    #           actionButton("revertSelectionButton", getLabelOrPrompt("revertSelectionButtonLabel", displayedLabelsAndPrompts))
    #           )
    #       } else {
    #         output$filterValTable <- renderRHandsontable({NULL})
    #         output$selectAllButton <- renderUI({NULL})
    #         output$unselectAllButton <- renderUI({NULL})
    #         output$revertSelectionButton <- renderUI({NULL})
    #       }
    #     } else {
    #       output$filterValTable <- renderRHandsontable({NULL})
    #       output$selectAllButton <- renderUI({NULL})
    #       output$unselectAllButton <- renderUI({NULL})
    #       output$revertSelectionButton <- renderUI({NULL})
    #     }
    #   }
    # })
    # 
    # # Get filter variable modalities with counts, assigning a special value to empty cells if any
    # getFilterTable <- function(){
    #   # df <- values$filedata
    #   df <- values$filteredFiledata
    #   if (is.null(df) || is.null(values$filterVariable) || values$filterVariable == "\u00A0") return(NULL)
    #   
    #   charVector <- df %>% pull(all_of(values$filterVariable)) %>% as.character()
    #   charVector[is.na(charVector)] = getLabelOrPrompt("emptyCellsLabel", displayedLabelsAndPrompts)
    #   
    #   # If a filter column is selected, include all unique values
    #   filterVarCounts = table(charVector)
    #   # # Discard categories where count = 1 (not suitable for histogram drawing)
    #   # filterVarCounts = filterVarCounts[filterVarCounts>1]
    #   
    #   subgroupsDF <- data.frame(
    #     nValues = filterVarCounts,
    #     selected = rep(TRUE, length(filterVarCounts))
    #   )
    #   colnames(subgroupsDF) = c(getLabelOrPrompt("groupColInFilterTable", displayedLabelsAndPrompts), getLabelOrPrompt("countsColInFilterTable", displayedLabelsAndPrompts), getLabelOrPrompt("includeColInFilterTable", displayedLabelsAndPrompts))
    #   return(subgroupsDF)
    # }
    # 
    # # Dropdown menu: value of the filter variable to be processed (all unique values of the filter variable, displayed with corresponding counts)
    # output$filterValTable <- renderRHandsontable({
    #   
    #   subgroupsDF <- getFilterTable()
    #   if(!is.null(subgroupsDF)) {
    #     rhandsontable(subgroupsDF, selectCallback = FALSE, stretchH = "all", rowHeaderWidth = 0) %>%
    #       hot_col(col = 1, readOnly = TRUE) %>%
    #       hot_col(col = 2, readOnly = TRUE)  %>%
    #       hot_col(col = 3, type = "checkbox") %>%
    #       hot_cols(columnSorting = TRUE, colWidths = c(150, 50, 50))
    #   } else {
    #     NULL
    #   }
    # })
    # 
    # # When filter modalities are checked or unchecked in the filter table, update data and display (figure and text)
    # observeEvent(input$filterValTable, {
    #   # Get the contents of the rhandsontable as data frame
    #   filterDF = hot_to_r(input$filterValTable)
    #   # Get all checked values
    #   filterValuesTmp = filterDF[filterDF[,3]==TRUE,1]
    #   if(length(filterValuesTmp)==0)
    #     filterValuesTmp = NULL
    #   if(is.null(values$filterValues) || (length(values$filterValues)==1 && values$filterValues=="")) {
    #     values$filterValues = filterValuesTmp
    #   } else if(!setequal(values$filterValues, filterValuesTmp)) {
    #     values$filterValues = filterValuesTmp
    #   }
    #   values$lastParameterChangeTime = Sys.time()
    #   # Check if empty cells are included in the filter
    #   values$NAinFilterValues = getLabelOrPrompt("emptyCellsLabel", displayedLabelsAndPrompts) %in% values$filterValues
    #   # Update data and display
    #   values$filteredFiledata <- filterData()
    #   values$scatterplotData <- getScatterplotData()
    #   values$currentDatasetScatterplot <- plottedFigure()
    #   output$plotScatter <- renderPlot({values$currentDatasetScatterplot})
    #   values$descriptiveStatisticsInfoText <- descriptiveStatText()
    #   output$descriptiveStatisticsInfo <- renderUI({HTML(values$descriptiveStatisticsInfoText)})
    #   updateCorrelationsTable()
    # })
    # 
    # # Action buttons for multiple modalities (un)checking in the filter table
    # output$selectAllButton <- renderUI({
    #   if(!is.null(values$filterVariable) && values$filterVariable!="\u00A0") {
    #     actionButton("selectAllButton", getLabelOrPrompt("selectAllButtonLabel", displayedLabelsAndPrompts))
    #   }
    # })
    # output$unselectAllButton <- renderUI({
    #   if(!is.null(values$filterVariable) && values$filterVariable!="\u00A0") {
    #     actionButton("unselectAllButton", getLabelOrPrompt("unselectAllButtonLabel", displayedLabelsAndPrompts))
    #   }
    # })
    # output$revertSelectionButton <- renderUI({
    #   if(!is.null(values$filterVariable) && values$filterVariable!="\u00A0") {
    #     actionButton("revertSelectionButton", getLabelOrPrompt("revertSelectionButtonLabel", displayedLabelsAndPrompts))
    #   }
    # })
    # 
    # # Update filter table and values when the user hits the 'select all' button
    # observeEvent(input$selectAllButton, {
    #   # Get the contents of the rhandsontable as data frame
    #   filterDF = hot_to_r(input$filterValTable)
    #   if(!setequal(filterDF[,3], rep(TRUE, nrow(filterDF)))) {
    #     values$lastParameterChangeTime = Sys.time()
    #     # Set all modalities as checked
    #     filterDF[,3] = TRUE
    #     values$filterValues = filterDF[,1]
    #     # Update table display
    #     output$filterValTable <- renderRHandsontable(
    #       rhandsontable(filterDF, selectCallback = FALSE, stretchH = "all", rowHeaderWidth = 0) %>%
    #         hot_col(col = 1, readOnly = TRUE) %>%
    #         hot_col(col = 2, readOnly = TRUE)  %>%
    #         hot_col(col = 3, type = "checkbox") %>%
    #         hot_cols(columnSorting = TRUE, colWidths = c(150, 50, 50))
    #     )
    #     # Update data and display
    #     values$filteredFiledata <- filterData()
    #     values$scatterplotData <- getScatterplotData()
    #     values$currentDatasetScatterplot <- plottedFigure()
    #     output$plotScatter <- renderPlot({values$currentDatasetScatterplot})
    #     values$descriptiveStatisticsInfoText <- descriptiveStatText()
    #     output$descriptiveStatisticsInfo <- renderUI({HTML(values$descriptiveStatisticsInfoText)})
    #     updateCorrelationsTable()
    #   }
    # })
    # 
    # # Update filter table and values when the user hits the 'unselect all' button
    # observeEvent(input$unselectAllButton, {
    #   # Get the contents of the rhandsontable as data frame
    #   filterDF = hot_to_r(input$filterValTable)
    #   if(!setequal(filterDF[,3], rep(FALSE, nrow(filterDF)))) {
    #     values$lastParameterChangeTime = Sys.time()
    #     # Set all modalities as unchecked
    #     filterDF[,3] = FALSE
    #     values$filterValues = filterDF[NULL,1]
    #     # Update table display
    #     output$filterValTable <- renderRHandsontable(
    #       rhandsontable(filterDF, selectCallback = FALSE, stretchH = "all", rowHeaderWidth = 0) %>%
    #         hot_col(col = 1, readOnly = TRUE) %>%
    #         hot_col(col = 2, readOnly = TRUE)  %>%
    #         hot_col(col = 3, type = "checkbox") %>%
    #         hot_cols(columnSorting = TRUE, colWidths = c(150, 50, 50))
    #     )
    #     # Update data and display
    #     values$filteredFiledata <- filterData()
    #     values$scatterplotData <- getScatterplotData()
    #     values$currentDatasetScatterplot <- plottedFigure()
    #     output$plotScatter <- renderPlot({values$currentDatasetScatterplot})
    #     values$descriptiveStatisticsInfoText <- descriptiveStatText()
    #     output$descriptiveStatisticsInfo <- renderUI({HTML(values$descriptiveStatisticsInfoText)})
    #     updateCorrelationsTable()
    #   }
    # })
    # 
    # # Update filter table and values when the user hits the 'revert selection' button
    # observeEvent(input$revertSelectionButton, {
    #   values$lastParameterChangeTime = Sys.time()
    #   # Get the contents of the rhandsontable as data frame
    #   filterDF = hot_to_r(input$filterValTable)
    #   # Set checked modalities as unchecked and conversely
    #   filterDF[,3] = !as.logical(filterDF[,3])
    #   values$filterValues = filterDF[filterDF[,3]==TRUE,1]
    #   # Update table display
    #   output$filterValTable <- renderRHandsontable(
    #     rhandsontable(filterDF, selectCallback = FALSE, stretchH = "all", rowHeaderWidth = 0) %>%
    #       hot_col(col = 1, readOnly = TRUE) %>%
    #       hot_col(col = 2, readOnly = TRUE)  %>%
    #       hot_col(col = 3, type = "checkbox") %>%
    #       hot_cols(columnSorting = TRUE, colWidths = c(150, 50, 50))
    #   )
    #   # Update data and display
    #   values$filteredFiledata <- filterData()
    #   values$scatterplotData <- getScatterplotData()
    #   values$currentDatasetScatterplot <- plottedFigure()
    #   output$plotScatter <- renderPlot({values$currentDatasetScatterplot})
    #   values$descriptiveStatisticsInfoText <- descriptiveStatText()
    #   output$descriptiveStatisticsInfo <- renderUI({HTML(values$descriptiveStatisticsInfoText)})
    #   updateCorrelationsTable()
    # })
    
    # Checkbox: reverse the orientation of the X axis
    output$reverseXaxis <- renderUI({
      df <-values$filedata
      if (is.null(df)) return(NULL)
      checkboxInput("reverseXaxis", "X", FALSE, width="100%")
    })
    # Redraw figure on change of the checkbox state
    observeEvent(input$reverseXaxis, {
      if(values$reverseXaxis != input$reverseXaxis) {
        values$reverseXaxis = input$reverseXaxis
        values$currentDatasetScatterplot <- plottedFigure()
        output$plotScatter <- renderPlot({values$currentDatasetScatterplot})
      }
    })
    
    # Checkbox: reverse the orientation of the Y axis
    output$reverseYaxis <- renderUI({
      df <-values$filedata
      if (is.null(df)) return(NULL)
      checkboxInput("reverseYaxis", "Y", FALSE, width="100%")
    })
    # Redraw figure on change of the checkbox state
    observeEvent(input$reverseYaxis, {
      if(values$reverseYaxis != input$reverseYaxis) {
        values$reverseYaxis = input$reverseYaxis
        values$currentDatasetScatterplot <- plottedFigure()
        output$plotScatter <- renderPlot({values$currentDatasetScatterplot})
      }
    })
    
    # Update reactive value and displayed text when the number of decimal values to be displayed for X values changes
    observeEvent(input$nDecimalPlacesX, {
      if(is.na(input$nDecimalPlacesX) || values$nDecimalPlacesX != input$nDecimalPlacesX) {
        values$nDecimalPlacesX = input$nDecimalPlacesX
        values$descriptiveStatisticsInfoText <- descriptiveStatText()
        output$descriptiveStatisticsInfo <- renderUI({HTML(values$descriptiveStatisticsInfoText)})
      }
    })
    
    # Update reactive value and displayed text when the number of decimal values to be displayed for Y values changes
    observeEvent(input$nDecimalPlacesY, {
      if(is.na(input$nDecimalPlacesY) || values$nDecimalPlacesY != input$nDecimalPlacesY) {
        values$nDecimalPlacesY = input$nDecimalPlacesY
        values$descriptiveStatisticsInfoText <- descriptiveStatText()
        output$descriptiveStatisticsInfo <- renderUI({HTML(values$descriptiveStatisticsInfoText)})
      }
    })
    
    # Get a numeric vector with data to be plotted in the scatterplot, and update the plot limits according to values range if set to automatic
    getScatterplotData <- function(){
      
      if (is.null(input$datafile)) {
        return(NULL)
      }
      # Get the filtered data frame
      df <- filterData()
      
      if (is.null(df) || is.null(input$depVarX) || is.null(values$dependentVariableX) || values$dependentVariableX=="\u00A0" || values$dependentVariableX==0  || is.null(input$depVarY) || is.null(values$dependentVariableY) || values$dependentVariableY=="\u00A0" || values$dependentVariableY==0) {  
        return(NULL)
      }
      else {
        if(values$dependentVariableX != "" && values$dependentVariableY != "") {
          # Extract the 2 columns with the chosen dependent variables, discarding possible NA values
          allValuesXY = df[,c(values$dependentVariableX, values$dependentVariableY)]
          # Add columns with independent variable(s) if selected
          if(!is.null(values$independentVariable1) && values$independentVariable1!="\u00A0") {
            allValuesXY = cbind(allValuesXY, df[,values$independentVariable1])
            colnames(allValuesXY)[ncol(allValuesXY)] = values$independentVariable1
            # Replace empty cells by corresponding label
            allValuesXY[,values$independentVariable1] = as.character(allValuesXY[,values$independentVariable1])
            allValuesXY[is.na(allValuesXY[,values$independentVariable1]), values$independentVariable1] = getLabelOrPrompt("emptyCellsLabel", displayedLabelsAndPrompts)
            allValuesXY[,values$independentVariable1] = as.factor(allValuesXY[,values$independentVariable1])
          }
          if(!is.null(values$independentVariable2) && values$independentVariable2!="\u00A0" && (is.null(values$independentVariable1) || values$independentVariable2!=values$independentVariable1)) {
            allValuesXY = cbind(allValuesXY, df[,values$independentVariable2])
            colnames(allValuesXY)[ncol(allValuesXY)] = values$independentVariable2
            # Replace empty cells by corresponding label
            allValuesXY[,values$independentVariable2] = as.character(allValuesXY[,values$independentVariable2])
            allValuesXY[is.na(allValuesXY[,values$independentVariable2]), values$independentVariable2] = getLabelOrPrompt("emptyCellsLabel", displayedLabelsAndPrompts)
            allValuesXY[,values$independentVariable2] = as.factor(allValuesXY[,values$independentVariable2])
          }
          # # Discard unchecked modalities of independent variables
          # if(!is.null(values$independentVariable1) && values$independentVariable1!="\u00A0") {
          #   if(!is.null(values$indepVar1Values) && values$indepVar1Values != "")
          #     allValuesXY = allValuesXY[allValuesXY[,values$independentVariable1] %in% values$indepVar1Values,]
          #   else if(is.null(values$indepVar1Values))
          #     allValuesXY = allValuesXY[NULL,]
          # }
          # if(!is.null(values$independentVariable2) && values$independentVariable2!="\u00A0") {
          #   if(!is.null(values$indepVar2Values) && values$indepVar2Values != "")
          #     allValuesXY = allValuesXY[allValuesXY[,values$independentVariable2] %in% values$indepVar2Values,]
          #   else if(is.null(values$indepVar2Values))
          #     allValuesXY = allValuesXY[NULL,]
          # }
            
          # if(!is.null(values$independentVariable1) && values$independentVariable1!="\u00A0") {
          #   allValuesXY = df[,c(values$dependentVariableX, values$dependentVariableY, values$independentVariable1)]
          #   if(!is.null(values$indepVar1Values) && values$indepVar1Values != "")
          #     allValuesXY = allValuesXY[allValuesXY[,values$independentVariable1] %in% values$indepVar1Values,]
          #   else if(is.null(values$indepVar1Values))
          #     allValuesXY = allValuesXY[NULL,]
          # } else {
          #   allValuesXY = df[,c(values$dependentVariableX, values$dependentVariableY)]
          # }
            
          # Discard non-finite values of X or Y
          finiteValuesXY <- suppressWarnings(
            allValuesXY %>%
              mutate(across(all_of(c(values$dependentVariableX, values$dependentVariableY)), as.numeric)) %>% 
              filter(complete.cases(.))
          )
          # finiteValuesXY = suppressWarnings(allValuesXY[is.finite(as.numeric(as.character(allValuesXY[,values$dependentVariableX]))) & is.finite(as.numeric(as.character(allValuesXY[,values$dependentVariableY]))),])
          # finiteValuesXY[,values$dependentVariableX] = as.numeric(as.character(finiteValuesXY[,values$dependentVariableX]))
          # finiteValuesXY[,values$dependentVariableY] = as.numeric(as.character(finiteValuesXY[,values$dependentVariableY]))
          
          # Check that some valid points remain in the dataset
          if(nrow(finiteValuesXY)==0) {
            return(NULL)
          } else {
            # Update the min and max values if set to automatic mode
            if(is.na(values$scatterplotLowLimitX) || (!is.null(values$scatterplotLimitsMode) && values$scatterplotLimitsMode==getLabelOrPrompt("scatterplotLimitsModeChoiceAuto", displayedLabelsAndPrompts))) {
              minValX = min(finiteValuesXY[,values$dependentVariableX])
              maxValX = max(finiteValuesXY[,values$dependentVariableX])
              dataRangeX = maxValX - minValX
              # # add 1% of the range on each side to make sure all points will be displayed
              # minValX = minValX - dataRangeX*0.01
              # maxValX = maxValX + dataRangeX*0.01
              # dataRangeX = maxValX - minValX
              
              limitsInputStepX = 10^round(log10(dataRangeX/nStepsInScaleSettings))
              updateNumericInput(session, "scatterplotLowLimitX", value = minValX, step = limitsInputStepX)
              updateNumericInput(session, "scatterplotHighLimitX", value = maxValX, step = limitsInputStepX)
              
              minValY = min(finiteValuesXY[values$dependentVariableY])
              maxValY = max(finiteValuesXY[values$dependentVariableY])
              dataRangeY = maxValY - minValY
              # # add 1% of the range on each side to make sure all points will be displayed
              # minValY = minValY - dataRangeY*0.01
              # maxValY = maxValY + dataRangeY*0.01
              # dataRangeY = maxValY - minValY
              
              limitsInputStepY = 10^round(log10(dataRangeY/nStepsInScaleSettings))
              updateNumericInput(session, "scatterplotLowLimitY", value = minValY, step = limitsInputStepY)
              updateNumericInput(session, "scatterplotHighLimitY", value = maxValY, step = limitsInputStepY)
            }

            return(finiteValuesXY)
          }
        } else {
          return(NULL)
        }
      }
    }
    
    # Set the image format of the figure to be exported
    output$plotDownloadFormat <- renderUI({
      selectInput("plotDownloadFormat", getLabelOrPrompt("plotDownloadFormatLabel", displayedLabelsAndPrompts),availableOutputFormats)
    })
    
    # Set the width of the figure when exporting to image file
    output$exportedPlotWidth <- renderUI({
      numericInput("exportedPlotWidth", getLabelOrPrompt("exportedPlotWidthLabel", displayedLabelsAndPrompts), value = defaultExportedPlotWidth)
    })
    
    # Set the height of the figure when exporting to image file
    output$exportedPlotHeight <- renderUI({
      numericInput("exportedPlotHeight", getLabelOrPrompt("exportedPlotHeightLabel", displayedLabelsAndPrompts), value = defaultExportedPlotHeight)
    })
    
    # Set the units for figure export as image
    output$exportedPlotUnits <- renderUI({
      selectInput("exportedPlotUnits", getLabelOrPrompt("exportedPlotUnitsLabel", displayedLabelsAndPrompts),availableUnitsForImageExport)
    })
    
    # Allow the user to download the figure as an image file
    output$downloadPlot <- downloadHandler(
      filename = function() {
        # Get the default image file name using dataset file name
        paste0(tools::file_path_sans_ext(input$datafile),
               getLabelOrPrompt("scatterplotSubstringInExportedImageName", displayedLabelsAndPrompts),
               availableOutputFormatExtensions[which(availableOutputFormats == input$plotDownloadFormat)])
      },
      # Export the image with ggsave, using format and size parameters picked by the user
      content = function(file) {
        if(input$plotDownloadFormat=="EMF") {
          targetDevice = emf
        } else if(input$plotDownloadFormat=="PDF") {
          targetDevice = cairo_pdf
        } else if(input$plotDownloadFormat=="PNG") {
          targetDevice = png
        } else if(input$plotDownloadFormat=="EPS") {
          targetDevice = cairo_ps
        } else {
          targetDevice = NULL
        }
          
        ggsave(file, plot = plottedFigure(), device = targetDevice, width = input$exportedPlotWidth, height = input$exportedPlotHeight, units = exportedImagesUnit)
      }
    )
    
    updateCorrelationsTable <- function() {
      output$correlationsTable <- renderRHandsontable(
        rhandsontable(correlationsTableContents(), selectCallback = FALSE, rowHeaderWidth = 0) %>%
          hot_cols(readOnly = TRUE, columnSorting = TRUE, halign = "htCenter")
      )
    }
    
    correlationsTableContents <- function() {
      if(!is.null(input$datafile)) {
        currentDataset = values$scatterplotData
        if(!is.null(currentDataset)) {
          # Get correlations for each subset of independent variables 1 * 2 (if defined)
          donneesCorr <- currentDataset %>% 
            group_by(across(any_of(c(values$independentVariable1, values$independentVariable2)))) %>% 
            summarise(
              !!getLabelOrPrompt("correlationsTablePearsonCorrelationLabel", displayedLabelsAndPrompts) := cor(!!sym(values$dependentVariableX), !!sym(values$dependentVariableY), method = "pearson"),
              !!getLabelOrPrompt("correlationsTableSpearmanCorrelationLabel", displayedLabelsAndPrompts) := cor(!!sym(values$dependentVariableX), !!sym(values$dependentVariableY), method = "spearman"),
              !!getLabelOrPrompt("correlationsTableNvaluesLabel", displayedLabelsAndPrompts) := n(),
              .groups = "drop"
            )
          
          # Get correlations after pooling together all modalities of independent variables 1 and/or 2
          if(!is.null(values$independentVariable1) && values$independentVariable1!="\u00A0") {
            donneesCorr_indepVar1_merged <- currentDataset
            if(!is.null(values$independentVariable2) && values$independentVariable1!=values$independentVariable2) {
              donneesCorr_indepVar1_merged <- donneesCorr_indepVar1_merged %>% 
                group_by(across(any_of(c(values$independentVariable2))))
            }
            donneesCorr_indepVar1_merged <- donneesCorr_indepVar1_merged %>% 
              summarise(
                !!getLabelOrPrompt("correlationsTablePearsonCorrelationLabel", displayedLabelsAndPrompts) := cor(!!sym(values$dependentVariableX), !!sym(values$dependentVariableY), method = "pearson"),
                !!getLabelOrPrompt("correlationsTableSpearmanCorrelationLabel", displayedLabelsAndPrompts) := cor(!!sym(values$dependentVariableX), !!sym(values$dependentVariableY), method = "spearman"),
                !!getLabelOrPrompt("correlationsTableNvaluesLabel", displayedLabelsAndPrompts) := n(),
                .groups = "drop"
              ) %>% 
              mutate(!!values$independentVariable1 := getLabelOrPrompt("correlationsTablePooledModalitiesLabel", displayedLabelsAndPrompts))
          } else {
            donneesCorr_indepVar1_merged <- donneesCorr %>% 
              filter(FALSE)
          }
          if(!is.null(values$independentVariable2) && values$independentVariable2!="\u00A0" && (!is.null(values$independentVariable1) && values$independentVariable1!=values$independentVariable2)) {
            donneesCorr_indepVar2_merged <- currentDataset %>% 
              group_by(across(any_of(c(values$independentVariable1)))) %>% 
              summarise(
                !!getLabelOrPrompt("correlationsTablePearsonCorrelationLabel", displayedLabelsAndPrompts) := cor(!!sym(values$dependentVariableX), !!sym(values$dependentVariableY), method = "pearson"),
                !!getLabelOrPrompt("correlationsTableSpearmanCorrelationLabel", displayedLabelsAndPrompts) := cor(!!sym(values$dependentVariableX), !!sym(values$dependentVariableY), method = "spearman"),
                !!getLabelOrPrompt("correlationsTableNvaluesLabel", displayedLabelsAndPrompts) := n(),
                .groups = "drop"
              ) %>% 
              mutate(!!values$independentVariable2 := getLabelOrPrompt("correlationsTablePooledModalitiesLabel", displayedLabelsAndPrompts))
          } else {
            donneesCorr_indepVar2_merged <- donneesCorr %>% 
              filter(FALSE)
          }
          if(!is.null(values$independentVariable1) && values$independentVariable1!="\u00A0" && !is.null(values$independentVariable2) && values$independentVariable2!="\u00A0" && values$independentVariable2!=values$independentVariable1) {
            donneesCorr_indepVars_merged <- currentDataset %>% 
              summarise(
                !!getLabelOrPrompt("correlationsTablePearsonCorrelationLabel", displayedLabelsAndPrompts) := cor(!!sym(values$dependentVariableX), !!sym(values$dependentVariableY), method = "pearson"),
                !!getLabelOrPrompt("correlationsTableSpearmanCorrelationLabel", displayedLabelsAndPrompts) := cor(!!sym(values$dependentVariableX), !!sym(values$dependentVariableY), method = "spearman"),
                !!getLabelOrPrompt("correlationsTableNvaluesLabel", displayedLabelsAndPrompts) := n(),
                .groups = "drop"
              ) %>% 
              mutate(
                !!values$independentVariable1 := getLabelOrPrompt("correlationsTablePooledModalitiesLabel", displayedLabelsAndPrompts),
                !!values$independentVariable2 := getLabelOrPrompt("correlationsTablePooledModalitiesLabel", displayedLabelsAndPrompts)
              )
          } else {
            donneesCorr_indepVars_merged <- donneesCorr %>% 
              filter(FALSE)
          }
          
          # Bind subsets
          donneesCorr <- donneesCorr %>% 
            bind_rows(donneesCorr_indepVar1_merged) %>% 
            bind_rows(donneesCorr_indepVar2_merged) %>% 
            bind_rows(donneesCorr_indepVars_merged)
          
          return(donneesCorr)
        } else {
          return(NULL)
        }
      } else {
        return(NULL)
      }
    }
    
    # Allow the user to download the correlations table as an Excel file
    output$downloadCorrelationsTable <- downloadHandler(
      filename = function() {
        # Get the default image file name using dataset file name
        str_c(
          getLabelOrPrompt("exportedCorrelationsTableDefaultPrefix", displayedLabelsAndPrompts),
          "_",
          tools::file_path_sans_ext(input$datafile),
          ".xlsx"
        )
      },
      # Export the image with ggsave, using format and size parameters picked by the user
      content = function(file) {
        write_xlsx(correlationsTableContents(), path = file, format_headers = F)
      }
    )
    
    # Get descriptive statistics on the current data subset and format information in a characer string
    descriptiveStatText <- function() {
      if(!is.null(input$datafile)) {
        currentDataset = values$scatterplotData
        if(!is.null(currentDataset)) {
          # Compute mode and range not directly given by base functions
          uniqueValsX = unique(currentDataset %>% pull(all_of(values$dependentVariableX)))
          modeInCurrentDatasetX = uniqueValsX[which.max(tabulate(match(currentDataset %>% pull(all_of(values$dependentVariableX)), uniqueValsX)))]
          rangeInCurrentDatasetX = max(currentDataset %>% pull(all_of(values$dependentVariableX))) - min(currentDataset %>% pull(all_of(values$dependentVariableX)))
          
          uniqueValsY = unique(currentDataset %>% pull(all_of(values$dependentVariableY)))
          modeInCurrentDatasetY = uniqueValsY[which.max(tabulate(match(currentDataset %>% pull(all_of(values$dependentVariableY)), uniqueValsY)))]
          rangeInCurrentDatasetY = max(currentDataset %>% pull(all_of(values$dependentVariableY))) - min(currentDataset %>% pull(all_of(values$dependentVariableY)))
          
          nDecimalPlacesX = values$nDecimalPlacesX
          nDecimalPlacesY = values$nDecimalPlacesY
          
          # HTML text formatting
          return(paste0(
            "<p><b>", getLabelOrPrompt("descriptiveStatisticsGeneralInfo", displayedLabelsAndPrompts), "</b></p>",
            "<p>", getLabelOrPrompt("nValuesPrompt", displayedLabelsAndPrompts), length(currentDataset %>% pull(all_of(values$dependentVariableX))), "</p>",
            "<p><b>", getLabelOrPrompt("xVariableInDescriptiveStats", displayedLabelsAndPrompts), " ", values$dependentVariableX, "</b><br>",
            getLabelOrPrompt("meanValuePrompt", displayedLabelsAndPrompts), round(mean(currentDataset %>% pull(all_of(values$dependentVariableX))), digits = nDecimalPlacesX), " — ",
            getLabelOrPrompt("sdValuePrompt", displayedLabelsAndPrompts), round(sd(currentDataset %>% pull(all_of(values$dependentVariableX))), digits = nDecimalPlacesX), "<br>",
            getLabelOrPrompt("medianValuePrompt", displayedLabelsAndPrompts), round(median(currentDataset %>% pull(all_of(values$dependentVariableX))), digits = nDecimalPlacesX), " — ",
            getLabelOrPrompt("firstQuartileValuePrompt", displayedLabelsAndPrompts), round(quantile(currentDataset %>% pull(all_of(values$dependentVariableX)),.25), digits = nDecimalPlacesX), " — ",
            getLabelOrPrompt("thirdQuartileValuePrompt", displayedLabelsAndPrompts), round(quantile(currentDataset %>% pull(all_of(values$dependentVariableX)),.75), digits = nDecimalPlacesX), " — ",
            getLabelOrPrompt("interquartileRangeValuePrompt", displayedLabelsAndPrompts), round(quantile(currentDataset %>% pull(all_of(values$dependentVariableX)),.75) - quantile(currentDataset %>% pull(all_of(values$dependentVariableX)),.25), digits = nDecimalPlacesX), "<br>",
            getLabelOrPrompt("minValuePrompt", displayedLabelsAndPrompts), round(min(currentDataset %>% pull(all_of(values$dependentVariableX))), digits = nDecimalPlacesX), " — ",
            getLabelOrPrompt("maxValuePrompt", displayedLabelsAndPrompts), round(max(currentDataset %>% pull(all_of(values$dependentVariableX))), digits = nDecimalPlacesX), " — ",
            getLabelOrPrompt("rangeValuePrompt", displayedLabelsAndPrompts), round(rangeInCurrentDatasetX, digits = nDecimalPlacesX), " — ",
            getLabelOrPrompt("modeValuePrompt", displayedLabelsAndPrompts), round(modeInCurrentDatasetX, digits = nDecimalPlacesX),
            "</p>",
            "<p><b>", getLabelOrPrompt("yVariableInDescriptiveStats", displayedLabelsAndPrompts), " ", values$dependentVariableY, "</b><br>",
            getLabelOrPrompt("meanValuePrompt", displayedLabelsAndPrompts), round(mean(currentDataset %>% pull(all_of(values$dependentVariableY))), digits = nDecimalPlacesY), " — ",
            getLabelOrPrompt("sdValuePrompt", displayedLabelsAndPrompts), round(sd(currentDataset %>% pull(all_of(values$dependentVariableY))), digits = nDecimalPlacesY), "<br>",
            getLabelOrPrompt("medianValuePrompt", displayedLabelsAndPrompts), round(median(currentDataset %>% pull(all_of(values$dependentVariableY))), digits = nDecimalPlacesY), " — ",
            getLabelOrPrompt("firstQuartileValuePrompt", displayedLabelsAndPrompts), round(quantile(currentDataset %>% pull(all_of(values$dependentVariableY)),.25), digits = nDecimalPlacesY), " — ",
            getLabelOrPrompt("thirdQuartileValuePrompt", displayedLabelsAndPrompts), round(quantile(currentDataset %>% pull(all_of(values$dependentVariableY)),.75), digits = nDecimalPlacesY), " — ",
            getLabelOrPrompt("interquartileRangeValuePrompt", displayedLabelsAndPrompts), round(quantile(currentDataset %>% pull(all_of(values$dependentVariableY)),.75) - quantile(currentDataset %>% pull(all_of(values$dependentVariableY)),.25), digits = nDecimalPlacesY), "<br>",
            getLabelOrPrompt("minValuePrompt", displayedLabelsAndPrompts), round(min(currentDataset %>% pull(all_of(values$dependentVariableY))), digits = nDecimalPlacesY), " — ",
            getLabelOrPrompt("maxValuePrompt", displayedLabelsAndPrompts), round(max(currentDataset %>% pull(all_of(values$dependentVariableY))), digits = nDecimalPlacesY), " — ",
            getLabelOrPrompt("rangeValuePrompt", displayedLabelsAndPrompts), round(rangeInCurrentDatasetY, digits = nDecimalPlacesY), " — ",
            getLabelOrPrompt("modeValuePrompt", displayedLabelsAndPrompts), round(modeInCurrentDatasetY, digits = nDecimalPlacesY),
            "</p>"
          ))
        } else {
          return("")
        }
      } else {
        return("")
      }
    }
    
    # When clicking on the figure, get the closest point, and display corresponding details in the table
    observeEvent(input$plot_click, {
      currentDataset = values$scatterplotData
      currentDatasetScatterplot = values$currentDatasetScatterplot
      
      # Update the plot to remove point highlight if displayed
      values$currentDatasetScatterplot = delete_layers(delete_layers(values$currentDatasetScatterplot, "GeomVline"), "GeomHline")
      output$plotScatter <- renderPlot({values$currentDatasetScatterplot})
      
      # Get detailed info about the plot using ggplot_build
      currentDatasetScatterplotInfo=ggplot_build(currentDatasetScatterplot)
      # Get the plot limits to normalize distance from displayed points to click position
      x.range = currentDatasetScatterplotInfo$layout$coord$limits$x
      y.range = currentDatasetScatterplotInfo$layout$coord$limits$y
      # Adjust x and/or y range if axes orientation are reversed
      if(values$reverseXaxis)
        x.range = c(-x.range[2],-x.range[1])
      if(values$reverseYaxis)
        y.range = c(-y.range[2],-y.range[1])
      if(!is.null(input$plot_click) && !is.null(input$plot_click$x) && is.finite(input$plot_click$x)) {
        currentDatasetScatterplotData = currentDatasetScatterplotInfo$data[[1]]
        # Get the row corresponding to the closest point in the original dataframe
        # (if several points are at the same minimal distance from clicked point, they will all be included)
        currentDatasetFull = values$filteredFiledata
        if(values$nonNumericColsIncludedInDependentVariableChoiceFlag && (!is.numeric(currentDatasetFull[values$dependentVariableX]) || !is.numeric(currentDatasetFull[values$dependentVariableY]))) {
          numConverted_currentDatasetFull <- suppressWarnings(
            currentDatasetFull %>%
              mutate(across(all_of(c(values$dependentVariableX, values$dependentVariableY)), as.numeric)) # %>% 
              # filter(complete.cases(.))
          )
          valuesX <- numConverted_currentDatasetFull %>% pull(all_of(values$dependentVariableX))
          valuesY <- numConverted_currentDatasetFull %>% pull(all_of(values$dependentVariableY))
          # valuesX = suppressWarnings(as.numeric(as.character(currentDatasetFull %>% pull(all_of(values$dependentVariableX)))))
          # valuesY = suppressWarnings(as.numeric(as.character(currentDatasetFull %>% pull(all_of(values$dependentVariableX)))))
        } else {
          valuesX <- currentDatasetFull %>% pull(all_of(values$dependentVariableX))
          valuesY <- currentDatasetFull %>% pull(all_of(values$dependentVariableY))
        }

        # Normalize distances to account for scale differences
        diffX <- (valuesX - input$plot_click$x)/diff(x.range)
        diffY <- (valuesY - input$plot_click$y)/diff(y.range)
        euclideanDistanceToClickedPoint <- sqrt(diffX * diffX + diffY * diffY)
        minDistToClickedPointIndices <- which(euclideanDistanceToClickedPoint == min(euclideanDistanceToClickedPoint, na.rm = T))
        clickedSubset <- currentDatasetFull[minDistToClickedPointIndices,]
        values$clickedSubset <- clickedSubset
        
        # Update table display
        if(!is.null(values$clickedSubset) && nrow(values$clickedSubset)>0) {
          output$selectedDataDetailsTable <- renderRHandsontable({
            rhandsontable(values$clickedSubset, selectCallback = TRUE, readOnly = TRUE, stretchH = "all", rowHeaderWidth = 0) %>% 
              hot_table(highlightRow = TRUE) %>% 
              hot_cols(columnSorting = TRUE)
            })
          values$selectedRowInClickedSubset = NULL
        } else {
          output$selectedDataDetailsTable <- renderRHandsontable({NULL})
        }
        
        # If the clicked point corresponds to a single row, highlight it on the plot
        if(!is.null(values$clickedSubset) && nrow(values$clickedSubset)==1) {
          # Get the selected row
          highlightedPointXval = as.numeric(as.character(values$clickedSubset[1, values$dependentVariableX]))
          highlightedPointYval = as.numeric(as.character(values$clickedSubset[1, values$dependentVariableY]))
          # Draw a cross on the plot to highlight the selected point
          values$currentDatasetScatterplot = values$currentDatasetScatterplot +
            geom_vline(xintercept = highlightedPointXval, colour = "grey40", linetype = "dashed") +
            geom_hline(yintercept = highlightedPointYval, colour = "grey40", linetype = "dashed")
          output$plotScatter <- renderPlot({values$currentDatasetScatterplot})
        }
      }
    })
    
    # When a rectangular region is selected, get the corresponding data subset and display it in the table
    observeEvent(input$plot_brush, {
      currentDataset <- values$scatterplotData
      currentDatasetScatterplot <- values$currentDatasetScatterplot
      
      # Update the plot the remove point higlight if displayed
      values$currentDatasetScatterplot <- delete_layers(delete_layers(values$currentDatasetScatterplot, "GeomVline"), "GeomHline")
      output$plotScatter <- renderPlot({values$currentDatasetScatterplot})
      
      # Get detailed info about the plot using ggplot_build
      currentDatasetScatterplotInfo=ggplot_build(currentDatasetScatterplot)
      # Check that the selected region is whithin limits
      x.range <- currentDatasetScatterplotInfo$layout$coord$limits$x
      y.range <- currentDatasetScatterplotInfo$layout$coord$limits$y
      # Adjust x and/or y range if axes orientation are reversed
      if(values$reverseXaxis)
        x.range <- c(-x.range[2],-x.range[1])
      if(values$reverseYaxis)
        y.range <- c(-y.range[2],-y.range[1])
      # if(!is.null(input$plot_brush$xmin) && is.finite(input$plot_brush$xmin) && input$plot_brush$xmin>=x.range[1]  && input$plot_brush$xmax<=x.range[2] && input$plot_brush$ymin>=y.range[1]  && input$plot_brush$ymax<=y.range[2]) {
      if(!is.null(input$plot_brush$xmin) && is.finite(input$plot_brush$xmin) && !is.null(input$plot_brush$xmax) && is.finite(input$plot_brush$xmax) && !is.null(input$plot_brush$ymin) && is.finite(input$plot_brush$ymin) && !is.null(input$plot_brush$ymax) && is.finite(input$plot_brush$ymax)) {
        currentDatasetScatterplotData <- currentDatasetScatterplotInfo$data[[1]]
        # Get the rows corresponding to the selected region in the original dataframe
        currentDatasetFull <- values$filteredFiledata
        if(values$nonNumericColsIncludedInDependentVariableChoiceFlag!=0 && !is.numeric(currentDatasetFull[values$dependentVariable])) {
          # valuesX <- suppressWarnings(as.numeric(as.character(currentDatasetFull[,values$dependentVariableX])))
          # valuesY <- suppressWarnings(as.numeric(as.character(currentDatasetFull[,values$dependentVariableY])))
          valuesX <- suppressWarnings(as.numeric(as.character(currentDatasetFull %>% pull(values$dependentVariableX))))
          valuesY <- suppressWarnings(as.numeric(as.character(currentDatasetFull %>% pull(values$dependentVariableY))))
        } else {
          # valuesX <- currentDatasetFull[,values$dependentVariableX]
          # valuesY <- currentDatasetFull[,values$dependentVariableY]
          valuesX <- currentDatasetFull %>% pull(values$dependentVariableX)
          valuesY <- currentDatasetFull %>% pull(values$dependentVariableY)
        }
        clickedSubset <- currentDatasetFull[is.finite(valuesX) & is.finite(valuesY) & valuesX>=input$plot_brush$xmin & valuesX<=input$plot_brush$xmax & valuesY>=input$plot_brush$ymin & valuesY<=input$plot_brush$ymax,]
        
        values$clickedSubset <- clickedSubset
        
        # Update table display
        if(!is.null(values$clickedSubset) && nrow(values$clickedSubset)>0) {
          output$selectedDataDetailsTable <- renderRHandsontable({
            rhandsontable(values$clickedSubset, selectCallback = TRUE, readOnly = TRUE, stretchH = "all", rowHeaderWidth = 0) %>% 
              hot_table(highlightRow = TRUE) %>% 
              hot_cols(columnSorting = TRUE)
          })
          values$selectedRowInClickedSubset = NULL
        } else {
          output$selectedDataDetailsTable <- renderRHandsontable({NULL})
        }
      }
    })
    
    
    # When a row is selected in the table, store its index to highlight the corresponding point when plotting the figure
    observeEvent(input$selectedDataDetailsTable_select, {
      # Get the selected row index
      values$selectedRowInClickedSubset = input$selectedDataDetailsTable_select$select$r
      # Update the figure display
      values$currentDatasetScatterplot = delete_layers(delete_layers(values$currentDatasetScatterplot, "GeomVline"), "GeomHline")
      # values$currentDatasetScatterplot <- plottedFigure()
      
      # If a row is selected on the table, highlight it on the plot
      if(!is.null(values$clickedSubset) && nrow(values$clickedSubset)>0 && !is.null(values$selectedRowInClickedSubset)) {
        # Get the indices of the X and Y cols in the table
        clickedSubsetColnames <- colnames(values$clickedSubset)
        xColIndexInClickedSubset <- which(values$dependentVariableX == clickedSubsetColnames)
        yColIndexInClickedSubset <- which(values$dependentVariableY == clickedSubsetColnames)
        # Get the selected row
        highlightedPointXval = as.numeric(as.character(input$selectedDataDetailsTable_select$data[[values$selectedRowInClickedSubset]][[xColIndexInClickedSubset]]))
        highlightedPointYval = as.numeric(as.character(input$selectedDataDetailsTable_select$data[[values$selectedRowInClickedSubset]][[yColIndexInClickedSubset]]))
        # Draw a cross on the plot to highlight the selected point
        values$currentDatasetScatterplot = values$currentDatasetScatterplot +
          geom_vline(xintercept = highlightedPointXval, colour = "grey40", linetype = "dashed") +
          geom_hline(yintercept = highlightedPointYval, colour = "grey40", linetype = "dashed")
      }
      
      output$plotScatter <- renderPlot({values$currentDatasetScatterplot})
    })
    
    # Build the scatterplot
    # Use reactivePoll to detect changes in data or parameters which require figure update, taking last parameter change time as reference.
    plottedFigure <- reactivePoll(figureDisplayRefreshRateMilliseconds, session, checkFunc = function() {
      values$lastParameterChangeTime
    },
    valueFunc = function() {
      # Build the figure
      if(!is.null(input$datafile) && !is.null(values$filteredFiledata)) {
        currentDataset = values$scatterplotData
        
        if(!is.null(currentDataset) && nrow(currentDataset)>0) {
        # if(!is.null(currentDataset) && !is.na(currentDataset) && nrow(currentDataset)>0) {
          par(mar=c(5,3,2,2)+0.1) # No additional space aroud the scatterplot
          
          indepVar1defined <- FALSE
          indepVar2defined <- FALSE
          # Set points colors depending on independent variable 1 if selected
          if(!is.null(values$independentVariable1) && values$independentVariable1!="\u00A0") {
            indepVar1defined <- TRUE
          }
          # Set points shape (and regressions lines type if requested) depending on independent variable 2 if selected
          if(!is.null(values$independentVariable2) && values$independentVariable2!="\u00A0") {
            indepVar2defined <- TRUE
          }
          # Build base figure
          if(indepVar1defined) {
            if(indepVar2defined) {
              # indepVar1defined & indepVar2defined
              currentDatasetFigure <- currentDataset %>% ggplot(aes(x = .data[[values$dependentVariableX]], y = .data[[values$dependentVariableY]], colour = .data[[values$independentVariable1]], shape = .data[[values$independentVariable2]], linetype = .data[[values$independentVariable2]]))
            } else {
              # indepVar1defined & !indepVar2defined
              currentDatasetFigure <- currentDataset %>% ggplot(aes(x = .data[[values$dependentVariableX]], y = .data[[values$dependentVariableY]], colour = .data[[values$independentVariable1]]))
            }
          } else {
            if(indepVar2defined) {
              # !indepVar1defined & indepVar2defined
              currentDatasetFigure <- currentDataset %>% ggplot(aes(x = .data[[values$dependentVariableX]], y = .data[[values$dependentVariableY]], shape = .data[[values$independentVariable2]], linetype = .data[[values$independentVariable2]]))
            } else {
              # !indepVar1defined & !indepVar2defined
              currentDatasetFigure <- currentDataset %>% ggplot(aes(x = .data[[values$dependentVariableX]], y = .data[[values$dependentVariableY]]))
            }
          }
          
          # Display regression lines if requested
          if(values$displayRegressionLines) {
            currentDatasetFigure <- currentDatasetFigure +
              geom_smooth(linewidth = values$regressionLinesSize, method = "lm", formula = y ~ x, se = FALSE)
          }
          # Display points
          currentDatasetFigure <- currentDatasetFigure +
            geom_point(size = values$pointsSize)
          
          # # Set points colors depending on independent variable 1 if selected
          # if(!is.null(values$independentVariable1) && values$independentVariable1!="\u00A0") {
          #   plotColors = unlist(values$scatterplotData[,values$independentVariable1])
          # } else
          #   plotColors = rep(scatterplotPointsDefaultColor, nrow(currentDataset))
          # # Set points shape depending on independent variable 2 if selected
          # if(!is.null(values$independentVariable2) && values$independentVariable2!="\u00A0") {
          #   plotShapes = unlist(values$scatterplotData[,values$independentVariable2])
          # } else
          #   plotShapes = rep(scatterplotPointsDefaultShape, nrow(currentDataset))
          # 
          # df = data.frame(
          #   x = currentDataset %>% pull(all_of(values$dependentVariableX)),
          #   y = currentDataset %>% pull(all_of(values$dependentVariableY)),
          #   plotColors = plotColors,
          #   plotShapes = plotShapes
          # )
          # 
          # # Build base figure
          # currentDatasetFigure <- ggplot(df, aes(x = x, y = y))
          # 
          # # Display regression lines if requested
          # if(values$displayRegressionLines) {
          #   currentDatasetFigure <- currentDatasetFigure +
          #     geom_smooth(aes(colour = factor(plotColors), linetype = factor(plotShapes)), size = values$regressionLinesSize, method = "lm", formula = y ~ x, se = FALSE)
          # }
          # # Display points
          # currentDatasetFigure <- currentDatasetFigure +
          #   geom_point(aes(colour = factor(plotColors), shape = factor(plotShapes)), size = values$pointsSize)
          # 
          # # Show legend for independent variable 1 only if selected
          # if(!is.null(values$independentVariable1) && values$independentVariable1!="\u00A0")
          #   currentDatasetFigure <- currentDatasetFigure +
          #     scale_colour_discrete(name = values$independentVariable1)
          # else
          #   currentDatasetFigure <- currentDatasetFigure +
          #     scale_colour_discrete(guide = "none")
          # 
          # # Show legend for independent variable 2 only if selected
          # if(!is.null(values$independentVariable2) && values$independentVariable2!="\u00A0")
          #   currentDatasetFigure <- currentDatasetFigure +
          #   scale_shape_discrete(name = values$independentVariable2)
          # else
          #   currentDatasetFigure <- currentDatasetFigure +
          #     scale_shape_discrete(guide = "none")

          # Apply general graphical parameters to the plot to make it nicer
          currentDatasetFigure <- currentDatasetFigure +
            theme_bw() +
            theme(panel.border=element_rect(fill=NA, colour = "grey")) +
            theme(text = element_text(size=values$figureTextSize), axis.text.x = element_text(size=.8*values$figureTextSize), axis.text.y = element_text(size=.8*values$figureTextSize)) +
            xlab(values$dependentVariableX) +
            ylab(values$dependentVariableY) +
            coord_cartesian(
              xlim = c(values$scatterplotLowLimitX, values$scatterplotHighLimitX),
              ylim = c(values$scatterplotLowLimitY, values$scatterplotHighLimitY)
              )
          
          # Reverse axes if requested
          if(values$reverseXaxis)
            currentDatasetFigure <- currentDatasetFigure +
              scale_x_reverse()
          if(values$reverseYaxis)
            currentDatasetFigure <- currentDatasetFigure +
            scale_y_reverse()
          
          # # If a row is selected on the table with selected points details, higlight it on the plot
          # if(!is.null(values$clickedSubset) && nrow(values$clickedSubset)>0 && !is.null(values$selectedRowInClickedSubset)) {
          #   # Get the selected row
          #   highlightedPointXval = as.numeric(as.character(values$clickedSubset[values$selectedRowInClickedSubset, values$dependentVariableX]))
          #   highlightedPointYval = as.numeric(as.character(values$clickedSubset[values$selectedRowInClickedSubset, values$dependentVariableY]))
          #   # Draw a cross on the plot to highlight the selected point
          #   currentDatasetFigure = currentDatasetFigure +
          #     geom_vline(xintercept = highlightedPointXval, colour = "grey70", linetype = "dashed") +
          #     geom_hline(yintercept = highlightedPointYval, colour = "grey70", linetype = "dashed")
          # }
          
          return(currentDatasetFigure)
        } else {
          return(NULL)
        }
      } else {
        return(NULL)
      }
    })
    
    # Display the figure
    output$plotScatter <- renderPlot({
      if(!is.null(values$currentDatasetScatterplot)) {
        values$currentDatasetScatterplot
      }
    })

  })
