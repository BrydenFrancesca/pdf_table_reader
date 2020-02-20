setwd("L:/Prices/Dashboards/PDF_reading")
if (!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load(dplyr, data.table, pdftools, shiny, writexl, DT, tidyr, tibble)

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("PDF reader"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(width = 2,
      
      # Input: Select a file ----
      fileInput("pdf_input_file", "Choose PDF of current month data",
                multiple = F,
                accept = c("pdf",
                           ".pdf")),
      conditionalPanel("output.fileUploaded != 0", uiOutput("table_selector")),
      radioButtons("auto_manual_columns", label = h5("Define columns automatically or manually?"),
                   choices = list("Automatic" = 1, "Manual" = 2), 
                   selected = 1),
      conditionalPanel("output.fileUploaded != 0", uiOutput("header_column")),
      conditionalPanel("input.auto_manual_columns == 1", textInput("column_number", label = h5("Number of divisions"), value = 2)),
      sliderInput("row_height_slider", 
                  label = h5("Select to account for smaller or taller rows"), 
                  min = 0, 
                  max = 4,
                  step = 0.1,
                  value = 1),
      textInput("trim_top", label = h5("Number of rows to trim from top"), value = 0),
      textInput("trim_bottom", label = h5("Number of rows to trim from bottom"), value = 0),
      # Horizontal line ----
      hr(),
      hr(),
      
      ##download results
     downloadButton("dl_oilseed", "Download")
     ), #End of sidebar Panel
    
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("PDF output",
                           # Output: Data file ----
                           DTOutput("pdf_output_file"))
                  )#end of tabset
      )#end of main panel
    )#end of sidebar layout
  )#end of fluid page
# Define server logic to read selected file ----
server <- function(input, output) {
  ##Hide selectors when no file
  getData <- reactive({
    if(is.null(input$pdf_input_file)) return(NULL)
    ......
  })
  output$fileUploaded <- reactive({
    return(!is.null(getData()))
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
  ##Conditional selectors
  ##Table number selector
  output$table_selector = renderUI({
    sliderInput("table_number_slider", 
                label = h5("Select table number in PDF"), 
                min = 0, 
                max = total_tables(),
                round = T,
                value = 1)
  })
  
  output$header_column = renderUI({
    sliderInput("header_column_slider", 
                label = h5("Select width of header column"), 
                min = 2, 
                max = max(select_table()$x), 
                round = T,
                value = 20)
  })
  
  ##Numeric values from user input
  #Which table in PDF to select
  table_number_value <- reactive({as.numeric(input$table_number_slider)})
  #Header column width
  header_column_value <- reactive({as.numeric(input$header_column_slider)})
  #How many columns to give table
  column_number_auto <- reactive({as.numeric(input$column_number)}) 
  ##Height of rows
  row_height <- reactive({as.numeric(input$row_height_slider)})
  #Trim values:
  top_trim <- reactive({as.numeric(input$trim_top)+1})
  bottom_trim <- reactive({nrow(processed_pdf())-as.numeric(input$trim_bottom)})
  
  
  ##Output datatable of raw data  
  total_rows = reactive({processed_pdf() %>% nrow()})
  output$pdf_output_file <- renderDT({
    datatable(trimmed_pdf(), rownames = FALSE,
              options = list(
                scrollX = TRUE,
                scrollY = TRUE,
                pageLength = total_rows()))
  })
  
  ##Read in pdf
  raw_pdf = reactive({
    pdf_data(input$pdf_input_file$datapath)
  })
  
  ##Count number of tables in pdf
  total_tables = reactive({as.numeric(length(raw_pdf()))})
  
  #Select table to read
  select_table = reactive({as_tibble(raw_pdf()[[table_number_value()]])})  
  
  ##Data processing
  processed_pdf = reactive({
    validate(
      need(input$pdf_input_file != "", "Please select a PDF"))
    ##Identify columns of interest
    if(input$auto_manual_columns == 1){
    find_cols <- seq(from = header_column_value(), to = max(select_table()$x), length.out = column_number_auto())}
    
    #Calculate number of rows
    row_n <- round(length(unique(select_table()$y))/row_height(), 0)+1
    ##Identify rows of interest
    find_rows <- seq(from = min(select_table()$y), to = max(select_table()$y), length.out = row_n)
    
    ###Split into columns
    data <-  select_table() %>% 
      mutate(col = cut(x, breaks = c(1, find_cols, Inf))) %>% 
      mutate(row = cut(y, breaks = c(1, find_rows, Inf))) %>%
      arrange(col, row) %>%
      group_by(col, row) %>%
      mutate(text = paste(text, collapse = " ")) %>%
      ungroup() %>%
      select(row, text, col) %>%
      unique() %>%
      spread(col, text) 
  
    }) #end of reactive
  
  trimmed_pdf = reactive({
    data <- processed_pdf()[top_trim():bottom_trim(),]
    data <- Filter(function(x)!all(is.na(x)), data)
    data
    })
  
  output$dl_oilseed <- downloadHandler(
    filename = function() {"read_pdf.xlsx"},
    content = function(file) {write_xlsx(list(trimmed_pdf()), path = file)}
  )
  }
# Run the app ----
shinyApp(ui, server)
