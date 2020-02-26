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
      radioButtons("auto_manual_columns", label = h5("Column width setting?"),
                   choices = list("Automatic" = 1, "Advanced" = 2, "From file" = 3), 
                   selected = 1,
                   inline = T),
      conditionalPanel("input.auto_manual_columns == 2", radioButtons("number_headers", label = h5("Select number of header colums"),
                                                                choices = list(1, 2, 3), 
                                                                selected = 1,
                                                                inline = T)),
                       
      conditionalPanel("output.fileUploaded != 0", uiOutput("header_column")),
      textInput("column_number", label = h5("Number of divisions"), value = 2),
      conditionalPanel("input.auto_manual_columns == 2", sliderInput("row_height_slider", 
                  label = h5("Select to account for smaller or taller rows"), 
                  min = 0, 
                  max = 4,
                  step = 0.1,
                  value = 1)),
      conditionalPanel("input.auto_manual_columns == 2", textInput("header_rows", label = h5("Number of rows to include in headers"), value = 1)),
      textInput("trim_top", label = h5("Number of rows to trim from top"), value = 0),
      textInput("trim_bottom", label = h5("Number of rows to trim from bottom"), value = 0),
      conditionalPanel("input.auto_manual_columns == 2", checkboxInput("set_col_names", label = "Use header row as column names?", value = FALSE)),
      checkboxInput("keep_params", label = "Download a copy of these parameters?", value = FALSE),
    
      # Horizontal line ----
      hr(),
      hr(),
      
      ##download results
      downloadButton("dl_xl", "Download xlsx"),
      downloadButton("dl_csv", "Download csv")
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
    if(input$auto_manual_columns == 1){
    sliderInput("header_column_slider", 
                label = h5("Select width of header column"), 
                min = 2, 
                max = max(select_table()$x), 
                round = T,
                value = 20)}
    else if(input$auto_manual_columns == 2){
      sliderInput("header_column_slider", 
                  label = h5("Select width of header columns"), 
                  min = 2, 
                  max = max(select_table()$x), 
                  round = T,
                  value = 20)}
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
    ##Number of header columns
  header_column_number <- reactive({as.numeric(input$number_headers)})
  ##Number of header rows
  header_row_number <- reactive({as.numeric(input$header_rows)})
  
   ##Read in pdf
  raw_pdf = reactive({
    pdf_data(input$pdf_input_file$datapath)
  })
  
  ##Count number of tables in pdf
  total_tables = reactive({as.numeric(length(raw_pdf()))})
  
  #Select table to read
  select_table = reactive({as_tibble(raw_pdf()[[table_number_value()]])})  
  
  ##Data processing; simple
  processed_pdf = reactive({
    validate(
      need(input$pdf_input_file != "", "Please select a PDF"))
    ##Identify columns of interest
    find_cols <- seq(from = header_column_value(), to = max(select_table()$x), length.out = column_number_auto())
    
    #Calculate number of rows
    row_n <- round(length(unique(select_table()$y))/row_height(), 0)+1
    ##Identify rows of interest
    find_rows <- seq(from = min(select_table()$y), to = max(select_table()$y), length.out = row_n+1)
    
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
      spread(col, text) %>%
      select(-row)
    }) #end of reactive
  
  ##Data processing; customised
  custom_pdf = reactive({
    validate(
      need(input$pdf_input_file != "", "Please select a PDF"))
    ##Identify columns of interest
    header_cols <- seq(from = 1, to = header_column_value(), length.out = header_column_number()+1)  
    find_cols <- seq(from = header_column_value()+1, to = max(select_table()$x), length.out = column_number_auto() - header_column_number())
    
    ##Filter out header rows before processing
    new_y_vals <- unique(select_table()$y) #%>% sort()
    new_y_vals <- if(top_trim()==1){new_y_vals}
    else{new_y_vals[-(1:(top_trim()-1))]}
    
    ##Identify header rows
    header_y_vals <- new_y_vals[1:header_row_number()]
    find_header_rows <- c(1, max(header_y_vals))
    #Identify non-header rows
    non_header_y_vals <- new_y_vals[(header_row_number()+1):length(new_y_vals)]
    #Calculate number of rows
    row_n <- round(length(non_header_y_vals)/row_height(), 0)+1
    find_rows <- seq(from = min(non_header_y_vals), to = max(non_header_y_vals), length.out = row_n)
    
      ###Split into columns
    data <-  select_table() %>% 
      filter(y %in% new_y_vals) %>%
      mutate(col = cut(x, breaks = c(header_cols, find_cols, Inf))) %>% 
      mutate(row = cut(y, breaks = c(find_header_rows, find_rows, Inf))) %>%
      arrange(col, row) %>%
      group_by(col, row) %>%
      mutate(text = paste(text, collapse = " ")) %>%
      ungroup() %>%
      select(row, text, col) %>%
      unique() %>%
      spread(col, text) %>%
      select(-row) 
    
  }) #end of reactive
  
  trimmed_pdf = reactive({
    if(input$auto_manual_columns == 1){
      data <- processed_pdf()
      data <- data[top_trim():(nrow(processed_pdf())-as.numeric(input$trim_bottom)),]
      data <- Filter(function(x)!all(is.na(x)), data)
      data
    }
    else if(input$auto_manual_columns == 2){
      data <- custom_pdf()
    data <- data[1:(nrow(custom_pdf())-as.numeric(input$trim_bottom)),]}
  })
  
  ##Select column names
  final_data = reactive({
  if(input$set_col_names==1){
    data <- trimmed_pdf()
    colnames(data) <- data[1,]
    data <- data[-1,]
    
  } else{
    trimmed_pdf()
  }
  })
  
  ##Output datatable of raw data  
  total_rows = reactive({final_data() %>% nrow()})
  output$pdf_output_file <- renderDT({
    datatable(final_data(), rownames = FALSE,
              options = list(
                scrollX = TRUE,
                scrollY = TRUE,
                pageLength = total_rows()))
  })
  
  ##Save parameters from advanced manipulation
  params = reactive({
   tibble("table_number" = table_number_value(), "header_number" = header_column_value()) 
  })
  
  output$dl_xl <- downloadHandler(
    filename = function() {"read_pdf.xlsx"},
    content = function(file) {write_xlsx(list("Data" = final_data(), "Parameters" = params()), path = file)}
  )
  
  output$dl_csv <- downloadHandler(
    filename = function() {"read_pdf.csv"},
    content = function(file) {write.csv(final_data(), file = file, row.names = F)}
  )
}
# Run the app ----
shinyApp(ui, server)
