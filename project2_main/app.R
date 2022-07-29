# https://shiny.rstudio.com/gallery/
# https://shiny.rstudio.com/gallery/reactivity.html
library(shiny)
library(tidyverse)
library(lubridate)
# ===============
# FUNCTION TO BUILD PATHS LOCAL/REMOTE
# Check your system information, look for nodename
print(Sys.info())
# Function to create full path
getFullPath <- function(localName) {
  if(Sys.info()[4]=="ip-172-31-45-184") {
    fPath <- "~/R/climatrade/"
  } else {
    fPath <- ""
  }
  return(paste0(fPath, localName))
}

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("databellum"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for choosing dataset ----
      selectInput(inputId = "dataset",
                  label = "Choose:",
                  choices = c("Recommendations", "Close prices")),
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Verbatim text for data summary ----
      verbatimTextOutput("summary"),
      
      # Output: HTML table with requested number of observations ----
      tableOutput("view")
      
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  tmpRecomms <- readRDS(getFullPath("recommendationsNN_all.rds")) %>% arrange(desc(date))# %>% filter(date_txn == max(date_txn))
  tmpRecomms$date_txn <- format(tmpRecomms$date_txn,'%d-%m-%Y')  
  tmpRecomms$date <- format(tmpRecomms$date,'%d-%m-%Y') 
  tmpRecomms <- tmpRecomms %>% select("Open on" = date_txn, "Open at" = VIX_txn, Action = action, "Close on" = date)
  tmpClosePrices <- readRDS(getFullPath("dataUptodate.rds")) %>% select(date, "Close Price" = VIX) %>% arrange(desc(date))

  tmpClosePrices$date <- format(tmpClosePrices$date,'%d-%m-%Y')
  # Return the requested dataset ----
  # By declaring datasetInput as a reactive expression we ensure
  # that:
  #
  # 1. It is only called when the inputs it depends on changes
  # 2. The computation and result are shared by all the callers,
  #    i.e. it only executes a single time
  datasetInput <- reactive({
    switch(input$dataset, 
           "Recommendations" = tmpRecomms, 
           "Close prices" = tmpClosePrices
           )
  })
  

  # Show the first "n" observations ----
  # The output$view depends on both the databaseInput reactive
  # expression and input$obs, so it will be re-executed whenever
  # input$dataset or input$obs is changed
  output$view <- renderTable({
    head(datasetInput(), n = 10)
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
