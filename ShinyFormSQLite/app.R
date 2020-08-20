# Store form imput, save to AWS with this app

# Recreation of a form submission shiny app found here:
# https://deanattali.com/blog/shiny-persistent-data-storage/

library(tidyverse)
library(shiny)
library(DT)
library(digest)
library(RSQLite)

# Set SQLite paths
sqlitePath <- "C:/Users/jcummings/OneDrive - UMASS Dartmouth/SMAST/Learning/R/ShinyWithStorage/ShinyFormSQLite/ShinyFormTest.db"
table <- "responses"

# Function to save entries to SQLite
saveData <- function(data) {
    # Connect to the database
    db <- dbConnect(SQLite(), sqlitePath)
    # Construct the update query by looping over the data fields
    query <- sprintf(
        "INSERT INTO %s (%s) VALUES ('%s')",
        table, 
        paste(names(data), collapse = ", "),
        paste(data, collapse = "', '")
    )
    # Submit the update query and disconnect
    dbExecute(db, query)
    dbDisconnect(db)
}

loadData <- function() {
    # Connect to the database
    db <- dbConnect(SQLite(), sqlitePath)
    # Construct the fetching query
    query <- sprintf("SELECT * FROM %s", table)
    # Submit the fetch query and disconnect
    data <- dbGetQuery(db, query)
    dbDisconnect(db)
    data
}

# Define the fields we want to save from the form
fields <- c("name", "used_shiny", "r_num_years")

# Define UI
ui <- fluidPage(
    
    # Application title
    titlePanel("Mimicking a Google Form with a Shiny app"),
    
    # Sidebar
    sidebarLayout(
        # Create three inputs
        sidebarPanel(
            textInput("name", "Name", ""),
            checkboxInput("used_shiny", "I've built a Shiny app in R before", FALSE),
            sliderInput("r_num_years", "Number of years using R",
                        0, 25, 2, ticks = FALSE),
            actionButton("submit", "Submit")
        ), # end sidebarPanel
        
        # Show a table of responses
        mainPanel(
            DT::dataTableOutput("responses", width = 300), tags$hr()
        ) # end mainPanel
    ) # end sdiebarLayout
) # end fluidpage

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    # Whenever a field is filled, aggregate all form data
    formData <- reactive({
        data <- sapply(fields, function(x) input[[x]])
        data
    })
    
    # When the Submit button is clicked, save the form data
    observeEvent(input$submit, {
        saveData(formData())
    })
    
    # Show the previous responses
    # (update with current response when Submit is clicked)
    output$responses <- DT::renderDataTable({
        input$submit
        loadData()
    })
} # send server

# Run the application 
shinyApp(ui = ui, server = server)