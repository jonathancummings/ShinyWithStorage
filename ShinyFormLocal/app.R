# Store form imput locally with this app

# Recreation of a form submission shiny app found here:
# https://deanattali.com/blog/shiny-persistent-data-storage/


# Load libraries
library(tidyverse)
library(shiny)
library(DT)
library(digest)

# add a directory to store the responses
outputDir <- "responses"

# Function to save entries to a .csv
saveData <- function(data) {
    # transpose the data (why?)
    data <- t(data)
    # Create a unique file name
    fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
    # Write the file to the local system
    write.csv(
        x = data,
        file = file.path(outputDir, fileName), 
        row.names = FALSE, quote = TRUE
    )
}

loadData <- function() {
    # Read all the files into a list
    files <- list.files(outputDir, full.names = TRUE)
    data <- lapply(files, read.csv, stringsAsFactors = FALSE) 
    # Concatenate all data together into one data.frame
    data <- do.call(rbind, data)
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