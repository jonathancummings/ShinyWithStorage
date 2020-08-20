# Store form imput, save to AWS with this app

# Recreation of a form submission shiny app found here:
# https://deanattali.com/blog/shiny-persistent-data-storage/

library(tidyverse)
library(shiny)
library(DT)
library(digest)
library(aws.s3)

# add a directory to store the responses
s3BucketName <- "shinyformawsbucket"
source("global.R")


get_time_human <- function() {
    format(Sys.time(), "%Y%m%d-%H%M%OS")
}

# Function to save entries to a .csv
saveData <- function(data) {
    # Create a plain-text representation of the data
    data <- paste0(
        paste(names(data), collapse = ","), "\n",
        paste(unname(data), collapse = ",")
    )
    # Create a unique file name
    file_name <- paste0(
        paste(
            get_time_human(),
            digest(data, algo = "md5"),
            sep = "_"
        ),
        ".csv"
    )
    # Upload the file to S3
    put_object(file = charToRaw(data), object = file_name, bucket = s3BucketName)
}

loadData <- function() {
    # Get a list of all files
    file_names <- get_bucket_df(s3BucketName)[["Key"]]
    # Read all files into a list
    data <- lapply(file_names, function(x) {
        object <- get_object(x, s3BucketName)
        object_data <- readBin(object, "character")
        read.csv(text = object_data, stringsAsFactors = FALSE)
    })
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