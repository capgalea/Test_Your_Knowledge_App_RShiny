#
# This is a Shiny web Question & Answer application
#
# Author: cgalea
# Date: 05/07/2019
#

# Load R packages
library(shiny)
library(shinydashboard)
library(rmarkdown)
library(dplyr)
library(shinyBS)
library(data.table)
library(tidyr)
library(DT)
library(rhandsontable)
library(tibble)

# Load dataset
quesAns <- fread('zendesk_challenge.tsv', encoding='UTF-8', quote = "")

# Define UI for application
ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "Test Your Knowledge",
                  titleWidth = 400),
  
  
#  dropdownMenu(type = "notifications",
#               notificationItem(
#                 text = "Version Number 01",
#                 icon("laptop-code")
#               )
#  ),
               
   # Sidebar content 
  dashboardSidebar(
    sidebarMenu(
      
      # Question and Answer tab for main panel
      menuItem("Question and Answer", tabName = "QA", icon = icon("graduation-cap")),
      
      # helpText("Note: while the data view will show only the specified",
      #          "number of observations, the summary will still be based",
      #          "on the full dataset."),
      
      # Data table tab to view data
      menuItem("Data Table", tabName = "table", icon = icon("table")),
      
      # ReadMe tab to view background information
      menuItem("ReadMe", tabName = "readme", icon = icon("book-reader")),
      
      # Code tab to view code
      menuItem("Code", tabName = "code", icon = icon("code")),
      
      hr(),
      br(),
      br()
      
      )
    ),
      
      
      dashboardBody(
        tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 24px;
      }'
                                  )
                            )
                    ),

        
        tabItems(
          # Question tab content
          tabItem(tabName = "QA",
                  fluidRow(
                    
                    # Box 1. Random Question
                    box(title = "Question", status = "primary", height = 200, width = 12,
                        solidHeader = TRUE,
                        helpText("1. When you click the button to see",
                                 "a random question."),
                        actionButton("action", label = "Pick Random Question"),
                        
                        verbatimTextOutput("questionSample"),
                        helpText("2. Click on the best answer in the table below.")
                        ),
                    
                    # Box 2. Possible Answers
                    box(title = "Possible Answers", status = "primary", 
                        height = 680, width = 9, solidHeader = TRUE,
                        column(12, DT::dataTableOutput('answerSample'))
                        ),

                    # Box 3. Result
                    box(title = "Result", status = "primary", 
                        height = 680, width = 3, solidHeader = TRUE,
                        column(12, verbatimTextOutput("info", placeholder = TRUE))
                        )
                    )
                ),
          
          
          # Data Table tab content
          tabItem(tabName = "table",
                  
                  fluidPage(
                    titlePanel("Question and Answer Dataset"),
                    
                    fluidRow(
                      #Create a new row for selectInputs
                      column(4,
                             selectInput("DT", "Document Title:", 
                                         c("All", unique(as.character(quesAns$DocumentTitle)))),
                             
                      #Create a new row for the table
                            dataTableOutput("dataTable"), 
                                style = "font-size: 100%; width: 100%")
                      )
                    )
                  ),
          
          
          # ReadMe tab content
          tabItem(tabName = "readme",
                  fluidRow(includeMarkdown("readMe.Rmd")
                  )
          ),
          
          # Code tab content
          tabItem(tabName = "code",
                  fluidRow(includeMarkdown("code.Rmd")
                  )
              )
          )
      )
  )


# Define server logic required generate random question and corresponding 
# table of possible answers
server <- function(input, output) {
  
  # Filter to find random Question
  filtered_data <- eventReactive(input$action, {
    samp <- unique(quesAns$Question)
    samp <- sample(samp, 1, replace = FALSE)
  samp
  })
  
  
  # Output random question
  output$questionSample <- renderPrint({
    filt_random_sample <- filtered_data()
    filt_random_sample
  })
  
  
  
  # Output possible answers
  output$answerSample <- DT::renderDataTable(server = FALSE, {
    filt_random_sample <- filtered_data()
    # Filter the data based on the random question
    filt_random_sample <- filter(quesAns, Question == filt_random_sample)
    # Select only Answer and Label columns
    filt_random_sample <- filt_random_sample[, c("Answer")]
    # Use DT options to make the table more user-friendly
    DT::datatable(filt_random_sample, 
                  rownames = FALSE,
                  options = list(pageLength = 5),
                  selection = 'single')
  })
  
   
  # Make table of possible answers clickable to allow generation of result (correct or not)
   proxy = dataTableProxy('answerSample')
  
    
  
  # Output result
  output$info = renderPrint({
    s = input$answerSample_rows_selected
    if(length(s) == 0) {
      cat('Please select an answer')
    } else {
      # Get the selected answer and check if it's correct
      selected_row <- isolate(filter(quesAns, Question == filtered_data()))[s, ]
      if (length(selected_row) > 0 && !is.na(selected_row$Label) && selected_row$Label == 1) {
        cat('Correct Answer')
      } else {
        cat('Sorry that pick is not correct. \n\n')
        cat('Pick another question.')
      }
    }
  })
  
  
  
    
  # Filter table (Data Table tab) based on selections
   output$dataTable <- DT::renderDataTable(DT::datatable({
     data <- quesAns
     if (input$DT != "All") {
       data <- data[data$DocumentTitle == input$DT,]
     }
     data
   }))
}

# Run the application 
shinyApp(ui = ui, server = server)

