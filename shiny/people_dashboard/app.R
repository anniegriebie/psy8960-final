library(tidyverse)
library(shiny)

ui <- fluidPage(
  #Added Title
  titlePanel("people_dashboard"),
  sidebarLayout(
    sidebarPanel(
      # Added option to allow users to choose whether they want to examine monthly pay, turnover status, or overall job satisfactino
      selectInput("outcome",
                  "What do you want to examine?",
                  c("Monthly Income", "Turnover Status", "Job Satisfaction"),
                  selected = "Monthly Income"),
      #Added option to allow users to select subset of data by department
      selectInput("department",
                  "Subset by department",
                  c("Human Resources", "Research & Development", "Sales", "All"),
                  selected="All"),
      #Added option to allow users to select subset of data based by field of education
      selectInput("education",
                  "Subset by department",
                  c("Human Resources","Life Sciences", "Marketing","Medical", "Other", "Technical Degree", "All"),
                  selected="All"),
      #Added option to allow users to select subset of data by employee gender
      selectInput("gender",
                  "Subset by employee gender",
                  c("Male","Female","All"),
                  selected="All"),
      #Added option to allow users to select subset of data by job role
      selectInput("job",
                  "Subset by job role",
                  c("Healthcare Representative", "Human Resources", "Laboratory Technician", "Manager", "Manufacturing Director", "Research Director", "Research Scientist", "Sales Executive", "Sales Representative","All"),
                  selected="All")
    ),
    #selected plot to show and table for later when make the table showing means and sds of those within the output based on selection 
    mainPanel(
      plotOutput("plot"), 
      tableOutput("table")
    )
  )
)


#Server function
server <- function(input, output) {
  #using readRDS to rename skinny data
  shiny_tbl <- readRDS("shiny_input.RDS")
   output$plot <- renderPlot({
    shiny_tbl <- shiny_tbl
    # Department
    if (input$department != "All") {
      shiny_tbl <- shiny_tbl %>%
        filter(Department == input$department)
    }
    # Education Filter
    if (input$education != "All") {
      shiny_tbl <- shiny_tbl %>%
        filter(`Education Field` = input$education)
    }
    # Gender Filter
    if (input$gender != "All") {
      shiny_tbl <- shiny_tbl %>%
        filter(Gender = input$gender)
    }
    # Job Filter
    if (input$job != "All") {
      shiny_tbl <- shiny_tbl %>%
        filter(`Job Role` = input$job)
    }
    shiny_tbl
  })
}   

#Need to come back and add in output table with means and SDs later 

#Run the app
shinyApp(ui = ui, server = server)