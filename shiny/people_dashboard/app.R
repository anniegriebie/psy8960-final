library(tidyverse)
library(shiny)

ui <- fluidPage(
  #Added Title
  titlePanel("people_dashboard"),
  sidebarLayout(
    sidebarPanel(
      # Added option to allow users to choose whether they want to examine monthly pay, turnover status, or overall job satisfaction.Changed Monthly income to Monthly Pay because that is what it was explicitly called in the Final directions 
      selectInput("outcome", 
                  "What do you want to examine", 
                  c("Monthly Pay", "Turnover Status", "Job Satisfaction"),
                  selected = "Monthly Pay"),
      #Added option to allow users to select subset of data by department
      selectInput("department", 
                  "Subset by department", 
                  c("Human Resources", "Research & Development", "Sales", "All"),
                  selected = "All"),
      #Added option to allow users to select subset of data based by field of education    
      selectInput("education", 
                  "Subset by education", 
                  c("Human Resources","Life Sciences", "Marketing","Medical", "Other", "Technical Degree", "All"),
                  selected = "All"),
      #Added option to allow users to select subset of data by employee gender
      selectInput("gender", 
                  "Subset by employee gender", 
                  c("Male", "Female", "All"),
                  selected = "All"),
      #Added option to allow users to select subset of data by job role
      selectInput("job", 
                  "Subset by job role", 
                  c("Healthcare Representative", "Human Resources", "Laboratory Technician", "Manager", "Manufacturing Director", "Research Director", "Research Scientist", "Sales Executive", "Sales Representative", "All"),
                  selected = "All")
    ),
    
    mainPanel(
      plotOutput("plot"),
      tableOutput("table"),
      textOutput("plottext")
    )
  )
)

# Server Function
server <- function(input, output) {
  #using readRDS to rename skinny data
  shiny_tbl <- readRDS("shiny_input.RDS")
  # tbl specifically for sidebar panel user selections filtering
  sidebar_tbl <- shiny_tbl
  # Filter the data based on user selections. Need to use reactive function so that continues to change dynamically as users select different inputs 
  outcome_tbl <- reactive({
    if (input$department != "All") {
      sidebar_tbl <- sidebar_tbl %>% 
        filter(Department == input$department)
    }
    if (input$education != "All") {
      sidebar_tbl <- sidebar_tbl %>% 
        filter(EducationField == input$education)
    }
    if (input$gender != "All"){
      sidebar_tbl <- sidebar_tbl %>% 
        filter(Gender == input$gender)
    }
    if (input$job != "All"){
      sidebar_tbl <- sidebar_tbl %>% 
        filter(JobRole == input$job)
    }
    #calling so shows up in final shiny app
    sidebar_tbl
  })
  
  # Creating histogram and bar plot for visualization based on user inputs 
  output$plot <- renderPlot({
    if (input$outcome == "Monthly Pay") {
      ggplot(outcome_tbl(), aes(x = MonthlyIncome)) +
        #changed binwidth to 500 because got a warning that said bins was set to 30. 
        geom_histogram(binwidth = 500) +
        labs(x = "Monthly Income $", y = "# of employees")
    } else if (input$outcome == "Turnover Status") {
      ggplot(outcome_tbl(), aes(x = Attrition)) + 
        geom_bar() +
        labs(x = "Turnover Status", y = "# of employees")
    } else if (input$outcome == "Job Satisfaction") {
      ggplot(outcome_tbl(), aes(x = JobSatisfaction)) + 
        geom_bar() +
        labs(x = "Job Satisfaction", y = "# of employees")
    }
  })
  
  #Adding text output for when user picks "Turnover" because confusing that it is a zero one value rather than yes or no/true or false. Did not add note for the other outcome variables because they seemed more self-explanatory. Considered adding text for Job Satisfaction variable but did not have more information about what the values 1-4 corresponded to from the original data. 
  output$plottext <- renderText({
    if(input$outcome == "Turnover Status") {
      plottext <- " Figure Note: value of 0 indicates employee DID NOT turnover, value of 1 indicates employee DID turnover"
    }
  })
  
  #Creating output table with descriptives.
  output$table <- renderTable({
    #Need to group variables to call into table, tried creating a reactive function because according to "https://shiny.rstudio.com/tutorial/written-tutorial/lesson4/" a reactive output is useful for dynamically generating an automatic response with changing selections, when tried wrapping in reactive expression received an error code on app that table input needed to be a variable list, not sure if I wrapped the function incorrectly or if this is common.
    selection <- c()
      if(input$department != "All"){
        selection <- append(selection, "Department")
      }
      if(input$education != "All"){
        selection <- append(selection, "EducationField")
      }
      if(input$gender != "All"){
        selection <- append(selection, "Gender")
      }
      if(input$job != "All"){
        selection <- append(selection, "JobRole")
        }
  if(input$outcome == "Monthly Pay"){
    outcome <- "MonthlyIncome"
    } else if(input$outcome == "Turnover Status"){
    outcome <- "Attrition"
    } else if(input$outcome == "Job Satisfaction"){
    outcome <- "JobSatisfaction"
  }
  shiny_tbl %>%
    group_by_at(selection) %>%
    summarise("Mean" = mean(.data[[outcome]]),
              "Standard Deviation" = sd(.data[[outcome]]))
})
}
# Run the app
shinyApp(ui = ui, server = server)
##rsconnect::deployApp