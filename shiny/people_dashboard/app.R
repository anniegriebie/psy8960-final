library(shiny)
library(tidyverse)


#Do I need this one?
selectInput("errorSelect",
            "Display error band?",
            c(
              "Display Error Band"=T,
              "Suppress Error Band"=F)
            
            
# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Scatterplot Comparing Scale Means"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      # Drop down selectors, containing names
      # and display options
      selectInput("outcomeSelect",
                  "What do you want to examine?",
                  c("Monthly Pay", "Turnover Status", "Overall Job Satisfaction", "All"),
                  selected="All"),
     
      ),
      selectInput("departmentselect",
                  "Subset data by department",
                  c("Sales", "Research & Development", "Human Resources", "All" ),
                  selected= "All"
                  ),
      selectInput("education select",
                  "subset data by employee field of education",
                  c("Medical", "Life Scienes", "Technical Degree", "Human Resources", "Marketing", "Other", "All"))),
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("scatterPlot"),
    )
  )
)
server <- function(input, output) {
  week8_tbl <- readRDS("shiny_input.RDS")  
  output$scatterPlot <- renderPlot({
    
    # define working tbl to iteratively modify 
    # for later display in ggplot
    week8_disp_tbl <- week8_tbl
    
    # don't need to change it if set to All; otherwise
    # filter by gender selection
    if (input$genderSelect != "All") {
      week8_disp_tbl <- week8_disp_tbl %>%
        filter(gender == input$genderSelect)
    }
    
    # don't need to change it if set to not Exclude; 
    # otherwise rmeove the cases before aug2017
    if (input$excludeSelect == T) {
      week8_disp_tbl <- week8_disp_tbl %>%
        filter(after_aug2017_flag == T)
    }
    
    # same plot for week8.Rmd...
    ggplot(week8_disp_tbl,
           aes(x = q1q6_mean, y = q8q10_mean)) +
      geom_point() +
      geom_smooth(method="lm", color="purple",
                  # ...except modified by this input
                  se=as.logical(input$errorSelect))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)