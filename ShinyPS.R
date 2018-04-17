############################
# Jonathan Gross
# Pol Sci 5625
# Shiny PS
# SP 2018

############################
#### Directions ############

## Let's build our own (This is due next Tuesday as a problem set)

#1 ) 
#### As our first step, we are going to make a UI that does nothing.  We are going to say:

# Presidential Forecasts

# Here are the results of presidential forecasts from 1952-2008
# (this shoudl be in a lower font)

#2)
## As our second step, we are going to follow example 2 above and have it show the last X elections (as selectd by the user)

#3) Now we are going to have it plot the election results 

# https://shiny.rstudio.com/reference/shiny/1.0.2/plotOutput.html

# 4) Now we are going to add a line to add a dropdown window to add a specific forecast to the plot

# 5) Now we are going to make it so it prints out the data points when clicked on

# https://shiny.rstudio.com/articles/plot-interaction.html


## MONTGOMERY: NEED TO DO
#PLOT ACTUAL RESULTS
# DROP DOWN MENU TO ADD A FORECAST TO PLOT
# HAVE LEGEND
# WHEN DATA POINT IS CLICKED ON, IT SHOWS EXACT VALUE

###########################

library(shiny)
library(EBMAforecast)

data("presidentialForecast")

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Presidential Forceast"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for choosing dataset ----
 
       selectInput(inputId = "dataset",
                   label = "Choose Prediction to Plot:",
                   choices = c("Campbell" ,
                               "Lewis-Beck",
                               "EWT2C2",
                               "Fair" ,
                               "Hibbs",
                               "Abramowitz")),

    # Input: Numeric entry for number of elections to view ----
      numericInput(inputId = "elections",
                   label = "Number of elections to view:",
                   value = 4),
      helpText("Presidential Elections")
     ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      #Display results and predictions via table
     tableOutput("view"), 
     # Plot election results with Click Interaction
     plotOutput("plot1", click ="plot_click"),
     verbatimTextOutput("info")
      
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  library(EBMAforecast)
  data("presidentialForecast")
  
  # Convert to shiny format
  datasetInput = reactive({
    input$newplot
    presidentialForecast})
  
  #Display dataset with appropriate number of rows
  output$view = renderTable({
    head(datasetInput(), n = input$elections)
  })
  
  # Determine which prediction to plot
  predictions = reactive({
    input$newplot
    switch (input$dataset,
            "Campbell" =presidentialForecast$Campbell,
            "Lewis-Beck" = presidentialForecast$"Lewis-Beck",
            "EWT2C2" = presidentialForecast$EWT2C2,
            "Fair" = presidentialForecast$Fair,
            "Hibbs" = presidentialForecast$Hibbs,
            "Abramowitz" = presidentialForecast$Abramowitz
    )
  })
  
  #Display Plot -- Actual Election Results
  #Note: Since not specified in the directions I am plotting the election results from all years
  output$plot1 = renderPlot(
    {
    plot(
      # Plot Actual Results
      rownames(datasetInput()),
      datasetInput()$Actual,
      main = "Election Results",
      xlab = "Year",
      ylab = "Percent of Vote",
      type = "l",
      pch = "o", 
      col = "blue")
      
  # Plot Specified Prediction
    lines(
      rownames(datasetInput()),
      predictions(),
      type = "l",
      pch = "o",
      col = "red"
         )
    
    # Add Legend
    legend("topright",legend = c("Actual Result", "Prediction"), col = c("blue", "red"), lty = c(1, 1))
   } )
  
  # Click Interaction -- Print Output of clicked Point
  output$info = renderText({
    paste0("year=", input$plot_click$x, "\nvote share = ", input$plot_click$y)
  })
  
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
