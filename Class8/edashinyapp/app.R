# Shiny app to generate basic histograms for a set of data

library(shiny)

# Load data
x = read.csv(file = "/home/rstudio/external/DataFrame.csv")

# Define UI for application that draws a histogram
ui = fluidPage(
   
   # Application title
   titlePanel("RCA Auto-Report EDA"),
   
   # Sidebar with a drop-down for variable selection
   sidebarLayout(
      sidebarPanel(
         selectInput(inputId = "dataVar",
                     label = "Select variable",
                     choices = unique(names(x)),
                     selected = unique(names(x))[1])
         #submitButton(text = "Generate EDA")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput(outputId = "histogram"),
         textOutput(outputId = "notes")
      )
   )
)

# Server logic required to draw a histogram
server = function(input, output) {

  # # Select data
  # selectVar = reactive({
  #   req(input$dataVar)
  #   z = x[, input$dataVar]
  # })
  # 
  output$histogram = renderPlot({
    # Get data for selected variable
    y = x[, input$dataVar]
    # y = selectVar()
    
    # Graph based on variable data type
    if(is.factor(y)){
      plot(y)
    } else{
      if(is.numeric(y)){
        hist(y)
      } else{
        output$notes = paste0(input$dataVar,
                              " does not have a recognized data",
                              " type.")
      }
    }
   })
}

# Run the application
shinyApp(ui = ui, server = server)

