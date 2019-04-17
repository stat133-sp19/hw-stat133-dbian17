#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(tidyr)


ui <- fluidPage(
   
   # Application title
   titlePanel("Investments Visualizer"),
   
   # Sidebar with a slider input for number of bins 
    fluidRow(
      
        column(4,
                  sliderInput("initial_amount",
                     "Initial Amount",
                     min = 1,
                     max = 100000,
                     value = 10000),
         
                  sliderInput("contribution",
                     "Annual Contribution",
                     min = 0,
                     max = 50000,
                     value = 2000)),
         
         column(4, 
         sliderInput("return_rate",
                     "Return Rate (in %)",
                     min = 0,
                     max = 20,
                     value = 5),
         
         sliderInput("growth_rate",
                     "Growth Rate (in %)",
                     min = 0,
                     max = 20,
                     value = 2)),
         
         column(4,
         sliderInput("years",
                     "Years",
                     min = 0,
                     max = 50,
                     value = 10),
         selectInput("facet",
                     "Facet?",
                     choices = list("Yes" = 1, "No" = 0), selected = 1)
        )
      ),
        
         
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("valuesplot", width = "150%"),
         tableOutput("values")
      )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  future_value <- function(amount, rate, years){
    return (amount*(1+rate)^years)
  }
  
  annuity <- function(contrib, rate, years){
    return((contrib/rate)*(((1+rate)^years)-1))
  }
  
  growing_annuity <- function(contrib, rate, growth, years){
    return((contrib/(rate-growth))*(((1+rate)^years)-((1+growth)^years)))
  }
  
  no_contrib_reg <- c()
  fixed_contrib_reg <- c()
  growing_contrib_reg <- c()
  
  sliderValues_nofacet <- reactive({
    return_rate <- input$return_rate/100
    growth_rate <- input$growth_rate/100
    
    for (i in c(0:input$years)){
      no_contrib_reg[i+1] <- future_value(input$initial_amount,return_rate,i)
      
      fixed_contrib_reg[i+1] <-future_value(input$initial_amount,return_rate,i) + annuity(input$contribution, return_rate, i)

      growing_contrib_reg[i+1] <- future_value(input$initial_amount,return_rate,i) + growing_annuity(input$contribution, return_rate, growth_rate, i)
    }
    
  data.frame(Year = c(0:input$years), No_Contribution = no_contrib_reg, Fixed_Contribution = fixed_contrib_reg, Growing_Contribution = growing_contrib_reg, stringsAsFactors = FALSE)
  })
  
  sliderValues_facet <- reactive({
    
    return_rate <- input$return_rate/100
    growth_rate <- input$growth_rate/100
    
    for (i in c(0:input$years)){
      no_contrib_reg[i+1] <- future_value(input$initial_amount,return_rate,i)
      
      fixed_contrib_reg[i+1] <-future_value(input$initial_amount,return_rate,i) + annuity(input$contribution, return_rate, i)
      
      growing_contrib_reg[i+1] <- future_value(input$initial_amount,return_rate,i) + growing_annuity(input$contribution, return_rate, growth_rate, i)
    }
    
    df <- data.frame(Year = c(0:input$years), No_Contribution = no_contrib_reg, Fixed_Contribution = fixed_contrib_reg, Growing_Contribution = growing_contrib_reg, stringsAsFactors = FALSE)
    gather(df, key="Type", value = "Savings", c("No_Contribution", "Fixed_Contribution", "Growing_Contribution"))
  })
  
  
  output$values <- renderTable(sliderValues_nofacet())
  
  
    output$valuesplot <- renderPlot({ 
      if (input$facet==0){
      ggplot(sliderValues_nofacet()) +
        geom_line(aes(x = Year, y = No_Contribution, color = "#ff0000")) + geom_point(aes(x = Year, y = No_Contribution, color = "#ff0000")) + 
        geom_line(aes(x = Year, y = Fixed_Contribution, color = "#00ff00")) + geom_point(aes(x = Year, y = Fixed_Contribution, color = "#00ff00")) + 
        geom_line(aes(x = Year, y = Growing_Contribution, color = "#0000ff")) + geom_point(aes(x = Year, y = Growing_Contribution, color = "#0000ff")) + 
        labs(title = 'Timeline') +
        xlab('Year') +
        ylab('Investment in Dolllars') +
      
      scale_color_discrete(name = "Modality", labels = c('Growing_Contribution', 'Fixed_Contribution',  'No_Contribution'))}
    
    
    else{
        ggplot(sliderValues_facet()) +
        geom_line(aes(x = Year, y = Savings, color = Type)) + geom_point(aes(x = Year, y = Savings, color = Type)) + 
        geom_area(aes(x = Year, y = Savings, color = Type, fill = Type)) +
        labs(title = "Timeline") + 
        facet_wrap (.~Type) +
        xlab('Year') +
        ylab('Investment in Dolllars') 
      
      }
  })
}

    

      
  

   


# Run the application 
shinyApp(ui = ui, server = server)

