#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Figuring out Factorial Figures"),
   titlePanel("Created by A. Hashemi"),
   titlePanel("v1.1 - last updated October 5, 2021"),
   
   # Sidebar with a slider input for number of bins 
   actionButton("runif", "Reset data"),
   hr(),
   plotOutput("barPlot"),
   splitLayout(
     actionButton("runif2", "Show Main Effect of A"),
     actionButton("runif3", "Show Main Effect of B")
     ),
   splitLayout(
    plotOutput("barPlot2"),
    plotOutput("barPlot3")
   ),
   
   titlePanel("Added features for 1.1: random numbers are now drawn from a uniform distribution from -1 to 1.")
   
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  my_data <- reactiveValues(data = matrix(sample(runif(4,min=-1,max=1), 4, replace=T),2,2), #c(-1,-0.5,0,0.5,1)
                            maineffectA = NULL,
                            maineffectB = NULL
                            #maineffectA = rowMeans(my_data$data),
                            #maineffectB = colMeans(my_data$data)
                            )
  #my_data = matrix(sample(c(0.5,1), 4, replace=T),2,2)
  
  observeEvent(input$runif, {
    my_data$data = matrix(sample(runif(4,min=-1,max=1), 4, replace=T),2,2)
    my_data$maineffectA = NULL
    my_data$maineffectB = NULL
    #my_data$maineffectA = rowMeans(my_data$data)
    #my_data$maineffectB = colMeans(my_data$data)
  })
  
  observeEvent(input$runif2, {
    my_data$maineffectA = rowMeans(my_data$data)
  })
  
  observeEvent(input$runif3, {
    my_data$maineffectB = colMeans(my_data$data)
  })
  
  
  
  # Fill in the spot we created for a plot
  output$barPlot <- renderPlot({
    
    # Render a barplot
    bp <- barplot(my_data$data,
            col=c("white","grey"),
            legend=c("A1","A2"),
            beside=T,
            args.legend = list(x="bottom"),
            ylim=c(-1.1,1.1))
    axis(1, at=colMeans(bp), labels=c("B1","B2"))
    abline(h=0,lwd=1.5)
    box(lwd=1.5)
  })
  
  # main effect A
  output$barPlot2 <- renderPlot({
    if (is.null(my_data$maineffectA)) return()
    
    # Render a barplot
    bp2 <- barplot(my_data$maineffectA,
                   beside=T,
                   ylim=c(-1,1),
                   col=c("black","black"),
                   main="Main Effect of A")
    axis(1, at=bp2, labels=c("A1","A2"))
    abline(h=0,lwd=1.5)
    box(lwd=1.5)
  })

  # main effect B
  output$barPlot3 <- renderPlot({
    if (is.null(my_data$maineffectB)) return()
    
    # Render a barplot
    bp3 <- barplot(my_data$maineffectB,
                   beside=T,
                   ylim=c(-1,1),
                   col=c("black","black"),
                   main="Main Effect of B")
    axis(1, at=bp3, labels=c("B1","B2"))
    abline(h=0,lwd=1.5)
    box(lwd=1.5)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

