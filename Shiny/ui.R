#ui.R

library(shiny)
library(shinyBS)
library(ggplot2)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Moral Sense Test"),
  
  # Sidebar with controls 
    sidebarPanel(
      
      # this generates the radio buttons for the three tests
      # the default selected one will depend on which test the user
      # just took!
      uiOutput("curr"),
     
      br(),
      
      ## histogram bin slider, if its the whole population histogram
      conditionalPanel(condition = "input.conditionedPanels== 'Whole Population'",
      sliderInput("n_bins", 
                  "Choose the number of bins to display:", 
                  value = 20,
                  min = 5, 
                  max = 30)),
      
      #tell the user they can go back and take more studies :)
      uiOutput("link")
    ),
      

      
    
    ## tabset that includes different variables to visualize
    mainPanel(h4(p(strong(htmlOutput("queryText")))),
              #alert user if they haven't taken the test they're
              #currently visualizing
              bsAlert("alert"),
              
              #current graph is dependent on tabsets
              tabsetPanel(type = "tabs", 
                  tabPanel("Whole Population", plotOutput("whole_plot")),
                  tabPanel("By Gender", plotOutput("gender_plot")),
                  tabPanel("By Religion", plotOutput("religion_plot")),
                  id = "conditionedPanels"
              ),
      #generate a debriefing form dependent on the test
      #the user just came from
      conditionalPanel(condition = "output.debrief!= ' '",
      uiOutput("debrief")))
  
  ))

