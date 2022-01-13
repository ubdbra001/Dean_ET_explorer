#
# This is a Shiny web application for exploring the ET data from the OSF repo:
# https://osf.io/53gh2/

# Helper functions for downloading the data and converting it from matthe mat
# files are included, but not currently implemented in the app.

# You can run the application by clicking the 'Run App' button above.

library(shiny)
library(tidyverse)
library(cowplot)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Bilingual ET data explorer"),
    
    tabsetPanel(type = "tabs",
                tabPanel("AOI selections Plot",
                         plotOutput('AOI_selection_plot')),
                tabPanel("AOI looks Plot",
                         checkboxInput("split_groups", "Split groups"),
                         plotlyOutput('looking_proportion_plot')),
                tabPanel("First look Plot",
                         plotOutput('firstLook_plot'))),
    
    hr(),
    
    fluidRow(
        column(5,
               sliderInput("L_AOI_X", "Left AOI X vals",
                           min = 0, max = 1,
                           value = c(0,0.4), step = 0.01),
               
               sliderInput("L_AOI_Y", "Left AOI Y vals",
                           min = 0, max = 1,
                           value = c(0, 1), step = 0.01)
               
        ),
        column(5,
               sliderInput("R_AOI_X", "Right AOI X vals",
                           min = 0, max = 1,
                           value = c(0.6,1), step = 0.01),
               sliderInput("R_AOI_Y", "Right AOI Y vals",
                           min = 0, max = 1,
                           value = c(0, 1), step = 0.01)
        ),
        column(2,
               selectInput('trial_choice', "Trial Number",
                           unique(ET_data$trial_ID)),
               
               numericInput('samples_for_look', "Number of samples in a look",
                            value = 12, min = 1))
    )

)

# Define server logic required to draw a histogram
server <- function(input, output) {


}

# Run the application 
shinyApp(ui = ui, server = server)
