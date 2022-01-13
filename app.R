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

# Load data
ET_data <- load_data()

# Define UI for application
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
                           min = lower_limit, max = upper_limit,
                           value = Left_x, step = step_size),
               
               sliderInput("L_AOI_Y", "Left AOI Y vals",
                           min = lower_limit, max = upper_limit,
                           value = Left_y, step = step_size)
               
        ),
        column(5,
               sliderInput("R_AOI_X", "Right AOI X vals",
                           min = lower_limit, max = upper_limit,
                           value = Right_x, step = step_size),
               sliderInput("R_AOI_Y", "Right AOI Y vals",
                           min = lower_limit, max = upper_limit,
                           value = Right_y, step = step_size)
        ),
        column(2,
               selectInput('trial_choice', "Trial Number",
                           unique(ET_data$trial_ID)),
               
               numericInput('samples_for_look', "Number of samples in a look",
                            value = look_length_default, min = 1))
    )

)

# Define server logic required to draw a histogram
server <- function(input, output) {


}

# Run the application 
shinyApp(ui = ui, server = server)
