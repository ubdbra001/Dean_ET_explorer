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

    # Reactive function for processing the data
    ET_categorised <- reactive({
        
        # Generate list of AOIs from the inputs
        AOIs_list <- AOI_inputs_to_list(input$L_AOI_X, input$L_AOI_Y,
                                        input$R_AOI_X, input$R_AOI_Y)
        
        # Categorise whether sample is in L or R AOI
        data_out <- categorise_look(ET_processed, AOIs_list)
        
    })
    
    # Render plot for AOI selection
    output[['AOI_selection_plot']] <- renderPlot({
        
        # Calculate the corner points for the AOIs selected
        AOI_points <- calculate_AOIs(input$L_AOI_X,
                                     input$L_AOI_Y,
                                     input$R_AOI_X,
                                     input$R_AOI_Y)
        
        # Plot the AOI polygons
        ggplot(data = AOI_points, aes(x = x, y = y)) +
            geom_polygon(data = stimuli_areas(), colour = "black", fill = NA) +
            geom_polygon(aes(group = group, fill = group), alpha = 0.5) +
            scale_x_continuous(limits = c(0,1), expand = c(0,0),
                               breaks = seq(from = 0, to = 1, by = 0.2)) +
            scale_y_continuous(limits = c(0,1), expand = c(0,0),
                               breaks = seq(from = 0, to = 1, by = 0.2)) +
            theme_bw() +
            theme(legend.position = "none",
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank())
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
