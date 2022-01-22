#
# This is a Shiny web application for exploring the ET data from the OSF repo:
# https://osf.io/53gh2/

# Helper functions for downloading the data and converting it from the mat
# files are included, but not currently implemented in the app.

# You can run the application by clicking the 'Run App' button above.

library(shiny)
library(shinyFiles)
library(tidyverse)
library(cowplot)
library(plotly)

# Load data
ET_data <- load_data()
ET_processed <- initial_processing(ET_data)

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
                         plotOutput('firstLook_plot'))
                ),
    
    hr(),
    
    fluidRow(
        column(3,
               sliderInput("L_AOI_X", "Left AOI X vals",
                           min = lower_limit, max = upper_limit,
                           value = Left_x, step = step_size),
               
               sliderInput("L_AOI_Y", "Left AOI Y vals",
                           min = lower_limit, max = upper_limit,
                           value = Left_y, step = step_size)
               
        ),
        column(3,
               sliderInput("R_AOI_X", "Right AOI X vals",
                           min = lower_limit, max = upper_limit,
                           value = Right_x, step = step_size),
               sliderInput("R_AOI_Y", "Right AOI Y vals",
                           min = lower_limit, max = upper_limit,
                           value = Right_y, step = step_size)
        ),
        column(3,
               selectInput('trial_choice', "Trial Number",
                           unique(ET_data$trial_ID)),
               
               numericInput('samples_for_look', "Number of samples in a look",
                            value = look_length_default, min = 1)),
        column(3,
               checkboxInput('remove_outliers', "Remove outliers?"),
               conditionalPanel(
                   condition = "input.remove_outliers == true",
                   selectInput('outlier_choice',
                               "Criteria for removing outliers",
                               c("Screen looking", "AOI looking")),
               
                   sliderInput('proportion_look',
                               "Minimum proportion of looking per trial",
                               min = lower_limit, max = upper_limit,
                               value = 0.8, step = step_size)
                   )),
        
    )

)

# Define server logic
server <- function(input, output) {

    # Reactive function for processing the data
    ET_categorised <- reactive({
        
        # Generate list of AOIs from the inputs
        AOIs_list <- AOI_inputs_to_list(input$L_AOI_X, input$L_AOI_Y,
                                        input$R_AOI_X, input$R_AOI_Y)
        
        ET_filtered <- filter(ET_processed, trial_ID == input$trial_choice)
        
        # Categorise whether sample is in L or R AOI
        data_out <- categorise_look(ET_filtered, AOIs_list)
        
    })
    
    
    ET_firstlooks <- reactive({
        
        # Updates the categorised ET dataframe with a per trial and per
        # participant first look, categorises whether each sample matches that,
        # and then summarises across participants
        
        ET_cat <- ET_categorised()
        
        ET_fl <- group_by(ET_cat, part_ID)
        
        # Calculate when the first look in a specific AOI occurs
        # (first look defined as first incidence of a run of samples categorised
        # as within an AOI for the user input specified amount, default is 12)
        ET_fl <- group_by(ET_cat, part_ID) %>%
          summarise(.groups = "keep",
            first_L = find_first_look(AOI_L, input$samples_for_look),
            first_R = find_first_look(AOI_R, input$samples_for_look))
        
        # Categorise which of these two first looks came first and thus which is
        # the true first look
        ET_fl <- mutate(ET_fl, 
            FL = case_when(is.na(first_L) & is.na(first_R) ~ NA_character_,
                           ((first_L < first_R) | is.na(first_R)) ~ "L",
                           ((first_R < first_L) | is.na(first_L)) ~ "R"))
        
        # Add the first look for each participant and trial back to the samples
        # for those trials
        ET_cat <- left_join(ET_cat, ET_fl, by = "part_ID")
        
        # Determine whether an individual sample matches the first look for that
        # participant/trial
        ET_cat <- mutate(ET_cat,
                         at_first_look = case_when(FL == "L" ~ AOI_L,
                                                   FL == "R" ~ AOI_R,
                                                   is.na(FL) ~ NA),
                         at_first_look = case_when(
                             (AOI_L | AOI_R) == T ~ at_first_look))
        
        ET_cat <- group_by(ET_cat, sample_ID, Group)
        
        ET_cat <- summarise(ET_cat, .groups = "keep", 
                            looking_at_AOI = mean(!is.na(at_first_look)),
                            at_first_look = mean(at_first_look, na.rm = T))
        
        ET_cat <- arrange(ET_cat, sample_ID)
        
        # Converts IDs to sample times
        # (Might be possible to avoid by rounding epoch_time)
        ET_cat <- mutate(ET_cat, sample_time = (sample_ID-1)/sample_rate)
            
            
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
    
    
    # Render plot for Looking proportions
    output[['looking_proportion_plot']] <- renderPlotly({
        
        ET_cat <- ET_categorised()
        
        # Group data differently depending on whether split data checkbox is ticked
        if (input$split_groups) {
            plot_data <- group_by(ET_cat, sample_ID, Group)
        } else {
            plot_data <- group_by(ET_cat, sample_ID)
            
        }
        
        # Do the steps to process the data
        plot_data <- summarise(plot_data, AOI_L = mean(AOI_L), AOI_R = mean(AOI_R),
                               .groups = "keep") %>%
            arrange(sample_ID) %>%
            mutate(sample_time = (sample_ID-1)/sample_rate) %>% # convert sample_ID
            pivot_longer(cols = c("AOI_L", "AOI_R"), names_to = "AOI_location") %>%
            mutate(AOI_location = recode(AOI_location,
                                         AOI_L = "Left AOI",
                                         AOI_R = "Right AOI"))
        
        
        if (input$split_groups) {
            plot_data <- unite(plot_data, Group_AOI, c("Group", "AOI_location"),
                               remove = F, sep = ": ")
            
            base_plot <- ggplot(data = plot_data,
                                aes(x = sample_time, colour = Group_AOI, y = value))
            
        } else {
            base_plot <- ggplot(data = plot_data,
                                aes(x = sample_time, y = value, colour = AOI_location))
        }
        
        looking_plot <- base_plot +
            # Plot traces for L and R AOIs
            geom_line() +
            ylim(y_lims) +
            ylab("Proportion of Participants looking") +
            xlab("Time(s)") +
            theme_bw()
        
        ggplotly(looking_plot, tooltip = "y") %>%
            config(displayModeBar = F)
        
    })
    
    output[['firstLook_plot']] <- renderPlot({ 
        
        line_size = 0.8
        
        fl_plotdata <- ET_firstlooks()
        
        First_look_plot <- ggplot(data = fl_plotdata, aes(x = sample_time)) +
            geom_line(aes(y = at_first_look, colour = Group), size = line_size) +
            ylab("Proportion looking") +
            ylim(y_lims) +
            annotate("text", x=Inf, y = Inf, label = "First look AOI",
                     vjust = vjust, hjust = hjust) +
            theme_bw() +
            theme(axis.title.x = element_blank(),
                  axis.text.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  legend.position = "right")
        
        
        Looking_at_AOI_plot <- ggplot(data = fl_plotdata, aes(x = sample_time)) +
            geom_line(aes(y = looking_at_AOI, colour = Group), size = line_size) +
            ylab("Proportion looking") +
            xlab("Time (s)") +
            annotate("text", x=Inf, y = Inf, label = "Either AOI",
                     vjust = vjust, hjust = hjust) +
            ylim(y_lims) +
            theme_bw() +
            theme(legend.position = "none")
        
        plot_grid(First_look_plot, Looking_at_AOI_plot, ncol = 1,
                  align = "v", axis = "lr")
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
