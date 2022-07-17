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
    
    # Define tabs for app
    tabsetPanel(type = "tabs",
                tabPanel("AOI selections Plot",
                         plotOutput('AOI_selection_plot')),
                tabPanel("AOI looks Plot",
                         checkboxInput("split_groups", "Split groups"),
                         plotlyOutput('looking_proportion_plot')),
                tabPanel("First look Plot",
                         plotOutput('firstLook_plot')),
                tabPanel("Binned look Plot",
                         plotOutput('binLook_plot'),
                         downloadButton('download_BinnedData', 'Download data')),
                ),
    
    hr(),
    
    # Define bottom panel with inputs
    fluidRow(
        # L AOI sliders
        column(3,
               sliderInput("L_AOI_X", "Left AOI X vals",
                           min = lower_limit, max = upper_limit,
                           value = Left_x, step = step_size),
               
               sliderInput("L_AOI_Y", "Left AOI Y vals",
                           min = lower_limit, max = upper_limit,
                           value = Left_y, step = step_size)
               
        ),
        # R AOI sliders
        column(3,
               sliderInput("R_AOI_X", "Right AOI X vals",
                           min = lower_limit, max = upper_limit,
                           value = Right_x, step = step_size),
               sliderInput("R_AOI_Y", "Right AOI Y vals",
                           min = lower_limit, max = upper_limit,
                           value = Right_y, step = step_size)
        ),
        # Trial N, look samples, and bin length selction
        column(3,
               selectInput('trial_choice', "Trial Number",
                           unique(ET_data$trial_ID)),
               
               numericInput('samples_for_look', "Number of samples in a look",
                            value = look_length_default, min = 1),
               
               numericInput('bin_length', "Length of bin in Seconds",
                            value = bin_length_default, min = 0.1, step = 0.1)
               ),
        # Outlier removal selection
        column(3,
               checkboxInput('remove_outliers', "Remove outliers?"),
               
               # Only show if remove_outliers box is checked
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

    ## Reactive functions
  
  
    ET_filtered <- reactive({
      # Filter the data so only selected trial is processed
      
      ET_filtered <- filter(ET_processed, trial_ID == input$trial_choice)
      
    })
  
    
    ET_categorised <- reactive({
      # Categorise each sample to indicate whether it falls into one of the AOIs
      
      ET_filtered <- ET_filtered()
      
      # Generate list of AOIs from the inputs
      AOIs_list <- AOI_inputs_to_list(input$L_AOI_X, input$L_AOI_Y,
                                      input$R_AOI_X, input$R_AOI_Y)
      
      # Categorise whether sample is in L or R AOI
      data_out <- categorise_look(ET_filtered, AOIs_list)
      
    })


    ET_outliersRemoved <- reactive({
        
        # Removes participants and trials that do not meet the minimum looking
        # criteria
      
        ET_data <- ET_categorised()
      
        # Calculate screen looking and AOI looking proportions on a per trial
        # and participant basis
        ET_looking <- group_by(ET_data, part_ID, trial_ID) %>%
          summarise(screen_looking = mean(screen_looking),
                    AOI_looking = mean(AOI_L | AOI_R),
                    .groups = "drop")
        
        # Filter based on user inputs
        if (input$outlier_choice == "Screen looking"){
          ET_looking <- filter(ET_looking,
                               screen_looking > input$proportion_look)
            
        } else if (input$outlier_choice == "AOI looking") {
          ET_looking <- filter(ET_looking,
                               AOI_looking > input$proportion_look)
        }
        
        # Semi_join keeps participants and trials that weren't filtered 
        ET_data <- semi_join(ET_data, ET_looking, by = c("part_ID", "trial_ID"))
        
    })


    ET_firstlooks <- reactive({
        
        # Updates the categorised ET dataframe with a per trial and per
        # participant first look, categorises whether each sample matches that,
        # and then summarises across participants
        if (input$remove_outliers){
          ET_cat <- ET_outliersRemoved()
        } else {
          ET_cat <- ET_categorised()
        }
        
        # 
        ET_cat <- add_first_look(ET_cat, input$samples_for_look)
            
    })
    
    
    ET_binned <- reactive({
      # Add bins to the data
      ET_data <- ET_firstlooks()
      
      ET_binned <- add_bins(ET_data, bin_width = input$bin_length)
      
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
        
        if (input$remove_outliers){
          ET_cat <- ET_outliersRemoved()
        } else {
          ET_cat <- ET_categorised()
        }
        
        # Do the steps to process the data
        plot_data <- summarise_looking(ET_cat, input$split_groups)
        
        # What should the colour grouping be based on 
        if (input$split_groups) {
            plot_colour_opt <- "Group_AOI"    
        } else {
            plot_colour_opt <- "AOI_location"
        }
        
        # Generate plot
        looking_plot <- ggplot(data = plot_data,
                               aes(x = sample_time,
                                   y = value,
                                   colour = !!sym(plot_colour_opt))) +
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
        
        ET_fl <- ET_firstlooks()
        
        plot_data <- summarise_first_looks(ET_fl)
        
        First_look_plot <- ggplot(data = plot_data, aes(x = sample_time)) +
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
        
        
        Looking_at_AOI_plot <- ggplot(data = plot_data, aes(x = sample_time)) +
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
    
    
    output[["binLook_plot"]] <- renderPlot({
      
      ET_data <- ET_binned()
      
      ET_summ <- summarise_bins(ET_data)
      
      ggplot(ET_summ, aes(x = bin_label,
                          y = prop_FL,
                          group = Group,
                          color = Group)) +
        geom_point(alpha = 0.4,
                   position = position_jitterdodge()) +
        #geom_vline(xintercept=seq(from = 1.5, to = 9.5, by = 1 ), color="black")+ 
        stat_summary(fun.data = "mean_cl_normal",
                     geom = "pointrange",
                     position = position_dodge(width = 0.5)) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    })
    
    # Add ability to download the first look data visualized on screen
    output[["download_BinnedData"]] <- downloadHandler(
      filename = function() {
        paste("dataset-", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        data_binned <- ET_binned()
        
        data_summ <- summarise_bins(data_binned)
        
        write_csv(data_summ, file)
      },
      contentType = "text/csv"
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
