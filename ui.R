#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(sortable)
library(DT)
library(plotly)
source("R/assistant.R")



dashboardPage(
  dashboardHeader(title = "WIBApp 0.1.0"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "tbi_home", icon = icon("house")),
      menuItem("Data", tabName = "tbi_data", icon = icon("upload")),
      menuItem("Settings", tabName = "tbi_settings", icon = icon("gear")),
      menuItem("Results", tabName = "tbi_results", icon = icon("table")),
      menuItem("Plots", tabName = "tbi_plots", icon = icon("chart-line")),
      menuItem("Sensitivity", tabName = "tbi_sensitivity", icon = icon("magnifying-glass-chart")),
      menuItem("Export", tabName = "tbi_export", icon = icon("file-export")),
      menuItem("Help", tabName = "tbi_help", icon = icon("info"),
               menuItem("Input",  tabName = "tbi_help_input", icon = icon("info")),
               menuItem("Output", tabName = "tbi_help_output", icon = icon("info"))
      )
    )
  ),
  
  
  dashboardBody(
    tabItems(
      tabItem(
        "tbi_home",
        box(
          title = NULL,
          status = "info",
          includeHTML("www/home.html"),
          width = NULL
        )
      ),
      
      tabItem(
        "tbi_data",
        box(
          includeHTML("www/data.html"),
          status = "info",
          fileInput(
            inputId = "file_loaded",
            label = "Choose a .xlsx (or .ods) file to upload",
            accept = c(".xlsx", ".ods")
          ),
          width = NULL
        ),
        box(
          width = NULL,
          collapsible = T,
          title = "Summary",
          valueBoxOutput(outputId = "vbx_criteria"),
          valueBoxOutput(outputId = "vbx_solutions"),
          valueBoxOutput(outputId = "vbx_evaluations")
        ),
        box(
          title = "Criteria",
          width = NULL,
          collapsible = T,
          DT::DTOutput(outputId = "dtb_criteria")
        ),
        box(
          title = "Alternatives",
          width = NULL,
          collapsible = T,
          DT::DTOutput(outputId = "dtb_solutions")
        ),
        box(
          title = "Evaluations",
          width = NULL,
          collapsible = T,
          DT::DTOutput(outputId = "dtb_evaluations")
        )
      ),
      
      tabItem(
        tabName = "tbi_settings",
        box(
          includeHTML("www/settings.html"),
          status = "info",
          width = NULL
        ),
        box(width = NULL, collapsible = F, title = "Criteria preference", collapsed = T,
            htmlOutput("rkl_criteria_preference"),
            actionButton("btn_results", "Compute results")
        )
      ),
      
      tabItem(tabName = "tbi_results",
              box(
                width = NULL,
                status = "info",
                includeHTML("www/results.html")
              ),
              box(
                title = "Alternatives",
                width = NULL,
                sidebarLayout(
                  position = "left",
                  fluid = F,
                  sidebarPanel = sidebarPanel(
                    width = 3,
                    #h4("Selection of solutions of interest (SOI)"),
                    p("To filter the alternatives of interest, choose an attitude and a number in the slider."),
                    br(),
                    selectInput(inputId = "sel_attitudes", label = "Attitude", choices = lst_attitudes),
                    uiOutput("sld_number_sois"),
                    checkboxInput(inputId = "cbx_show_long_names_dt", label = "Show long name of alternatives", value = F)
                    #actionButton("btn_filter_results", "Filter", icon = icon("filter"))
                  ),
                  mainPanel = mainPanel(
                    
                    DT::dataTableOutput(outputId = "dtb_results")
                    
                  )
                )
              )
      ),
      
      tabItem(tabName = "tbi_plots",
              box(
                includeHTML("www/plots.html"),width = NULL,status = "info"
              ),
              box(title = "Intervals",
                  width = NULL, collapsible = TRUE,
                  p('This plot shows through segments the range 
                    of scores for each alternative, as well as the score 
                    corresponding to the case in which all criteria are 
                    equally important. The latter information is indicated 
                    by a dot for each alternative.'),
                  sidebarLayout(
                    sidebarPanel = sidebarPanel(
                      selectInput(inputId = "sel_element_to_be_ordered_by", label = "Order by", choices = lst_element_to_be_ordered_by),
                      checkboxInput(inputId = "cbx_descending", label = "Descending", value = F),
                      checkboxInput(inputId = "cbx_flip_coords", label = "Flip the coordinates", value = T),
                      checkboxInput(inputId = "cbx_rotate_x", label = "Rotate x-axis labels", value = F),
                      checkboxInput(inputId = "cbx_rotate_y", label = "Rotate y-axis labels", value = F),
                      checkboxInput(inputId = "cbx_show_legend", label = "Show legend", value = T),
                      checkboxInput(inputId = "cbx_show_solutions_long_names_int", label = "Show the long names of alternatives", value = F)
                    ),
                    
                    mainPanel = mainPanel(
                      
                      plotlyOutput(outputId = "plt_intervals")
                      
                    )
                    
                  )
              ),
              box(title = "Alternative comparison",
                  width = NULL, collapsible = TRUE,
                  p('This plot allows pairwise comparison of problem 
                    alternatives for each evaluation criterion. The value of 
                    the original evaluations has been normalized in the 
                    range [0,1] so that a value close to 1 means that 
                    the evaluation is "very good", while a value close 
                    to 0 means the opposite.'),
                  sidebarLayout(
                    sidebarPanel = sidebarPanel(
                      p("Select two alternatives to compare:"),
                      br(),
                      uiOutput("sel_solution1"),
                      uiOutput("sel_solution2"),
                      p("Plot settings"),
                      checkboxInput(inputId = "cbx_show_solutions_long_names_comp", label = "Show the long names of alternatives", value = F),
                      checkboxInput(inputId = "cbx_show_criteria_long_names_comp", label = "Show the long names of criteria", value = F),
                      checkboxInput(inputId = "cbx_flip_coords_comp", label = "Flip the coordinates", value = F)
                    ),
                    
                    mainPanel = mainPanel(
                      plotlyOutput(outputId = "plt_comparison")
                    ),
                    
                    
                  )
              )
      ),
      tabItem(
        tabName = "tbi_sensitivity",
        box(
          includeHTML("www/sensitivity.html"),width = NULL,status = "info"
        ),
        box(title = "Local sensitivity analysis",
            width = NULL, collapsible = TRUE,
            p('This functionality allows you to perform sensitivity 
              analyses in relation to the preference of criteria. 
              Specifically, different preferences obtained as perturbations 
              to the one set out in Settings are explored.'),
            sidebarLayout(
              sidebarPanel = sidebarPanel(
                uiOutput("sel_ranking_criteria"),
                uiOutput("sld_swapping_strength")
              ),
              
              mainPanel = mainPanel(
                
                DT::dataTableOutput(outputId = "dtb_sensitivity_cp"),
                HTML("<br>"),
                htmlOutput("summary_label"),
                DT::dataTableOutput(outputId = "dtb_sensitivity_cp_stats")
                
              )
              
            )
        )
      ),
      tabItem(
        tabName = "tbi_export",
        box(
          includeHTML("www/export.html"),width = NULL,status = "info"
        ),
        box(
          width = NULL,
          checkboxGroupInput(inputId = "cbg_data_to_export", 
                             label = "Data to export", 
                             choices = lst_data_to_export, 
                             selected = lst_data_to_export, 
                             inline = T),
          downloadButton(outputId = "btn_download_excel", label = "Excel", icon = icon("file-excel"))
          #downloadButton(outputId = "btn_download_html", label = "HTML", icon = icon("code"))
          
        )
      ),
      
      tabItem("tbi_help_input",
              box(
                title = NULL,  status = "info",
                includeHTML("www/help_input.html"),
                width = NULL
              )
      ),
      
      tabItem("tbi_help_output",
              box(
                title = NULL,  status = "info",
                includeHTML("www/help_output.html"),
                width = NULL
              )
      )
      
    ))
  
)
