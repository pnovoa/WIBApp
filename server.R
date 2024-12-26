#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(sortable)
library(openxlsx)
library(patchwork)
library(readODS)


function(input, output, session) {
  
  
  app_data <- reactiveValues(dfr_criteria = NULL, 
                             dfr_solutions = NULL, 
                             dfr_evaluations = NULL,
                             dfr_results = NULL,
                             dfr_results_all = NULL,
                             lst_weights = NULL,
                             dfr_sensitivity = NULL,
                             dfr_sensitivity_stats = NULL
  )
  
  data_is_loaded <- function(){
    return(!is.null(app_data$dfr_criteria))
  }
  
  result_is_computed <- function(){
    return(!is.null(app_data$dfr_results))
  }
  
  
  compute_results_ <- function(){
    
    
    app_data$dfr_results_all <- compute_results(
      criteria=app_data$dfr_criteria,
      evaluations=app_data$dfr_evaluations,
      solutions=app_data$dfr_solutions
    )
    
    app_data$dfr_results <- app_data$dfr_results_all
  }
  
  perform_sensitivity_analysis_ <- function(){
    
    if(is.null(input$sld_swapping_strength)){
      rkg_criteria = lst_ranking_criteria[1]
      strng = 1
    } else{
      rkg_criteria = input$sel_ranking_criteria
      strng = input$sld_swapping_strength
    }
    
    app_data$dfr_sensitivity <- perform_criteria_sensitivity_analysis(
      app_data$dfr_criteria,
      app_data$dfr_evaluations,
      rkg_criteria,
      strng
    ) 
    
  }
  
  
  compute_stats_ <- function(){
    
    app_data$dfr_sensitivity_stats <- app_data$dfr_sensitivity %>%
      pivot_longer(cols = -Swap, names_to = "Coefficient", values_to = "value") %>%
      group_by(Coefficient) %>%
      summarise(Mean = mean(value), SD = sd(value), Min = min(value), Max = max(value)) %>%
      mutate(Coefficient = factor(Coefficient, levels = colnames(app_data$dfr_sensitivity)[-1])) %>%
      arrange(Coefficient)
    
  }
  
  
  read_data_ <- function(file_path, file_ext){
    
    read_func <- readxl::read_excel
    
    if(file_ext == "ods"){
      
      read_func <- readODS::read_ods
      
    }
    
    name_repair <- function(x){
      str_replace_all(x, pattern = "\\.", " ")
    }
    
    app_data$dfr_criteria <- read_func(path = file_path, sheet = "criteria", .name_repair=name_repair)
    app_data$dfr_criteria <- app_data$dfr_criteria %>% mutate(Goal = factor(Goal, levels = c("min", "max")))
    app_data$dfr_solutions <- read_func(path = file_path, sheet = "alternatives", .name_repair=name_repair)
    app_data$dfr_evaluations <- read_func(path = file_path, sheet = "evaluations")
    app_data$n_solutions <- nrow(app_data$dfr_solutions)
    
    
    
  }
  
  
  observeEvent(input$file_loaded,{
    
    loaded_file <- input$file_loaded
    ext <- tools::file_ext(loaded_file$datapath)
    req(loaded_file)
    validate(need((ext == "xlsx" || ext == "ods"), "Please upload a xlsx file"))
    
    read_data_(file_path = loaded_file$datapath, file_ext = ext)
    #app_data$df_comp_orig <- app_data$df_comp
    compute_results_()
    
    
    
    #app_data$criteria_val_lbl <- setNames(app_data$df_criteria$`Short name`, app_data$df_criteria$`Long name`)
    #app_data$criteria_lbl_val <- setNames(app_data$df_criteria$`Long name`, app_data$df_criteria$`Short name`)
    
  })
  
  
  # Cajas de valor para el resumn (Data->Summary)
  
  
  output$vbx_criteria <- renderValueBox({
    n_ <- nrow(app_data$dfr_criteria)
    valueBox(n_, 
             "Criteria", icon = icon("rectangle-list"), color = "blue")
    
  })
  
  output$vbx_solutions <- renderValueBox({
    n_ <- nrow(app_data$dfr_solutions)
    valueBox(n_, 
             "Alternatives", icon = icon("rectangle-list"), color = "yellow")
  })
  
  output$vbx_evaluations <- renderValueBox({
    n_ <- nrow(app_data$dfr_evaluations)*(ncol(app_data$dfr_evaluations)-1)
    valueBox(n_, 
             "Evaluations", icon = icon("rectangle-list"), color = "red")
  })
  
  
  
  #Tablas de entrada de datos (Data)
  
  output$dtb_criteria <- DT::renderDT(app_data$dfr_criteria, selection='none', server = T, 
                                      editable=list(target = "cell", disable = list(columns = c(1))))
  observeEvent(input$dtb_criteria_cell_edit, {
    
    app_data$dfr_criteria <<- editData(app_data$dfr_criteria, input$dtb_criteria_cell_edit, "dtb_criteria")
    
    crit_labels <- app_data$dfr_criteria %>% 
      arrange(Importance) %>%
      select(`Long name`)
    
    # update list of criteria in Results
    app_data$lst_weights <- as.list(crit_labels$`Long name`)
    
    # update Results
    #app_data$df_comp <- compute_possib()
    #app_data$df_comp_orig <- app_data$df_comp
  })
  
  
  output$dtb_solutions <- DT::renderDT(app_data$dfr_solutions, selection='none', server = T, 
                                       editable=list(target = "cell", disable = list(columns = c(1))))
  observeEvent(input$dtb_solutions, {
    app_data$dfr_solutions <<- editData(app_data$dfr_solutions, input$dtb_solutions_cell_edit, "dtb_solutions")
    
    #app_data$dfr_eval$Solution <- app_data$dfr_solutions$`Short name`
    
    #app_data$df_comp <- compute_possib()
    #app_data$df_comp_orig <- app_data$df_comp
  })
  
  
  output$dtb_evaluations <- DT::renderDT({
    if(data_is_loaded()){
      datatable(
        data = app_data$dfr_evaluations,
        selection='none', 
        editable=list(target = "cell", disable = list(columns = c(1)))
      ) %>% formatRound(columns=colnames(app_data$dfr_evaluations)[-1], digits=3)
    }
  }
  )
  observeEvent(input$dbl_evaluations_cell_edit, {
    app_data$dfr_evaluations <<- editData(app_data$dfr_evaluations, input$dbl_evaluations_cell_edit, "dbl_evaluations")
    
  })
  
  
  output$rkl_criteria_preference <- renderUI({
    if(data_is_loaded()){
      dfr_crit_labels <- app_data$dfr_criteria %>% 
        arrange(Importance) %>%
        select(`Long name`)
      
      rklist <- rank_list(
        text = "Drag the elements in the desired order to establish the importance of each criterion. 
      The top criterion is the most important, the bottom criterion the least.",
        labels = dfr_crit_labels$`Long name`,
        input_id = "rkl_criteria_preference_input"
      )
      
      return(
        rklist
      )
    }
    
  })
  
  observeEvent(input$btn_results, {
    
    if(data_is_loaded()){
      
      dfr_new_crit_imp <- tibble(
        `Long name` =  input$rkl_criteria_preference_input,
        Importance = 1:length(input$rkl_criteria_preference_input)
      )
      
      app_data$dfr_criteria <<- app_data$dfr_criteria %>% select(-Importance) %>%
        inner_join(dfr_new_crit_imp, by="Long name")
      
      #sorted_weights <- input$weights_rank_list
      
      #datafile$df_criteria$Importance <- order(sorted_weights)
      
      compute_results_()
      #datafile$df_comp_orig <- datafile$df_comp
    }
    
  })
  
  
  
  output$dtb_results <- DT::renderDataTable({
    
    if(result_is_computed()){
      
      crit_names <- app_data$dfr_criteria$`Short name`
      
      col_to_exclude <- app_data$dfr_results %>%
        select(all_of(crit_names), all_of(starts_with("VE_")), all_of("REF")) %>% 
        colnames()
      
      lst_criteria <- app_data$dfr_results %>% 
        select(-all_of(c(col_to_exclude, "Alternative", "Long name"))) %>% 
        colnames()
      
      eval_range <- range(app_data$dfr_results[lst_criteria])
      
      
      
      dt_to_show <- app_data$dfr_results %>% select(-all_of(col_to_exclude))
      
      if(input$cbx_show_long_names_dt){
        dt_to_show <- dt_to_show %>% 
          select(-Alternative) %>%
          rename("Alternative" = `Long name`)
        
        dt_to_show <- dt_to_show %>% 
          select(all_of(c("Alternative", lst_criteria)))
        
      }else{
        dt_to_show <- dt_to_show %>% select(-`Long name`)
      }
      
      max_lb <- dt_to_show[app_data$dfr_results$REF==1, "Alternative"]
      
      datatable(dt_to_show, 
                extensions = 'Buttons', options = list(
                  dom = 'Bfrtip',
                  buttons = 
                    list('copy', 'print', list(
                      extend = 'collection',
                      buttons = c('csv', 'excel', 'pdf'),
                      text = 'Download'
                    )),
                  paging = FALSE
                ), 
      ) %>% 
        formatRound(columns=lst_criteria, digits=3) %>%
        formatStyle(lst_criteria,
                    background = styleColorBar(eval_range, '#FFCB42'),
                    backgroundSize = '98% 88%',
                    backgroundRepeat = 'no-repeat',
                    backgroundPosition = 'center') %>%
        formatStyle(
          1,
          target = "row",
          fontWeight = styleEqual(max_lb, "bold")
        )
    }
  })
  
  output$sld_number_sois <- renderUI({
    
    if(!result_is_computed()) {
      return(NULL)
    }
    
    sliderInput("sld_number_sois_", label = "Number of solutions with the highest degree of possibility",
                min = 1, 
                max = app_data$n_solutions,
                step = 1,
                value = nrow(app_data$dfr_results),
                ticks = T)
  })
  
  
  filter_results <- function(){
    
    if(result_is_computed()){
      
      the_attitude <- input$sel_attitudes
      the_n_of_sois <- input$sld_number_sois_
      
      app_data$dfr_results <- app_data$dfr_results_all %>% 
        top_n(wt = !!sym(the_attitude), n=the_n_of_sois)
        #slice_max(order_by = !!sym(the_attitude), n = the_n_of_sois)
      
    }
  }
  
  
  observeEvent(input$sld_number_sois_, {
    
    filter_results()
    
  })
  
  observeEvent(input$sel_attitudes, {
    
    filter_results()
    
  })
  
  
  # Plots
  
  output$plt_intervals <- renderPlotly({
    if(result_is_computed()){
      
      pgg <- ggplot_intervals(input, app_data$dfr_results)
      
      p <- ggplotly(
        pgg, tooltip = "text"
      ) 
      
      showleg <- input$cbx_show_legend
      p <- p %>% layout(showlegend = showleg)
      
      p
      
    }
  })
  
  
  get_solutions_to_plot <- function(){
    solutions <- app_data$dfr_results$Alternative
    names(solutions) <- app_data$dfr_results$`Long name`
    return(solutions)
  }
  
  
  output$sel_solution1 <- renderUI({
    
    if(!result_is_computed()) {
      return(NULL)
    }
    
    
    
    solutions <- get_solutions_to_plot()
    
    selectInput(inputId = "sel_solution1", 
                label = "Alternative 1:", 
                choices = solutions)
  })
  
  
  output$sel_solution2 <- renderUI({
    
    if(!result_is_computed()) {
      return(NULL)
    }
    
    solutions <- get_solutions_to_plot()
    
    selectInput(inputId = "sel_solution2", 
                label = "Alternative 2:", 
                choices = solutions,
                selected = ifelse(length(solutions)>1, solutions[2], solutions[1])
    )
  })
  
  
  output$plt_comparison <- renderPlotly({
    if(result_is_computed() && !is.null(input$sel_solution1) && !is.null(input$sel_solution2)){

      pgg <- ggplot_comparison(input, 
                               app_data$dfr_results, 
                               app_data$dfr_criteria, 
                               app_data$dfr_solutions)

      if(!is.null(pgg)){
        p <- ggplotly(
          pgg
        )
        p  
      }
      
    }
  })
  
  
  
  # Export
  
  
  output$btn_download_excel <- downloadHandler(
    
    filename = function() {
      "sofi_export.xlsx"
    },
    
    content = function(file) {
      
      
      if(result_is_computed()){
        
        choics <- input$cbg_data_to_export
        
        if(length(choics) > 0){
          
          my_workbook <- createWorkbook()
          
          data_to_export <- list()
          
          if(lst_data_to_export[1] %in% choics){
            
            data_to_export$criteria <- app_data$dfr_criteria
            data_to_export$solutions <- app_data$dfr_solutions
            data_to_export$evaluations <- app_data$dfr_evaluations
            
          } 
          
          if (lst_data_to_export[2] %in% choics){
            
            data_to_export$results_all <- app_data$dfr_results_all
            
          }
          
          if (lst_data_to_export[3] %in% choics){
            
            data_to_export$results_soi <- app_data$dfr_results
            
          }
          
          if (lst_data_to_export[5] %in% choics){
            
            data_to_export$sensitivity <- app_data$dfr_sensitivity
            
            data_to_export$sensitivity_summary <- app_data$dfr_sensitivity_stats
            
          }
          
          
          for(n in names(data_to_export)){
            
            sheet_name <- n
            sheet_data <- data_to_export[[n]]
            
            addWorksheet(
              wb = my_workbook,
              sheetName = sheet_name
              
            )
            
            writeDataTable(
              my_workbook,
              sheet = sheet_name,
              x = sheet_data
            )
            
          }
          
          if (lst_data_to_export[4] %in% choics){
            
            addWorksheet(my_workbook, "plots", gridLines = F)
            
            p1 <- ggplot_intervals(input, app_data$dfr_results)
            
            
            p2 <- ggplot_comparison(input,
                                    app_data$dfr_results, 
                                    app_data$dfr_criteria,
                                    app_data$dfr_solutions)
            
            p3 <- p1 + p2
            
            ggsave(filename = "p3.png", plot = p3, width = 20, height = 6)
            insertImage(my_workbook, "plots", "p3.png")
            
          }
          
          saveWorkbook(my_workbook, file)
          
        }
        
        
      }
      
    }
  )
  
  
  # Sensitivity
  
  
  output$sel_ranking_criteria <- renderUI({
    
    if(!result_is_computed()) {
      return(NULL)
    }
    
    selectInput(inputId = "sel_ranking_criteria", 
                  label = "Ranking indicator", 
                  choices = lst_ranking_criteria
                  )
    }
  )
  
  
  output$sld_swapping_strength <- renderUI({
    
    if(!result_is_computed()) {
      return(NULL)
    }
    
    max_strength <- nrow(app_data$dfr_criteria)-1
    
    sliderInput(inputId = "sld_swapping_strength", 
                label = "Swap strength", 
                min = 1, 
                max = max_strength, 
                value = 1, 
                step = 1)
  }
  )
  
  output$dtb_sensitivity_cp <- DT::renderDT({
    
    if(!result_is_computed()) {
      return(NULL)
    }
    
    perform_sensitivity_analysis_()
    
    lst_criteria <- c("WS Coeff.", "Blest's coeff.", "Weighted coeff.", "Kendall", "Spearman")
    
    max_lb <- which.max(app_data$dfr_sensitivity$`WS Coeff.`)
    
    eval_range <- c(0,1)
    
    datatable(app_data$dfr_sensitivity, 
              extensions = 'Buttons', options = list(
                dom = 'Bfrtip',
                buttons = 
                  list('copy', 'print', list(
                    extend = 'collection',
                    buttons = c('csv', 'excel', 'pdf'),
                    text = 'Download'
                  )),
                paging = TRUE
              ), 
    ) %>% 
      formatRound(columns=lst_criteria, digits=3) %>%
      formatStyle(lst_criteria,
                  background = styleColorBar(eval_range, '#FFCB42'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center') %>%
      formatStyle(
        1,
        target = "row",
        fontWeight = styleEqual(max_lb, "bold")
      )
    
  })
  
  
  output$dtb_sensitivity_cp_stats <- DT::renderDT({
    
    if(!result_is_computed()) {
      return(NULL)
    }
    
    compute_stats_()
    
    lst_criteria <- c("Mean", "SD", "Min", "Max")
    
    max_lb <- which.max(app_data$dfr_sensitivity_stats$Mean)
    
    eval_range <- c(0,1)
    
    datatable(app_data$dfr_sensitivity_stats, 
              extensions = 'Buttons', options = list(
                dom = 'Bfrtip',
                buttons = 
                  list('copy', 'print', list(
                    extend = 'collection',
                    buttons = c('csv', 'excel', 'pdf'),
                    text = 'Download'
                  )),
                paging = FALSE
              ), 
    ) %>% 
      formatRound(columns=lst_criteria, digits=3) %>%
      formatStyle(lst_criteria,
                  background = styleColorBar(eval_range, '#FFCB42'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center') %>%
      formatStyle(
        1,
        target = "row",
        fontWeight = styleEqual(max_lb, "bold")
      )
    
  })
  
  output$summary_label <- renderUI({
    if(data_is_loaded()){
      HTML(text = "<h3>Summary</h3>")
    }
    
  }
    )
  
}
