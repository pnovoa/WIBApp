require(tidyverse)
library(iosoi)


plots_colors <- c("#0373B7", "#F39C13", "#DD4B39", "#08944F")

lst_of_ref_sol_crit <- c("Lower bound score", 
                         "Upper bound score", 
                         "Equal weights score")

lst_of_ref_sol_crit_abbrv <- c("LB", "UB","EW")

lst_attitudes <- c("Neutral attitude", 
                   "Optimistic attitude", 
                   "Pessimistic attitude")

lst_element_to_be_ordered_by <- c("Lower bound score", 
                                  "Upper bound score", 
                                  "Equal weights score", 
                                  "Neutral attitude", 
                                  "Optimistic attitude", 
                                  "Pessimistic attitude", 
                                  "Solution")

lst_ranking_criteria <- c("LB", 
                                  "UB", 
                                  "neutral", 
                                  "optimistic", 
                                  "pessimistic")

names(lst_ranking_criteria) <- lst_element_to_be_ordered_by[-c(3,7)]


lst_data_to_export <- c("Input data (current version)", 
                        "Results (all solutions)", 
                        "Results (only solutions of interest)", 
                        "Plots (only solutions of interest)",
                        "Sensitivity analysis")


# Retorna un dataframe con los indicadores del enfoque para evaluar
# a cada solución

# compute_extreme_weights <- function(n_crit){
#   m_extpoints <- matrix(1/c(n_crit:1), n_crit, n_crit) 
#   m_extpoints[lower.tri(m_extpoints)] <- 0
#   m_extpoints <- t(m_extpoints)
#   return(m_extpoints)
# }


get_sorted_criteria <- function(criteria){
  
  sorted_criteria <- criteria$`Short name`[order(criteria$Importance)]
  
  return(sorted_criteria)
  
}


get_data_frame_criteria <- function(criteria){
  
  sorted_criteria <- get_sorted_criteria(criteria)
  
  dfr_criteria <- criteria %>% 
    mutate(`Short name` = factor(`Short name`, levels = sorted_criteria)) %>% 
    rename("Criterion" = `Short name`, "LB" = `Domain min`, "UB" = `Domain max`)
  
  return(dfr_criteria)
  
}


get_normalized_evaluations <- function(criteria, evaluations){
  
  dfr_criteria <- get_data_frame_criteria(criteria)
  
  dfr_evaluations <- evaluations %>% 
    gather(key = "Criterion", value = "Evaluation", -Solution) %>% 
    inner_join(dfr_criteria, by="Criterion") %>% 
    mutate(Norm.Eval = ifelse(
      Goal == "min",
      1-(Evaluation - LB)/(UB - LB),
      (Evaluation - LB)/(UB - LB))) %>% 
    select(Solution, Criterion, Norm.Eval) %>% 
    spread(key="Criterion", value="Norm.Eval")
  
  return(dfr_evaluations)
  
}

get_evaluation_matrix <- function(criteria, evaluations){
  
  sorted_criteria <- get_sorted_criteria(criteria)
  
  dfr_evaluations <- get_normalized_evaluations(criteria, evaluations)
  
  m_evaluations <- as.matrix(dfr_evaluations[,-1][, sorted_criteria])
  rownames(m_evaluations) <- dfr_evaluations$Solution
  colnames(m_evaluations) <- sorted_criteria
  
  return(m_evaluations)
}


compute_results <- function(criteria, evaluations, solutions){
  
  n_crit <- nrow(criteria)
  
  m_evaluations <- get_evaluation_matrix(criteria, evaluations)
  
  m_results <- m_evaluations %>% 
    iosoi::score() %>%
    iosoi::intervals() %>%
    iosoi::reference() %>%
    iosoi::poss_assess(by = "all") %>%
    rank_sois(by = "neutral")
  
  coleqw <- which(str_starts(colnames(m_results), "VE_"))[n_crit]
  
  colnames(m_results)[coleqw] <- "Equal weights score"
    
  dfr_computed_results <- as_tibble(m_results, rownames = "Solution")
  
  dfr_computed_results <- dfr_computed_results %>% 
    rename(
    "Neutral attitude" = neutral,
    "Pessimistic attitude" = pessimistic,
    "Optimistic attitude" = optimistic,
    "Lower bound score" = LB,
    "Upper bound score" = UB,
  )
  
  dfr_computed_results <- dfr_computed_results %>%
    inner_join(solutions, by=c("Solution"="Short name"))
  
  return(dfr_computed_results)
  
}



ggplot_intervals <- function(app_input, dfresults){
  
  ordered_element <- app_input$sel_element_to_be_ordered_by
  descending <- app_input$cbx_descending
  flip_coords <- app_input$cbx_flip_coords
  rotate_x_axis_label <- app_input$cbx_rotate_x
  rotate_y_axis_label <- app_input$cbx_rotate_y
  
  show_long_names <- app_input$cbx_show_solutions_long_names_int
  
  results <- dfresults
  
  if(show_long_names){
    results <- results %>%
      select(-Solution) %>%
      rename("Solution"=`Long name`)
  } 
  
  #rs_goal <- app_input$sel_rs_goal
  #rs_crit <- app_input$sel_rs_criteria
  
  ref_sol_lb_score <- as.numeric(results[results$REF == 1, "Lower bound score"])
  
  if(descending){
    dfr_alt <- results %>% 
      arrange(desc(!!sym(ordered_element)))
  }else{
    dfr_alt <- results %>% 
      arrange(!!sym(ordered_element))
  }
  dfr_alt <- dfr_alt %>% select(Solution)
  
  dfr_results <- results %>% 
    mutate(Type = ifelse(REF == 1, "Reference solution", "Others"),
           Solution = factor(Solution, levels = dfr_alt$Solution))
  
  xlab_text <- "Solution"
  if(nrow(dfr_results) > 100){
    xlab_text <- "Solution (only the first 100 are shown)"
  }
  
  p <- dfr_results %>% slice_head(n = min(nrow(dfr_results), 100)) %>%
    ggplot(aes(x=Solution, y=`Equal weights score`, ymin=`Lower bound score`, ymax=`Upper bound score`, color=Type)) +
    geom_pointrange() + 
    geom_point(shape=21, fill="white") +
    scale_color_discrete(type = plots_colors[c(1,3)]) +
    geom_hline(yintercept = ref_sol_lb_score, linetype="dotted") +
    ylab("Score") +
    xlab(xlab_text) +
    theme_classic() + 
    ggtitle(label = "Comparison of scoring intervals")
  
  if(rotate_x_axis_label){
    p <- p + theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
    )
  }
  
  if(rotate_y_axis_label){
    p <- p + theme(
      axis.text.y = element_text(angle = 90, vjust = 1, hjust=0.5)
    )
  }
  
  if(flip_coords){
    p <- p + coord_flip()
  }
  p 
}


ggplot_comparison <- function(app_input, dfr_evaluations, dfr_criteria, dfr_solutions){
  
  sol1 <- app_input$sel_solution1
  sol2 <- app_input$sel_solution2
  
  flip_coords <- app_input$cbx_flip_coords_comp
  
  wrap_size <- 6
  
  if(sol1 != sol2){
    sorted_criteria <- dfr_criteria$`Short name`[order(dfr_criteria$Importance)]
    sorted_criteria_long <- dfr_criteria$`Long name`[order(dfr_criteria$Importance)]
    
    #sorted_criteria_long <- lapply(sorted_criteria_long, function(strx) ifelse(str_length(strx)>=wrap_size, paste0(str_sub(strx, 1, wrap_size), "..."), strx)) %>% 
    #  as_vector()
    
    
    dfr_data <- dfr_evaluations %>% 
      filter(Solution %in% c(sol1, sol2)) %>%
      select(Solution, all_of(sorted_criteria)) %>%
      pivot_longer(cols = -Solution, names_to = "Criteria", values_to = "Evaluation") %>%
      mutate(Criteria = factor(Criteria, levels=sorted_criteria),
             Solution = factor(Solution, levels=c(sol1, sol2)))
    
    show_criteria_long_names <- app_input$cbx_show_criteria_long_names_comp
    if(show_criteria_long_names){
      dfr_data <- dfr_data %>% inner_join(dfr_criteria, by=c("Criteria"="Short name")) %>%
        mutate(Criteria = `Long name`) %>% select(-`Long name`) %>%
        #mutate(Criteria = factor(Criteria, levels=sorted_criteria_long)) %>%
        #mutate(Criteria = ifelse(str_length(Criteria) >= wrap_size, paste0(str_sub(Criteria, 1, wrap_size), "..."), Criteria)) %>%
        mutate(Criteria = factor(Criteria, levels=sorted_criteria_long))
    }
    
    show_solutions_long_names <- app_input$cbx_show_solutions_long_names_comp
    if(show_solutions_long_names){
      dfr_data_ <- dfr_data %>% 
        inner_join(dfr_solutions, by=c("Solution"="Short name")) %>%
        mutate(Solution = factor(Solution, levels=c(sol1, sol2)))
        
      
      sol_names <- dfr_data_ %>% select(Solution, `Long name`) %>% unique() %>%
        arrange(Solution) %>% select(`Long name`)
      
      dfr_data <- dfr_data_ %>%
        mutate(Solution = `Long name`) %>%
        select(-`Long name`) %>%
        mutate(Solution = factor(Solution, levels=sol_names$`Long name`))
    }
    
    
    p <- dfr_data %>% 
      ggplot(aes(x=Criteria, y=Evaluation, fill=Solution)) +
      geom_bar(stat = "identity", position = "dodge", width = 0.7) + 
      scale_fill_discrete(type = plots_colors) + 
      theme_classic() + 
      ggtitle(label = "Comparison by criteria")
    
    if(flip_coords){
      p <- p + coord_flip()
    }
    
    return(p)
  }

  return(NULL)
  
}


perform_criteria_sensitivity_analysis <- function(criteria, evaluations, rank_criterion, strength){
  
  m_eval <- get_evaluation_matrix(criteria, evaluations)
  
  sorted_criteria <- get_sorted_criteria(criteria)
  
  crit_pref <- 1:length(sorted_criteria)
  
  names(crit_pref) <- sorted_criteria
  
  sens_results <- one_to_one_swap_sensitivity_analysis(m_eval, crit_pref, rank_criterion, strength)
  
  sens_results <- sens_results %>%
    select(1, 4:8) %>%
    rename(
      "Swap" = swap,
      "WS Coeff." = ws,
      "Blest's coeff." = blest,
      "Weighted coeff." = weighted,
      "Kendall" = kendall,
      "Spearman" = spearman
    ) %>%
    mutate(across(-Swap, as.double)) %>%
    mutate(Swap = str_replace_all(Swap, "<=>", " ↔ "))
  
  return(
    sens_results
  )
  
}






