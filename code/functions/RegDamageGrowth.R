#This function conducts the regression analysis for the damage function
#by Jiaru Zhong, 04.09.2023





RegDamageGrowth <- function(data,model){
  # Initialize an empty list to store results with names
  RegResults <- list()
  
  #Determine the model function, whether and how adatptive capability is incooperated  
  if (model == "NoAdaptation") {
    alist <- c("temp", "I(temp^2)", "prec", "I(prec^2)")
    col_select <- c("Growth_income","temp", "prec","Year","state","panelid")
    
  } else if (model == "AdaptationPeron") {
    alist <- c("temp", "I(temp^2)", "prec", "I(prec^2)", 
               "IG_hetero:temp", "IG_hetero:I(temp^2)", 
               "IG_hetero:prec", "IG_hetero:I(prec^2)")
    col_select <- c("Growth_income","temp", "prec","IG_hetero","Year","state","panelid")
  } else if (model == "AdaptationGilli") {
    alist <- c("temp", "I(temp^2)", "prec", "I(prec^2)", 
               "PTNI05L:temp", "PTNI05L:I(temp^2)", 
               "PTNI05L:prec", "PTNI05L:I(prec^2)")
    col_select <- c("Growth_income","temp", "prec","PTNI05L","Year","state","panelid")
  } else {
    # Handle other cases or provide a default value if necessary
    print('Please enter the correct model, NoAdaptation,AdaptationPeron,or AdaptationGilli')}
    
  #Complete the regression function by adding fixed effects.  
  formula_str <- paste("Growth_income ~ ", paste(alist, collapse = " + "),
                        "+ as.numeric(Year) : state + I(as.numeric(Year)^2) : state  + state + Year")  

  

  #Regression across each income group
  for (i in na.omit(unique(data$cpercentile))) {
    
    subset_data <- data %>%
      dplyr::filter(cpercentile == i) %>%
      select(all_of(col_select))
    
    RegResults [[i]] <- lm(formula_str, data = subset_data)
    
    }
  
  print(formula_str)  
  
  return(RegResults)
  
}

