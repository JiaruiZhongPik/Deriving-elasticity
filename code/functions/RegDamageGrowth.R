#This function conducts the regression analysis for the damage function
#by Jiaru Zhong, 04.09.2023





RegDamageGrowth <- function(data,model){
  # Initialize an empty list to store results with names
  RegResults <- list()
  
  #Determine the model function, whether and how adatptive capability is incooperated  
  if (model == "NoAdaptation") {
    alist <- c("temp", "I(temp^2)", "prec", "I(prec^2)")
    col_select <- c("Growth_income","temp", "prec","Year","state","panelid","ctemp_gswp3")
    
  } else if (model == "AdaptationPeron") {
    alist <- c("temp", "I(temp^2)", "prec", "I(prec^2)", 
               "IG_hetero:temp", "IG_hetero:I(temp^2)", 
               "IG_hetero:prec", "IG_hetero:I(prec^2)")
    col_select <- c("Growth_income","temp", "prec","IG_hetero","Year","state","panelid","ctemp_gswp3")
  } else if (model == "AdaptationGilli") {
    alist <- c("temp", "I(temp^2)", "prec", "I(prec^2)", 
               "wid_stateincome_ppp05:temp", "wid_stateincome_ppp05:I(temp^2)", 
               "wid_stateincome_ppp05:prec", "wid_stateincome_ppp05:I(prec^2)")
    col_select <- c("Growth_income","temp", "prec","wid_stateincome_ppp05","Year","state","panelid","ctemp_gswp3")
  } else {
    # Handle other cases or provide a default value if necessary
    print('Please enter the correct model, NoAdaptation,AdaptationPeron,or AdaptationGilli')}
    
  #Complete the regression function by adding fixed effects.  
  formula_str <- paste("Growth_income ~ ", paste(alist, collapse = " + "),
                        "+ as.numeric(Year) : state  + Year + panelid")  

  

  #Regression across each income group
  for (i in na.omit(unique(data$cpercentile))) {
    
    subset_data <- data %>%
      dplyr::filter(cpercentile == i) %>%
      select(all_of(col_select))
    
    subset_data <- subset_data[complete.cases(subset_data[,col_select[col_select != "ctemp_gswp3"]]), ]
    
    RegResults [[i]] <- lm(formula_str, data = subset_data)
    
    }
  
  print(formula_str)  
  
  return(RegResults)
  
}
