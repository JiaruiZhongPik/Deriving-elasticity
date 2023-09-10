#This function predicts the growth rate given observed and counterfactual temperature.
#by Jiaru Zhong, 04.09.2023


PredictrRegDG <- function(data,regmodel,model){
  
  #Determine the model function, whether and how adatptive capability is incooperated  
  if (model == "NoAdaptation") {
    col_select <- c("Growth_income","temp", "prec","Year","state","panelid","ctemp_gswp3")
    
  } else if (model == "AdaptationPeron") {
    col_select <- c("Growth_income","temp", "prec","IG_hetero","Year","state","panelid","ctemp_gswp3")
  } else if (model == "AdaptationGilli") {
    col_select <- c("Growth_income","temp", "prec","wid_stateincome_ppp05","Year","state","panelid","ctemp_gswp3")
  } else {
    # Handle other cases or provide a default value if necessary
    print('Please enter the correct model, NoAdaptation,AdaptationPeron,or AdaptationGilli')}
  
  ## Create an empty dataframe to store yhat1 and yhat2 predictions
  prediction_df <- data.frame(state = numeric(0), panelid = numeric(0), Year = numeric(0), yhat1 = numeric(0), yhat2 = numeric(0))
  
  for (i in na.omit(unique(data$cpercentile))) {
    
    subset_data <- data %>%
      dplyr::filter(cpercentile == i) %>%
      select(all_of(col_select))
   
    subset_data <- subset_data[complete.cases(subset_data[,col_select[col_select != "ctemp_gswp3"]]), ]
    
    # Predictions using observed temperature
    subset_data$yhat1 = predict(regmodel[[i]],subset_data[, -which(names(subset_data) == "ctemp_gswp3")])
    
    #replace with conterfactural temperature
    subset_data  <- subset_data %>% rename(temp=ctemp_gswp3,ctemp_gswp3=temp)
    
    # Predictions using counterfactual temperature
    subset_data$yhat2 = predict(regmodel[[i]],subset_data[, -which(names(subset_data) == "ctemp_gswp3")])
    
    # Bind yhat1 and yhat2 predictions to prediction_df
    prediction_df <- rbind(prediction_df, subset_data %>% select(state, panelid, Year, yhat1, yhat2))
    #print(paste("income group",i))
    print(summary(subset_data$yhat2))
    
  }
  
  # Left join prediction_df with pdata
  data <- left_join(data, prediction_df, by = c("state", "panelid", "Year"))
  return(data)
}
