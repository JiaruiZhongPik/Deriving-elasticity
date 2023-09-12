# this function estimate climage change impact through comparing GDP
# between counterfactual and observed pathways with different growth rate 
# by Jiaru Zhong,04.09.2023



EstimateDamage <- function(data, Startyear,HL){
  
  data <- data %>%
    mutate(
          # STEP1 Calculating the difference in growth rate between the observed temperature and the counterfactual temperature
          delta_growth = yhat2 - yhat1,
        
          # STEP2 Calculating the counterfactual growth rate
          growth_counterfactual = Growth_income + delta_growth,

          # STEP3 Translate continuous growth rate to annual growth rate
          growth_income_a = exp(Growth_income) - 1,
          growth_counterfactual_a = exp(growth_counterfactual)-1,
          delta_growth_a = growth_counterfactual_a - growth_income_a
          )
        
  
  # STEP3 Calculating current-year counterfactual pre-tax national income
  if(is.numeric(HL)){
    Persistence <- 2 ^ ((Startyear - Startyear:as.numeric(max(data$Year))) / HL)
  } else if(HL =="IP"){
    Persistence <- rep(1,length(Startyear:as.numeric(max(data$Year))))
  } else {Persistence <- rep(NA,length(Startyear:as.numeric(max(data$Year))))}
  

  
  for(i in unique(pdata$panelid)){
    subset_data <- data %>%
      dplyr::filter(panelid == i,Year >= Startyear) %>%
      mutate(
        Year = as.numeric(Year),
        growth_cf_cum = NA
        )
    
    subset_data$t = subset_data$Year - Startyear + 1
    
    for (n in unique(subset_data$t)[-1]){
      subset_data[subset_data$t == n,]$growth_cf_cum=
        prod(1 + subset_data$growth_income_a[2:n] + subset_data$delta_growth_a[2:n] * rev(Persistence[1:(n-1)]))
    }
    
    #compute counterfactual GDP
    subset_data$Counterfactual_income = ifelse(subset_data$Year == Startyear, subset_data$PTNI05, 
                                             ifelse(subset_data$Year > Startyear, 
                                                    subset_data$PTNI05[subset_data$Year == Startyear] * subset_data$growth_cf_cum,NA))
    
    data[which(data$panelid == i&data$Year >= Startyear),"Counterfactual_income"] = subset_data[,"Counterfactual_income"]
 
     }
  
    data=data %>% 
    mutate(Damage=Counterfactual_income-PTNI05)
    
    return(data[c("Counterfactual_income","Damage")])
}

