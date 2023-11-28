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
  
  
  growth_cf_cum <- function(growth,delta,persistence){
    
    growth_cf_cum = rep(NA,length(growth))
    growth_cf_cum[1] = 1
    
    for (n in 2:length(growth_cf_cum)){
      growth_cf_cum[n] =
        prod(1 + growth[2:n] + delta[2:n] * rev(Persistence[1:(n-1)]))
      
    }
    return(growth_cf_cum)
  }
  
    
    #data[data$panelid==i,]$t = data[data$panelid==i,]$Year - Startyear +1
    
    data<- data %>% 
      group_by(panelid) %>%
      mutate( Year= as.numeric(Year),
              t = Year- Startyear +1,
             growth_cf_cum = growth_cf_cum(growth_income_a,delta_growth_a,Persistence),
             Counterfactual_income = PTNI05[1] * growth_cf_cum,
             Damage = Counterfactual_income - PTNI05)

    
    return(data[c("Counterfactual_income","Damage")])
}

