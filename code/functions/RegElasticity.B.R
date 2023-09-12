# this function conducts the regression analysis to estimate elasticity
# this function implements REMIND approach
# by Jiaru Zhong,06.09.2023


RegElasticity.B <- function(Polypara,data,persistence){
  
  data <- data %>%
    select(ends_with(persistence),all_of(c("Year", "state","panelid", "temp","cpercentile"))) %>%
    dplyr::filter(Year >  1987) %>%
    rename_with(~ gsub(paste("_",persistence,"$",sep=''), "", .), ends_with(persistence)) %>%
    mutate(dep = Damage/Counterfactual_income,
           cov = Counterfactual_income) 
   
  covariate.labels=list(c("log(IncomePreDMG)","Temperature"),
                        c("log(IncomePreDMG)","log(IncomePreDMG) square","Temperature"),
                        c("log(IncomePreDMG)","log(IncomePreDMG) square","log(IncomePreDMG) cube","Temperature")
  )
  
  Bmodel_linear <-felm(
    log(dep) ~ poly(log(cov), Polypara, raw = TRUE),
    data = data
  )
  
  Bmodel_linear_fet <- felm(
    log(dep) ~  poly(log(cov) , Polypara, raw = TRUE)| Year,
    data = data
  )
  
  Bmodel_linear_fetid <- felm(
    log(dep) ~ poly(log(cov) , Polypara, raw = TRUE)| Year + state,
    data = data
  )
  
  Bmodel_linear_fetidt <- felm(
    log(dep) ~ poly(log(cov) , Polypara, raw = TRUE) + temp | Year + state,
    data = data
  )
  
  modellist=list(Bmodel_linear,
                 Bmodel_linear_fet,
                 Bmodel_linear_fetid,
                 Bmodel_linear_fetidt)
  
  
  
  #model comparison
  bic_values <- sapply(modellist, BIC)
  
  stargazer(modellist,
            type= "text",
            title = "Estimating income elasticity",align=TRUE,
            add.lines=list(c('Year FE', 'No','Yes','Yes',"Yes"),
                           c('State FE', 'No','No','Yes',"Yes"),
                           c('BIC',round(bic_values,2))
            ),
            dep.var.labels=c("log(damage/IncomePreDMG)"),
            covariate.labels=covariate.labels[[Polypara]],
            out=paste("result/","Bmodel_",persistence,"_",Polypara,".html", sep='')
            
  )
  
  return(modellist)
  
}

