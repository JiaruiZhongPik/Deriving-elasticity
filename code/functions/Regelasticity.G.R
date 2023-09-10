#This function estimate income elasticity of climate impact according to Gilli's aproach
#By Jiarui Zhong

RegElasticity.G <- function (data,persistence){
  
  data <- data %>%
    select(ends_with(persistence),all_of(c("Year", "state","panelid"))) %>%
    dplyr::filter(Year >  1987)%>%
    rename_with(~ gsub(paste("_",persistence,"$",sep=''), "", .), ends_with(persistence)) %>%
    group_by(state, Year) %>%
    mutate(State_Damage = sum(Damage, na.rm=F),
           State_preDMG_Income = sum(Counterfactual_income,na.rm=F),
           dep = Damage / State_Damage,
           cov = Counterfactual_income/State_preDMG_Income)
  

  # Pooled OLS regression to estimate income elasticity parameter
  ols_model <- lm(log(dep) ~ log(cov) , data = data)
  #summary(ols_model)
  
  # Pooled OLS regression with year fixed effects
  ols_fe_model <- lm(log(dep) ~ log(cov) +factor(Year), data = data)
  #summary(ols_fe_model)
  
  # Pooled OLS regression with year and country fixed effects
  ols_fe2_model <- lm(log(dep) ~ log(cov) + factor(Year) + factor(state), data = data)
  #summary(ols_fe2_model)
  
  # Pooled OLS regression with year, country fixed effects, and country-specific time trends
  ols_fe3_model <- lm(log(dep) ~ log(cov) + factor(Year) + factor(state) +as.numeric(Year) : state +I(as.numeric(Year)^2):factor(state), data = data)
  #summary(ols_fe3_model)
  
  
  modellistG=list(ols_model,
                  ols_fe_model,
                  ols_fe2_model,
                  ols_fe3_model)
  bic_valuesG <- sapply(modellistG, BIC)
  
  stargazer(modellistG,
            type='text',align=TRUE,
            title = "Estimating income elasticity",
            dep.var.labels=c("Log Damage Share"),
            keep = c("cov","Intercept"),
            covariate.labels=c('Log Income Share'),
            add.lines=list(c('Year FE', 'No','Yes','Yes',"Yes"),
                           c('Country FE', 'No','No','Yes',"Yes"),
                           c('Time trend','No','No','No','Yes'),
                           c('BIC',round(bic_valuesG,2))
            ),
            out=paste("result/Gmodel","_",persistence,".html", sep='')
            )
  return(modellistG)
  
}
