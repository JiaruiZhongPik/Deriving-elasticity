library(dplyr)
library(lmtest) 
library(broom)
library(lfe)# For clustering standard errors
library(ggplot2)
library(stargazer)
options(scipen = 200)

# Clear the entire workspace
rm(list = ls())

#load pdata for analysis
load("pdata.RData")

# Creating a list of regressors


# Sorting data for the regression analysis
pdata <- pdata %>%
  arrange(panelid, Year)


#generate average state income ppp05
pdata$wid_stateincome_ppp05 <- pdata$avg_inc / pdata$PPP2005


#model without adaptation
a0list <- c("temp","I(temp^2)", "prec","I(prec^2)")

#model with adaptation
a1list <- c("temp","I(temp^2)", "prec","I(prec^2)", "IG_hetero:temp",
            "IG_hetero:I(temp^2)", "IG_hetero:prec",
            "IG_hetero:I(prec^2)")

#model with adaptation, Gilli
a2list <- c("temp","I(temp^2)", "prec","I(prec^2)", "wid_stateincome_ppp05:temp",
            "wid_stateincome_ppp05:I(temp^2)", "wid_stateincome_ppp05:prec",
            "wid_stateincome_ppp05:I(prec^2)")


formula_str0 <- paste("Growth_income ~ ", paste(a0list, collapse = " + "),
                     "+ as.numeric(Year) : state  + Year + panelid")

formula_str1 <- paste("Growth_income ~ ", paste(a1list, collapse = " + "),
                      "+ as.numeric(Year) : state  + Year + panelid")

formula_str2 <- paste("Growth_income ~ ", paste(a2list, collapse = " + "),
                      "+ as.numeric(Year) : state  + Year + panelid")


## Create an empty dataframe to store yhat1 and yhat2 predictions
prediction_df <- data.frame(state = numeric(0), panelid = numeric(0), Year = numeric(0), yhat1 = numeric(0), yhat2 = numeric(0))

# Assuming 'pdata' is your original dataframe and 'i' is the specific 'cpercentile' value
for (i in na.omit(unique(pdata$cpercentile))) {
  selected_columns <- c("Growth_income","temp", "prec", "IG_hetero","Year","state","panelid","ctemp_gswp3")
  subset_data <- pdata %>%
    dplyr::filter(cpercentile == i) %>%
    select(all_of(selected_columns))%>%
    dplyr::filter(!is.na(Growth_income) & !is.na(temp) & !is.na(IG_hetero) &!is.na(prec) & !is.na(state) & !is.na(Year) & !is.na(panelid))
  
  temp_model <- lm(formula_str1, data = subset_data)

  # Predictions using observed temperature
  subset_data$yhat1 = predict(temp_model,subset_data[, -which(names(subset_data) == "ctemp_gswp3")])
  
  #replace with conterfactural temperature
  subset_data  <- subset_data %>% rename(temp=ctemp_gswp3,ctemp_gswp3=temp)
  
  # Predictions using counterfactual temperature
  subset_data$yhat2 = predict(temp_model,subset_data[, -which(names(subset_data) == "ctemp_gswp3")])
  
  # Bind yhat1 and yhat2 predictions to prediction_df
  prediction_df <- rbind(prediction_df, subset_data %>% select(state, panelid, Year, yhat1, yhat2))
  
  print(paste("income group",i))
  print(summary(subset_data$yhat2))
}



# Left join prediction_df with pdata
pdata <- left_join(pdata, prediction_df, by = c("state", "panelid", "Year"))


#-------------------------------------New experiment with absolute value--------------------------------------



# Calculating the difference in growth rate between the observed temperature and the counterfactual temperature
pdata$delta_growth = pdata$yhat2 - pdata$yhat1

# Calculating the counterfactual growth rate
pdata$growth_counterfactual = pdata$Growth_income + pdata$delta_growth

# Calculating current-year counterfactual pre-tax national income
pdata$Counterfactual_income=NA
for(i in unique(pdata$panelid)){
  subset_data <- pdata %>%
    filter(panelid==i) %>%
    select(all_of(c('state','panelid','Year','PTNI05','growth_counterfactual',"Counterfactual_income")))

  subset_data[which(subset_data$Year==1987),"Counterfactual_income"]=
    subset_data[which(subset_data$Year==1987),"PTNI05"]
    for (t in c(1988:2021)){
      subset_data[which(subset_data$Year==t),"Counterfactual_income"]=
        subset_data[which(subset_data$Year==(t-1)),"Counterfactual_income"]*
      (1+subset_data[which(subset_data$Year==(t)),"growth_counterfactual"])
  }
  pdata[which(pdata$panelid==i),"Counterfactual_income"]=subset_data[,"Counterfactual_income"]
  print(i)
}

# Calculating damages
pdata$Damage = pdata$Counterfactual_income - pdata$PTNI05

# Calculating damage sum over income groups
pdata <- pdata %>%
  group_by(state, Year) %>%
  mutate(State_Damage = sum(Damage, na.rm=F))

# Calculating income sum over income groups 
pdata <- pdata %>%
  group_by(state, Year) %>%
  mutate(State_iNCOME = sum(PTNI05,na.rm=F))

# Estimating income groups share of country-level damages
pdata <- pdata %>%
  mutate(dep = log(Damage / State_Damage),
         dep1 = Damage / PTNI05,
         dep2 = Damage / State_Damage)  # Share of damages without logs to identify damage outliers

# Estimating income groups share of country-level income
pdata <- pdata %>%
  mutate(cov = log(PTNI05/ State_iNCOME),
         cov1 = PTNI05/ State_iNCOME)  # Two sets of country level income available


# Generate variable for estimating elasticity according to Bjoern's approach
pdata$dmgS=pdata$Damage/pdata$PTNI05

# Dropping damage outliers - damages "greater than or equal to +1" or "equal to or greater than -1"
data_outlier <- pdata %>%
  filter(dep1 < 2 & dep1 > -2)

save.image("regression.RData")


#---------------ESTIMATING THE INCOME ELASTICITY PARAMETER-----------------------------------------------------




load("regression.RData")

# Pooled OLS regression to estimate income elasticity parameter
ols_model <- lm(dep ~ cov , data = pdata)
#summary(ols_model)

# Pooled OLS regression with year fixed effects
ols_fe_model <- lm(dep ~ cov +factor(Year), data = pdata)
#summary(ols_fe_model)

# Pooled OLS regression with year and country fixed effects
ols_fe2_model <- lm(dep ~ cov + factor(Year) + factor(state), data = pdata)
#summary(ols_fe2_model)

# Pooled OLS regression with year, country fixed effects, and country-specific time trends
ols_fe3_model <- lm(dep ~ cov + factor(Year) + factor(state) +as.numeric(Year) : state +I(as.numeric(Year)^2):factor(state), data = pdata)
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
          out=paste("Gmodel",".html", sep=''))

# ESTIMATING THE INCOME ELASTICITY PARAMETER - RESULTS EXCLUDING OUTLIERS

# Pooled OLS regression to estimate income elasticity parameter
ols_model_outliers <- lm(dep ~ cov, data = data_outlier)
summary(ols_model_outliers)

# Pooled OLS regression with year fixed effects
ols_fe_model_outliers <- lm(dep ~ cov + factor(Year), data = data_outlier)
summary(ols_fe_model_outliers)

# Pooled OLS regression with year and country fixed effects
ols_fe2_model_outliers <- lm(dep ~ cov + factor(Year) + factor(state), data =data_outlier)
summary(ols_fe2_model_outliers)

# Pooled OLS regression with year, country fixed effects, and country-specific time trends
ols_fe3_model_outliers <- lm(dep ~ cov + factor(Year) + factor(state) ++as.numeric(Year) : state +I(as.numeric(Year)^2):factor(state), data =data_outlier)
summary(ols_fe3_model_outliers)


modellistG_O=list(ols_model_outliers,
                ols_fe_model_outliers,
                ols_fe2_model_outliers,
                ols_fe3_model_outliers)
bic_valuesG_O <- sapply(modellistG_O, BIC)


stargazer(modellistG_O,
          type='text',align=TRUE,
          title = "Estimating income elasticity",
          dep.var.labels=c("Log Damage Share"),
          keep = c("cov","Intercept"),
          covariate.labels=c('Log Income Share'),
          add.lines=list(c('Year FE', 'No','Yes','Yes',"Yes"),
                         c('Country FE', 'No','No','Yes',"Yes"),
                         c('Time trend','No','No','No','Yes'),
                         c('BIC',round(bic_valuesG_O,2))
          ),
          out=paste("Gmodel_outlier",".html", sep=''))



#---------------------------------------estimate Bjoern's model-----------------------------------
# Dropping damage outliers - damages "greater than or equal to +1" or "equal to or greater than -1"
data_outlier1<- pdata %>%
  filter(dep1 >0)


covariate.labels=list(c("log(Income)","Temperature"),
                      c("log(Income)","log(Income) square","Temperature"),
                      c("log(Income)","log(Income) square","log(Income) cube","Temperature")
)

Regress.result.B <- function(Polypara,data){
  dataset=data
  Bmodel_linear <-felm(
    log(dmgS) ~ poly(log(PTNI05), Polypara, raw = TRUE),
    data = dataset
  )
  
  Bmodel_linear_fet <- felm(
    log(dmgS) ~  poly(log(PTNI05) , Polypara, raw = TRUE)| Year,
    data = dataset
  )
  
  Bmodel_linear_fetid <- felm(
    log(dmgS) ~ poly(log(PTNI05) , Polypara, raw = TRUE)| Year + panelid,
    data = dataset
  )
  
  Bmodel_linear_fetidt <- felm(
    log(dmgS) ~ poly(log(PTNI05) , Polypara, raw = TRUE) + temp | Year + panelid,
    data = dataset
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
                           c('Income group FE', 'No','No','Yes',"Yes"),
                           c('BIC',round(bic_values,2))
            ),
            dep.var.labels=c("log(damage)"),
            covariate.labels=covariate.labels[[Polypara]],
            out=paste("Bmodel_",Polypara,".html", sep='')
  
  )
  
  return(modellist)
  
  
  
  }

modellist1<-Regress.result.B(1,data_outlier1)
modellist2<-Regress.result.B(2,data_outlier1)
modellist3<-Regress.result.B(3,data_outlier1)

#----------------------------Compute elasticity---------------------------

elasticity_B <- function(model,Polypara,income){
  
  if("(Intercept)" %in% rownames(model$coefficients)){
    if (Polypara==1){
      elasticity=1+model$coefficients[2]
    } else if (Polypara==2){
      elasticity=1+coef(model)[2]+2*coef(model)[3]*log(income)
    } else if (Polypara==3){
      elasticity=1+coef(model)[2]+
        2*coef(model)[3]*log(income)+
        3*coef(model)[4]*log(income)^2
    }
  } else {
    if (Polypara==1){
      elasticity=1+model$coefficients[1]
    } else if (Polypara==2){
      elasticity=1+coef(model)[1]+2*coef(model)[2]*log(income)
    } else if (Polypara==3){
      elasticity=1+coef(model)[1]+
        2*coef(model)[2]*log(income)+
        3*coef(model)[3]*(log(income)^2)
    }
  }
  
  return(elasticity)
}


elasticityB1=sapply(modellist1,elasticity_B,Polypara=1,income=median(pdata$PTNI05))
elasticityB2=sapply(modellist2,elasticity_B,Polypara=2,income=median(pdata$PTNI05))
elasticityB3=sapply(modellist3,elasticity_B,Polypara=3,income=median(pdata$PTNI05))

names(elasticityB2)<-NULL
names(elasticityB3)<-NULL

print(paste("elasticity of linear model",list(round(elasticityB1,2))))
print(paste("elasticity of sqaured model",list(round(elasticityB2,2))))
print(paste("elasticity of cubid model",list(round(elasticityB3,2))))
