rm(list = ls())

load("pdata.RData")
source("code/utils.R")

pdata <- pdata %>%
  group_by(panelid) %>%
  mutate(PTNI05L = shift(PTNI05,n=1)) 


RegResults=RegDamageGrowth(pdata,"AdaptationPeron")

# level the counterfactual temperature so that the counterfacual and observed temperatreu are the same at starting year
pdata <- pdata %>% 
  dplyr::filter(Year >= 1987 & Year <= 2019) %>%
  group_by(panelid) %>%
  mutate(level = temp[1] - ctemp_gswp3[1],
         ctemp =  ctemp_gswp3 +level
  )


pdata=PredictrRegDG(pdata,1987,RegResults,"AdaptationPeron")

result1=EstimateDamage(pdata,1987,HL=10)



#_----------------------------------------------------------------------------

load("pdata.RData")
source("code/utils.R")

# level the counterfactual temperature so that the counterfacual and observed temperatreu are the same at starting year
pdata <- pdata %>% 
  dplyr::filter(Year >= 1987 & Year <= 2019) %>%
  group_by(panelid) %>%
  mutate(level = temp[1] - ctemp_gswp3[1],
         ctemp =  ctemp_gswp3 +level,
         temp = temp + seq(0,2,length=33)
  )


pdata=PredictrRegDG(pdata,1987,RegResults,"AdaptationPeron")


result2=EstimateDamage(pdata,1987,HL=10)

print("finished")


#------------------------------------------------------------------------------------------

load("pdata.RData")
source("code/utils.R")


# level the counterfactual temperature so that the counterfacual and observed temperatreu are the same at starting year
pdata <- pdata %>% 
  dplyr::filter(Year >= 1987  & Year <= 2019 ) %>%
  group_by(panelid) %>%
  mutate(level = temp[1] - ctemp_gswp3[1],
         ctemp =  ctemp_gswp3 +level,
         temp2 = temp + seq(0,2,length=33)
  )

pdata$Counterfactual_income_p10 = result1$Counterfactual_income
pdata$Counterfactual2_income_p10 = result2$Counterfactual_income

subpdata = pdata[,c('CountryCode','Year','cpercentile','panelid','IG_hetero','PTNI05','Counterfactual_income_p10',
                    'Counterfactual2_income_p10','ctemp','temp','temp2')]

library(writexl)
write_xlsx(subpdata, path = 'result/income path.xlsx')


#----------------------------test elasticity--------------------------
pdata$Counterfactual_income_P10=result2$Counterfactual_income
pdata$Damage_P10=result2$Damage
