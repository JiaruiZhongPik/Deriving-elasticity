
source("code/utils.R")
options(scipen = 200)

#--------------STEP1----------------
#----preparing Peron's dataset------
#-----------------------------------
pdata<-PrepareData()
save.image("pdata.RData")
#can be saved if pdata.Rdata already exist



#-------------Step2--------------------
#conduct regression for damage function
#--------------------------------------
#load("pdata.RData")

RegResults=RegDamageGrowth(pdata,"AdaptationPeron")



#--------------Step3----------------------------------------
#predict growth with observed and counterfactual temperature
#------------------------------------------------------------

pdata=PredictrRegDG(pdata,RegResults,"AdaptationPeron")





#----------------Step4-----------------------------------
#predict damage as difference between counterfactual and observed gdp
#--------------------------------------------------------

result=EstimateDamage(pdata,1987,HL='IP') #infinite persistence
pdata$Counterfactual_income_IP=result$Counterfactual_income
pdata$Damage_IP=result$Damage

result=EstimateDamage(pdata,1987,HL=5)
pdata$Counterfactual_income_P5=result$Counterfactual_income
pdata$Damage_P5=result$Damage

result=EstimateDamage(pdata,1987,HL=10)
pdata$Counterfactual_income_P10=result$Counterfactual_income
pdata$Damage_P10=result$Damage

result=EstimateDamage(pdata,1987,HL=20)
pdata$Counterfactual_income_P20=result$Counterfactual_income
pdata$Damage_P20=result$Damage

result=EstimateDamage(pdata,1987,HL=30)
pdata$Counterfactual_income_P30=result$Counterfactual_income
pdata$Damage_P30=result$Damage

summary(pdata[c("Damage_IP",'Damage_P30','Damage_P20','Damage_P10','Damage_P5')])


save.image("pdata_dmg.RData")

#---------------Step 5--------------------------------------
#Estimate income elasticity of climate impacts
#-----------------------------------------------------------

#load("pdata_dmg.RData")


#----5.1 Estimating elasticity according to Bjoern's approach-----


#linear model model with different persistence
modellist1.IP<-RegElasticity.B(1,pdata,"IP")
modellist1.P30<-RegElasticity.B(1,pdata,"P30")
modellist1.P20<-RegElasticity.B(1,pdata,"P20")
modellist1.P10<-RegElasticity.B(1,pdata,"P10")
modellist1.P5<-RegElasticity.B(1,pdata,"P5")

#squared model model with different persistence
modellist2.IP<-RegElasticity.B(2,pdata,"IP")
modellist2.P30<-RegElasticity.B(2,pdata,"P30")
modellist2.P20<-RegElasticity.B(2,pdata,"P20")
modellist2.P10<-RegElasticity.B(2,pdata,"P10")
modellist2.P5<-RegElasticity.B(2,pdata,"P5")

#cubed linear model model with different persistence
modellist3.IP<-RegElasticity.B(3,pdata,"IP")
modellist3.P30<-RegElasticity.B(3,pdata,"P30")
modellist3.P20<-RegElasticity.B(3,pdata,"P20")
modellist3.P10<-RegElasticity.B(3,pdata,"P10")
modellist3.P5<-RegElasticity.B(3,pdata,"P5")

elasticityB=OutElasticity.B(IncomeRef = 10000)
#results are exported to the file "/result"

#----5.2 Estimating elasticity according to Gilli's approach-----


ModellistG.IP=RegElasticity.G(pdata,"IP")
ModellistG.P30=RegElasticity.G(pdata,"P30")
ModellistG.P20=RegElasticity.G(pdata,"P20")
ModellistG.P10=RegElasticity.G(pdata,"P10")
ModellistG.P5=RegElasticity.G(pdata,"P5")


elasticityG=OutElasticity.G()
