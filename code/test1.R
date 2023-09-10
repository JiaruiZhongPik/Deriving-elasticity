ggplot(pdata[which(pdata$state==90&pdata$Year==1998),],aes(x=Counterfactual_income_IP,y=Damage_IP/Counterfactual_income_IP))+
  geom_point()
  #geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed")

ggplot(pdata,aes(x=factor(cpercentile),y=Damage_IP/Counterfactual_income_IP))+
  geom_boxplot()

dep = Damage / State_Damage,
cov = Counterfactual_income/State_preDMG_Income)

#

ggplot(pdata[which(pdata$state==36&pdata$Year==2010),],aes(x=Counterfactual_income_IP,y=Damage_IP/Counterfactual_income_IP))+
  geom_point()


ggplot(data[which(data$REMIND==1),],aes(x=Counterfactual_income_IP,y=Damage_IP))+
  geom_point()

 
data = pdata[which(pdata$Counterfactual_income_IP<500000),]


ggplot(data,aes(x=temp, y=Damage/Counterfactual_income))+
  geom_point()
  