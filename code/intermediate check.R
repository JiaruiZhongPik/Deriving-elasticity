

#Intermediate Check (1)
#delta growth against temperature
CheckPlot1 <- ggplot(pdata,aes(x=temp,y=yhat2-yhat1))+
  geom_point()+
  ggtitle("Delta growth against temperature")
CheckPlot1


#Damage/Income against temperature
CheckPlot2 <- ggplot(pdata,aes(x=temp,y=Damage_IP/Counterfactual_income_IP))+
  geom_point()+
  ggtitle("Damage share (over income) against temperature")
CheckPlot2


