library(viridis)

Polypara=1
persistence="IP"

data <- pdata %>%
  select(ends_with(persistence),all_of(c("Year", "state","panelid", "temp","cpercentile","PTNI05"))) %>%
  dplyr::filter(Year >  1987) %>%
  rename_with(~ gsub(paste("_",persistence,"$",sep=''), "", .), ends_with(persistence)) %>%
  group_by(state, Year) %>%
  mutate(State_Damage = sum(Damage, na.rm=F),
         State_preDMG_Income = sum(Counterfactual_income,na.rm=F)) 

plot1=ggplot(data, aes(x = factor(cpercentile), y = Damage)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "text", aes(label = sprintf("%.0f", ..y..)),
               vjust = -0.5, size = 3, color = "red") + # Add mean text labels
  labs(x = "Income groups", y = "Damage") +
  ggtitle("Box Plot of Damage over Income Groups")+
  theme_bw()+
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.title = element_text(size = 12),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "#f5f5f5"))+
  scale_y_log10() 
ggsave(paste("figure/1.absolute dmg over income groups.png",sep=''), 
       plot = plot1, width = 5, height = 4, dpi = 300)


plot2=ggplot(data, aes(x = factor(cpercentile), y = Damage/Counterfactual_income)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "text", aes(label = sprintf("%.2f", ..y..)),
               vjust = -0.5, size = 3, color = "red") + # Add mean text labels
  labs(x = "Income groups", y = "Damage/Income") +
  ggtitle("Box Plot of Damage Share")+
  theme_bw()+
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.title = element_text(size = 12),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "#f5f5f5"))
ggsave(paste("figure/2.box plot over dmgS income groups.png",sep=''), 
       plot = plot2, width = 5, height = 4, dpi = 300)


plot3 <- ggplot(data[which(data$Year==2019),], aes(x = Counterfactual_income, y = Damage/Counterfactual_income, color = factor(cpercentile))) +
  geom_point(size=1.2) +
  scale_color_viridis(discrete = TRUE) +  # Use the viridis color palette
  labs(x = "Income per capita", y = "Damage/Income") +
  ggtitle("Damage share over income level (2019)") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.title = element_text(size = 12),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "#f5f5f5")) +
  scale_x_log10()  # Apply log10 transformation to x-axis

ggsave(paste("figure/3.dmgS over Income_B.png",sep=''), 
       plot = plot3, width = 6, height = 4, dpi = 300)


plot4 <- ggplot(data[which(data$Year==2019),], aes(x = Counterfactual_income/State_preDMG_Income, y = Damage/State_Damage, color =cpercentile)) +
  geom_point(size=1.2) +
  scale_color_viridis() +  #Use the viridis color palette
  labs(x = "Relative Income", y = "Relative Income") +
  ggtitle("Relative Damage against Relative Income") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.title = element_text(size = 12),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "#f5f5f5"))+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray")  # Add y=x reference line

ggsave(paste("figure/4.dmgS over IncomeS.png",sep=''), 
       plot = plot4, width = 6, height = 4, dpi = 300)





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
  log(dep) ~ poly(log(cov) + PTNI05, Polypara, raw = TRUE)| Year + state,
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
                         c('Income group FE', 'No','No','Yes',"Yes"),
                         c('BIC',round(bic_values,2))
          ),
          dep.var.labels=c("log(damage/IncomePreDMG)"),
          covariate.labels=covariate.labels[[Polypara]],
          out=paste("result/","Bmodel_",persistence,"_",Polypara,".html", sep='')
          
)
