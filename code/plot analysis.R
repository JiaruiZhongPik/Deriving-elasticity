library(viridis)

Polypara=1
persistence="P10"

data <- pdata %>%
select(ends_with(persistence),all_of(c("Year", "state","CountryCode","panelid", "temp","ctemp_gswp3","cpercentile","PTNI05","Growth_income",'IG_hetero'))) %>%
  dplyr::filter(Year >= 1987) %>%
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
ggsave(paste("figure/1.absolute dmg over income groups1_",persistence,".png",sep=''), 
       plot = plot1, width = 5, height = 4, dpi = 300)


plot2=ggplot(data, aes(x = factor(cpercentile), y = Damage/Counterfactual_income)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "text", aes(label = sprintf("%.3f", ..y..)),
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
ggsave(paste("figure/2.box plot over dmgS income groups_",persistence,".png",sep=''), 
       plot = plot2, width = 5, height = 4, dpi = 300)


plot3 <- ggplot(data[which(data$Year == 2019), ], aes(x = log(Counterfactual_income), y = log(Damage/Counterfactual_income), color = factor(cpercentile))) +
  geom_point(size = 0.8) +
  scale_color_viridis(discrete = TRUE) +  # Use the viridis color palette
  labs(x = "Income per capita", y = "Damage/Income") +
  ggtitle("Damage share over income level (2019)") +
  theme_bw() +
  theme(
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    axis.title = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_rect(fill = "#f5f5f5")
  ) +
  scale_x_log10(
    limits = c(1, 1200000),
    breaks = c(100, 10000, 1000000),
    labels = c(100, 10000, 1000000)
  ) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "darkblue")  # Add a single linear regression line for all points

  #ylim(-3,1)
ggsave(paste("figure/3.dmgS over Income_PeronAdpt_",persistence,"_2.png",sep=''), 
       plot = plot3, width = 8, height = 4, dpi = 300)


plot4 <- ggplot(data[which(data$Year==2019),], aes(x = Counterfactual_income/State_preDMG_Income, y = Damage/State_Damage, color = as.factor(cpercentile))) +
  geom_point(size=1.2) +
  scale_color_viridis(discrete = T) +  #Use the viridis color palette
  labs(x = "Relative Income", y = "Relative Damage") +
  ggtitle("Relative Damage against Relative Income (2019)") +
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

ggsave(paste("figure/4.dmgS over IncomeS_",persistence,".png",sep=''), 
       plot = plot4, width = 6, height = 4, dpi = 300)


for(i in c('USA',"IND",'NOR','NGA')){
  plot5 <- data %>%
    dplyr::filter(CountryCode == i) %>%
    ggplot(aes(x = as.numeric(Year))) +
    geom_line(aes(y = Counterfactual_income,color="Counterfactual_income"), size = 0.8) +
    geom_line(aes(y = PTNI05,color = "PTNI05"), size = 0.8) +
    labs(x = "Year", y = "Income", title = paste("Income Over Time for ",i," by Income Group",sep='')) +
    scale_y_log10()+
    scale_color_discrete(name = "Income Group") +
    facet_wrap(~ cpercentile, ncol = 5, scales = 'free_y') +  # Create separate plots for each cpercentile
    theme_bw() +
    theme(axis.text.y = element_text(size = 10),
          axis.text.x = element_text(size = 10),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          axis.title = element_text(size = 12),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "#f5f5f5"),
          legend.position = "bottom" )
  ggsave(paste("figure/plot5_compare income_",i,"_",persistence,".png",sep=''), 
         plot = plot5, width = 12, height = 6, dpi = 300)
}


plot6 <- ggplot(data, aes(x =PTNI05, y = yhat2 - yhat1, color = factor(cpercentile))) +
  geom_point(size=0.5) +
  scale_color_viridis(discrete = T) +  #Use the viridis color palette
  labs(x = "Temperature", y = "Difference in growth rate") +
  ggtitle("What is the projected difference between growth rate?") +
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
  scale_x_log10( limits=c(1,1200000),
                 breaks = c(100,10000,1000000),
                 labels= c(100,10000,1000000)) # Apply log10 transformation to x-axis
ggsave(paste("figure/plot6_deltaY.png",sep=''), 
       plot = plot6, width = 6, height = 6, dpi = 300)



ggplot(data, aes(x =yhat2 - yhat1, y = Growth_income, color = factor(cpercentile))) +
  geom_point() +
  scale_color_viridis(discrete = T) +  #Use the viridis color palette
  labs(x = "Growth_income", y = " yhat2 - yhat1") +
  #stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "text", aes(label = sprintf("%.5f", ..y..)),
  #             vjust = -0.5, size = 3, color = "red") + # Add mean text labels
  ggtitle("What is the projected difference between growth rate?") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.title = element_text(size = 12),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "#f5f5f5"))



