library(tidyr)

plotdt=pdata[which(pdata$panelid==3),c('state','panelid','Year','temp','ctemp_gswp3','Growth_income','delta_growth','growth_counterfactual','PTNI05',"Counterfactual_income")]


plotdt=plotdt %>%
  pivot_longer(cols = c('PTNI05',"Counterfactual_income"),
               names_to = "Scenario",
               values_to = "Income")

plot=ggplot(plotdt, aes(x = as.numeric(Year), y = Income, color = Scenario, group = Scenario)) +
  geom_line() +
  labs(x = "Year", y = "Income", title = "Income Comparison by Scenario") +
  scale_color_manual(values = paletteer::paletteer_d("feathers::princess_parrot")) +
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
    panel.background = element_rect(fill = "#f5f5f5"),
    legend.position = "bottom",  # Change legend position
    legend.box = "horizontal"    # Arrange legend items horizontally
  )

ggsave("line_panelid3.png", plot = plot, width = 5, height = 4, dpi = 300)
