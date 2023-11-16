#This file analyses the impact of CC on inequality (represented by Gini index)

#======================0.save computed GINI data==================================

# save gini data for convenience
gini <- pdata[,c('state','Year','panelid','cpercentile','IG_hetero','ctemp','temp','gini_index','gini_nonp','gini_gb2','gini_gb2_counterIP','gini_gb2_counterP5',
                 'gini_gb2_counterP10','gini_gb2_counterP20','gini_nonp_global','gini_nonp_counterP10_global')]
write.csv(gini, "result/gini_dataset.csv", row.names = FALSE)





#======================1.The efficiency of GB2 approach to replicate the survey index============================
#This is showed by comparing the gini_gb2, which is calculated with PTNI05
#(the observed quintile income), with the survey Gini from WID data base

#load("pdata_dmg.RData")


#---------1.1 histogram of deviation of gini_gb2-------------------------
# Calculate the absolute difference and express in percentage
gini$abs_diff_percent <- (abs(gini$gini_gb2 - gini$gini_index))/gini$gini_index * 100

# Create a histogram with ggplot2
ggplot(gini, aes(x = abs_diff_percent)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black", alpha = 0.7, , boundary = 0) +
  #scale_x_continuous(breaks = seq(0.1, 1.2, 0.2), 
  #                   labels = c("[0~0.2]", "[0.2~0.4]", "[0.4~0.6]", "[0.6~0.8]", "[0.8~1.0]", "[1.0~1.2]")) +
  labs(title = "Absolute Difference between survey Gini and estimated Gini (%)",
       x = "Absolute Difference (%)") +
  theme_minimal()

dev.off()
#------------------------------------------------------------------------



#------1.2 compare the deviation of gb2 approach wiht nonpara approach----------
# Calculate the additional variable
gini$abs_diff_nonp_percent <- (abs(gini$gini_nonp - gini$gini_index))/gini$gini_index * 100


# Define the new breaks for different ranges
new_breaks <- c(0, 1, 3, 5, 10, Inf)

# Create a factor variable indicating the bin for the new variable
gini$abs_diff_bin <- cut(gini$abs_diff_percent, new_breaks, include.lowest = TRUE)
gini$abs_diff_nonp_bin <- cut(gini$abs_diff_nonp_percent, new_breaks, include.lowest = TRUE)


# Count the occurrences in each bin for the new variable
count_table <- table(gini$abs_diff_bin)
count_table_nonp <- table(gini$abs_diff_nonp_bin)

# Calculate the share of the sample in each bin for the new variable
share_table <- prop.table(count_table) * 100
share_table_nonp <- prop.table(count_table_nonp) * 100

# Combine the two tables
combined_table <- rbind(share_table, share_table_nonp)

# Print the combined table
cat("Share of Sample in Different Ranges for Both Variables:\n")
print(combined_table)

rownames(combined_table) <- c('% deviation from parametric approach (GB2)', '% devation from nonparametric approach')

# Save the rounded table to a CSV file
write.csv(combined_table, "result/share_table.csv")
#============================================================================================================







#========================2.How climate change impact Gini====================================================


#------------------------2.1 Global level-----------------------------

df <- na.omit(pdata[,c('Year','panelid','PTNI05','Counterfactual_income_P10')])

df <- df %>%
  group_by(Year) %>%
  mutate(
    gini_nonp_global = ineq(PTNI05, type = 'Gini', na.rm = TRUE),
    gini_nonp_counterP10_global = ineq(Counterfactual_income_P10, type = 'Gini', na.rm = TRUE)
  ) %>%
  dplyr::filter(panelid == 1)

df <- read.csv2('input/WID_Data_Gini_world.csv')%>%
  rename(gini_WID = gptinc_z_WO) %>%
  select(c('Year','gini_WID')) %>%
  merge(df,by.y = 'Year')

df$gini_WID=as.numeric(df$gini_WID)

plot1 <- ggplot(df, aes(x = as.numeric(Year))) +
  geom_line(aes(y = gini_nonp_counterP10_global, color = "Counterfactual with no CC"), size = 1) +
  geom_line(aes(y = gini_nonp_global, color = "Observed path with CC"), size = 1) +
  #geom_line(aes(y = gini_WID, color = "WID"), size = 1.5) +
  labs(
    title = "Global Gini Over Time",
    x = "Year",
    y = "Gini Index",
    color = "Legend"
  ) +
  guides(color = FALSE)+
  theme_bw()+
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.title = element_text(size = 12),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "#f5f5f5"))+
  annotate('text', x = 2015, y= 0.708, label="With Climate change", color= "#418aad")+
  annotate('text', x = 2010, y= 0.7, label="No climate change", color= "#d3605c")

ggsave(paste("figure/gini/global gini over time.png",sep=''), 
       plot = plot1, width = 7, height = 3, dpi = 300)


#--------------------------------------------------------------------



#--------------------------2.2 Country level analysis---------------------------


#---------2.2.1.Gini impact with temperature---------

plot2 <- ggplot(gini[which(gini$cpercentile==3),], aes(x = temp, y = gini_gb2-gini_gb2_counterP10)) +
  geom_point(size = 1,alpha=0.3, color = "#418aad") +
  scale_color_viridis(discrete = T) +  # Use the viridis color palette
  labs(x = "observed country average temperature", y = "CC impact on Gini") +
  ggtitle("Change in Gini over average temperature") +
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
  )

ggsave(paste("figure/gini/Gini change with temperature.png",sep=''), 
       plot = plot2, width = 6, height = 4, dpi = 300)




#---------2.2.2. world map--------
world <- ne_countries(scale = "medium", returnclass = "sf")

df1 <- pdata[pdata$Year==2019,c('CountryCode','gini_gb2','gini_gb2_counterP10')]
colnames(df1)[1] <- 'adm0_a3'
#Merge df1 with world

world <- merge(world, df1, by.x = "adm0_a3")

world$gini_diff=(world$gini_gb2 - world$gini_gb2_counterP10)

plot3 <- ggplot(data = world) +
  geom_sf(aes(fill = gini_diff))+
  scale_fill_gradientn(colors = c("#1f618d", "white", "#CD5C5C"),
                      na.value = "grey50") +
  theme_minimal()+
  labs(fill = "Gini Index Change")+
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 10))
  

ggsave(paste("figure/gini/Gini impacts on map.png",sep=''), 
       plot = plot3, width = 7, height = 4, dpi = 300)



#-----------------Does inequality jepordize
plot4 <- ggplot(gini[which(gini$cpercentile==3&gini$Year==2019),], aes(x = gini_gb2_counterP10, y = gini_gb2-gini_gb2_counterP10)) +
  geom_point(size = 2,alpha=0.3, color = "#418aad") +
  scale_color_viridis(discrete = T) +  # Use the viridis color palette
  labs(x = "Gini withouth CC in 2019", y = "CC impact on Gini") +
  ggtitle("Change in Gini over average temperature") +
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
  )

ggsave(paste("figure/gini/inequal regions are getting more inequal.png",sep=''), 
       plot = plot4, width = 6, height = 4, dpi = 300)

