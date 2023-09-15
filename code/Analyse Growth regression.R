library(gridExtra)

#---1.Our income data seems larger than Gilli's sample----
plot1 <- ggplot(pdata, aes(PTNI05)) +
  geom_density(fill = "blue", color = "black", alpha = 0.5) +
  labs(title = "Density Plot", x = "X-axis Label", y = "Density")+
  scale_x_log10()+
  labs(x = "Income level", y = "Density") +
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
        panel.background = element_rect(fill = "#f5f5f5"))

ggsave(paste("figure/PTNI distribution.png",sep=''), 
       plot = plot1, width = 5, height = 4, dpi = 300)


#------2. Simulation with hypothetical climate change ------------- 

#------2.1 Our regression------------------

covlist=c('temp','I(temp^2)','temp:IG_hetero1','temp:IG_hetero2','temp:IG_hetero3',
          'I(temp^2):IG_hetero1','I(temp^2):IG_hetero2','I(temp^2):IG_hetero3')

RegTable<- data.frame(
  Column1 = numeric(0),
  Column2 = numeric(0),
  Column3 = numeric(0),
  Column4 = numeric(0),
  Column5 = numeric(0),
  Column6 = numeric(0),
  Column7 = numeric(0),
  Column8 = numeric(0)
)


for (i in 1:10){
  RegTable <- rbind(RegTable,coef(RegResults[[i]])[covlist])
}
colnames(RegTable)<-covlist



png("figure/impact pattern_low.png", width = 2000, height = 1600, res = 200)    # Adjust width and height as needed
par(mfrow = c(4,4))
for (i in seq(0,30,2)){
  temp0=i #hypothetical temperature without climate change
  temp1=temp0+1 #hypothetical temperature with climate change
  plot(RegTable$temp *  (temp1-temp0) + RegTable$`I(temp^2)`* (temp1^2-temp0^2) + 
             RegTable$`temp:IG_hetero1`*(temp1-temp0) +RegTable$`I(temp^2):IG_hetero1` * (temp1^2-temp0^2),
       ylab='delta y',xlab = 'Income Group')
  title(main=paste(i,' degree',sep=''))
}
dev.off()


png("figure/impact pattern_lower middle.png", width = 2000, height = 1600, res = 200)    # Adjust width and height as needed
par(mfrow = c(4,4))
for (i in seq(0,30,2)){
  temp0=i #hypothetical temperature without climate change
  temp1=temp0+1 #hypothetical temperature with climate change
  plot(RegTable$temp *  (temp1-temp0) + RegTable$`I(temp^2)`* (temp1^2-temp0^2) + 
         RegTable$`temp:IG_hetero2`*(temp1-temp0) +RegTable$`I(temp^2):IG_hetero2` * (temp1^2-temp0^2),
       ylab='delta y',xlab = 'Income Group')
  title(main=paste(i,' degree',sep=''))
}
dev.off()


png("figure/impact pattern_upper middle.png", width = 2000, height = 1600, res = 200)    # Adjust width and height as needed
par(mfrow = c(4,4))
for (i in seq(0,30,2)){
  temp0=i #hypothetical temperature without climate change
  temp1=temp0+1 #hypothetical temperature with climate change
  plot(RegTable$temp *  (temp1-temp0) + RegTable$`I(temp^2)`* (temp1^2-temp0^2) + 
         RegTable$`temp:IG_hetero3`*(temp1-temp0) +RegTable$`I(temp^2):IG_hetero3` * (temp1^2-temp0^2),
       ylab='delta y',xlab = 'Income Group')
  title(main=paste(i,' degree',sep=''))
}
dev.off()


png("figure/impact pattern_high.png", width = 2000, height = 1600, res = 200)    # Adjust width and height as needed
par(mfrow = c(4,4))
for (i in seq(0,30,2)){
  temp0=i #hypothetical temperature without climate change
  temp1=temp0+1 #hypothetical temperature with climate change
  plot(RegTable$temp *  (temp1-temp0) + RegTable$`I(temp^2)`* (temp1^2-temp0^2),
       ylab='delta y',xlab = 'Income Group')
  title(main=paste(i,' degree',sep=''))
}
dev.off()

#--------------------------------------------------------------------------------------------------
# Gilli's regression result

library(readxl)
results.Gilli <- read_excel("input/Gilli regression.xlsx", 
                        col_types = c("text", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric"))
RegTableG=data.frame(t(results.Gilli [,2:11]))
colnames(RegTableG)<-t(results.Gilli[,1])



png("figure/impact pattern_Gilli_low.png", width = 2000, height = 1600, res = 200)    # Adjust width and height as needed
par(mfrow = c(4,4))
for (i in seq(0,30,2)){
  temp0=i #hypothetical temperature without climate change
  temp1=temp0+1 #hypothetical temperature with climate change
  deltay.G = RegTableG$temp *  (temp1-temp0) + RegTableG$temp2 * (temp1^2-temp0^2) + RegTableG$tempXgdp_1 *
    (temp1-temp0) *log(1300) +RegTableG$temp2Xgdp_1 * (temp1^2-temp0^2) *log(1300)
  plot(deltay.G, ylab='delta y',xlab = 'Income Group')
  title(main=paste(i,' degree',sep=''))
}
dev.off()


png("figure/impact pattern_Gilli_middle.png", width = 2000, height = 1600, res = 200)    # Adjust width and height as needed
par(mfrow = c(4,4))
for (i in seq(0,30,2)){
  temp0=i #hypothetical temperature without climate change
  temp1=temp0+1 #hypothetical temperature with climate change
  deltay.G = RegTableG$temp *  (temp1-temp0) + RegTableG$temp2 * (temp1^2-temp0^2) + RegTableG$tempXgdp_1 *
    (temp1-temp0) *log(3463) +RegTableG$temp2Xgdp_1 * (temp1^2-temp0^2) *log(3463)
  plot(deltay.G, ylab='delta y',xlab = 'Income Group')
  title(main=paste(i,' degree',sep=''))
}
dev.off()



png("figure/impact pattern_Gilli_high.png", width = 2000, height = 1600, res = 200)    # Adjust width and height as needed
par(mfrow = c(4,4))
for (i in seq(0,30,2)){
  temp0=i #hypothetical temperature without climate change
  temp1=temp0+1 #hypothetical temperature with climate change
  deltay.G = RegTableG$temp *  (temp1-temp0) + RegTableG$temp2 * (temp1^2-temp0^2) + RegTableG$tempXgdp_1 *
    (temp1-temp0) *log(12968) +RegTableG$temp2Xgdp_1 * (temp1^2-temp0^2) *log(12968)
  plot(deltay.G, ylab='delta y',xlab = 'Income Group')
  title(main=paste(i,' degree',sep=''))
}
dev.off()




#+==============================Boxplot Gilli=================
delta.G.middle=data.frame(
  Column1 = numeric(0),
  Column2 = numeric(0),
  Column3 = numeric(0),
  Column4 = numeric(0),
  Column5 = numeric(0),
  Column6 = numeric(0),
  Column7 = numeric(0),
  Column8 = numeric(0),
  Column9 = numeric(0),
  Column10 = numeric(0)
)


for (i in seq(0,30,2)){
  temp0=i #hypothetical temperature without climate change
  temp1=temp0+1 #hypothetical temperature with climate change
  deltay.G = RegTableG$temp *  (temp1-temp0) + RegTableG$temp2 * (temp1^2-temp0^2) + RegTableG$tempXgdp_1 *
    (temp1-temp0) *log(3000) +RegTableG$temp2Xgdp_1 * (temp1^2-temp0^2) *log(3000)
  delta.G.middle=rbind(delta.G.middle,deltay.G)
}

colnames(delta.G.middle)=seq(1,10)


png("figure/boxplot_figure_Gilli.png", width = 500, height = 300)
boxplot(-delta.G.middle)
dev.off()

#===========================Box plot peron============================
delta.P.lmiddle=data.frame(
  Column1 = numeric(0),
  Column2 = numeric(0),
  Column3 = numeric(0),
  Column4 = numeric(0),
  Column5 = numeric(0),
  Column6 = numeric(0),
  Column7 = numeric(0),
  Column8 = numeric(0),
  Column9 = numeric(0),
  Column10 = numeric(0)
)


for (i in seq(0,30,2)){
  temp0=i #hypothetical temperature without climate change
  temp1=temp0+1 #hypothetical temperature with climate change
  deltay.P = RegTable$temp *  (temp1-temp0) + RegTable$`I(temp^2)`* (temp1^2-temp0^2) + 
    RegTable$`temp:IG_hetero2`*(temp1-temp0) +RegTable$`I(temp^2):IG_hetero2` * (temp1^2-temp0^2)
  delta.P.lmiddle=rbind(delta.P.lmiddle,deltay.P)
  deltay.P = RegTable$temp *  (temp1-temp0) + RegTable$`I(temp^2)`* (temp1^2-temp0^2) + 
    RegTable$`temp:IG_hetero3`*(temp1-temp0) +RegTable$`I(temp^2):IG_hetero3` * (temp1^2-temp0^2)
  delta.P.lmiddle=rbind(delta.P.lmiddle,deltay.P)
}

colnames(delta.P.lmiddle)=seq(1,10)

png("figure/boxplot_figure_Peron.png", width = 500, height = 300)
boxplot(-delta.P.lmiddle)
dev.off()



