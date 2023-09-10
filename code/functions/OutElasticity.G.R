#This function exports the elasticity 


OutElasticity.G <- function(){
  
  elasticityG <- data.frame(persistence=character(5),model1_noFE=numeric(5),model2_YearFE=numeric(5),
                            model3_YearStateFE=numeric(5),model4_YearStateFE_lTT=numeric(5))
  
  
  elasticityG[1,'persistence']="IP"
  elasticityG[1,2:5]=sapply(ModellistG.IP,function(model) coef(model)['log(cov)'])
  
  elasticityG[2,'persistence']="P30"
  elasticityG[2,2:5]=sapply(ModellistG.P30,function(model) coef(model)['log(cov)'])  
  
  elasticityG[3,'persistence']="P20"
  elasticityG[3,2:5]=sapply(ModellistG.P20,function(model) coef(model)['log(cov)'])
  
  elasticityG[4,'persistence']="P10"
  elasticityG[4,2:5]=sapply(ModellistG.P10,function(model) coef(model)['log(cov)'])
  
  elasticityG[5,'persistence']="P5"
  elasticityG[5,2:5]=sapply(ModellistG.P5,function(model) coef(model)['log(cov)'])
  
  return(elasticityG)
  openxlsx::write.xlsx(elasticityG,file=paste("result/elasticityG",'.xlsx',sep=''))
}