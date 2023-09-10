#This function summarizes the elasticity estimated according Bjoern's approach
#by Jiarui Zhong 06.09.2023

OutElasticity.B <- function(IncomeRef){
  elasticityB <- data.frame(model=character(15), persistence=character(15),model1_noFE=numeric(15),model2_YearFE=numeric(15),
                            model3_YearIndiFE=numeric(15),model4_YearIndiFE_Temp=numeric(15))
  
  elasticityB[1,c('model','persistence')]=c("linear",'IP')
  elasticityB[1,3:6]=sapply(modellist1.IP,GetElasticityB,Polypara=1,income=IncomeRef)
  
  elasticityB[2,c('model','persistence')]=c("linear",'P30')
  elasticityB[2,3:6]=sapply(modellist1.P30,GetElasticityB,Polypara=1,income=IncomeRef)
  
  elasticityB[3,c('model','persistence')]=c("linear",'P20')
  elasticityB[3,3:6]=sapply(modellist1.P20,GetElasticityB,Polypara=1,income=IncomeRef)
  
  elasticityB[4,c('model','persistence')]=c("linear",'P10')
  elasticityB[4,3:6]=sapply(modellist1.P10,GetElasticityB,Polypara=1,income=IncomeRef)
  
  elasticityB[5,c('model','persistence')]=c("linear",'P5')
  elasticityB[5,3:6]=sapply(modellist1.P5,GetElasticityB,Polypara=1,income=IncomeRef)
  
  elasticityB[6,c('model','persistence')]=c("squared",'IP')
  elasticityB[6,3:6]=sapply(modellist2.IP,GetElasticityB,Polypara=2,income=IncomeRef)
  
  elasticityB[7,c('model','persistence')]=c("squared",'P30')
  elasticityB[7,3:6]=sapply(modellist2.P30,GetElasticityB,Polypara=2,income=IncomeRef)
  
  elasticityB[8,c('model','persistence')]=c("squared",'P20')
  elasticityB[8,3:6]=sapply(modellist2.P20,GetElasticityB,Polypara=2,income=IncomeRef)
  
  elasticityB[9,c('model','persistence')]=c("squared",'P10')
  elasticityB[9,3:6]=sapply(modellist2.P10,GetElasticityB,Polypara=2,income=IncomeRef)
  
  elasticityB[10,c('model','persistence')]=c("squared",'P5')
  elasticityB[10,3:6]=sapply(modellist2.P5,GetElasticityB,Polypara=2,income=IncomeRef)
  
  elasticityB[11,c('model','persistence')]=c("cubic",'IP')
  elasticityB[11,3:6]=sapply(modellist3.P5,GetElasticityB,Polypara=3,income=IncomeRef)
  
  elasticityB[12,c('model','persistence')]=c("cubic",'P30')
  elasticityB[12,3:6]=sapply(modellist3.P30,GetElasticityB,Polypara=3,income=IncomeRef)
  
  elasticityB[13,c('model','persistence')]=c("cubic",'P20')
  elasticityB[13,3:6]=sapply(modellist3.P20,GetElasticityB,Polypara=3,income=IncomeRef)
  
  elasticityB[14,c('model','persistence')]=c("cubic",'P10')
  elasticityB[14,3:6]=sapply(modellist3.P10,GetElasticityB,Polypara=3,income=IncomeRef)
  
  elasticityB[15,c('model','persistence')]=c("cubic",'P5')
  elasticityB[15,3:6]=sapply(modellist3.P5,GetElasticityB,Polypara=3,income=IncomeRef)
  
  openxlsx::write.xlsx(elasticityB,file=paste("result/elasticityB_",IncomeRef,'.xlsx',sep=''))
  return(elasticityB)
}
  
  
  
  



