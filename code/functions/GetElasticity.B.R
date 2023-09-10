#This function calculates elasticity based on regression according to Bjoern's approach
#By Jiaru Zhong, 06.09.2023

GetElasticityB <- function(model,Polypara,income){
  
  if("(Intercept)" %in% rownames(model$coefficients)){
    if (Polypara==1){
      elasticity=1+model$coefficients[2]
    } else if (Polypara==2){
      elasticity=1+coef(model)[2]+2*coef(model)[3]*log(income)
    } else if (Polypara==3){
      elasticity=1+coef(model)[2]+
        2*coef(model)[3]*log(income)+
        3*coef(model)[4]*log(income)^2
    }
  } else {
    if (Polypara==1){
      elasticity=1+model$coefficients[1]
    } else if (Polypara==2){
      elasticity=1+coef(model)[1]+2*coef(model)[2]*log(income)
    } else if (Polypara==3){
      elasticity=1+coef(model)[1]+
        2*coef(model)[2]*log(income)+
        3*coef(model)[3]*(log(income)^2)
    }
  }
  
  return(elasticity)
}
