#We assume our virus titer is 1e8/mL(infectious unit).
#Based upon the dilution factor and cell number(50,000/well) used for transduction.
#We can extrapolate the theoretical MOI for each test well.
#The MOIns,with descend ranking, are 6.667,2.222,0.741,0.247,0.082,0.0273
 
#########THE ONLY THING YOU NEED TO DO IS IMPORT THE DATA########
###run this function first#######

Titer_calculation_plot <- function(data, ncol, nrow){
  library(ggplot2)
  data <- data
  x <-c(6.667, 2.222,0.741,0.247,0.082,0.0273)
  #function adapted from poisson modeling.
  #B correction parameter for our theoretical virus titer.
  #A the Among the cells received virus that also expressed protein.
  
  if (TRUE){
    titerFunc <- function(x,A,B){
      100*B*(1-exp(-(A*x)))
    }
    
    ####Calculate QV.F.M titer.
    #MOIn=c(6.667, 2.222,0.741,0.247,0.082,0.0273)
    
    MOIcalculation <- function(y){
      y = y
      MOIfit <- nls(y~titerFunc(x,A,B), start = list(A=1, B=1))
      #Save the summary results to cof
      cof <- summary(MOIfit)
      
      #extract the B value from nls fitting.
      coeffA <- cof$coefficients[1,1]
      coeffB <- cof$coefficients[2,1]
      
      MOI = 1e8*coeffA
      return(MOI*coeffB)
    }
    
    Bvalue <- function(y){
      y = y
      MOIfit <- nls(y~titerFunc(x,A,B), start = list(A=1, B=1))
      #Save the summary results to cof
      cof <- summary(MOIfit)
      
      #extract the B value from nls fitting.
      coeffA <- cof$coefficients[1,1]
      coeffB <- cof$coefficients[2,1]
      
      #MOI = 1e8*coeffA
      return(coeffB)
    }
    Avalue <- function(y){
      y = y
      MOIfit <- nls(y~titerFunc(x,A,B), start = list(A=1, B=1))
      #Save the summary results to cof
      cof <- summary(MOIfit)
      
      #extract the B value from nls fitting.
      coeffA <- cof$coefficients[1,1]
      coeffB <- cof$coefficients[2,1]
      
      #MOI = 1e8*coeffA
      return(coeffA)
    }
    
    table <- apply(data,2,MOIcalculation)
    table2 <- as.data.frame(apply(data,2,Bvalue))
    table3 <- as.data.frame(apply(data,2,Avalue))
    
    table <- as.data.frame(table)
    colnames(table)<- "Virus titer"
    table$Bvalue <- table2$`apply(data, 2, Bvalue)`
    table$Avalue <- table3$`apply(data, 2, Avalue)`
  }
  
  
  write.csv(table,file = "virus titer by poisson.csv")
  
  
  #############If you want to plot fitting curve.##################
  myplots <- vector('list', ncol(data))
  for (i in 1:ncol(data)){
    myplots[[i]]<- local(
      {
        i=i
        p1 <-ggplot(data=data,
                    aes(x=x, y = as.matrix(data)[,i]))+
          xlab("Theoretical MOI")+
          ylab("% Positive")+
          geom_point()+
          ylim(0,105)+
          ggtitle(label = colnames(data)[i])+
          annotate("text",x=5, y=25, label=paste("Virus titer:",formatC(table$`Virus titer`[i], format='e', digits = 2)))+
          annotate("text", x=5, y = 15,label=paste("B Value:",formatC(table$`Bvalue`[i], digits = 2)))+
          geom_smooth(se=FALSE, method = 'lm', formula=y~titerFunc(x,table$Avalue[i],table$Bvalue[i]))
      }
    )
  }
  ggpubr::ggarrange(plotlist = myplots, ncol = ncol,nrow = nrow)#please modify ncol and nrow to fit your plot arrangement.
}











