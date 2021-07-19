#################################
### Creation of the data sets ###
#################################

###################
#### Settings #####
###################

if(!require(ggdag)){
  install.packages("ggdag")
  library(ggdag)
}

#install.packages("stringr")


####################
###### D -> Y ######
####################
## no confounding ##
### no mediation ###
####################

  #Causal paths between the Variables
    # 1. D -(+)-> Y

  #Creation of the data
    set.seed(548)
    D <- rnorm(5000, mean=7, sd=3)
    
    set.seed(206)
    Y <- D*2 + rnorm(5000, mean=0, sd=10)
    
    data.1 <- data.frame(D=D, Y=Y)
    
  #DAG
    dag.1 <- dagify(Y ~ D,
                     labels = c("D" = "D", 
                                "Y" = "Y"),
                     
                     exposure = "D",
                     outcome = "Y")
    
    ggdag(dag.1, text = FALSE, use_labels = "label") # DAG
    ggdag_status(dag.1) # DAG with variable status (exposure/outcome/latent)
    #ggdag_paths(dag.1, text = FALSE, use_labels = "label") #exposure and outcome must be defined
    #ggdag_adjustment_set(dag.1, text = FALSE, use_labels = "label") #sets of covariates needed for unbiased estimation
    
  #Plotting data & estimating correlations
    scatter.smooth(x=data.1$D, y=data.1$Y, main="D -> Y") # scatter plot with smooth curve (computed by loess)
  
  #With confidence intervall (takes time...)
    #ggplot(data.1,aes(x = D, y = Y)) + 
    #  geom_point() + 
    #  stat_smooth(method = "loess", se = TRUE,level = 0.90)
    
    fit.1<- lm(Y ~ D, data.1) # fitting the regression model (adjustet)
    summary(fit.1) # beta-coefficients & p-value
    confint(fit.1,'D', level = 0.95) # confidence interval(s)   
    
    

    
    
####################
###### D -> Y ######
####################
##  + confounding ##
### no mediation ###
####################
    
    #Causal paths between the Variables
    # 1. D -(+)-> Y
    # 2. D <-(-)- Z -(+)-> Y (back-door path) 
    
      # -> Path 1.) is covered 
    
    #Creation of the data
      set.seed(548)
      Z <- rnorm(10000, mean=4, sd=3.5)    
      
      set.seed(678)
      D <- Z*(-1) + rnorm(10000, mean=7, sd=3)
      
      set.seed(206)
      Y <- D*2 + Z*3.5 + rnorm(10000, mean=0, sd=15)
      
      data.2 <- data.frame(Z=Z, D=D, Y=Y)
    
    #DAG
      dag.2 <- dagify(Y ~ D + Z,
                      D ~ Z, 
                      labels = c("D" = "D",
                                 "Z" = "Z",
                                 "Y" = "Y"),
                      
                      exposure = "D",
                      outcome = "Y")
      
      #ggdag(dag.2, text = FALSE, use_labels = "label") # DAG
      ggdag_status(dag.2) # DAG with variable status (exposure/outcome/latent)
      ggdag_paths(dag.2, text = FALSE, use_labels = "label") #exposure and outcome must be defined
      ggdag_adjustment_set(dag.2, text = FALSE, use_labels = "label") #sets of covariates needed for unbiased estimation
    
    #Plotting data & estimating correlations
      scatter.smooth(x=data.2$D, y=data.2$Y, main="D -> Y") # scatter plot with smooth curve (computed by loess)
      
      fit.2u<- lm(Y ~ D, data.2) # fitting the regression model (unadjustet)
      fit.2a<- lm(Y ~ D + Z, data.2) # fitting the regression model (adjustet)
      summary(fit.2u) # beta-coefficients & p-value
        confint(fit.2u,'D', level = 0.95) # confidence interval(s)   
      summary(fit.2a) # beta-coefficients & p-value
        confint(fit.2a,'D', level = 0.95) # confidence interval(s)  
      
      
####################
###### D -> Y ######
####################
##  + confounding ##
### no mediation ###
####################
      
    #Causal paths between the Variables
      # 1. D -(+)-> Y
      # 2. D <-(+)- Z -(+)-> Y (back-door path) 
      
      # -> Path 1.) appears oversized
      
    #Creation of the data
      set.seed(548)
      Z <- rnorm(10000, mean=4, sd=3.5)    
      
      set.seed(303)
      D <- Z*(+1) + rnorm(10000, mean=7, sd=3)
      
      set.seed(206)
      Y <- D*2 + Z*3.5 + rnorm(10000, mean=0, sd=20)
      
      data.3 <- data.frame(Z=Z, D=D, Y=Y)
      
    #DAG
      dag.3 <- dagify(Y ~ D + Z,
                      D ~ Z, 
                      labels = c("D" = "D",
                                 "Z" = "Z",
                                 "Y" = "Y"),
                      
                      exposure = "D",
                      outcome = "Y")
      
      #ggdag(dag.3, text = FALSE, use_labels = "label") # DAG
      ggdag_status(dag.3) # DAG with variable status (exposure/outcome/latent)
      ggdag_paths(dag.3, text = FALSE, use_labels = "label") #exposure and outcome must be defined
      ggdag_adjustment_set(dag.3, text = FALSE, use_labels = "label") #sets of covariates needed for unbiased estimation
      
    #Plotting data & estimating correlations
      scatter.smooth(x=data.3$D, y=data.3$Y, main="D -> Y") # scatter plot with smooth curve (computed by loess)
      
      fit.3u<- lm(Y ~ D, data.3) # fitting the regression model (unadjustet)
      fit.3a<- lm(Y ~ D + Z, data.3) # fitting the regression model (adjustet)
      summary(fit.3u) # beta-coefficients & p-value
        confint(fit.3u,'D', level = 0.95) # confidence interval(s)   
      summary(fit.3a) # beta-coefficients & p-value
        confint(fit.3a,'D', level = 0.95) # confidence interval(s) 
      
      
####################
###### D -> Y ######
####################
##  + confounding ##
### no mediation ###
####################
      
    #Causal paths between the Variables
      # 1. D -(+)-> Y
      # 2. D <-(+)- Z1 -(+)-> Z2 -(+)-> Y (back-door path)
      
      # -> Path 1.) appears oversized
      
    #Creation of the data
      set.seed(548)
      Z1 <- rnorm(10000, mean=4, sd=3.5)  
      
      set.seed(449)
      Z2 <- Z1*(+0.5) + rnorm(10000, mean=1, sd=2.5)       
      
      set.seed(303)
      D <- Z1*(+1) + rnorm(10000, mean=7, sd=3)
      
      set.seed(206)
      Y <- D*2 + Z2*1.5 + rnorm(10000, mean=0, sd=10)
      
      data.4 <- data.frame(Z1=Z1, Z2=Z2, D=D, Y=Y)
      
    #DAG
      dag.4 <- dagify(Y ~ D +Z2,
                      D ~ Z1, 
                      Z2 ~ Z1,
                      labels = c("D" = "D",
                                 "Z1" = "Z1",
                                 "Z2" = "Z2",
                                 "Y" = "Y"),
                      
                      exposure = "D",
                      outcome = "Y")
      
      #ggdag(dag.4, text = FALSE, use_labels = "label") # DAG
      ggdag_status(dag.4) # DAG with variable status (exposure/outcome/latent)
      ggdag_paths(dag.4, text = FALSE, use_labels = "label") #exposure and outcome must be defined
      ggdag_adjustment_set(dag.4, text = FALSE, use_labels = "label") #sets of covariates needed for unbiased estimation
      
    #Plotting data & estimating correlations
      scatter.smooth(x=data.4$D, y=data.4$Y, main="D -> Y") # scatter plot with smooth curve (computed by loess)
      
      fit.4u<- lm(Y ~ D, data.4) # fitting the regression model (unadjustet)
      fit.4a1<- lm(Y ~ D + Z1, data.4) # fitting the regression model (adjustet)
      fit.4a2<- lm(Y ~ D + Z2, data.4) # fitting the regression model (adjustet)
      summary(fit.4u) # beta-coefficients & p-value
        confint(fit.4u,'D', level = 0.95) # confidence interval(s)   
      summary(fit.4a1) # beta-coefficients & p-value
        confint(fit.4a1,'D', level = 0.95) # confidence interval(s)       
      summary(fit.4a2) # beta-coefficients & p-value
        confint(fit.4a2,'D', level = 0.95) # confidence interval(s)      
    
  
####################
###### D -> Y ######
####################
##  + confounding ##
### no mediation ###
####################
      
    #Causal paths between the Variables
      # 1. D -(+)-> Y
      # 2. D <-(+)- Z1 -(+)-> Z2 -(+)-> Z3 -(-)-> Y (1. back-door path)  
      # 3. D <-(+)- Z1 -(+)-> Z2 -(-)-> Z4 -(-)-> Y (2. back-door path)  
      
      # -> Path 1.) is covered
      
    #Creation of the data
      set.seed(548)
      Z1 <- rnorm(10000, mean=4, sd=3.5)  
      
      set.seed(449)
      Z2 <- Z1*(+1.5) + rnorm(10000, mean=1, sd=2.5)
      
      set.seed(329)
      Z3 <- Z2*(+2) + rnorm(10000, mean=1.5, sd=1.5)  
      
      set.seed(741)
      Z4 <- Z2*(-0.5) + rnorm(10000, mean=2.5, sd=4)      
      
      set.seed(303)
      D <- Z1*(+1) + rnorm(10000, mean=7, sd=3)
      
      set.seed(206)
      Y <- D*2 + Z3*(-1.5) + Z4*(-1) + rnorm(10000, mean=0, sd=30)
      
      data.5 <- data.frame(Z1=Z1, Z2=Z2, Z3=Z3, Z4=Z4, D=D, Y=Y)
      
    #DAG
      dag.5 <- dagify(Y ~ D + Z3 + Z4,
                      D ~ Z1, 
                      Z2 ~ Z1,
                      Z3 ~ Z2,
                      Z4 ~ Z2,
                      labels = c("D" = "D",
                                 "Z1" = "Z1",
                                 "Z2" = "Z2",
                                 "Z3" = "Z3",
                                 "Z4" = "Z4",
                                 "Y" = "Y"),
                      
                      exposure = "D",
                      outcome = "Y")
      
      #ggdag(dag.5, text = FALSE, use_labels = "label") # DAG
      ggdag_status(dag.5) # DAG with variable status (exposure/outcome/latent)
      ggdag_paths(dag.5, text = FALSE, use_labels = "label") #exposure and outcome must be defined
      ggdag_adjustment_set(dag.5, text = FALSE, use_labels = "label") #sets of covariates needed for unbiased estimation
      
      #unmeasured Faktors
      dag.5b <- dagify(Y ~ D + Z3 + Z4,
                      D ~ Z1, 
                      Z2 ~ Z1,
                      Z3 ~ Z2,
                      Z4 ~ Z2,
                      labels = c("D" = "D",
                                 "Z1" = "Z1",
                                 "Z2" = "Z2",
                                 "Z3" = "Z3",
                                 "Z4" = "Z4",
                                 "Y" = "Y"),
                      
                      exposure = "D",
                      outcome = "Y",
                      latent = c("Z2","Z1"))
      
      #ggdag(dag.5b, text = FALSE, use_labels = "label") # DAG
      ggdag_status(dag.5b) # DAG with variable status (exposure/outcome/latent)
      ggdag_paths(dag.5b, text = FALSE, use_labels = "label") #exposure and outcome must be defined
      ggdag_adjustment_set(dag.5b, text = FALSE, use_labels = "label") #sets of covariates needed for unbiased estimation
      
      
    #Plotting data & estimating correlations
      scatter.smooth(x=data.5$D, y=data.5$Y, main="D -> Y") # scatter plot with smooth curve (computed by loess)
      
      fit.5u<- lm(Y ~ D, data.5) # fitting the regression model (unadjustet)
      fit.5a1<- lm(Y ~ D + Z1, data.5) # fitting the regression model (adjustet)
      fit.5a2<- lm(Y ~ D + Z2, data.5) # fitting the regression model (adjustet)
      fit.5a3<- lm(Y ~ D + Z3 + Z4, data.5) # fitting the regression model (adjustet)
      summary(fit.5u) # beta-coefficients & p-value
        confint(fit.5u,'D', level = 0.95) # confidence interval(s)   
      summary(fit.5a1) # beta-coefficients & p-value
        confint(fit.5a1,'D', level = 0.95) # confidence interval(s)       
      summary(fit.5a2) # beta-coefficients & p-value
        confint(fit.5a2,'D', level = 0.95) # confidence interval(s)       
      summary(fit.5a3) # beta-coefficients & p-value
        confint(fit.5a3,'D', level = 0.95) # confidence interval(s)       
      
    
      
####################
###### D -> Y ######
####################
## no confounding ##
###  + mediation ###
####################
      
    #Causal paths between the Variables
      # 1. D -(+)-> M -(+) -> Y (true effekt: 2*0,5=1)
      
        # -> Control of M underestimates the true effect 
      
    #Creation of the data
      set.seed(548)
      D <- rnorm(10000, mean=4, sd=3.5)    
      
      set.seed(303)
      M <- D*(+2) + rnorm(10000, mean=7, sd=3)
      
      set.seed(206)
      Y <- M*0.5 + rnorm(10000, mean=0, sd=30)
      
      data.6 <- data.frame(M=M, D=D, Y=Y)
      
    #DAG
      dag.6 <- dagify(Y ~ M,
                      M ~ D, 
                      labels = c("D" = "D",
                                 "M" = "M",
                                 "Y" = "Y"),
                      
                      exposure = "D",
                      outcome = "Y")
      
      #ggdag(dag.6, text = FALSE, use_labels = "label") # DAG
      ggdag_status(dag.6) # DAG with variable status (exposure/outcome/latent)
      ggdag_paths(dag.6, text = FALSE, use_labels = "label") #exposure and outcome must be defined
      ggdag_adjustment_set(dag.6, text = FALSE, use_labels = "label") #sets of covariates needed for unbiased estimation
      
    #Plotting data & estimating correlations
      scatter.smooth(x=data.6$D, y=data.6$Y, main="D -> M -> Y") # scatter plot with smooth curve (computed by loess)
      
      fit.6a<- lm(Y ~ D + M, data.6) # fitting the regression model (unadjustet)
      fit.6u<- lm(Y ~ D, data.6) # fitting the regression model (adjustet)
      summary(fit.6a) # beta-coefficients & p-value
        confint(fit.6a,'D', level = 0.95) # confidence interval(s)   
      summary(fit.6u) # beta-coefficients & p-value
        confint(fit.6u,'D', level = 0.95) # confidence interval(s)
        
      
####################
###### D -> Y ######
####################
##  + confounding ##
###  + mediation ###
####################
        
      #Causal paths between the Variables
        # 1. D -(+)-> M -(+) -> Y (true effekt: 2*0,5=1)
        # 2. M <-(-)- Z -(-)-> Y (back-door path for M->Y but not for D->Y)
        
        # -> Control of Z: no influence 
        
      #Creation of the data
        set.seed(548)
        D <- rnorm(10000, mean=4, sd=3.5)    
        
        set.seed(312)
        Z <- rnorm(10000, mean=2, sd=2.5)        
        
        set.seed(303)
        M <- D*(+2) + Z*(-2.5) + rnorm(10000, mean=7, sd=3)
        
        set.seed(206)
        Y <- M*0.5 + Z*(-3) + rnorm(10000, mean=0, sd=30)
        
        data.7 <- data.frame(M=M, Z=Z, D=D, Y=Y)
        
      #DAG
        dag.7 <- dagify(Y ~ M + X,
                        M ~ D + X, 
                        labels = c("D" = "D",
                                   "M" = "M",
                                   "Z" = "Z",
                                   "Y" = "Y"),
                        
                        exposure = "D",
                        outcome = "Y")
        
        #ggdag(dag.7, text = FALSE, use_labels = "label") # DAG
        ggdag_status(dag.7) # DAG with variable status (exposure/outcome/latent)
        ggdag_paths(dag.7, text = FALSE, use_labels = "label") #exposure and outcome must be defined
        ggdag_adjustment_set(dag.7, text = FALSE, use_labels = "label") #sets of covariates needed for unbiased estimation
        
      #Plotting data & estimating correlations
        scatter.smooth(x=data.7$D, y=data.7$Y, main="D -> M -> Y") # scatter plot with smooth curve (computed by loess)
        
        fit.7a<- lm(Y ~ D + Z, data.7) # fitting the regression model (unadjustet)
        fit.7u<- lm(Y ~ D, data.7) # fitting the regression model (adjustet)
        summary(fit.7a) # beta-coefficients & p-value
          confint(fit.7a,'D', level = 0.95) # confidence interval(s)   
        summary(fit.7u) # beta-coefficients & p-value
          confint(fit.7u,'D', level = 0.95) # confidence interval(s)
          
          
####################
###### D -> Y ######
####################
##  + confounding ##
### no mediation ###
####################

  # Don't interpret other beta coefficients then the one of the main effect !
          
        #Causal paths between the Variables
          # 1. D -(+)-> M -(+) -> Y (true effekt: 2*0,5=1)
          # 2. D <-(-)- Z1 -(-)-> Y (back-door path for D->Y)
          # 2. Z1 <-(+)- Z2 -(-)-> Y (back-door path for X1->Y)
          
            # -> Estimate X1->Y biased (with & without control for X2) 
          
        #Creation of the data
          set.seed(181)
          Z2 <- rnorm(10000, mean=1.5, sd=3.5)          
          
          set.seed(318)
          Z1 <- Z2*(+2.5) + rnorm(10000, mean=2, sd=3) 
          
          set.seed(548)
          D <- Z1*(-1.5) + rnorm(10000, mean=4, sd=3.5)    
          
          set.seed(303)
          M <- D*(+2) + rnorm(10000, mean=7, sd=3)
          
          set.seed(206)
          Y <- M*0.5 + Z1*(-2) + Z2*(-2.5) + rnorm(10000, mean=0, sd=30)
          
          (Z1 ->(-2)-> Y)
          (Z1->(-1.5)-> D ->(2)-> M ->(0.5)-> Y) = -1.5*2*0.5 =-1.5
          
          data.8 <- data.frame(M=M, Z1=Z1, Z2=Z2, D=D, Y=Y)
          
        #DAG
          dag.8 <- dagify(Y ~ M + Z1,
                          D ~ Z1,
                          M ~ D,
                          labels = c("D" = "D",
                                     "M" = "M",
                                     "Z1" = "Z1",
                                     "Y" = "Y"),
                          
                          exposure = "D",
                          outcome = "Y")
          
          #ggdag(dag.8, text = FALSE, use_labels = "label") # DAG
          ggdag_status(dag.8) # DAG with variable status (exposure/outcome/latent)
          ggdag_paths(dag.8, text = FALSE, use_labels = "label") #exposure and outcome must be defined
          ggdag_adjustment_set(dag.8, text = FALSE, use_labels = "label") #sets of covariates needed for unbiased estimation
          
          
        #Plotting data & estimating correlations
          scatter.smooth(x=data.8$D, y=data.8$Y, main="D -> M -> Y") # scatter plot with smooth curve (computed by loess)
          
          fit.8u<- lm(Y ~ D, data.8) # fitting the regression model (unadjustet)
          fit.8a1<- lm(Y ~ D + Z1, data.8) # fitting the regression model (adjustet)
          summary(fit.8u) # beta-coefficients & p-value
            confint(fit.8u,'D', level = 0.95) # confidence interval(s)   
          summary(fit.8a1) # beta-coefficients & p-value
            confint(fit.8a1,'D', level = 0.95) # confidence interval(s) 
            confint(fit.8a1,'Z1', level = 0.95) # confidence interval(s)
 
        #DAG mit Confounder von Z1->Y
        dag.8b <- dagify(Y ~ M + Z1 + Z2,
                        D ~ Z1,
                        M ~ D,
                        Z1 ~ Z2,
                        labels = c("D" = "D",
                                   "M" = "M",
                                   "Z1" = "Z1",
                                   "Z2" = "Z2",
                                   "Y" = "Y"),
                        
                        exposure = "D",
                        outcome = "Y")
        
        #ggdag(dag.8b, text = FALSE, use_labels = "label") # DAG
        ggdag_status(dag.8b) # DAG with variable status (exposure/outcome/latent)
        ggdag_paths(dag.8b, text = FALSE, use_labels = "label") #exposure and outcome must be defined
        ggdag_adjustment_set(dag.8b, text = FALSE, use_labels = "label")            
            
        fit.8a2<- lm(Y ~ D + Z1 + Z2, data.8) # fitting the regression model (adjustet)
        summary(fit.8a2) # beta-coefficients & p-value
          confint(fit.8a2,'D', level = 0.95) # confidence interval(s)          
          confint(fit.8a2,'Z1', level = 0.95) # confidence interval(s)
        
        #Warum immer noch nicht richtiger Effekt von Z1?
          #-> D ist ein Mediator für den Einfluss von Z1 auf Y (Z1->D->M->Y)
          
        fit.8a3<- lm(Y ~ Z1 + Z2, data.8) # fitting the regression model (adjustet)
        summary(fit.8a3) # beta-coefficients & p-value
          confint(fit.8a3,'Z1', level = 0.95) # confidence interval(s)
          
            
####################
###### D -> Y ######
####################
##  + confounding ##
#### + collider ####
### no mediation ###
####################
            
  # Don't adjust for colliders
    
    #Causal paths between the Variables
    # 1. D -(+)-> Y
    # 2. D <-(-)- Z1 -(-)-> Z2 <-(-)- Z3 -(-)-> Y (back-door path for D->Y) 
    
    # -> 
    
    #Creation of the data
    set.seed(181)
    Z1 <- rnorm(10000, mean=2.5, sd=1) 
    
    set.seed(195)
    Z3 <- rnorm(10000, mean=2, sd=1.5) 
    
    set.seed(318)
    Z2 <- Z1*(2) + Z3*(1.5) + rnorm(10000, mean=2, sd=3) 
    
    set.seed(548)
    D <- Z1*(2) + rnorm(10000, mean=4, sd=2.5)    
    
    set.seed(206)
    Y <- D*1.5 + Z3*(4) + rnorm(10000, mean=0, sd=2.5)
    
    data.9 <- data.frame(Z1=Z1, Z2=Z2, Z3=Z3, D=D, Y=Y)
    
    #DAG
    dag.9 <- dagify(Y ~ D + Z3,
                    D ~ Z1,
                    Z2 ~ Z1 + Z3,
                    labels = c("D" = "D",
                               "Z1" = "Z1",
                               "Z2" = "Z2",
                               "Z3" = "Z3",
                               "Y" = "Y"),
                    
                    exposure = "D",
                    outcome = "Y")
    
    #ggdag(dag.9, text = FALSE, use_labels = "label") # DAG
    ggdag_status(dag.9) # DAG with variable status (exposure/outcome/latent)
    ggdag_paths(dag.9, text = FALSE, use_labels = "label") #exposure and outcome must be defined
    ggdag_adjustment_set(dag.9, text = FALSE, use_labels = "label") #sets of covariates needed for unbiased estimation
    
    #Plotting data & estimating correlations
    scatter.smooth(x=data.9$D, y=data.9$Y, main="D -> M -> Y") # scatter plot with smooth curve (computed by loess)
    
    fit.9u<- lm(Y ~ D, data.9) # fitting the regression model (unadjustet)
    fit.9a1<- lm(Y ~ D + Z2, data.9) # fitting the regression model (adjustet)
    fit.9a2<- lm(Y ~ D + Z1 + Z3, data.9) # fitting the regression model (adjustet)
    
    summary(fit.9u) # beta-coefficients & p-value
      confint(fit.9u,'D', level = 0.95) # confidence interval(s)   
    summary(fit.9a1) # beta-coefficients & p-value
      confint(fit.9a1,'D', level = 0.95) # confidence interval(s) 
    summary(fit.9a2) # beta-coefficients & p-value
      confint(fit.9a2,'D', level = 0.95) # confidence interval(s)          

