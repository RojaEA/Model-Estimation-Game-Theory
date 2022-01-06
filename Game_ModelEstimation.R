library(tidyverse)
library(dplyr)
library(purrr)
library(pipeR)
library(data.table)

#Dataset
data <- read.table("Game_Outcome.csv", dec = ".", header = TRUE, sep = ",")
data <- data[2:15]

#select one conflict sample
conflict <- filter(data, ID== "p:10011:c:11140:1") 

#Create Game Solution Function
Game.Solution <- function(data) {
  conflict_list <- split(data, data$ID)
  
  # Create an empty list
  Result_List <- list()
  
  # Iterate over conflict_list and add column Prob_User
  for (i in 1:length(conflict_list)) {
    
    conflict <- conflict_list[[i]]
    
    conflict$Prob_User <- as.numeric("")
    
    Total_U_carL_carP <- conflict %>%
      filter(leader == "car" & player == "car") %>%
      select(utility) %>%
      pull(.) %>%
      sum(.)
    
    Total_U_pedL_pedP <- conflict %>%
      filter(leader == "ped" & player == "ped") %>%
      select(utility) %>%
      pull(.) %>%
      sum(.)
    
    Total_U_carL_pedP_contc <- conflict %>%
      filter(leader == "car" & player == "ped" & Total_Strategy %in% c("contc_contp", "contc_decp", "contc_leftdevp",
                                                                       "contc_rightdevp")) %>%
      select(utility) %>%
      pull(.) %>%
      sum(.)
    
    Total_U_carL_pedP_decc <- conflict %>%
      filter(leader == "car" & player == "ped" & Total_Strategy %in% c("decc_contp", "decc_decp",
                                                                       "decc_leftdevp", "decc_rightdevp")) %>%
      select(utility) %>%
      pull(.) %>%
      sum(.)
    
    Total_U_pedL_carP_contp <- conflict %>%
      filter(leader == "ped" & player == "car" & Total_Strategy %in% c("contc_contp", "decc_contp")) %>%
      select(utility) %>%
      pull(.) %>%
      sum(.)
    
    Total_U_pedL_carP_decp <- conflict %>%
      filter(leader == "ped" & player == "car" & Total_Strategy %in% c("contc_decp", "decc_decp")) %>%
      select(utility) %>%
      pull(.) %>%
      sum(.)
    
    Total_U_pedL_carP_leftdevp <- conflict %>%
      filter(leader == "ped" & player == "car" & Total_Strategy %in% c("contc_leftdevp", "decc_leftdevp")) %>%
      select(utility) %>%
      pull(.) %>%
      sum(.)
    
    Total_U_pedL_carP_rightdevp <- conflict %>%
      filter(leader == "ped" & player == "car" & Total_Strategy %in% c("contc_rightdevp", "decc_rightdevp")) %>%
      select(utility) %>%
      pull(.) %>%
      sum(.)
    
    for (j in 1:nrow(conflict)) {
      
      if (conflict$leader[j] == "car" & conflict$player[j] == "car") 
      { conflict$Prob_User[j] = conflict$utility[j] / Total_U_carL_carP }
      
      else if (conflict$leader[j] == "car" & conflict$player[j] == "ped" & conflict$Total_Strategy[j] %in% c("contc_contp", "contc_decp",
                                                                                                             "contc_leftdevp", "contc_rightdevp")) 
      { conflict$Prob_User[j] = conflict$utility[j] / Total_U_carL_pedP_contc }
      
      else if (conflict$leader[j] == "car" & conflict$player[j] == "ped" & conflict$Total_Strategy[j] %in% c("decc_contp", "decc_decp",
                                                                                                             "decc_leftdevp", "decc_rightdevp")) 
      { conflict$Prob_User[j] = conflict$utility[j] / Total_U_carL_pedP_decc }
      
      else if (conflict$leader[j] == "ped" & conflict$player[j] == "ped") 
      { conflict$Prob_User[j] = conflict$utility[j] / Total_U_pedL_pedP }
      
      else if (conflict$leader[j] == "ped" & conflict$player[j] == "car" & conflict$Total_Strategy[j] %in% c("contc_contp", "decc_contp")) 
      { conflict$Prob_User[j] = conflict$utility[j] / Total_U_pedL_carP_contp }
      
      else if (conflict$leader[j] == "ped" & conflict$player[j] == "car" & conflict$Total_Strategy[j] %in% c("contc_decp", "decc_decp")) 
      { conflict$Prob_User[j] = conflict$utility[j] / Total_U_pedL_carP_decp }
      
      else if (conflict$leader[j] == "ped" & conflict$player[j] == "car" & conflict$Total_Strategy[j] %in% c("contc_leftdevp", "decc_leftdevp")) 
      { conflict$Prob_User[j] = conflict$utility[j] / Total_U_pedL_carP_leftdevp }
      
      else if (conflict$leader[j] == "ped" & conflict$player[j] == "car" & conflict$Total_Strategy[j] %in% c("contc_rightdevp", "decc_rightdevp")) 
      { conflict$Prob_User[j] = conflict$utility[j] / Total_U_pedL_carP_rightdevp }
      
      
    }
    # Compute the probability of strategy pairs in the game
    for (x in 1:length(conflict)) {
      
      conflict$Prob_User <- as.numeric(conflict$Prob_User)
      
      conflict$Prob_Game <- as.numeric("")
      
      Total_Prob_contc_contp <- conflict %>%
        filter(Total_Strategy == "contc_contp") %>%
        select(Prob_User) %>%
        pull(.) %>%
        prod(.)
      
      Total_Prob_contc_decp <- conflict %>%
        filter(Total_Strategy == "contc_decp") %>%
        select(Prob_User) %>%
        pull(.) %>%
        prod(.)
      
      Total_Prob_contc_leftdevp <- conflict %>%
        filter(Total_Strategy == "contc_leftdevp") %>%
        select(Prob_User) %>%
        pull(.) %>%
        prod(.)
      
      Total_Prob_contc_rightdevp <- conflict %>%
        filter(Total_Strategy == "contc_rightdevp") %>%
        select(Prob_User) %>%
        pull(.) %>%
        prod(.)
      
      Total_Prob_decc_contp <- conflict %>%
        filter(Total_Strategy == "decc_contp") %>%
        select(Prob_User) %>%
        pull(.) %>%
        prod(.)
      
      Total_Prob_decc_decp <- conflict %>%
        filter(Total_Strategy == "decc_decp") %>%
        select(Prob_User) %>%
        pull(.) %>%
        prod(.)
      
      Total_Prob_decc_leftdevp <- conflict %>%
        filter(Total_Strategy == "decc_leftdevp") %>%
        select(Prob_User) %>%
        pull(.) %>%
        prod(.)
      
      Total_Prob_decc_rightdevp <- conflict %>%
        filter(Total_Strategy == "decc_rightdevp") %>%
        select(Prob_User) %>%
        pull(.) %>%
        prod(.)
      
      for (k in 1:nrow(conflict)) {
        if (conflict$Total_Strategy[k] == "contc_contp") 
        {conflict$Prob_Game[k] = Total_Prob_contc_contp} 
        
        else if (conflict$Total_Strategy[k] == "contc_decp") 
        {conflict$Prob_Game[k] = Total_Prob_contc_decp}
        
        else if (conflict$Total_Strategy[k] == "contc_leftdevp") 
        {conflict$Prob_Game[k] = Total_Prob_contc_leftdevp}
        
        else if (conflict$Total_Strategy[k] == "contc_rightdevp") 
        {conflict$Prob_Game[k] = Total_Prob_contc_rightdevp}
        
        else if (conflict$Total_Strategy[k] == "decc_contp") 
        {conflict$Prob_Game[k] = Total_Prob_decc_contp}
        
        else if (conflict$Total_Strategy[k] == "decc_decp") 
        {conflict$Prob_Game[k] = Total_Prob_decc_decp}
        
        else if (conflict$Total_Strategy[k] == "decc_leftdevp") 
        {conflict$Prob_Game[k] = Total_Prob_decc_leftdevp}
        
        else if (conflict$Total_Strategy[k] == "decc_rightdevp") 
        {conflict$Prob_Game[k] = Total_Prob_decc_rightdevp}
        
        
      }
      # Compute the Game outcome
        
        conflict$Prob_Game <- as.numeric(conflict$Prob_Game)
        
    }
    Result_List[[i]] <- conflict  %>%
      filter(conflict$Game_Choice == 1) %>%
      select(Prob_Game)
    
  }
  return(bind_rows(Result_List))
}


#Apply Game.Solution() function on each individuals
Probability <- data  %>% 
  filter(Game_Choice==1)  %>% 
  do(data.frame(prob= Game.Solution(data)))
Probability
sum(log(Probability$Prob_Game))

Likelihood <- function(theta){
  # Calculating utility for each alternative for all the individuals
  #data$constant= theta[1]
  data$utility = exp(theta[1]*data$Safety + theta[2]*data$Time + theta[3]*data$Detour +
                  theta[4]*data$Dec + theta[5]*data$Group + theta[6]*data$Lane + theta[7]*data$Row)
  # Calculating choice probability for each individual
  Probability=data %>% 
    filter(Game_Choice == 1) %>% # select choice
    do(data.frame(prob = Game.Solution(data)))  # use Game.Solution() function on each individual
  return(-sum(log(Probability$Prob_Game), log = TRUE)) # return the sum log likelihood 
}

#Maximum Likelihood Estimation
est <- optim(c(1.95,1.14,1.14,1.65,1.8,1.8, 1.8),Likelihood,method = "BFGS",control=list(trace=1))

#Optimal input arguments
est$par

#if TRUE, the optimization converged to a minimum
est$convergence == 0

est$value



