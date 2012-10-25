probs <- c(0, 1/4, 1/2, 3/4, 1)

num_of_trials <- 10**6

############## code for 1.10.20.b #####################
# want to find P(H_2 | H_1) <=> P(#H=2) ... P ~ 0.376...
success <- 0
H1_has_occured <- 0

#two tosses of the same coin is rbinom(2,1,p)

coin <- floor(runif(num_of_trials,1,6))
toss1 <- rbinom(num_of_trials,1,probs[coin])
toss2 <- rbinom(num_of_trials,1,probs[coin])
  
for(i in 1:num_of_trials){
  if(toss1[i] == 1){ # count the number of trials that yield heads in first toss
     H1_has_occured <- H1_has_occured + 1
     if(toss2[i] == 1){
       success <- success + 1
     }
   }
}
# 
print(success/H1_has_occured)

# for(i in 1:num_of_trials) {
#   coin <- floor(runif(1,1,6))
#   experiment <- rbinom(2,1,probs[coin])
#   if(sum(experiment) == 2){
#     success <- success+1    
#   }
# }
########### code for 1.10.20.c ##############
# C <- c(0,0,0,0,0) #have this keep track of the number of times C_i = 1 given B_4
# 
# for(i in 1:num_of_trials){
#   coin <- floor(runif(1,1,6)) # pick a coin
#   experiment <- rbinom(4,1,probs[coin]) # flip it 4 times
#   
#   if(sum(experiment) == 1 & experiment[4] == 1){
#     C[coin] <- C[coin]+1
#   }
# }

