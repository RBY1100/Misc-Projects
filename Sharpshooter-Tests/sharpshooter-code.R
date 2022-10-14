library(random)
library(tidyverse)
library(purrr)

withouts <- function(x,y,z){
  placehold <- 1:1000; placehold
  for(i in 1:1000){
    hit <- sample(1:20, 1, replace = TRUE) + x
      if(hit == 20 + x){
        placehold[i] <- sum(sample(1:8, 2, replace = TRUE)) + z
      }else if(hit >= y){
        placehold[i] <- sample(1:8, 1, replace = TRUE) + z
      }
      else{
        placehold[i] <- 0
      }
  }
  return(sum(placehold)/1000)
}

withs <- function(x,y,z){
  placehold <- 1:1000; placehold
  for(i in 1:1000){
    hit <- sample(1:20, 1, replace = TRUE) + x - 5
      if(hit == 20 + x - 5){
        placehold[i] <- sum(sample(1:8, 2, replace = TRUE)) + z + 10
      }else if(hit >= y){
        placehold[i] <- sample(1:8, 1, replace = TRUE) + z + 10
    }
    else{
      placehold[i] <- 0
    }
  }
  return(sum(placehold)/1000)
}

multisim <- function(w,x,y,z){
  test <- 1:1000; test
  if(w == "y"){
    for(i in 1:1000){
      test[i] <- withs(x,y,z)
  }
    }else{
      for(i in 1:1000){
       test[i] <- withouts(x,y,z)
      }
    }
  return(sum(test)/1000)
}

withsf <- c(multisim("y", 8, 1, 4), multisim("y", 8, 2, 4), multisim("y", 8, 3, 4), multisim("y", 8, 4, 4),
            multisim("y", 8, 5, 4), multisim("y", 8, 6, 4), multisim("y", 8, 7, 4), multisim("y", 8, 8, 4), 
           multisim("y", 8, 9, 4), multisim("y", 8, 10, 4), multisim("y", 8, 11, 4), multisim("y", 8, 12, 4), 
           multisim("y", 8, 13, 4), multisim("y", 8, 14, 4), multisim("y", 8, 15, 4), multisim("y", 8, 16, 4), 
           multisim("y", 8, 17, 4), multisim("y", 8, 18, 4), multisim("y", 8, 19, 4), multisim("y", 8, 20, 4), 
           multisim("y", 8, 21, 4), multisim("y", 8, 22, 4), multisim("y", 8, 23, 4), multisim("y", 8, 24, 4), 
           multisim("y", 8, 25, 4))
           
withoutsf <- c(multisim("n", 8, 1, 4), multisim("n", 8, 2, 4), multisim("n", 8, 3, 4), multisim("n", 8, 4, 4),
            multisim("n", 8, 5, 4), multisim("n", 8, 6, 4), multisim("n", 8, 7, 4), multisim("n", 8, 8, 4), 
            multisim("n", 8, 9, 4), multisim("n", 8, 10, 4), multisim("n", 8, 11, 4), multisim("n", 8, 12, 4), 
            multisim("n", 8, 13, 4), multisim("n", 8, 14, 4), multisim("n", 8, 15, 4), multisim("n", 8, 16, 4), 
            multisim("n", 8, 17, 4), multisim("n", 8, 18, 4), multisim("n", 8, 19, 4), multisim("n", 8, 20, 4), 
            multisim("n", 8, 21, 4), multisim("n", 8, 22, 4), multisim("n", 8, 23, 4), multisim("n", 8, 24, 4), 
            multisim("n", 8, 25, 4))

AC <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25)

dataframe <- data.frame(AC, withoutsf, withsf)
