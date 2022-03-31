res <- read.csv("~/Documents/magistrValya/data_clean_methods/lab4/data-2/results_of_care.csv", colClasses = "character")
head(res) 
res[, 11] <- as.numeric(res[, 11])
hist(res[, 11], xlab = "День", ylab = "Количество смертей", main = "30-дневные показатели смертности от сердечного приступа")

getBestHospital <- function(state, criteria) {
  if (criteria=="heart attack"){
    criteria_column = 11
  }
  else if (criteria=="heart failure"){
    criteria_column = 17
  }
  else if (criteria=="pneumonia"){
    criteria_column = 23
  }
  else{
    stop("недопустимый показатель")
  }
  res_by_criteria <- res[, c(2, 7, criteria_column)]
  res_by_criteria <- res_by_criteria[res_by_criteria$State==state,]
  if (nrow(res_by_criteria) == 0) {
    stop("недопустимый штат")
  } 
  
  res_by_criteria <- res_by_criteria[complete.cases(res_by_criteria), ]
  res_by_criteria <- res_by_criteria[res_by_criteria[,3] != 'Not Available',]
  min_by_criteria <- min(as.numeric(res_by_criteria[,3]))
  res_by_min_criteria <- res_by_criteria[res_by_criteria[,3]==min_by_criteria,]
  res_by_min_criteria <- res_by_min_criteria[order(res_by_min_criteria[,1]),]
  return (res_by_min_criteria[1,1])
}

print(getBestHospital("MD", "pneumonia"))


getHospitalRating <- function(state, criteria, n = "best") {
  if (criteria=="heart attack"){
    criteria_column = 11
  }
  else if (criteria=="heart failure"){
    criteria_column = 17
  }
  else if (criteria=="pneumonia"){
    criteria_column = 23
  }
  else{
    stop("недопустимый показатель")
  }
  res_by_criteria <- res[, c(2, 7, criteria_column)]
  res_by_criteria <- res_by_criteria[res_by_criteria$State==state,]
  if (nrow(res_by_criteria) == 0) {
    stop("недопустимый штат")
  }
  res_by_criteria <- res_by_criteria[complete.cases(res_by_criteria), ]
  res_by_criteria <- res_by_criteria[res_by_criteria[,3] != 'Not Available',]
  #criteria_range <- unique(as.numeric(res_by_criteria[,3]))
  res_by_criteria <- res_by_criteria[order(as.numeric(res_by_criteria[,3]),res_by_criteria[,1]),]
  #criteria_range <- criteria_range[order(criteria_range)]
  hospital_amount <- nrow(res_by_criteria)
  if (n=="best"){
    return (res_by_criteria[1,1])
  }
  else if(n=="worst"){
    return (res_by_criteria[hospital_amount,1])
  }
  else if(n>hospital_amount){
    return (NA)
  }
  else{
    return (res_by_criteria[n,1])
  }
  #pos_in_rating <- criteria_range[n]
  #print(res_by_criteria)
  #res_by_pos <- res_by_criteria[res_by_criteria[,3] == pos_in_rating,]

}

print(getHospitalRating("TX", "heart failure", 4))



getRaiting <- function(criteria, n = "best") {
  unic_states <- unique(res[,7])
  rating_df <- data.frame()
  #df <- data.frame(matrix(ncol = 3, nrow = 0))
  #x <- c("name", "age", "gender")
  #colnames(df) <- x
  if (criteria=="heart attack"){
    criteria_column = 11
  }
  else if (criteria=="heart failure"){
    criteria_column = 17
  }
  else if (criteria=="pneumonia"){
    criteria_column = 23
  }
  else{
    stop("недопустимый показатель")
  }
  res_by_criteria <- res[, c(2, 7, criteria_column)]
  res_by_criteria <- res_by_criteria[complete.cases(res_by_criteria), ]
  res_by_criteria <- res_by_criteria[res_by_criteria[,3] != 'Not Available',]
  for (st in unic_states){
    res_by_state <- res_by_criteria[res_by_criteria$State==st,]
    res_by_state <- res_by_state[order(as.numeric(res_by_state[,3]),res_by_state[,1]),]
    res_by_state <- res_by_state[, c(1, 2)]
    hospital_amount <- nrow(res_by_state)
    if (n=="best"){
      new_row <- res_by_state[1,]
    }
    else if(n=="worst"){
      new_row <- res_by_state[hospital_amount,]
    }
    else if(n>hospital_amount){
      new_row <- c(NA,st)
    }
    else{
      new_row <- res_by_state[n,]
    }
    rating_df <- rbind(rating_df,new_row)
  }
  return (rating_df)
}

head(getRaiting("pneumonia", 15), 10)
