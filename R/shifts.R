# Functions to shift the exposure into different scenarios

# Scenarios 1 (remained disable throughout the follow-up)

all_dis <-  function(data, trt){

  out <- list()

  a= data[[trt]]

  for (i in 1:length(a)) {
    if (a[i]==1) {
      out[[i]] <- a[i]
    } else {
      out[[i]] <- 1
    }
  }
  unlist(out)
}

# Scenarios 2 (remained disable throughout the follow-up)
non_dis <-  function(data, trt){

  out <- list()

  a= data[[trt]]

  for (i in 1:length(a)) {
    if (a[i]==0) {
      out[[i]] <- a[i]
    } else {
      out[[i]] <- 0
    }
  }
  unlist(out)
}


# Scenarios 3 (remained disable for 75% or more during the follow-up)
dis_75_or_more <-  function(data, trt){

  # calculate the percentage of disability
  perc<- data %>%
    mutate(sum_dis= rowSums(select(., paste0("dis_w",c(9:12)))),
           dis_per= sum_dis/4) %>%
    pull(dis_per)

  a= data[[trt]]

  out <- list()

  for (i in 1:length(a)) {
    if (perc[i]<0.75){
      out[[i]] <- 1
    }else{
      out[[i]] <- a[i]
    }
  }
  unlist(out)
}

# Scenarios 4 (remained disable for 50% or more during the follow-up)
dis_50_or_more <-  function(data, trt){
  # calculate the percentage of disability
  perc<- data %>%
    mutate(sum_dis= rowSums(select(., paste0("dis_w",c(9:12)))),
           dis_per= sum_dis/4) %>%
    pull(dis_per)

  a= data[[trt]]

  out <- list()

  for (i in 1:length(a)) {
    if (perc[i]<0.5){
      out[[i]] <- 1
    }else{
      out[[i]] <- a[i]
    }
  }
  unlist(out)
}


