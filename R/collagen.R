#' Function to set up the variables from the template dataframe
#' 
#' @param raw : the template dataframe obtained by the function collagen_read()
#' @return a list of variables (result, count, wecount, day, raww) for subsequent iterations
#' @examples 
#' c(result, count, wecount, day, raww) %<-% set_up_var(raw)
set_up_var = function(raw) {
  
  result = rep(NA, which(raw$day=="nil")[1] - 1)
  result = as.array(result)
  
  day = raw$day[1:which(raw$day=="nil")[1] - 1]
  raww = raw[1:which(raw$day=="nil")[1] - 1, 3:dim(raw)[2]]
  
  day = as.vector(day)
  PHcount = apply(raww, 1, function(i) {
    (length(which(i==2)))
  })
  PH = which(PHcount>0)
  day[PH] = "P"
  
  ind_count = which(raw$index=="count")
  count = as.numeric(as.character(unlist(raw[ind_count, 3:dim(raw)[2]])))
  names(count) = colnames(raww)
  count = as.data.frame(t(count))
  
  wecount = as.numeric(as.character(unlist(raw[34, 3:dim(raw)[2]]))) 
  names(wecount) = colnames(raww)
  wecount = as.data.frame(t(wecount))
  
  return(list(result, count, wecount, day, raww))
}

#' Function to allocate the "P" dates ("P"ublic holiday or "P"ersonalized dates)
#' 
#' @param result : a variable holding the temporary results
#' @param count : a variable holding the temporary counts (total number of calls to be allocated for each candidates)
#' @param wecount : a variable holding the temporary weekend counts (total number of weekend calls to be allocated for each candidates)
#' @param day : a variable signifying weekdays "D" and weekends "E" and PH "P" in a month
#' @param raww : a simplified dataframe extracted from the collagen template
#' @param raw : the template dataframe obtained by the function collagen_read()
#' @return a list of variables (result, count, wecount, day) for subsequent iterations
#' @examples 
#' c(result, count, wecount, day) %<-% gen_PH(result, count, wecount, day, raww, raw)
gen_PH = function(result, count, wecount, day, raww, raw) {
  
  PH = sample(which(day=="P"))
  
  for (i in PH) {
    candidates = colnames(raww)[which(raww[i,]==2)]
    if(length(candidates)>1) {
      for (j in sample(candidates)) {
        
        idr = which(colnames(raww)==j)
        if (count[,idr] == 0) {next}
        
        m = 0
        illegal = (i-2):(i+2)
        illegal = illegal[which(illegal>0)]
        illegal = illegal[which(illegal<which(raw$day=="nil")[1])]
        filled = as.numeric(which(result==j))
        for (k in filled) {
          try(if (k %in% illegal) {m <- 1})
        }
        if (m==1) {next}
        
        result[i] = j
        if (count[,idr] > 0) {count[,idr] = as.numeric(count[,idr]) - 1}
        break
      }
      
    } else {
      idr = which(colnames(raww)==candidates)
      if (count[,idr] == 0) {next}
      
      m = 0
      illegal = (i-2):(i+2)
      illegal = illegal[which(illegal>0)]
      illegal = illegal[which(illegal<which(raw$day=="nil")[1])]
      filled = as.numeric(which(result==candidates))
      for (k in filled) {
        try(if (k %in% illegal) {m <- 1})
      }
      if (m==1) {next}
      
      result[i] = candidates
      if (count[,idr] > 0) {count[,idr] = as.numeric(count[,idr]) - 1}
    }
    
  }
  
  day0 = as.vector(raw$day[1:which(raw$day=="nil")[1] - 1])
  for (x in PH) {
    if (is.na(result[x])) {day[x] = day0[x]}
  }
  return(list(result, count, wecount, day))
}

#' Function to allocate the weekend calls
#' 
#' @param result : a variable holding the temporary results
#' @param count : a variable holding the temporary counts (total number of calls to be allocated for each candidates)
#' @param wecount : a variable holding the temporary weekend counts (total number of weekend calls to be allocated for each candidates)
#' @param day : a variable signifying weekdays "D" and weekends "E" and PH "P" in a month
#' @param raww : a simplified dataframe extracted from the collagen template
#' @param raw : the template dataframe obtained by the function collagen_read()
#' @return a list of variables (result, count, wecount) for subsequent iterations
#' @examples 
#' c(result, count, wecount) %<-% gen_WE(result, count, wecount, day, raww, raw)
gen_WE = function (result, count, wecount, day, raww, raw) {
  
  rand = sample(which(day=="E" & is.na(result)==T))
  
  count_t = count[,which(count>0)] 
  wedr = colnames(wecount)[which(wecount>(as.numeric(max(unlist(wecount[1,])))-1))]
  count_t = count_t[,which(colnames(count_t) %in% wedr)]
  
  dr = colnames(count_t)
  q_dr = sample(dr)
  
  r_dr = vector()
  
  for (i in rand) {
    
    for (j in q_dr) {
      try(if (j %in% r_dr) {next})
      
      m = 0
      
      illegal = (i-2):(i+2)
      illegal = illegal[which(illegal>0)]
      illegal = illegal[which(illegal<which(raw$day=="nil")[1])]
      filled = as.numeric(which(result==j))
      for (k in filled) {
        try(if (k %in% illegal) {m <- 1})
      }
      if (m==1) {next}
      
      idr = which(colnames(raww)==j)
      nocall = which(unlist(raww[,idr]==1))
      try(if (i %in% nocall) {next})
      
      result[i] = j
      count[,idr] = as.numeric(count[,idr]) - 1
      wecount[,idr] = as.numeric(wecount[,idr]) - 1
      r_dr = c(r_dr, j)
      break
    }
  }
  
  return(list(result, count, wecount))
}

#' Function to allocate at least one call (e.g. first call specialist in PSY only have 1 call per month)
#' 
#' @param result : a variable holding the temporary results
#' @param count : a variable holding the temporary counts (total number of calls to be allocated for each candidates)
#' @param wecount : a variable holding the temporary weekend counts (total number of weekend calls to be allocated for each candidates)
#' @param day : a variable signifying weekdays "D" and weekends "E" and PH "P" in a month
#' @param raww : a simplified dataframe extracted from the collagen template
#' @param raw : the template dataframe obtained by the function collagen_read()
#' @return a list of variables (result, count, wecount) for subsequent iterations
#' @examples 
#' c(result, count, wecount) %<-% gen_WD_spec(result, count, wecount, day, raww, raw)
gen_WD_spec = function(result, count, wecount, day, raww, raw) {
  
  WD = which(day=="D" & is.na(result)==T) # excluding the filled dates
  rand = sample(WD)
  
  ind_count = which(raw$index=="count")
  count_t = raw[ind_count, 3:dim(raw)[2]]
  count_t = count_t[,which(count_t==1 & count==1)]
  
  dr = colnames(count_t)
  q_dr = sample(dr)
  
  r_dr = vector()
  
  for (i in rand) {
    
    for (j in q_dr) {
      try(if (j %in% r_dr) {next})
      
      m = 0
      
      illegal = (i-2):(i+2)
      illegal = illegal[which(illegal>0)]
      illegal = illegal[which(illegal<which(raw$day=="nil")[1])]
      filled = as.numeric(which(result==j))
      for (k in filled) {
        try(if (k %in% illegal) {m <- 1})
      }
      if (m==1) {next}
      
      idr = which(colnames(raww)==j)
      nocall = which(unlist(raww[,idr]==1))
      try(if (i %in% nocall) {next})
      
      result[i] = j
      count[,idr] = as.numeric(count[,idr]) - 1
      r_dr = c(r_dr, j)
      break
    }
  }
  
  return(list(result, count, wecount))
}

#' Function to allocate weekday calls
#' 
#' @param result : a variable holding the temporary results
#' @param count : a variable holding the temporary counts (total number of calls to be allocated for each candidates)
#' @param wecount : a variable holding the temporary weekend counts (total number of weekend calls to be allocated for each candidates)
#' @param day : a variable signifying weekdays "D" and weekends "E" and PH "P" in a month
#' @param raww : a simplified dataframe extracted from the collagen template
#' @param raw : the template dataframe obtained by the function collagen_read()
#' @return a list of variables (result, count, wecount) for subsequent iterations
#' @examples 
#' c(result, count, wecount) %<-% gen_WD(result, count, wecount, day, raww, raw)
gen_WD = function (result, count, wecount, day, raww, raw) {
  
  WD = which(day=="D" & is.na(result)==T) # excluding the filled dates
  rand = sample(WD)
  
  count_t = count
  count_t = count_t[,which(count_t>(as.numeric(max(unlist(count[1,])))-1))]
  dr = colnames(count_t)
  q_dr = sample(dr)
  
  r_dr = vector()
  
  for (i in rand) {
    
    for (j in q_dr) {
      try(if (j %in% r_dr) {next})
      
      m = 0
      
      illegal = (i-2):(i+2)
      illegal = illegal[which(illegal>0)]
      illegal = illegal[which(illegal<which(raw$day=="nil")[1])]
      filled = as.numeric(which(result==j))
      for (k in filled) {
        try(if (k %in% illegal) {m <- 1})
      }
      if (m==1) {next}
      
      idr = which(colnames(raww)==j)
      nocall = which(unlist(raww[,idr]==1))
      try(if (i %in% nocall) {next})
      
      result[i] = j
      count[,idr] = as.numeric(count[,idr]) - 1
      r_dr = c(r_dr, j)
      break
    }
  }
  
  return(list(result, count, wecount))
}

#' Function to generate the call list with 1 click
#' 
#' @param raw : the template dataframe obtained by the function collagen_read()
#' @return 'collagen_result-yyyy-mm-dd.csv' (the call list) and 'collagen_stat-yyyy-mm-dd.csv' (the statistics)
#' @examples 
#' raw = collagen_read()
#' collagen (raw)
collagen = function (raw) {
  
  library(zeallot)
  
  c(result, count, wecount, day, raww) %<-% set_up_var(raw)
  n_we = max(unlist(wecount[1,]))
  n_wd = max(unlist(count[1,]))
  wd_spec = F
  if (min(unlist(count[1,])) == 1) {wd_spec <- T}
  
  c(result, count, wecount, day) %<-% gen_PH(result, count, wecount, day, raww, raw)
  
  for (i in 1:n_we) {c(result, count, wecount) %<-% gen_WE(result, count, wecount, day, raww, raw)}
  
  if (wd_spec==T) {c(result, count, wecount) %<-% gen_WD_spec(result, count, wecount, day, raww, raw)}
  
  for (i in 1:n_wd) {c(result, count, wecount) %<-% gen_WD(result, count, wecount, day, raww, raw)}
  
  print(result)
  
  result = as.data.frame(cbind(1:length(result),day, result))
  colnames(result) = c("Date", "D/E/P", "Allocation")
  stat = table(result$"D/E/P", result$"Allocation")
  
  print(stat)
  
  write.csv(result,
            paste("~/", "collagen_result-", as.character(Sys.Date()), ".csv", sep = ""),
            row.names = F)
  
  write.csv(stat,
            paste("~/", "collagen_stat-", as.character(Sys.Date()), ".csv", sep = ""),
            row.names = T)
  
  print("Please check your working directory for the file 'collagen_result-yyyy-mm-dd.csv' and 'collagen_stat-yyyy-mm-dd.csv'")
}