#' Finding the weekdays and weekends in a month
#' 
#' @param year : Year (e.g. if the call list is for 2021-3 then year = 2021)
#' @param month : Month (e.g. if the call list is for 2021-3 then month = 3)
#' @param def_weekend : Weekend definitions, could be "SS" (Saturday and Sunday), "FS" (Friday and Saturday) and "FSS" (Friday, Saturday and Sunday)
#' @return a list of characters signifying weekdays "D" and weekends "E" in a month
#' @examples 
#' gen_days(year = 2021, month = 3, def_weekend = "SS")
gen_days = function(year,month,def_weekend) {
  day = as.Date(paste(year,month,"01", sep = "-"))
  md = Hmisc::monthDays(day)
  end = as.Date(paste(year,month,md, sep = "-"))
  days = seq.Date(day, end, by = "days")
  
  daysf = format(days, format = "%d")
  daysf = sapply(daysf, function (i) {
    stringr::str_remove(i,"^0+")
  })
  
  wd = sapply(days, function(i) {weekdays(i)})
  try(if (def_weekend == "FS") {
    we = which(wd == "Saturday" | wd == "Friday")
  } else if (def_weekend == "FSS") {
    we = which(wd == "Saturday" | wd == "Sunday" | wd == "Friday")
  } else if (def_weekend == "SS") {
    we = which(wd == "Saturday" | wd == "Sunday")
  })
  
  wewd = rep("D", length(daysf))
  wewd[we] = "E"
  
  return(wewd)
}

#' Generating the collagen template for a specific month in a year
#' 
#' @param candidate_no : Number of candidates in the call list
#' @param year : Year (e.g. if the call list is for 2021-3 then year = 2021)
#' @param month : Month (e.g. if the call list is for 2021-3 then month = 3)
#' @param def_weekend : Weekend definitions, could be "SS" (Saturday and Sunday), "FS" (Friday and Saturday) and "FSS" (Friday, Saturday and Sunday)
#' @return A collagen template in the working directory 'collagen_template-yyyy-mm-dd.csv'; yyyy-mm-dd refers to the date of generation, but not the concerned month for the call list
#' @examples 
#' collagen_template(candidate_no = 12, year = 2021, month = 3, def_weekend = "SS")
collagen_template = function(candidate_no, year, month, def_weekend) {
  
  d = gen_days(year,month,def_weekend) 
  
  mat = matrix(nrow = 34, ncol = 2 + candidate_no) 
  
  mat[,1] = c(1:32, "count", "wecount")
  
  mat[,2] = c(d, rep("nil", 34-length(d)))
  
  mat[(1:length(d)),(3:(candidate_no+2))] = 0
  mat[((length(d)+1):32),] = "nil"
  mat[33,(3:(candidate_no+2))] = (length(d) %/% candidate_no) + 1
  mat[34,(3:(candidate_no+2))] = (length(which(d=="E")) %/% candidate_no) + 1
  
  colnames(mat) = c("index", "day", LETTERS[seq(from = 1, to = candidate_no)])
  mat = as.data.frame(mat)
  
  write.csv(mat,
            paste("~/", "collagen_template-", as.character(Sys.Date()), ".csv", sep = ""),
            row.names = F)
  
  print("Please check your working directory for the file 'collagen_template-yyyy-mm-dd.csv'")
  print("yyyy-mm-dd refers to the date of today")
}

#' Function to read the collagen template
#' 
#' @return A dataframe extracted from the collagen template
#' @examples 
#' collagen_read()
collagen_read = function() {
  raw = read.csv(paste("~/", "collagen_template-", as.character(Sys.Date()), ".csv", sep = ""))
  return(raw)
}