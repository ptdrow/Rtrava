######
# format unicode text using escaped character, used for Shiny App
# output is parsed expression
# 'str_in' is character or factor vector of names to convert
# Contributed by Marcus Beck, Github: @fawda123
escaped_unicode <- function(str_in){
  char_out <- character(length(str_in))
  out <- as.character(str_in)
  out <- gsub("<U+", "\\u", out, fixed = T)
  out <- gsub(">|'", "", out)
  out <- parse(text = paste0("'", out, "'"))
  for(i in 1:length(str_in)) char_out[i]<-out[[i]]
  return(char_out)  
}

#Create logical vector of how the KOM times were lowered over time. TRUE means that effort lowered the KOM or QOM
KOMs_history <- function(Elapsed_Time){
      #browser()
      efforts <- length(Elapsed_Time)
      better <- logical(efforts)
      
      best <- Elapsed_Time[1]
      better[1] <- TRUE
      
      if(efforts > 1){
            for(j in 2:efforts){
                  if(Elapsed_Time[j] <= best){
                        better[j] <- TRUE
                        best <- Elapsed_Time[j]
                  }
            }
      }
      return(better)
}

plot_efforts <- function(dataTidy, KOM=TRUE, Males=TRUE, athlete = NULL, acolor){
      
      if(is.null(athlete)){
            if(Males==TRUE){
                  subGender <- dataTidy$Gender != "F"
                  color = "blue"
            }
            else{
                  subGender<- dataTidy$Gender == "F"
                  color = "violetred1"
            }}
      else{
            subGender <- dataTidy$Name == athlete
            color = acolor  
      }
      if(KOM==TRUE){
            with(subset(dataTidy, subGender), 
                 c(better <- KOMs_history(Elapsed_Time),
                   points(Date[!better], Elapsed_Time[!better], col = color, pch = 1, cex = 1.25),
                   points(Date[better], Elapsed_Time[better], col = color, pch = 19, cex = 1.5)))
      }
      else{
            with(subset(dataTidy, subGender), 
                 points(Date, Elapsed_Time, col = color, pch = 16, cex = 1.25))
      }
}