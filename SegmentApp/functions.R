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
