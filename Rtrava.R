# Rtrava 0.1.0
# Library for the Strava API v3 in R

# AUTHETICATION
strava_oauth <- function(app_name, app_client_id, app_secret, app_scope = NULL) {
      
      strava_app <- oauth_app(app_name, app_client_id, app_secret)  
      
      oauth2.0_token(oauth_endpoint(
                request = "https://www.strava.com/oauth/authorize?",
                authorize = "https://www.strava.com/oauth/authorize",
                access = "https://www.strava.com/oauth/token"),
                strava_app, scope = app_scope)
}

# GET
# Getting several pages of one type of request
get_pages<-function(url_, per_page = 30, page_id = 1, page_num = 1, All = FALSE){
      
      dataRaw <- list()
      
      if(All){
            per_page=200
            page_id=1
            page_num=300 #Substitute for left queries in the rate limit
      }
      
      i = page_id - 1
      repeat{
            i <- i + 1
            req <- GET(url_, stoken, query = list(per_page=per_page, page=i))
            stop_for_status(req)
            dataRaw <- c(dataRaw,content(req))
            if(length(content(req)) < 200) {
                  break
            }
            if(i>=page_num) {
                  break
            }
      }
      dataRaw
}

# ATHLETE
# Setting the url of the athlete to get data from
url_athlete <- function(id=NULL){
      url_ <- "https://www.strava.com/api/v3/athlete"
      if(!is.null(id))
            url_ <- paste(url_,"s/",id, sep = "")
      return(url_)
}
      
