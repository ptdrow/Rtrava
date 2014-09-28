# Rtrava 0.2.0
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
# Getting data with requests that doesn't require for queries or pagination
get_basic <- function(url_, stoken){
      
      req <- GET(url_, stoken)
      stop_for_status(req)
      dataRaw <- content(req)
      return (dataRaw)
}

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
      return(dataRaw)
}

# ATHLETE
# Setting the url of the athlete to get data from
# Leaving the id = NULL will get the authenticated user data
url_athlete <- function(id = NULL){
      
      url_ <- "https://www.strava.com/api/v3/athlete"
      if(!is.null(id))
            url_ <- paste(url_,"s/",id, sep = "")
      return(url_)
}
      
get_athlete <-function(stoken, id = NULL){
      
      dataRaw <- get_basic(url_athlete(id), stoken)
      return(dataRaw)
}

get_following <- function(following, stoken, id = NULL){
      
      #following must be equal to "friends", "followers" or "both-following"
      url_ <- paste(url_athlete(id),"/", following, sep = "")
      dataRaw <- get_basic(url_, stoken)
      return(dataRaw)
}

get_KOMs <- function(id){
      
      url_ <- paste(url_athlete(id),"/koms", sep = "")
      dataRaw <- get_basic(url_, stoken)
      return(dataRaw)
}

#ACTIVITIES
get_activity_list <- function(){
      url_ <- paste(url_athlete(),"/activities", sep = "")
      dataRaw <- get_basic(url_, stoken)
      return(dataRaw)
}
