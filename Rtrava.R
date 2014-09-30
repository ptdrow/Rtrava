# Rtrava 0.4.0
# Library for the Strava API v3 in R


# Dependencies: httr.

# AUTHETICATION
# Generate a token for an user and the desired scope. It sends the user to the strava authentication page
# if he/she hasn't given permission to the app yet:
strava_oauth <- function(app_name, app_client_id, app_secret, app_scope = NULL) {
      
      strava_app <- oauth_app(app_name, app_client_id, app_secret)  
      
      oauth2.0_token(oauth_endpoint(
                request = "https://www.strava.com/oauth/authorize?",
                authorize = "https://www.strava.com/oauth/authorize",
                access = "https://www.strava.com/oauth/token"),
                strava_app, scope = app_scope)
}

# The token should be configured to work in the httr functions. Use the next line of code to configure it.
#stoken <- config(token = strava_oauth(app_name, app_client_id, app_secret, app_scope))


# RATE LIMIT
# Checks the ratelimit values after the last request and stores the left requests in a global variable

ratelimit <- function(req){
      limit <- as.integer(strsplit(req$headers$`x-ratelimit-limit`, ",")[[1]])
      usage <- as.integer(strsplit(req$headers$`x-ratelimit-usage`, ",")[[1]])
      usage_left <<- limit - usage
}

# GET
# Getting data with requests that doesn't require for queries or pagination
get_basic <- function(url_, stoken){
      
      req <- GET(url_, stoken)
      ratelimit(req)
      stop_for_status(req)
      dataRaw <- content(req)
      return (dataRaw)
}

# Getting several pages of one type of request
get_pages<-function(url_, stoken, per_page = 30, page_id = 1, page_max = 1, queries=NULL, All = FALSE){
      
      dataRaw <- list()
      
      if(All){
            per_page=200 #200 is the max number of items per page. Setting it to 200 reduces the number of requests
            page_id=1
            page_max=usage_left[1]
      }
      else if(page_max > usage_left[1]){#Trying to avoid exceeding the 15 min limit
            page_max <- usage_left[1]
            print (paste("The number of pages would exceed the rate limit, retrieving only"), usage_left[1], "pages")
      }      
      
      i = page_id - 1
      repeat{
            i <- i + 1
            req <- GET(url_, stoken, query = c(list(per_page=per_page, page=i), queries))
            ratelimit(req)
            stop_for_status(req)
            dataRaw <- c(dataRaw,content(req))
            if(length(content(req)) < per_page) {#breaks when the last page retrieved less items than the per_page value
                  break
            }
            if(i>=page_max) {#breaks when the max number of pages or ratelimit was reached
                  break
            }
      }
      return(dataRaw)
}

# ATHLETE
# Set the url of the athlete to get data from (according to its ID)
# Leaving the id = NULL will get the authenticated user data
url_athlete <- function(id = NULL){
      
      url_ <- "https://www.strava.com/api/v3/athlete"
      if(!is.null(id))
            url_ <- paste(url_,"s/",id, sep = "")
      return(url_)
}

#Get the athlete's data
get_athlete <-function(stoken, id = NULL){
      
      dataRaw <- get_basic(url_athlete(id), stoken)
      return(dataRaw)
}

#Get the list of friends or followers from an user or the both-following according to another user
get_following <- function(following, stoken, id = NULL){
      
      #following must be equal to "friends", "followers" or "both-following"
      url_ <- paste(url_athlete(id),"/", following, sep = "")
      dataRaw <- get_basic(url_, stoken)
      return(dataRaw)
}

#Get the list of KOMs/QOMs/CRs of an athlete
get_KOMs <- function(id, stoken){
      
      url_ <- paste(url_athlete(id),"/koms", sep = "")
      dataRaw <- get_basic(url_, stoken)
      return(dataRaw)
}

#ACTIVITIES
#Set the url of activities for differents activities lists.
url_activities <- function(id=NULL, friends=FALSE, club=FALSE){
      url_ <- "https://www.strava.com/api/v3/activities/"
      if(!is.null(id)){
            if(club){#Url for the activities of the club with ID = id
                  url_ <- paste("https://www.strava.com/api/v3/clubs/", id,"/activities", sep="")
            }
            else{#Url for an specific activity
                  url_ <- paste(url_, id, sep = "")
            }
      }
      else if(friends){#Url for the activities of the authenticated user's friends
            url_ <- paste(url_,"following", sep = "")
      }
      else{#Url for the list of activities of the authenticated user
            url_ <- paste(url_athlete(),"/activities", sep = "")
      }
      
      return(url_)      
}

#Get the activities list of the desired type (club, friends, user)
get_activity_list <- function(stoken, id = NULL, club = FALSE, friends = FALSE){
      #This codes assumes requesting all the pages of activities. In other circunstances change the parameters of 'get_pages'
      if (friends | club){
            dataRaw <- get_pages(url_activities(id = id, club = club, friends=friends), stoken, per_page = 200, page_id = 1, page_max = 1)
      }
      else{
            dataRaw <- get_pages(url_activities(), stoken, All=TRUE)
      }
      
      return(dataRaw)
}

#Get detailed data of an activity. It includes the segment efforts
get_activity <- function(id, stoken){
            
      req <- GET(url_activities(id), stoken, query = list(include_all_efforts=TRUE)) 
      stop_for_status(req)
      dataRaw <- content(req)
      return(dataRaw)
}

#CLUBS
#Set the url of the clubs for the different requests
url_clubs <- function(id=NULL, request=NULL){
      if(is.null(id)){#Clubs of the authenticated athlete
            url_ <- paste(url_athlete(), "/clubs", sep = "")
      }
      else{ #request must be "members", "activities" or NULL for club details
            url_ <- paste("https://www.strava.com/api/v3/clubs/", id,"/", request, sep="")
      }
      return(url_)
}      

#Get the data according to the different requests or urls.
get_club <- function(stoken, id=NULL, request=NULL){
      if(is.null(id)){
            dataRaw <- get_basic(url_clubs(), stoken)
      }
      else{ #request must be "members", "activities" or NULL for club details
            switch(request,
                   NULL = dataRaw <- get_basic(url_clubs(id), stoken),
                   
                   activities = dataRaw <- get_activity_list(stoken, id, club = TRUE),
                   
                   members = dataRaw <- get_pages(url_clubs(id = id, request = request), stoken,
                                                  per_page = 200, page_id = 1, page_max = 1)
                   )
      }
      return(dataRaw)
}

#SEGMENTS
#Set the differente url for the segments requests
url_segment <- function(id=NULL, request=NULL) {
      if(!is.null(request)){
            if(!is.null(id) & request == "starred"){
                  url_ <- paste("https://www.strava.com/api/v3/athlete/", id,"/segments/starred", sep="")
            }
            else{#request must be "starred", "all_efforts", "leaderboard" or NULL for club details
                  url_ <- "https://www.strava.com/api/v3/segments/"
                  if(request == "starred"){
                        url_ <- paste(url_, "starred", sep="")
                  }
                  else{
                        url_ <- paste(url_, id, "/", request, sep = "")
                  }
            }
      }
      else{
            url_ <- paste("https://www.strava.com/api/v3/segments/", id, sep="")
      }
      return(url_)
}

#Retrieve details about a specific segment.
get_segment <- function(stoken, id=NULL, request=NULL){
      
      dataRaw <- get_basic(url_segment(id), stoken)
      return(dataRaw)
}

get_starred <- function(stoken, id=NULL){
      # Returns a summary representation of the segments starred by
      # the authenticated user if id=NULL, or by athelete's id.
      
      dataRaw <- get_basic(url_segment(id=id, request="starred"), stoken)
      return(dataRaw)
}

get_leaderboard <- function(stoken, id, nleaders=10, All=FALSE){
      #Returns the leaderboard if All=TRUE or the top nleaders of a segment specified by the id
      dataRaw <- get_pages(url_segment(id, request="leaderboard"), stoken, 
                           per_page = nleaders, All = All)
      return(dataRaw)
}

get_efforts_list <- function(stoken, id,athlete_id=NULL, start_date_local=NULL, end_date_local=NULL){
      #Retrieves all the efforts in a segment if no queries are specified
      #If is given the athlete_id it returns only his/her efforts
      #Dates can be given to change the time range for the efforts to retrive
      queries <- list(athlete_id=athlete_id,
                      start_date_local=start_date_local,
                      end_date_local=end_date_local)
      
      dataRaw <- get_pages(url_segment(id, request="all_efforts"), stoken, queries=queries, All=TRUE)
      return(dataRaw)
}

#STREAMS
#Set the url for the different requests of streams
url_streams  <- function(id, request="activities", types=list("latlng")){
      # 'types' should be a list with any combination of:
      # "time", "latlng", "distance", "altitude", "velocity_smooth", "heartrate",
      # "cadence", "watts", "temp", "moving", "grade_smooth"
      # 'request' must be equal to "activities", "segment_efforts", or "segments"
      
      #Converting the list of types into the proper string
      strtypes <- types[[1]]
      if(length(types)>1){
            for(i in 2:length(types)){
                  strtypes <- paste(strtypes,",", types[[i]], sep="")
            }
      }
      
      # Creating the url string
      url_ <- paste("https://www.strava.com/api/v3/", request, "/", id, "/streams/", strtypes, sep="")
      return(url_)
}

#Retrieve the streams
get_streams  <- function(stoken, id, request="activities",
                         types, resolution="all", series_type="distance"){
      
      #resolution can be "low", "medium", "high" or "all"
      #series_type can be "distance" or "time"
      req <- GET(url_streams(id, request, types), stoken,
                 query = list(resolution=resolution, series_type=series_type))
      ratelimit(req)
      stop_for_status(req)
      dataRaw <- content(req)

      return(dataRaw)
}