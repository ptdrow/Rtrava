# stravaUrbANA 0.2.0
# Dependencies: httr, Rtrava.

strava_friends <- function(following, stoken){
      #following must be equal to "friends", "followers" or "both-following"
      
      dataraw <- get_athlete(stoken)
      
      id <- dataraw$id
      firstname <- dataraw$firstname
      lastname <- dataraw$lastname
      city <- dataraw$city
      state <- dataraw$state
      country <- dataraw$country
      sex <- (dataraw$sex)
      friend <- NA
      follower <- NA
      premium <- dataraw$premium
      created_at <- dataraw$created_at
      
      friends <- data.frame(id, firstname, lastname, sex,
                            city, state, country,
                            friend, follower, premium, created_at,
                            stringsAsFactors=FALSE)
      
      dataraw <- get_following(following, stoken)
      
      for(i in 1:length(dataraw)){
                  
            id <- dataraw[[i]]$id
            
            firstname <- dataraw[[i]]$firstname
            
            lastname <- dataraw[[i]]$lastname
            
            if(!is.null(dataraw[[i]]$sex)){
                  sex <- dataraw[[i]]$sex}
            else{ sex <- NA}
            
            if(is.null(dataraw[[i]]$city)){
                  city <- NA}
            else{ city <- dataraw[[i]]$city}
            
            if(is.null(dataraw[[i]]$state)){
                  state <- NA}
            else{ state <- dataraw[[i]]$state}
            
            if(is.null(dataraw[[i]]$country)){
                  country <- NA}
            else{ country <- dataraw[[i]]$country}
            
            friend <- dataraw[[i]]$friend
            
            if(!is.null(dataraw[[i]]$follower)){
                  follower <- "follower"}
            else{ follower <- "not follower"}
            
            premium <- dataraw[[i]]$premium
            
            created_at <- dataraw[[i]]$created_at
            
            friends <- rbind(friends, c(id, firstname, lastname, sex,
                             city, state, country,
                             friend, follower, premium, created_at))
      }
      return(friends)
}

get_activity_all <- function(stoken){ #falta agregar seguros en contra del exceso del limite de requests 
      
      activity_list <- get_activity_list(stoken)
      activities_ids <- numeric(length(activity_list))
      
      for(i in seq_along(activity_list)){
            activities_ids[i] <- activity_list[[i]]$id
      }
      
      dataRaw <- list()
      i=0
      
      for(id in activities_ids){
            i=i+1
            dataRaw[[i]] <- get_activity(id, stoken)
            print(paste("Downloaded activity", i, "of",length(activities_ids), sep= " "))
      }
      
      return(dataRaw)
}

