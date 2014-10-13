# tidyExplore 0.1.0
tidy_explore <- function(dataRaw){
      #Colums initiation
      dataRaw <- dataRaw[[1]]
      name <- character()
      id <- numeric()
      climb_category <- character()
      climb_category_desc <- character()
      average_grade <- numeric()
      elevation_difference <- numeric()
      distance <- numeric()
      starred <- logical()
      
      #Searching data in dataRaw and retrieving it in each column
      for(i in seq_along(dataRaw)){
            id[i] <- dataRaw[[i]]$id
            name[i] <- dataRaw[[i]]$name
            climb_category[i] <- dataRaw[[i]]$climb_category
            climb_category_desc[i] <- dataRaw[[i]]$climb_category_desc
            average_grade[i] <- dataRaw[[i]]$avg_grade
            elevation_difference[i] <- dataRaw[[i]]$elev_difference
            distance[i] <- dataRaw[[i]]$distance
            starred[i] <- dataRaw[[i]]$starred
      }
      
      name <- escaped_unicode(name)
      return (data.frame(id, name, climb_category, climb_category_desc, average_grade, elevation_difference, distance, starred))
}

more_segments <- function(SWlat, SWlong, NElat, NElong){
      
      segments <- tidy_explore(get_explore(stoken, paste(SWlat, SWlong, NElat, NElong, sep = ",")))
      
      coordinates <- list(list(SWlat, SWlong, NElat, round((SWlong+NElong)/2, digits=6)),
                          list(SWlat, round((SWlong+NElong)/2, digits=6), NElat, NElong),
                          list(SWlat, SWlong, round((SWlat+NElat)/2, digits=6), NElong),
                          list(round((SWlat+NElat)/2, digits=6), SWlong, NElat, NElong),
                          list(SWlat, SWlong, round((SWlat+NElat)/2, digits=6), round((SWlong+NElong)/2, digits=6)),
                          list(round((SWlat+NElat)/2, digits=6), SWlong, NElat, round((SWlong+NElong)/2, digits=6)),
                          list(SWlat, round((SWlong+NElong)/2, digits=6), round((SWlat+NElat)/2, digits=6), NElong),
                          list(round((SWlat+NElat)/2, digits=6), round((SWlong+NElong)/2, digits=6), NElat, NElong)
                          )
      
      for(i in seq_along(coordinates)){
            segments <- rbind(segments, tidy_explore(get_explore(stoken, paste(coordinates[[i]][[1]], 
                                                                               coordinates[[i]][[2]],
                                                                               coordinates[[i]][[3]],
                                                                               coordinates[[i]][[4]],
                                                                               sep = ","
                                                                               )
                                                                 )
                                                     )
                              )
      }
      
      return(unique(segments))

}

surrounding_segments <- function(lat, long, radius){
      
      difference <- radius/(60*1.853)
      SWlat <- lat - difference 
      SWlong <- long - difference
      NElat <- lat + difference
      NElong <- long + difference
      
      return(more_segments(SWlat, SWlong, NElat, NElong))
      
}