#Extraer de cada actividad:
#segment_efforts[[i]]$segment$id
#segment_efforts[[i]]$segment$name
#segment_efforts[[i]]$segment$distance
#segment_efforts[[i]]$segment$city
segments_ids <- numeric()
k <- 0
for(i in seq_along(dataActivities)){
      for(j in seq_along(dataActivities[[i]]$segment_efforts)){
            k <- k+1
            segments_ids[k] <- dataActivities[[i]]$segment_efforts[[j]]$segment$id
      }   
}
segments_ids <- unique(segments_ids)
print(paste("Esfuerzos de segmentos conseguidos:", k, sep= " "))
print(paste("Nro. de segmentos diferentes:", length(segments_ids), sep=" "))
segment_effort_count <- numeric(length(segments_ids))
segment_names <- character(length(segments_ids))
segment_distances <- numeric(length(segments_ids))
#segment_cities <- character(length(segments_ids))

for(i in seq_along(dataActivities)){
      for(j in seq_along(dataActivities[[i]]$segment_efforts)){
            k <- 0
            repeat{
                  k <- k+1
                  if(dataActivities[[i]]$segment_efforts[[j]]$segment$id == segments_ids[k]){
                        segment_effort_count[k] <- segment_effort_count[k]+1
                        if(segment_effort_count[k]==1){
                              segment_names[k] <- dataActivities[[i]]$segment_efforts[[j]]$segment$name
                              segment_distances[k] <- dataActivities[[i]]$segment_efforts[[j]]$segment$distance
                              #segment_cities[k] <- dataActivities[[i]]$segment_efforts[[j]]$segment$city#resolver problema con city = NULL
                        }
                        break
                  }
            }
      }   
}

dataSegment <- data.frame(segments_ids, segment_names, segment_distances, segment_effort_count)

get_data_tidy_segments <- function(dataSegment){
      i<-0
      n <- length(dataSegment$segments_ids)
      for(id in dataSegment$segments_ids){
            i<-i+1
            dataLeaders <- get_leaderboard(stoken, id, All=TRUE)
            dataTidy <- merge_Data(raw_to_tidy_Efforts(get_efforts_list(stoken, id)),
                                   dataLeaders)
            dataTidy <- dataTidy[!is.na(dataTidy$Gender),]
            
            tKOM <- dataLeaders$entries[[1]]$elapsed_time
            dataTidy <- dataTidy[dataTidy$Elapsed_Time>=tKOM,]
            dput(dataTidy, file=paste(as.character(id),".R",sep=""))
            print(paste("Saved segment", i, "of",n, sep= " "))
      }
}