#modificar calc_segment_energy_Data para que use la info obtenida con mergeData

#Get the names and gender of a the athletes of a leaderboard according to the id
get_athletes_basic_data <- function(dataLeaders){
      Id <- numeric(length(dataLeaders[[3]]))
      Name <- character(length(dataLeaders[[3]]))
      Gender <- c(Name)
      for(i in seq_along(dataLeaders[[3]])){
            Id[i] <- dataLeaders[[3]][[i]]$athlete_id
            Name[i] <- dataLeaders[[3]][[i]]$athlete_name
            if(!is.null(dataLeaders[[3]][[i]]$athlete_gender)){
                  Gender[i] <- dataLeaders[[3]][[i]]$athlete_gender
            }
            else{
                  Gender[i] <- "I" 
            }
      }
      return (data.frame(Id,Name,Gender))#list()
}

#Merge the segement data with the basic data of athletes
merge_Data <- function(dataTidy, dataLeaders){
      athletes_basic_data <- get_athletes_basic_data(dataLeaders)
      mergedData <- merge(athletes_basic_data, dataTidy, all=TRUE)
      return (mergedData[order(mergedData$Date),])
}

#Subset data by gender
gender_Data <- function(mergedData, Gender="M"){#agregar capacidad para analizar stravers
      
      if(Gender == "F"){
            Data <- mergedData[mergedData$Gender == "F",]
      }
      else{#It supposes that the undefined gender (I) subjects are males as the proportion of males is way bigger than females
            Data <- mergedData[mergedData$Gender != "F",]
      }
      return(Data)
}

#Computes average values for the energy records from the data of the efforts
average_energy <- function(Data){
      average_times <- sapply(split(Data$Elapsed_Time, Data$Id),mean)
      mean_time <- mean(average_times)
      
      average_powers <- sapply(split(Data$Power, Data$Id),mean, na.rm = TRUE)
      mean_power <- mean(average_powers, na.rm=TRUE)
      
      average_velocities <- sapply(split(Data$Velocity, Data$Id),mean)
      mean_velocity <- mean(average_velocities)
      
      average_Cal <- average_powers*average_times*0.000239005736 
      mean_Cal <- mean(average_Cal, na.rm=TRUE)
      
      return(data.frame(mean_time, mean_velocity, mean_power, mean_Cal))
}
      
#Creates a tidy data set from the data downloaded of the segment efforts
raw_to_tidy_Efforts <- function(dataRaw){
      #Colums initiation
      Date <- character()
      Id <- numeric()
      Elapsed_Time <-integer()
      Power <- numeric()
      Velocity <- numeric()
      Distance <- dataRaw[[1]]$segment$distance
      
      #Searching data in dataRaw and retrieving it in each column
      for(i in seq_along(dataRaw)){
            Date[i] <- dataRaw[[i]]$start_date_local
            Id[i] <- dataRaw[[i]]$athlete$id
            Elapsed_Time[i] <- dataRaw[[i]]$elapsed_time
            #Checking for null values in the power data for avoiding errors
            if(!is.null(dataRaw[[i]]$average_watts) && dataRaw[[i]]$average_watts > 0){
                  Power[i] <- dataRaw[[i]]$average_watts                  
            }
            else{
                  Power[i] <- NA
            }
            Velocity[i] <- Distance/Elapsed_Time[i]*3.6
      }
      return (data.frame(Date, Id, Elapsed_Time, Power, Velocity))
}


#Computes energy data for the segment efforts
calc_segment_energy_data <-function(dataTidy, dataLeaders){
      
      athletes_basic_data <- get_athletes_basic_data(dataLeaders)
      mergedData <- merge_Data(dataTidy, dataLeaders)
      
      count_All <- length(unique(mergedData$Id))
      count_females <- length(unique(mergedData$Id[mergedData$Gender=="F"]))
      count_males <- count_all - count_females
      
      Males <- average_energy(gender_Data(mergedData, "M"))
      Females <- average_energy(gender_Data(mergedData, "F"))
            
      mean_time_All <- mean(c(Males$mean_time,Females$mean_time))
      mean_power_All <- mean(c(Males$mean_power,Females$mean_power))
      mean_velocity_All <- mean(c(Males$mean_velocity,Females$mean_velocity))
      mean_Cal_All <- mean_time_All*mean_power_All*0.000239005736
      
      All <- data.frame(mean_time=mean_time_All,
                        mean_velocity=mean_velocity_All,
                        mean_power=mean_power_All,
                        mean_Cal=mean_Cal_All)

      mean_time_Stravers <- (Males$mean_time*count_males + Females$mean_time*count_females)/count_All
      mean_power_Stravers <- (Males$mean_power*count_males + Females$mean_power*count_females)/count_All#no es exacto este resultado ya que el power_count es distinto al count_all
      mean_velocity_Stravers <- (Males$mean_velocity*count_males + Females$mean_velocity*count_females)/count_All
      mean_Cal_Stravers <- mean_time_Stravers*mean_power_Stravers*0.000239005736#no es exacto ya que mean_power_Stravers no está bien calculado, pero es una buena aproximación
      
      Stravers <- data.frame(mean_time=mean_time_Stravers,
                             mean_velocity=mean_velocity_Stravers,
                             mean_power=mean_power_Stravers,
                             mean_Cal=mean_Cal_Stravers)
      
      cbind(Grupo= c("UsuariosStrava", "Hombres", "Mujeres", "Estimado"),
            rbind(Stravers, Males, Females, All),
            Count=rbind(count_All, count_males, count_females, NA))  
}

#Create data frame of how the KOM times were lowered over time
KOMs_history <- function(dataRaw, dataLeaders, KOM_or_QOM="BOTH"){
      
      athletes_basic_data <- get_athletes_basic_data(dataLeaders)
      females <- logical()
      better <- logical()
      Gender <- character()
      Name <- character()
      Date <- character()
      Time <- numeric()
      best_times <- c(Time)
      best_athletes <- c(Name)
      best_dates <- c(Date)
      distance <- dataRaw[[1]]$segment$distance
      Velocity <- numeric()
      
      for(i in seq_along(dataRaw)){ #Ciclo para los efforts
            j <- 0
            repeat{                  
                  j <- j+1
                  if(dataRaw[[i]]$athlete$id == athletes_basic_data$Id[j]){ #Verifica si el effort corresponde al usuario
                        Gender[i] <- as.character(athletes_basic_data$Gender[j])
                        Name[i] <- as.character(athletes_basic_data$Name[j])
                        Time[i] <- dataRaw[[i]]$elapsed_time
                        Date[i] <- dataRaw[[i]]$start_date_local
                        break
                  }
                  if(j==length(athletes_basic_data$id)){
                        Gender[i] <- "I"
                        Name[i] <- paste("Id:", dataRaw[[i]]$athlete$id)
                        Time[i] <- dataRaw[[i]]$elapsed_time
                        Date[i] <- dataRaw[[i]]$start_date_local
                        break
                  }
            }
            females[i] <- Gender[i] == "F"
      }
      #agregar aquí discriminación por sexo
      if(KOM_or_QOM == "QOM"){
            Name <- Name[females]
            Time <- Time[females]
            Date <- Date[females]
      }
       else if(KOM_or_QOM == "KOM"){
             Name <- Name[!females]
             Time <- Time[!females]
             Date <- Date[!females]
       }
      
      best_times[1] <- Time[1]
      best_athletes[1] <- Name[1]
      best_dates[1]<- Date[1]
      Velocity[1] <- distance/best_times[1]*3.6
      
      i <- 1
      for(j in 2:length(Time)){
            if(Time[j] <= best_times[i]){
                  i <- i+1
                  best_times[i] <- Time[j]
                  best_athletes[i] <- Name[j]
                  best_dates[i] <- Date[j]
                  Velocity[i] <- distance/best_times[i]*3.6
            }
      }
      data.frame(best_dates, best_athletes, best_times, Velocity)
}

plot_segment_efforts <- function(stoken, id, athlete_id){
      
      dataLeaders <- get_leaderboard(stoken, id, All=TRUE) #sacar de la función
      dataRaw <- get_efforts_list(stoken, id) #sacar de la función
      dataTidy <- raw_to_tidy_Efforts(dataRaw) 
      dataMerged <- merge_Data(dataTidy, dataLeaders)
      dataAthlete <- dataMerged[dataMerged$Id==athlete_id,]
      plot(dataAthlete$Date, dataAthlete$Elapsed_Time)
      
}

#Creates a special tidy data set for the shiny SegmentApp
tidy_SegmentEfforts <- function(stoken, id){
      dataLeaders <- get_leaderboard(stoken=stoken, id=id, All=TRUE)
      dataTidy <- merge_Data(raw_to_tidy_Efforts(get_efforts_list(stoken, id=id)), #create tidy set with all efforts
                             dataLeaders
      )
      dataTidy <- dataTidy[!is.na(dataTidy$Gender),]#Erase records for erased accounts
      
      tKOM <- dataLeaders$entries[[1]]$elapsed_time#Get KOM time for checking times
      dataTidy <- dataTidy[dataTidy$Elapsed_Time>=tKOM,]#Erase invalidated times (faster than KOM)
      return(dataTidy)
}
      
      
#segment_energy_data <- calc_segment_energy_data(dataRaw, dataLeaders)