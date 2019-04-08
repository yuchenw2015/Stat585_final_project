library(tidyverse)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
#这里目前一共有7个function，其中有2个是读数据，另外5个是画图。
#还可以创造出更多的画图function
#可以run到258行（run所有到function定义），
#然后运行几个例子（从260行起）

#Function1:read the data of one location
current_weather <- function(id, type){
  assertthat::assert_that(is.character(id))
  assertthat::assert_that(stringr::str_length(id) == 4)
  a <- xml2::read_xml(paste("https://w1.weather.gov/xml/current_obs/", id, ".xml", sep = ""))
  b <- xml2::xml_children(a)
  name <- xml2::xml_name(b)
  text <- xml2::xml_text(b)
  n <- length(name)
  m <- length(type)
  data <- rep(NA, m)
  mark <- rep(0, m)
  if (m == 1){
    for (i in 1:n){
      if(name[i] == "location")
        location <- text[i]
      else if(name[i] == "station_id")
        station_id <- id
      else if(name[i] == "latitude")
        latitude <- as.numeric(text[i])
      else if(name[i] == "longitude")
        longitude  <- as.numeric(text[i])
      else if(name[i] == "observation_time")
        observation_time <- text[i]
      else if(name[i] == "weather")
        weather <- text[i]
      else if(name[i] == type){
        data <- text[i]
        mark <- 1
      }
        
    }
    obs <- data.frame(location, station_id, latitude, longitude, observation_time,
                      weather, data)
    colnames(obs)[7] <- type 
  }
  if (m > 1){
    for (i in 1:n){
      if(name[i] == "location")
        location <- text[i]
      else if(name[i] == "station_id")
        station_id <- id
      else if(name[i] == "latitude")
        latitude <- as.numeric(text[i])
      else if(name[i] == "longitude")
        longitude  <- as.numeric(text[i])
      else if(name[i] == "observation_time")
        observation_time <- text[i]
      else if(name[i] == "weather")
        weather <- text[i]
      else {
        for (k in 1:m){
          if(name[i] == type[k]){
            data[k] <- text[i]
            mark[k] <- 1
          }
        }
      }
    }
    obs <- data.frame(location, station_id, latitude, longitude, observation_time,
                      weather)  
    for (i in 1:m)
      obs <- cbind(obs, data[i])
    names(obs)[7:(6+m)] <- type
  }
  if(sum(mark) < m)
    print("Some of your types are not found! They are shown as NA!")
  assertthat::assert_that(is.data.frame(obs))
  obs
}

#Function2:read the data of several different locations
current_weather_more <- function(id_vector, type){
  n <- length(id_vector)
  obs <- NULL
  for (i in 1:n){
    obs <- rbind(obs, current_weather(id_vector[i], type))
  }
  assertthat::assert_that(is.data.frame(obs))
  obs
}


#Function 3: plot temp_f on a US map
plot_temp_f <- function(id_vector, label = T){
  data <- current_weather_more(id_vector, "temp_f")
  data$temp_f <- as.numeric(as.character(data$temp_f))
  states <- map_data("state")
  if (label == T){
    ggplot(data = states) + 
      geom_polygon(aes(x = long, y = lat, group = group),
                   color = "grey", alpha = 0.6) + 
      geom_point(data = data,aes(x = longitude, y = latitude,
                                 color = temp_f)) +
      geom_text(data = data, 
                aes(x = longitude, y = latitude, label = station_id),
                size = 2)+
      theme_bw() +
      labs(title = "Temperature in F", 
           subtitle = as.character(data$observation_time[1]))+
      coord_fixed(1.3) +
      guides(fill = FALSE)
  }else{
    ggplot(data = states) + 
      geom_polygon(aes(x = long, y = lat, group = group),
                   color = "grey", alpha = 0.6) + 
      geom_point(data = data,aes(x = longitude, y = latitude,
                                 color = temp_f)) +
      theme_bw() +
      labs(title = "Temperature in F", 
           subtitle = as.character(data$observation_time[1]))+
      coord_fixed(1.3) +
      guides(fill = FALSE)
  }
}

#Function 4: plot temp_c on a US map
plot_temp_c <- function(id_vector, label = T){
  data <- current_weather_more(id_vector, "temp_c")
  data$temp_c <- as.numeric(as.character(data$temp_c))
  states <- map_data("state")
  if (label == T){
    ggplot(data = states) + 
      geom_polygon(aes(x = long, y = lat, group = group),
                   color = "grey", alpha = 0.6) + 
      geom_point(data = data,aes(x = longitude, y = latitude,
                                 color = temp_c)) +
      geom_text(data = data, 
                aes(x = longitude, y = latitude, label = station_id),
                size = 2)+
      theme_bw() +
      labs(title = "Temperature in C", 
           subtitle = as.character(data$observation_time[1]))+
      coord_fixed(1.3) +
      guides(fill = FALSE)
  }else{
    ggplot(data = states) + 
      geom_polygon(aes(x = long, y = lat, group = group),
                   color = "grey", alpha = 0.6) + 
      geom_point(data = data,aes(x = longitude, y = latitude,
                                 color = temp_c)) +
      theme_bw() +
      labs(title = "Temperature in C", 
           subtitle = as.character(data$observation_time[1]))+
      coord_fixed(1.3) +
      guides(fill = FALSE)
  }
}


#Function 5: plot wind_mph
plot_wind_mph <- function(id_vector, label = T){
  data <- current_weather_more(id_vector, "wind_mph")
  data$wind_mph <- as.numeric(as.character(data$wind_mph))
  states <- map_data("state")
  if(label == T){
    ggplot(data = states) + 
      geom_polygon(aes(x = long, y = lat, group = group),
                   color = "grey", alpha = 0.6) + 
      geom_point(data = data,aes(x = longitude, y = latitude,
                                 color = wind_mph)) +
      geom_text(data = data, 
                aes(x = longitude, y = latitude, label = station_id),
                size = 2)+
      theme_bw() +
      labs(title = "Wind Speed in mph", 
           subtitle = as.character(data$observation_time[1]))+
      coord_fixed(1.3) +
      guides(fill = FALSE)
  }else{
    ggplot(data = states) + 
      geom_polygon(aes(x = long, y = lat, group = group),
                   color = "grey", alpha = 0.6) + 
      geom_point(data = data,aes(x = longitude, y = latitude,
                                 color = wind_mph)) +
      theme_bw() +
      labs(title = "Wind Speed in mph", 
           subtitle = as.character(data$observation_time[1]))+
      coord_fixed(1.3) +
      guides(fill = FALSE)
  }
  
}


#Function 6:plot wind_kt
plot_wind_kt <- function(id_vector, label = T){
  data <- current_weather_more(id_vector, "wind_kt")
  data$wind_kt <- as.numeric(as.character(data$wind_kt))
  states <- map_data("state")
  if(label == T){
    ggplot(data = states) + 
      geom_polygon(aes(x = long, y = lat, group = group),
                   color = "grey", alpha = 0.6) + 
      geom_point(data = data,aes(x = longitude, y = latitude,
                                 color = wind_kt)) +
      geom_text(data = data, 
                aes(x = longitude, y = latitude, label = station_id),
                size = 2)+
      theme_bw() +
      labs(title = "Wind Speed in kt", 
           subtitle = as.character(data$observation_time[1]))+
      coord_fixed(1.3) +
      guides(fill = FALSE)
  }else{
    ggplot(data = states) + 
      geom_polygon(aes(x = long, y = lat, group = group),
                   color = "grey", alpha = 0.6) + 
      geom_point(data = data,aes(x = longitude, y = latitude,
                                 color = wind_kt)) +
      theme_bw() +
      labs(title = "Wind Speed in kt", 
           subtitle = as.character(data$observation_time[1]))+
      coord_fixed(1.3) +
      guides(fill = FALSE)
  }
}

#Function 7:plot wind_kt
plot_wind_dir <- function(id_vector, label = T){
  data <- current_weather_more(id_vector, "wind_dir")
  states <- map_data("state")
  if(label == T){
    ggplot(data = states) + 
      geom_polygon(aes(x = long, y = lat, group = group),
                   color = "grey", alpha = 0.6) + 
      geom_point(data = data,aes(x = longitude, y = latitude,
                                 color = wind_dir)) +
      geom_text(data = data, 
                aes(x = longitude, y = latitude, label = station_id),
                size = 2)+
      theme_bw() +
      labs(title = "Wind direction", 
           subtitle = as.character(data$observation_time[1]))+
      coord_fixed(1.3) +
      guides(fill = FALSE)
  }else{
    ggplot(data = states) + 
      geom_polygon(aes(x = long, y = lat, group = group),
                   color = "grey", alpha = 0.6) + 
      geom_point(data = data,aes(x = longitude, y = latitude,
                                 color = wind_dir)) +
      theme_bw() +
      labs(title = "Wind direction", 
           subtitle = as.character(data$observation_time[1]))+
      coord_fixed(1.3) +
      guides(fill = FALSE)
  }
}

#example for reading data
current_weather("KAMW", c("wind_mph", "temp_f", "haha"))
current_weather("KAMW", "temp_c")
current_weather_more(c("KAMW", "KAIO", "KCID", "KCNC"), c("temp_f"))

#example for plots
plot_temp_f(c("KAMW", "KAIO", "KCID", "KCNC"))
plot_temp_c(c("KAMW", "KAIO", "KCID", "KCNC"), label = F)
plot_wind_mph(c("KAMW", "KAIO", "KCID", "KCNC"))
plot_wind_kt(c("KAMW", "KAIO", "KCID", "KCNC"), label = F)
plot_wind_dir(c("KAMW", "KAIO", "KCID", "KCNC"), label = F)
