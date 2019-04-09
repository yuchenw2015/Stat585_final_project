library(tidyverse)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
#这里目前一共有3个function，其中有2个是读数据，另外1个是画图。



#Function1:read the data of one location
#id 是机场代码，type是天气要素
#此函数可以读取一个机场的若干天气要素
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
  if (m==0){
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
      else if(name[i] == "weather"){
        weather <- text[i]
        mark <- 1
      }
      
    }
    obs <- data.frame(location, station_id, latitude, longitude, observation_time,
                      weather)
  }
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
      else if(name[i] == type){
        data <- text[i]
        mark <- 1
      }
      
    }
    obs <- data.frame(location, station_id, latitude, longitude, observation_time,
                      data)
    colnames(obs)[6] <- type 
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
      else {
        for (k in 1:m){
          if(name[i] == type[k]){
            data[k] <- text[i]
            mark[k] <- 1
          }
        }
      }
    }
    obs <- data.frame(location, station_id, latitude, longitude, observation_time)  
    for (i in 1:m)
      obs <- cbind(obs, data[i])
    names(obs)[6:(6+m-1)] <- type
  }
  if(sum(mark) < m)
    print("Some of your types are not found! They are shown as NA!")
  assertthat::assert_that(is.data.frame(obs))
  obs
}

#Function2:read the data of several different locations
#读取多个机场的多个天气要素
current_weather_more <- function(id_vector, type){
  n <- length(id_vector)
  obs <- NULL
  for (i in 1:n){
    obs <- rbind(obs, current_weather(id_vector[i], type))
  }
  assertthat::assert_that(is.data.frame(obs))
  obs
}

#Function3: plot an element
#label 是否标出机场code
#number 此天气要素是否是numeric
#此函数可以画出多个机场的同一个天气要素
plot_weather <- function(id_vector, type, label = T, number = F){
  data <- current_weather_more(id_vector, type)
  colnames(data)[6]<-"element"
  if (number == T){
    data$element <- as.numeric(as.character(data$element))
  }
  states <- map_data("state")
  if(label == T){
    ggplot(data = states) + 
      geom_polygon(aes(x = long, y = lat, group = group),
                   color = "grey", alpha = 0.6) + 
      geom_point(data = data,aes(x = longitude, y = latitude,
                                 color = element)) +
      geom_text(data = data, 
                aes(x = longitude, y = latitude, label = station_id),
                size = 1.5,
                vjust = -1)+
      theme_bw() +
      labs(title = type, 
           subtitle = as.character(data$observation_time[1]))+
      coord_fixed(1.3) +
      guides(fill = FALSE)
  }else{
    ggplot(data = states) + 
      geom_polygon(aes(x = long, y = lat, group = group),
                   color = "grey", alpha = 0.6) + 
      geom_point(data = data,aes(x = longitude, y = latitude,
                                 color = element)) +
      theme_bw() +
      labs(title = type, 
           subtitle = as.character(data$observation_time[1]))+
      coord_fixed(1.3) +
      guides(fill = FALSE)
  }
}

###plot for shiny app
plot_map <- function(id_vector, label = T){
  data <- current_weather_more(id_vector, type = NULL)
  states <- map_data("state")
  if(label == T){
    ggplot(data = states) + 
      geom_polygon(aes(x = long, y = lat, group = group),
                   color = "grey", alpha = 0.6) + 
      geom_point(data = data,aes(x = longitude, y = latitude),color="red") +
      geom_text(data = data, 
                aes(x = longitude, y = latitude, label = station_id),
                vjust=-1,
                size = 3)+
      theme_bw() 
  }else{
    ggplot(data = states) + 
      geom_polygon(aes(x = long, y = lat, group = group),
                   color = "grey", alpha = 0.6) + 
      geom_point(data = data,aes(x = longitude, y = latitude),color="red") +
      theme_bw() 
  }
}



#example for reading data
current_weather("KAMW", c("wind_mph", "temp_f", "haha"))
current_weather("KAMW", "temp_c")
current_weather_more(c("KAMW", "KAIO", "KCID", "KCNC"), c("temp_f"))

#example for plots
plot_weather(c("KAMW", "KAIO", "KCID", "KCNC"), 
             type = "temp_c", 
             label = T, number = T)
