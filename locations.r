library(ggmap)
library(dplyr)
library(tidyr)
library(plotly)

register_google(key = "AIzaSyDYKt-K7UfX2CgBt4amzU4WdvaHNz0DZ-E", write = TRUE)
collegeNames <- c('University of Alabama at Tuscaloosa', 
                  'University of Arkansas at Fayetteville', 'Auburn University', 'University of Florida','University of Georgia', 'University of Kentucky', 'Louisiana State University at Baton Rouge', 'Mississippi State University', 'University of Missouri at Columbia', 'University of Mississippi', 'University of South Carolina at Columbia', 'Texas A&M University at College Station', 'University of Tennessee at Knoxville', 'Vanderbilt University')
layerIDs <- c('Alabama', 'Arkansas', 'Auburn', 'Florida', 'Georgia', 'Kentucky', 'LSU', 'Mississippi', 'Missouri', 'OleMiss', 'USC', 'TAMU', 'Tennessee', 'Vanderbilt')
geocoded <- data.frame(name = character(), lon = numeric(), lat=numeric(), layerID = character())

counter = 1
for(i in 1:length(collegeNames))
{
  result <- geocode(collegeNames[i], output="latlona", source="google")
  list <- c(collegeNames[i], as.numeric(result[1]), as.numeric(result[2]), layerIDs[i])
  geocoded[i, ] <- list
  
}

geocoded$lon <- as.numeric(geocoded$lon)
geocoded$lat <- as.numeric(geocoded$lat)

