library(ggmap)
library(dplyr)
library(tidyr)
library(plotly)

collegeNames <- c('University of Alabama at Tuscaloosa', 
                  'University of Arkansas at Fayetteville', 'Auburn University', 'University of Florida','University of Georgia', 'University of Kentucky', 'Louisiana State University at Baton Rouge', 'Mississippi State University', 'University of Missouri at Columbia', 'University of Mississippi', 'University of South Carolina at Columbia', 'Texas A&M University at College Station', 'University of Tennessee at Knoxville', 'Vanderbilt University')

geocoded <- data.frame(name = character(), lon = numeric(), lat=numeric())

counter = 1
for(i in 1:length(collegeNames))
{
  result <- geocode(collegeNames[i], output="latlona", source="google")
  list <- c(collegeNames[i], as.numeric(result[1]), as.numeric(result[2]))
  geocoded[i, ] <- list
  
}

geocoded$lon <- as.numeric(geocoded$lon)
geocoded$lat <- as.numeric(geocoded$lat)
