####################################################################################################
#Script Name: NDST Bridge Log processor
#Script Function: This scrips concatenates logs files from NDST standalone computer (Kinuput brand), which logs GSP data sentences
#               from the ship's GPS, independtly of NDST other systems. The NMEA-1083 strings are logged using an instance of Advanced Serial Data Logger
#               All log files merged, and are searched for $GPGGA, $GPZDA and $GPRMC strings, and the
#               relevant data from each of these strings are extracted, and exported as a .CSV file and a .SHP file, for use in 
#               mapping software.
#Script Author: Ben Snow
#Script Initial Date: May 27, 2025
#R Version: 4.4.3
####################################################################################################

require(lubridate)
require(readr)
require(dplyr)
require(stringr)
require(imputeTS)
require(measurements)
require(stringi)
require(data.table)
require(geosphere)
require(sp) #To generate SpatialLines
require(sf) 

#Set cruise ID, ship name

Cruise_ID = 'PAC2025-037'
ship_name = "CCGS_Vector"

#Set working dir.

log_dir <- "E:/Logs"
setwd(log_dir)

#Create empty data.frame with 15 columns, to fill in the loop below. This ensures that any log files that start with a row
#that has LESS than 15 columns (files starting with $GPHDT or $GPZDA in the first row) will still have 15 columns with empty records for that row.

GPS_list <- list()
Ship_GPS_all <- data.frame()


#List files and read into one larger file. This is a comma seperated record, but some files have more columns than others, so need to
#read in files by skipping the first line of each file to start. THIS SECTION WILL TAKE A LONG TIME TO PROCESS!

GPS_files <- list.files(log_dir)

for(i in 1:length(GPS_files))
{
  input_file <- fread(GPS_files[i], blank.lines.skip = TRUE, skip = 1, data.table = FALSE, select = c(1:5), fill = Inf, header = FALSE, colClasses = "character")
  GPS_list[[i]] <- input_file
  Ship_GPS_all <- do.call(rbind, GPS_list)
} 

#Filter to only the NMEA-1083 position and time strings

Ship_GPS_filtered <- filter(Ship_GPS_all, V1 == "$GPGGA" | V1 == "$GPRMC" | V1 == "$GPZDA")

#Remove all rows in the date/time column ($V2) that are not valid UTF-8 strings

Ship_GPS_filtered <- slice(Ship_GPS_filtered, which(stri_enc_isutf8(Ship_GPS_all$V2)))

#Remove any rows where the <CR><LF> may have not separated the NMEA strings properly.

Ship_GPS_filtered <- filter(Ship_GPS_filtered, !grepl("GP", V2))

#Located date stamp values and time stamp values in the $GPZDA strings. Parse the date_time. Ignore warnings.

Ship_GPS_filtered$date <- dmy(paste(Ship_GPS_filtered$V3,Ship_GPS_filtered$V4, Ship_GPS_filtered$V5, sep = "-"))
Ship_GPS_filtered$time <- str_extract(Ship_GPS_filtered$V2, "\\d{6}")
Ship_GPS_filtered$date_time <- ymd_hms(paste(Ship_GPS_filtered$date, Ship_GPS_filtered$time, sep = " "))

#Impute the time series, before filtering out any values. Set it as an integer before putting back in to original DF, to get rid of
#milliseconds.

full <- na_interpolation(as.numeric(Ship_GPS_filtered$date_time))
full <- as.integer(full)
Ship_GPS_filtered$date_time <- as.POSIXct(full, origin = "1970-01-01", tz = "UTC") #Standard R origin value

#Extract the degrees, minutes and seconds information. Combine to a single value with a space in-between.

GPS_position <- filter(Ship_GPS_filtered, V1 == "$GPGGA")
GPS_position$Lat_deg <- str_extract(GPS_position$V3, "\\d{2}")
GPS_position$Lat_min <- str_extract(GPS_position$V3, "\\d{2}\\.\\d{5,}")
GPS_position$Long_deg <- str_extract(GPS_position$V5, "\\d{3}")
GPS_position$Long_min <- str_extract(GPS_position$V5, "\\d{2}\\.\\d{5,}")
GPS_position$Lat <- paste(GPS_position$Lat_deg, GPS_position$Lat_min, sep = " ")
GPS_position$Long <- paste(GPS_position$Long_deg, GPS_position$Long_min, sep = " ")

#Remove any items that are missing Lat/Long mins from the data set.

GPS_position <- filter(GPS_position, !is.na(Lat_min))
GPS_position <- filter(GPS_position, !is.na(Long_min))

#Convert to decimal degrees

GPS_position$Lat_conv <- conv_unit(GPS_position$Lat, "deg_dec_min", "dec_deg")
GPS_position$Long_conv <- conv_unit(GPS_position$Long, "deg_dec_min", "dec_deg")

#Drop unused columns. Keep date_time, Long_conv and Lat_conv (in that order)

GPS_position <- GPS_position[,c(8,16,15)]

#Write to a .CSV.

write.csv(GPS_position, paste(log_dir,"Ship_Bridge_Log.csv", sep = "/"), quote = F, row.names = F)

#Convert the GPS_position Date_Time column to a character column, in order to properly append it to the .SHP file.

GPS_position$date_time <- as.character(GPS_position$date_time)

#Set all Longitude values to negatives, to signify western hemisphere.

GPS_position$Lat_conv <- as.numeric(GPS_position$Lat_conv)
GPS_position$Long_conv <- as.numeric(GPS_position$Long_conv)
GPS_position$Long_conv <- -1*abs(GPS_position$Long_conv)

#Convert the GPS_position data.frame to a simple features object, necessary precursor to writing .SHP file.
GPS_position_sf <- st_as_sf(GPS_position, coords = c("Long_conv", "Lat_conv"),crs = "+proj=longlat +datum=WGS84")

#Write the sf object to a .SHP file.

st_write(GPS_position_sf, "Ship_Track.shp")


