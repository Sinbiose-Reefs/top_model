
# function  to save (only) the text

save_text <- function (url) {
  # Create URL handle
  h<-handle(url)
  
  # Get URL
  res<-GET(handle=h)
  
  #parse the HTML
  text2 <- content(res,as = "text")
  
  #return
  return (text2)
}

# ----------------------------------------------- #

# function to extract latlong from these text (not decimal degree format)

convertLongLat<-function(text2){
  
  # Define Patterns of search
  # East/West
  # North/South
  exprTot<-"[^0-9]([0-9]{1,3})[°|º][[:space:]]{0,1}([0-9]{0,2}[.|,]{0,1}[0-9]{0,4})[^.|,]{0,1}[[:space:]]{0,1}([0-9]{0,2}[.|,]{0,1}[0-9]{0,4}).{1,2}([N|S|n|s|W|E|L|l|w|e|O|o])[^0-9]{0,5}[^0-9]([0-9]{1,3})[°|º][[:space:]]{0,1}([0-9]{0,2}[.|,]{0,1}[0-9]{0,4})[^.|,]{0,1}[[:space:]]{0,1}([0-9]{0,2}[.|,]{0,1}[0-9]{0,4}).{1,2}([N|S|n|s|W|E|L|l|w|e|O|o])[^0-9|a-z|A-Z]"
  
  # Extract matching patterns
  coordsTot<- str_match_all(text2,exprTot)[[1]];coordsTot #str_match_all transform into list; extract the first element = [[1]]
  
  # extract is sensitive to coordinate order
  new_coordsTot <- lapply (seq (1,nrow (coordsTot)), function (k){
      # EW
      long_pos <- c("W","E","L","l","w","e","O","o")
      lat_pos <- c("N","S","n","s")
      seq_to_extractEW<- seq(which(coordsTot [k,] %in% long_pos)-3,which(coordsTot [k,] %in% long_pos))
      coordsEW <- coordsTot [k,c(1,seq_to_extractEW)]
      #NS
      seq_to_extractNS<- seq(which(coordsTot [k,] %in% lat_pos)-3,which(coordsTot [k,] %in% lat_pos))
      coordsNS <- coordsTot [k,c(seq_to_extractNS)]
      # bind into a matrix
      coordsTot <- matrix (c(coordsEW,coordsNS),ncol=length(c(coordsEW,coordsNS)))
      # return
      ;
      coordsTot
      
      }
  
    )
  
  # melt this list of rows (ie number of sites of each study)
  new_coordsTot <- do.call(rbind, new_coordsTot)
  
  # select cols
  coordsNS<-new_coordsTot[,c(1,6:9)]
  coordsEW<-new_coordsTot[,c(1,2:5)]
  
  # for cases with only one coordinate (one row is treated as character)
  # coordsEW
  if(class(coordsEW)=="character") {
    
     coordsEW <- matrix(coordsEW,ncol =length(coordsEW)) 
    
  } else {
   coordsEW
  }
  
  # coordsNS
  if(class(coordsNS)=="character") {
    
    coordsNS <- matrix(coordsNS,ncol =length(coordsNS)) 
    
  } else {
    coordsNS
  }
  
  # Convert in degrees
  
  # Get hemisphere
  north<-c("N","n")
  west <- c("W","w","O","o")
  multiplierEW<-ifelse(coordsEW[,5] %in% west,-1,1)
  multiplierNS<-ifelse(coordsNS[,5] %in% north,1,-1)
  
  # Replace comma by dot
  coordsEW_eng<-matrix(gsub(",",".",coordsEW[,-c(1,5)]),nrow(coordsEW))
  coordsNS_eng<-matrix(gsub(",",".",coordsNS[,-c(1,5)]),nrow(coordsNS))
  
  # Convert to numeric
  coordsEW_num<-apply(coordsEW_eng,1,as.numeric)
  coordsNS_num<-apply(coordsNS_eng,1,as.numeric)
  
  # Convert to degrees
  coordsEW_deg<-t(coordsEW_num*c(1,1/60,1/3600))
  coordsNS_deg<-t(coordsNS_num*c(1,1/60,1/3600))
  
  # Merge
  Long<-rowSums(coordsEW_deg,na.rm = TRUE)*multiplierEW
  Lat<-rowSums(coordsNS_deg,na.rm = TRUE)*multiplierNS
  
  coords<-cbind(Long,Lat)
  coords<-as.data.frame (coords)
  # cleaning duplicated and misplaced coordinates
  ## workaround with the need of species
  coords <- cbind(coords, species= 1)
  # round these coordinates
  coords$Lat <- round (coords$Lat,3)
  coords$Long <- round (coords$Long,3)
  
  # only run the cleaning if nrow (coord) > 1 [coordinates are treated as duplicated (and T in all tests) when nrow=1]
  if (nrow(coords) >1) {
    
    #flag data to remove
    flags_spatial <- CoordinateCleaner::clean_coordinates(
      x = (coords), 
      species = "species",
      lon = "Long", 
      lat = "Lat",
      tests = c("capitals", # radius around capitals
                "centroids", # radius around country and province centroids
                "duplicates", # records from one species with identical coordinates
                "equal", # equal coordinates
                #"equador", ## points at  lat 0
                "institutions", # radius around biodiversity institutions
                "urban", # within urban area
                "validity", # outside reference coordinate system
                "zeros" # plain zeros and lat = lon
                
      )
    )
    
    ## results
    flags_spatial %>% head
    summary(flags_spatial)
    flags_spatial$.summary
    #
    ## exclude records flagged by any test
    coords <- coords %>% 
      
      dplyr::filter(flags_spatial$.summary)
    
  } else {
    
    coords
    
  }
  
  # return list with the original coordinate, and extracted coordinates 
  coords_result <- list (original_coord_EW = coordsEW,
                         original_coord_NS = coordsNS,
                         extracted_coords = coords[,-which(colnames(coords)== "species")])
  return(coords_result)
}

# --------------------------------------------- #
# find lat long in decimal degrees

# function to extract latlong from these text

findtLongLat<-function(text2){
  
  # Define Patterns of search
  # East/West
  # North/South
  # East/West
  exprEW<-"[^0-9]([0-9]{1,2})[.]([0-9]{1,6}).[W|E][^0-9]"
  
  # North/South
  exprNS<-"[^0-9]([0-9]{1,2})[.]([0-9]{1,6}).[S|N][^0-9]"
  
  # match W
  coordsEW<-str_match_all(text2,exprEW)[[1]]
  coordsEW <- cbind (coordsEW,paste (coordsEW[,2],coordsEW[,3], sep="."))
  coordsEW <- cbind (coordsEW,ifelse (grep ("W",coordsEW)>=1, # bind position data
                                      "W",
                                      "E"))
  # match S
  coordsNS<-str_match_all(text2,exprNS)[[1]]
  coordsNS <- cbind (coordsNS,paste (coordsNS[,2],coordsNS[,3], sep="."))
  coordsNS <- cbind (coordsNS,ifelse (grepl ("S",coordsNS,fixed=T),  # bind position data
                                      "S",
                                      "N"))
  
  # for cases with only one coordinate (one row is treated as character)
  # coordsEW
  if(class(coordsEW)=="character") {
    
    coordsEW <- matrix(coordsEW,ncol =length(coordsEW)) 
    
  } else {
    coordsEW
  }
  
  # coordsNS
  if(class(coordsNS)=="character") {
    
    coordsNS <- matrix(coordsNS,ncol =length(coordsNS)) 
    
  } else {
    coordsNS
  }
  
  # Convert in degrees
  
  # Get hemisphere
  north<-c("N","n")
  west <- c("W","w","O","o")
  multiplierEW<-ifelse(coordsEW[,5] %in% west,-1,1)
  multiplierNS<-ifelse(coordsNS[,5] %in% north,1,-1)
  
  # multiply
  Long<-as.numeric(coordsEW[,4])*multiplierEW
  Lat<-as.numeric(coordsNS[,4])*multiplierNS
  
  coords<-cbind(Long,Lat)
  coords<-as.data.frame (coords)
  # cleaning duplicated and misplaced coordinates
  ## workaround with the need of species
  coords <- cbind(coords, species= 1)
  # round these coordinates
  coords$Lat <- round (coords$Lat,3)
  coords$Long <- round (coords$Long,3)
  
  # only run the cleaning if nrow (coord) > 1 [coordinates are treated as duplicated (and T in all tests) when nrow=1]
  if (nrow(coords) >1) {
    
    #flag data to remove
    flags_spatial <- CoordinateCleaner::clean_coordinates(
      x = (coords), 
      species = "species",
      lon = "Long", 
      lat = "Lat",
      tests = c("capitals", # radius around capitals
                "centroids", # radius around country and province centroids
                "duplicates", # records from one species with identical coordinates
                "equal", # equal coordinates
                #"equador", ## points at  lat 0
                "institutions", # radius around biodiversity institutions
                "urban", # within urban area
                "validity", # outside reference coordinate system
                "zeros" # plain zeros and lat = lon
                
      )
    )
    
    ## results
    flags_spatial %>% head
    summary(flags_spatial)
    flags_spatial$.summary
    #
    ## exclude records flagged by any test
    coords <- coords %>% 
      
      dplyr::filter(flags_spatial$.summary)
    
  } else {
    
    coords
    
  }
  
  # return list with the original coordinate, and extracted coordinates 
  coords_result <- list (original_coord_EW = coordsEW,
                         original_coord_NS = coordsNS,
                         extracted_coords = coords[,-which(colnames(coords)== "species")])
  
  return(coords_result)
}

