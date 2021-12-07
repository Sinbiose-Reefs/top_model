# load (or install) needed packages
source ("R/packages.R")

## extract coordinates in batch
# Load functions 
source("R/retLongLatFunction.R")

# load data with dois
csv_with_doi <- read.csv(here ("data", "papers_doi.csv"),sep=",")

# load data with DOI for is.na
csv_doi_NA <- read.csv(here ("data", "get_missing_dois.csv"),sep=";")
# replaced missing dois by the ones found by ALLuza
csv_with_doi[csv_doi_NA$X,"DI"] <- csv_doi_NA$DI

# rm papers lacking DOI
doi_to_get <- csv_with_doi$DI[is.na(csv_with_doi$DI) !=T]

# Convert to URLs (can be provided as a local html page instead of DOI, such as "Folder/Subfolder/index.html")
doisurl_dxdoi <- paste0("http://dx.doi.org/",gsub("http://dx.doi.org/","",doi_to_get))

## extract text to save
### you need to be connected to the internet of your institution (that pays for paper's access)
# text_to_save <- lapply(doisurl_dxdoi, function (i) 
  
#  tryCatch ( # try overcome errors
#    save_text (i),
#    error = function (e)
#      return (e))
  
#)

# save - you can find this result in the  folder "output"
#save(text_to_save,file=here ("output","saved_text.RData"))
# load extracted text (if you're not in your institution)
load(here ("output","saved_text.RData"))

# set DOI as names of text_to_save
names(text_to_save) <- doisurl_dxdoi

# ------------------------------------------------------------------ #
# used papers (from a larger universe of 1820 papers in papers_doi.csv)
# load info of used papers
csv_used_papers <- read.csv(here ("data", "papers_list_used_31_03_21.csv"),
                            encoding = "latin")

# try to find a common ID to filtering
csv_used_papers [grep ("BARBOSA-FILHO", 
                       csv_used_papers$ï..PAPER_ID),"ï..PAPER_ID"] 
# replace any "," by nothing
csv_used_papers$SR <- gsub (",","",csv_used_papers$SR)
# also in DOI's csv
csv_with_doi$SR <- gsub (",","", csv_with_doi$SR) # here it is, the "SR"

# load results of TOPIC-MODELING
res_topic_modeling <- read.csv (here ("data", "Planilha_paper_x_topicos.csv"))
# remove refs that are not in used papers
# to match these two datasets
res_topic_modeling <- res_topic_modeling [which(res_topic_modeling$Paper_ID %in% csv_used_papers$ï..PAPER_ID),]
# now the used papers
csv_used_papers <- csv_used_papers [which(csv_used_papers$ï..PAPER_ID %in%  res_topic_modeling$Paper_ID),]

# cf
table(res_topic_modeling$Paper_ID == csv_used_papers$ï..PAPER_ID)

# bind topics in used papers
csv_used_papers <- cbind (csv_used_papers,
       res_topic_modeling)

# find inconsistencies
(csv_used_papers$SR [which(csv_used_papers$SR %in% csv_with_doi$SR == F)])

csv_used_papers[grep("XAVIER", csv_used_papers$Paper_ID),]
csv_with_doi$SR[grep("XAVIER", csv_with_doi$SR)]
csv_with_doi$X[grep("XAVIER", csv_with_doi$X)]

table(is.na(csv_with_doi$DI [match(csv_used_papers$ï..PAPER_ID,gsub(",","",csv_with_doi$X))]))
table(is.na(csv_with_doi$DI [match(csv_used_papers$SR,gsub(",","",csv_with_doi$SR))]))

# match DOIs and bind
csv_used_papers <- cbind(csv_used_papers, 
                         used_dois = csv_with_doi$DI [match(csv_used_papers$SR,csv_with_doi$SR)])

# used DOIS
#used_dois <- csv_with_doi$DI [which(csv_with_doi$X %in% csv_used_papers$ï..PAPER_ID)]
csv_used_papers$used_dois <- paste0("http://dx.doi.org/",gsub("http://dx.doi.org/","",csv_used_papers$used_dois))

# subset extracted text
text_to_save <- text_to_save [which(names(text_to_save) %in% csv_used_papers$used_dois)]

# sucessfull extraction of
length(text_to_save) # all DOIs
table(unlist(lapply(text_to_save,class))) # successful extraction of some text(=="character")

# --------------------------------------------------------- #
# Run extraction of coordinates, based on saved texts
# dxdoi
#resu_dxdoi<- lapply(text_to_save, function (i) 
#  
#  tryCatch ( # try overcome errors
#    convertLongLat (i),
#      error = function (e)
#      return (e))
#
#  )

#set names
#names(resu_dxdoi)<-names(text_to_save)

#save(resu_dxdoi,file=here ("output","resu_dxdoi.RData"))
load(here ("output","resu_dxdoi.RData"))

# remove errors
class_resu <- (lapply (resu_dxdoi,class)) # get the class
class_resu<-unlist (lapply (class_resu,function (i)i[1])) # get simpleErrors

# without simpleError
resu_without_error <- resu_dxdoi[which(class_resu == "list")]
# without 0 rows
nrow_resu_without_error <- unlist(lapply (sapply (resu_without_error,"[","extracted_coords"),nrow))
sucess_extract <- resu_without_error[which(nrow_resu_without_error > 0)]
# extract only coordinates 
sucess_extract_coords <- sapply (sucess_extract, "[","extracted_coords")

#range(lapply (sucess_extract_coords, function (i) i [,"Lat"]),na.rm=T)
#range(lapply (sucess_extract_coords, function (i) i [,"Long"]),na.rm=T)

# list to df to use in maps
resu_set_to_map <-data.frame(do.call(rbind,sucess_extract_coords))
# remove NAs
resu_set_to_map <- resu_set_to_map[is.na(resu_set_to_map$Long) !=T,]
colnames(resu_set_to_map)<-c("decimallongitude","decimallatitude")

# mapping
maps::map(database="world",col="gray")
points(resu_set_to_map,
       pch=3)

unique(gsub (".extracted_coords.*","",rownames(resu_set_to_map)))

# ----------------------------------------------------------- #
# now finding latlong in decimal degree format

#findtLongLat_dxdoi<- lapply(text_to_save, function (i) 
 
#  tryCatch ( # try overcome errors
#    findtLongLat (i),
#    error = function (e)
#      return (e))
  
#)

#set names
#names(findtLongLat_dxdoi)<-names(text_to_save)
# save
#save (findtLongLat_dxdoi,
#      file = here ("output", "findtLongLat_dxdoi.RData"))

# load these latlong coords
load(here ("output","findtLongLat_dxdoi.RData"))

# remove errors
class_resu_latlong <- (lapply (findtLongLat_dxdoi,class)) # get the class
class_resu_latlong<-unlist (lapply (class_resu_latlong,function (i)i[1])) # get simpleErrors

# without simpleError
resu_without_error_latlong <- findtLongLat_dxdoi[which(class_resu_latlong == "list")]
# without 0 rows
nrow_resu_without_error_latlong <- unlist(lapply (sapply (resu_without_error_latlong,"[","extracted_coords"),nrow))
sucess_extract_latlong <- resu_without_error_latlong[which(nrow_resu_without_error_latlong > 0)]
# extract only coordinates 
sucess_extract_coords_latlong <- sapply (sucess_extract_latlong, "[","extracted_coords")

# remove garbage (those that are not coordinates (ncol =1))
sucess_extract_coords_latlong <- sucess_extract_coords_latlong[which((unlist(lapply (sucess_extract_coords_latlong, ncol)=="NULL"))!=T)]

# rm those already in other formats of coords
sucess_extract_coords_latlong <- sucess_extract_coords_latlong [which(names (sucess_extract_coords_latlong) %in% names(sucess_extract_coords)==F)]

# total number of papers with extracted coords
length(sucess_extract_coords_latlong) + length(sucess_extract_coords)

#
range(lapply (sucess_extract_coords_latlong, function (i) i[,"Lat"]),na.rm=T)
range(lapply (sucess_extract_coords_latlong, function (i) i [,"Long"]),na.rm=T)

# list to df to use in maps
resu_set_to_map_latlong <-data.frame(do.call(rbind,sucess_extract_coords_latlong))
# remove NAs
resu_set_to_map_latlong <- resu_set_to_map_latlong[is.na(resu_set_to_map_latlong$Long) !=T,]
colnames(resu_set_to_map_latlong)<-c("decimallongitude","decimallatitude")

# addpoints into the map
maps::map(database="world",col="gray")
points(resu_set_to_map_latlong,
       pch=3,col="green")
points(resu_set_to_map,
       pch=3,col="black")
legend (x=-150,y=40, c("DMS","DD"), pch=3,
        col=c("black","green"),
        bty="o")
#table(unique(gsub (".extracted_coords.*","",rownames(resu_set_to_map))) %in% unique(gsub (".extracted_coords.*","",rownames(resu_set_to_map_latlong))))

# ---------------------------------------------------------------- # 
# the complete set of coordinates
complet_set_of_coordinates_df <- rbind (resu_set_to_map,
                                       resu_set_to_map_latlong)

# ------------------------------------- #
# load south america shape
# for mapping, cropping, etc

southAme<- readOGR(dsn= here("data","South_America"),encoding="latin1", 
                   layer="South_America")

#BR_AR_URU<- southAme [southAme@data$COUNTRY == "Paraguay" | southAme@data$COUNTRY == "Brazil" | southAme@data$COUNTRY == "Argentina" | southAme@data$COUNTRY == "Uruguay", ]

# load ZEE for mapping, cropping, etc
ZEE <- readOGR(dsn= here("data","BaseLines_2"),encoding="latin1",use_iconv = T, 
                   layer="BR-BaseLine_total2019")
# transform ZEE in the same proj as south america
ZEE<-spTransform(ZEE, crs(southAme))

# create a buffer of 2 degrees around the ZEE line 
ZEE_buffer <- (buffer (ZEE,width=2))

plot(southAme)
plot(ZEE_buffer,add=T,border="red")

# -------------------------
# create raster using extracted coordinates
# help here : https://nceas.github.io/oss-lessons/spatial-data-gis-law/4-tues-spatial-analysis-in-r.html

# convert the data into spatial coordinates
complet_set_of_coordinates_sp <- complet_set_of_coordinates_df
coordinates(complet_set_of_coordinates_sp) <- ~decimallongitude + decimallatitude
crs (complet_set_of_coordinates_sp)<- crs(ZEE_buffer)
#class(resu_set_to_map_cleaning)

# check which points are in the ZEE
ZEE_points <- extract(ZEE_buffer,complet_set_of_coordinates_sp)
# remove those out of ZEE
complet_set_of_coordinates_sp<-complet_set_of_coordinates_sp[is.na(ZEE_points$poly.ID)!=T]

# grid
grd_df <- expand.grid(x = seq(from = extent (complet_set_of_coordinates_sp)[1]-8,
                           to = extent (complet_set_of_coordinates_sp)[2]+8, 
                           by = 1),
                   y = seq(from = extent (complet_set_of_coordinates_sp)[3]-8,                                           
                           to = extent (complet_set_of_coordinates_sp)[4]+8, 
                           by = 1))  # expand points to grid

# Convert grd object to a matrix and then turn into a spatial
# points object
coordinates(grd_df) <- ~x + y

# Sp points into raster
grd_raster <- (raster(grd_df,resolution = 1))
crs(grd_raster) <-crs(ZEE_buffer)

# find papers DOI
paper_doi <- gsub (".extracted_coords.*","",rownames(complet_set_of_coordinates_df))

# cbind
complet_set_of_coordinates_DOI_df <- cbind(complet_set_of_coordinates_df,
                                        DOI=paper_doi)
# subset based on ZEE
complet_set_of_coordinates_DOI_df<- (complet_set_of_coordinates_DOI_df[is.na(ZEE_points$poly.ID)!=T,])

# round these coordinates
complet_set_of_coordinates_DOI_df$decimallongitude <- round (complet_set_of_coordinates_DOI_df$decimallongitude,3)
complet_set_of_coordinates_DOI_df$decimallatitude <- round (complet_set_of_coordinates_DOI_df$decimallatitude,3)

# -------------------------------------------------
# find the number of points / sites per cell
# which cell each coordinate matches

overlap_grid <- lapply (seq(1,nrow(complet_set_of_coordinates_DOI_df)), function (i) {
  
  # subset df per point
  site <- complet_set_of_coordinates_DOI_df[i,]
  # into spdf
  coordinates(site) <- ~decimallongitude + decimallatitude
  # raterize to get cell
  overlap_grid <- rasterize(site,
                            grd_raster,
                            fun="count")
  
  # transform values
  overlap_grid<-clamp(overlap_grid, 
                      lower=0, 
                      upper=1,
                      useValues=T)

  })

# stack sites
stack_DOIS_site <- (stack(overlap_grid))
# sum the number of papers per cell
rs1_site <- calc(stack_DOIS_site, sum,na.rm=T)
#rs1_site<-clamp(rs1_site, lower=1, useValues=FALSE)

# ---------------------------------------------------------- #
# plot - number of sites

plot1 <- gplot(rs1_site) +
  geom_tile(aes(x=x, y=y, fill=value), alpha=1) + 
  coord_fixed (xlim = c( -57, -25), 
               ylim = c(-40, 10), ratio = 1) +
  scale_fill_viridis(direction=-1,begin=0,
                     breaks = seq (range(values(rs1_site),na.rm=T)[1],
                                   range(values(rs1_site),na.rm=T)[2],
                                   15),
                     limits=c(range(values(rs1_site),na.rm=T)[1]+1,
                              range(values(rs1_site),na.rm=T)[2]),
                     na.value=NA) +
  ggtitle ("Number of sites per cell") + 
  theme_classic() 

plot1 + geom_polygon(data=southAme, 
                     aes(x=long, y=lat, group=group),
                     size = 0.1, fill="gray90", 
                     colour="gray75",alpha=0.1) + 
  xlab("Longitude") + ylab("Latitude")+
  labs(fill='Number of\nsites') 

# png
ggsave(filename = here ("output","maps","static_site.png"),
       width = 8,height=8,dpi =150)
# pdf
ggsave(filename = here ("output","maps","static_site.pdf"),
       width = 8,height=8,dpi =150)

# ------------------------------- #
# find the number of papers per cell

# unique DOIs
list_dois <- unique(complet_set_of_coordinates_DOI_df$DOI)

# subsetting per DOI - apply to the complete list
complet_set_of_coordinates_DOI_count <- lapply (list_dois, function (i) {
    # subsetting based on DOI (also rm this column )
    data_DOI<-complet_set_of_coordinates_DOI_df [which(complet_set_of_coordinates_DOI_df$DOI %in% i),
                                                      -which(colnames(complet_set_of_coordinates_DOI_df) == "DOI")]
    
    # convert  data into spatial coordinates
    coordinates(data_DOI) <- ~decimallongitude + decimallatitude
    
    # rasterize to count the number of papers per cell
    overlap_grid_dois <- rasterize(data_DOI,
                                   grd_raster,
                                   fun="count")
    #clamp
    overlap_grid_dois<-clamp(overlap_grid_dois, 
                             lower=1,
                             upper=1,
                             useValues=F)
    
    
})

# stack rasters from the several papers
stack_DOIS <- (stack(complet_set_of_coordinates_DOI_count))
# sum the number of papers per cell
rs1 <- calc(stack_DOIS, sum,na.rm=T)
#rs1<-clamp(rs1, lower=1, useValues=FALSE)

# plot - number of papers

plot2 <- gplot(rs1) +
  geom_tile(aes(x=x, y=y, fill=value), alpha=1) + 
  coord_fixed (xlim = c( -57, -25), 
               ylim = c(-40, 10), ratio = 1) +
  scale_fill_viridis(direction=-1,begin=0,
                     breaks = seq (range(values(rs1),na.rm=T)[1],
                                   range(values(rs1),na.rm=T)[2],
                                   8),
                     limits=c(range(values(rs1),na.rm=T)[1]+1,
                              range(values(rs1),na.rm=T)[2]),
                     na.value=NA) +
  ggtitle ("Number of articles per cell") + 
  theme_classic() 

plot2 + geom_polygon(data=southAme, 
                     aes(x=long, y=lat, group=group),
                     size = 0.1, fill="gray90", 
                     colour="gray75",alpha=0.1) + 
  xlab("Longitude") + ylab("Latitude")+
  labs(fill='Number of\narticles') 

# png
ggsave(filename = here ("output","maps","static_article.png"),
       width = 8,height=8,dpi =150)
# pdf
ggsave(filename = here ("output","maps","static_article.pdf"),
       width = 8,height=8,dpi =150)

# -------------------------------------- # 
# number of papers per decade
years <- seq (min(unique(csv_with_doi$PY),na.rm=T),max(unique(csv_with_doi$PY),na.rm=T),1)

# subsetting DOI data, per year
papers_years <- lapply (years, function (i){

  doi <- csv_with_doi [which(csv_with_doi$PY %in% i),"DI"]
  doi <- paste0("http://dx.doi.org/",gsub("http://dx.doi.org/","",doi))
  ; # retunr
  doi
  }
)

# set names to papers_years
names(papers_years) <- years

# subsetting coordinates
complet_set_of_coordinates_DOI_count_years <- lapply (seq(1,length(papers_years)), function (i) {
  
  # subsetting based on DOI
  DOI_sub<-complet_set_of_coordinates_DOI_df [which(complet_set_of_coordinates_DOI_df$DOI %in% papers_years[[i]]),]
  
  # years without detected papers
  if (nrow (DOI_sub)>=1) {
  
  # list of dois per year
  overlap_grid_dois<-lapply (unique (DOI_sub$DOI), function (k){ # create a raster per year
    # subset per doi
    DOI_sub<-DOI_sub [which(DOI_sub$DOI %in% k),
                      -which(colnames(DOI_sub) == "DOI")]
    
    # convert  data into spatial coordinates
    coordinates(DOI_sub) <- ~decimallongitude + decimallatitude
  
    # rasterize to count the number of papers per cell
    overlap_grid_dois <- rasterize(DOI_sub,
                                   grd_raster,
                                 fun="count")
    #clamp
    overlap_grid_dois<-clamp(overlap_grid_dois, lower=1, useValues=FALSE)#upper=1 is needed - one study in one cell upper=1,
    ;
    overlap_grid_dois
    })
  # stack the raster of each paper
  stack_DOIS <- (stack(overlap_grid_dois))
  # if there are data, sum them
  if(dim(stack_DOIS)[3] == 1) {stack_DOIS} else {
        rs1 <- calc(stack_DOIS, sum,na.rm=T)
        #rs1<-clamp(rs1, lower=1, useValues=F,na.rm=T)
        ; # return
        rs1}
        
  } else {"No paper was found"}
  
  
  }
)

# set years as names
names(complet_set_of_coordinates_DOI_count_years) <- years
years_with_papers<-(unlist(lapply (complet_set_of_coordinates_DOI_count_years,nrow)))

# subsetting
subst_to_map_year <- complet_set_of_coordinates_DOI_count_years[which(names(complet_set_of_coordinates_DOI_count_years)%in% names(years_with_papers))]
subst_papers_years<- papers_years[which(names(papers_years)%in% names(years_with_papers))]

# sequence to get accumulated knowledge
seq_years <- seq (1,length(subst_to_map_year))

# plot the first year
plot1 <- gplot(subst_to_map_year[[1]]) +
  geom_tile(aes(x=x, y=y, fill=value), alpha=1) + 
  coord_fixed (xlim = c( -57, -25), 
               ylim = c(-40, 10), ratio = 1) +
  scale_fill_viridis(direction=-1,begin=0.1,
                     breaks = seq (range(values(subst_to_map_year[[17]]),na.rm=T)[1], # range of the last year
                                   range(values(subst_to_map_year[[17]]),na.rm=T)[2]),
                                   6),
                     limits=c(range(values(subst_to_map_year[[17]]),na.rm=T)[1],
                              range(values(subst_to_map_year[[17]]),na.rm=T)[2]),
                     na.value=NA) +
  ggtitle (paste ("Number of articles per cell, year", 
                  names(subst_to_map_year)[1],
                  ", total of articles:", length(subst_papers_years[[1]])),
           ) + 
  theme_classic() 

plot1 + geom_polygon(data=southAme, 
                     aes(x=long, y=lat, group=group),
                     size = 0.1, fill="gray90", 
                     colour="gray75",alpha=0.1) + 
  xlab("Longitude") + ylab("Latitude") +
  labs(fill='Number of\narticles') 

# save this map
ggsave(filename = here ("output","maps",paste0("year",names(subst_to_map_year)[1],".png")),
       width = 8,height=8,dpi =150)

#
# and then the subsequent years
lapply (seq (1,length (subst_to_map_year)-1), function (i) {#gain_knowledge_year
    
    plot1 <- gplot(subst_to_map_year[[i]]) +
      geom_tile(aes(x=x, y=y, fill=value), alpha=1) + 
      coord_fixed (xlim = c( -57, -25), 
                   ylim = c(-40, 10), ratio = 1) +
      scale_fill_viridis(direction=-1,begin=0.1,
                         breaks = seq (range(values(subst_to_map_year[[17]]),na.rm=T)[1],
                                       range(values(subst_to_map_year[[17]]),na.rm=T)[2],
                                       6),
                         limits=c(1,
                                  range(values(subst_to_map_year[[17]]),na.rm=T)[2]),
                         na.value=NA) +
      ggtitle (paste ("Number of articles per cell, year",
                      names(subst_to_map_year)[i+1],
                      ", total of articles:", length(subst_papers_years[[i+1]]))) + 
      theme_classic() 
    
    plot1 + geom_polygon(data=southAme, 
                         aes(x=long, y=lat, group=group),
                         size = 0.1, fill="gray90", 
                         colour="gray75",alpha=0.1) + 
      xlab("Longitude") + ylab("Latitude")+
    labs(fill='Number of\narticles') 
    
    # save that
    ggsave(filename = here ("output","maps",paste0("year",names(subst_to_map_year)[i+1],".png")),
           width = 8,height=8,dpi =150)
})

# load to animate
list_img <- list.files(path = here ("output","maps"), pattern = "year*.*png$", full.names = T)# %>% 

##https://cran.r-project.org/web/packages/magick/vignettes/intro.html
a<-image_read(list_img)
animation <-  image_animate(a, fps = 1)
image_write(animation, here ("output","maps","anime_year.gif"))

# ------------------------------- #
# accumulation of nstudies
# accumulate (sum of sequential rasters) 
gain_knowledge_year <- lapply (seq(2,length(seq_years)), function (i){
    
  test <- (stack(subst_to_map_year[1:i]))
  rs1 <- calc(test, sum,na.rm=T)
  #rs1<-clamp(rs1, lower=1, useValues=F,na.rm=T)
  ; # return
  rs1
})

# plot the first year
plot1 <- gplot(subst_to_map_year[[1]]) +
  geom_tile(aes(x=x, y=y, fill=value), alpha=1) + 
  coord_fixed (xlim = c( -57, -25), 
               ylim = c(-40, 10), ratio = 1) +
  scale_fill_viridis(direction=-1,begin=0.1,
                     breaks = seq (range(values(gain_knowledge_year[[19]]),na.rm=T)[1], # range of the last year
                                   range(values(gain_knowledge_year[[19]]),na.rm=T)[2],
                                   8),
                     limits=c(1,
                              range(values(gain_knowledge_year[[19]]),na.rm=T)[2]),
                     na.value=NA) +
  ggtitle (paste ("Number of articles per cell, year", 
                  names(subst_to_map_year)[1],
                  ", total of articles:", length(subst_papers_years[[1]])),
           ) + 
  theme_classic() 

plot1 + geom_polygon(data=southAme, 
                     aes(x=long, y=lat, group=group),
                     size = 0.1, fill="gray90", 
                     colour="gray75",alpha=0.1) + 
  xlab("Longitude") + ylab("Latitude") +
  labs(fill='Number of\narticles') 

# save this map
ggsave(filename = here ("output","maps",paste0("acc_year",names(subst_to_map_year)[1],".png")),
       width = 8,height=8,dpi =150)

#
# and then the subsequent years
lapply (seq (1,length (gain_knowledge_year)), function (i) {#
    
    plot1 <- gplot(gain_knowledge_year[[i]]) +
      geom_tile(aes(x=x, y=y, fill=value), alpha=1) + 
      coord_fixed (xlim = c( -57, -25), 
                   ylim = c(-40, 10), ratio = 1) +
      scale_fill_viridis(direction=-1,begin=0.1,
                         breaks = seq (range(values(gain_knowledge_year[[19]]),na.rm=T)[1],
                                       range(values(gain_knowledge_year[[19]]),na.rm=T)[2],
                                       8),
                         limits=c(range(values(gain_knowledge_year[[19]]),na.rm=T)[1]),
                                  range(values(gain_knowledge_year[[19]]),na.rm=T)[2]),
                         na.value=NA) +
      ggtitle (paste ("Number of articles per cell, year",
                      names(subst_to_map_year)[i+1],
                      ", total of articles:", length(subst_papers_years[[i+1]]))) + 
      theme_classic() 
    
    plot1 + geom_polygon(data=southAme, 
                         aes(x=long, y=lat, group=group),
                         size = 0.1, fill="gray90", 
                         colour="gray75",alpha=0.1) + 
      xlab("Longitude") + ylab("Latitude")+
    labs(fill='Number of\narticles') 
    
    # save that
    ggsave(filename = here ("output","maps",paste0("acc_year",names(subst_to_map_year)[i+1],".png")),
           width = 8,height=8,dpi =150)
})

# load to animate
list_img <- list.files(path = here ("output","maps"), pattern = "acc_year*.*png$", full.names = T)# %>% 

##https://cran.r-project.org/web/packages/magick/vignettes/intro.html
a<-image_read(list_img)
animation <-  image_animate(a, fps = 1)
image_write(animation, here ("output","maps","acc_anime_year.gif"))

# ------------------------------------
# accumulation of number of sites
# subsetting per DOI - apply to the complete list

complet_set_of_coordinates_DOI_count_years_site <- lapply (seq(1,length(papers_years)), function (i) {
  # subsetting based on DOI (also rm this column )
  data_DOI<-complet_set_of_coordinates_DOI_df [which(complet_set_of_coordinates_DOI_df$DOI %in% papers_years[[i]]),]
  
  # years without detected papers
  if (nrow (data_DOI)>=1) {
    
    # convert  data into spatial coordinates
    coordinates(data_DOI) <- ~decimallongitude + decimallatitude
      
    # rasterize to count the number of papers per cell
    overlap_grid_dois <- rasterize(data_DOI,
                                     grd_raster,
                                     fun="count")
    #clamp
    overlap_grid_dois<-clamp(overlap_grid_dois,lower=1, useValues=FALSE)
    return (overlap_grid_dois)
    
  } else {"No paper was found"}
  
})

# set years as names
names(complet_set_of_coordinates_DOI_count_years_site) <- years
years_with_papers_site<-(unlist(lapply (complet_set_of_coordinates_DOI_count_years_site,nrow)))

# subsetting
subst_to_map_year_site <- complet_set_of_coordinates_DOI_count_years_site[which(names(complet_set_of_coordinates_DOI_count_years_site)%in% names(years_with_papers_site))]

# number of sites per year
n_sites_year <- lapply (seq(1,length(papers_years)), function (i) {

  data_DOI<-complet_set_of_coordinates_DOI_df [which(complet_set_of_coordinates_DOI_df$DOI %in% papers_years[[i]]),]
  nsites<- nrow (data_DOI)
  ;
  nsites
})

# subset years with data
n_sites_year<-n_sites_year[which(names(papers_years)%in% names(years_with_papers_site))]

# sequence to get accumulated knowledge
seq_years_site <- seq (1,length(subst_to_map_year_site))

# accumulate (sum of sequential rasters) 
gain_knowledge_year_site <- lapply (seq(2,length(seq_years_site)), function (i){
  
  test <- (stack(subst_to_map_year_site[1:i]))
  rs1 <- calc(test, sum,na.rm=T)
  #rs1<-clamp(rs1, lower=1, useValues=FALSE,na.rm=T)
  ; # return
  rs1
})

# plot the first year
plot1 <- gplot(subst_to_map_year_site[[1]]) +
  geom_tile(aes(x=x, y=y, fill=value), alpha=1) + 
  coord_fixed (xlim = c( -57, -25), 
               ylim = c(-40, 10), ratio = 1) +
  scale_fill_viridis(direction=-1,begin=0.1,
                     breaks = seq (range(values(gain_knowledge_year_site[[18]]),na.rm=T)[1], # range of the last year
                                   range(values(gain_knowledge_year_site[[18]]),na.rm=T)[2],
                                   15),
                     limits=c(range(values(gain_knowledge_year_site[[18]]),na.rm=T)[1],
                              range(values(gain_knowledge_year_site[[18]]),na.rm=T)[2]),
                     na.value=NA) +
  ggtitle (paste ("Number of sites per cell, year", 
                  names(subst_to_map_year)[1],
                  ", total of sites:", n_sites_year[[1]])) + 
  theme_classic() 

plot1 + geom_polygon(data=southAme, 
                     aes(x=long, y=lat, group=group),
                     size = 0.1, fill="gray90", 
                     colour="gray75",alpha=0.1) + 
  xlab("Longitude") + ylab("Latitude") +
  labs(fill='Number of\nsites') 

# save this map
ggsave(filename = here ("output","maps",paste0("site",names(subst_to_map_year_site)[1],".png")),
       width = 8,height=8,dpi =150)

#
# and then the subsequent years
lapply (seq (1,length (gain_knowledge_year_site)), function (i) {
  
  plot1 <- gplot(gain_knowledge_year_site[[i]]) +
    geom_tile(aes(x=x, y=y, fill=value), alpha=1) + 
    coord_fixed (xlim = c( -57, -25), 
                 ylim = c(-40, 10), ratio = 1) +
    scale_fill_viridis(direction=-1,begin=0.1,
                       breaks = seq (0,
                                     range(values(gain_knowledge_year_site[[18]]),na.rm=T)[2],
                                     15),
                       limits=c(range(values(gain_knowledge_year_site[[18]]),na.rm=T)[1]),
                                range(values(gain_knowledge_year_site[[18]]),na.rm=T)[2]),
                       na.value=NA) +
    ggtitle (paste ("Number of sites per cell, year",
                    names(subst_to_map_year)[i+1],
                    ", total of sites:", n_sites_year[[i+1]])) + 
    theme_classic() 
  
  plot1 + geom_polygon(data=southAme, 
                       aes(x=long, y=lat, group=group),
                       size = 0.1, fill="gray90", 
                       colour="gray75",alpha=0.1) + 
    xlab("Longitude") + ylab("Latitude")+
    labs(fill='Number of\nsites') 
  
  # save that
  ggsave(filename = here ("output","maps",paste0("site",names(subst_to_map_year)[i+1],".png")),
         width = 8,height=8,dpi =150)
})

# load to animate
list_img <- list.files(path = here ("output","maps"), pattern = "site*.*png$", full.names = T)# %>% 

##https://cran.r-project.org/web/packages/magick/vignettes/intro.html
a<-image_read(list_img)
animation <-  image_animate(a, fps = 1)
image_write(animation, here ("output","maps","acc_anime_year_site.gif"))


# --------------------------------------
# found any issue..
# you can use function "locator()" on it and then press ESC 
maps::map(database="world",col="gray")
points(complet_set_of_coordinates,pch =3)

your.coord.long <- -68.48628
your.coord.lat <- -19.13883

# find the coords (among the coordinates not in decimal degrees)
# long
coords_to_match.long <- resu_set_to_map [which(abs(resu_set_to_map[,"decimallongitude"]-(your.coord.long))==
        min(abs(resu_set_to_map[,"decimallongitude"]-(your.coord.long)))),]
# lat
coords_to_match.lat <- resu_set_to_map [which(abs(resu_set_to_map[,"decimallatitude"]-(your.coord.lat))==
                                                          min(abs(resu_set_to_map[,"decimallatitude"]-(your.coord.lat)))),]

# find in the list

(find_it <- sucess_extract_coords [which(gsub (".extracted_coords.*","",names(sucess_extract_coords)) %in% gsub (".extracted_coords.*","",rownames(coords_to_match.long)))])
  
  
# check extracted coords
resu_without_error [which(names(resu_without_error) %in% gsub (".extracted_coords","",names(find_it)))]

#
# find the coords (among the coordinates in decimal degrees)
# long
coords_to_match.long <- resu_set_to_map_latlong [which(abs(resu_set_to_map_latlong[,"decimallongitude"]-(your.coord.long))==
                                                                 min(abs(resu_set_to_map_latlong[,"decimallongitude"]-(your.coord.long)))),]
# lat
coords_to_match.lat <- resu_set_to_map_latlong[which(abs(resu_set_to_map_latlong[,"decimallatitude"]-(your.coord.lat))==
                                                                min(abs(resu_set_to_map_latlong[,"decimallatitude"]-(your.coord.lat)))),]
# find in the list

(find_it <- sucess_extract_coords_latlong [which(gsub (".extracted_coords.*","",names(sucess_extract_coords_latlong)) %in% gsub (".extracted_coords.*","",rownames(coords_to_match.long)))])


# check extracted coords
resu_without_error_latlong [which(names(resu_without_error_latlong) %in% gsub (".extracted_coords","",names(find_it)))]

which(names(text_to_save) %in% gsub (".extracted_coords","",names(find_it)))
