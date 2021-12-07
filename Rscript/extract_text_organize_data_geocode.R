
########################################################

# load (or install) required packages
source ("R/packages.R")

## functions to extract coordinates in batch
# the functional already implements coordinate cleaning
# and rounding of coordinates
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
csv_used_papers$SR <- gsub (":","",csv_used_papers$SR)
csv_used_papers$ï..PAPER_ID <- gsub (",","",csv_used_papers$ï..PAPER_ID)

# before 2000s

b2000 <- (csv_used_papers[which(csv_used_papers$Year >= 1995 & csv_used_papers$Year < 2000),])
View(b2000 [order(as.numeric(b2000$Year)),])

# ------ hist
# research over time

most_research <- round(1810*0.95)


# papers per year
papers_year<- table(csv_used_papers$Year)[order(names(table(csv_used_papers$Year)),decreasing=T)]

# 

accum_sum <-lapply (seq(2,length(names(table(papers_year)))), function (i) 
  
  sum(papers_year [1:i])
)
# melt
accum_sum <- unlist(accum_sum)
# the year with accumulation of 95% papers
year95<-papers_year [which(abs(accum_sum - most_research) == min(abs((accum_sum - most_research))))]
accum_sum[which(abs(accum_sum - most_research) == min(abs((accum_sum - most_research))))]
# histogram
pdf(here ("output", "figs","hist_time.pdf"),width=5,height=5)
hist(csv_used_papers$Year, 
        breaks=20,
     main="",
     xlab="Year")

# plot
abline(v= names(year95),lty=2,col="red",lwd=2)
text ("95%",
      x= 2005,
      y=250)

dev.off()


# also in DOI's csv
csv_with_doi$SR <- gsub (",","", csv_with_doi$SR) # here it is, the "SR"
csv_with_doi$SR <- gsub (":","", csv_with_doi$SR) # here it is, the "SR"
csv_with_doi$X <- gsub (",","", csv_with_doi$X) # here it is, the "SR"

# load results of TOPIC-MODELING
res_topic_modeling <- read.csv (here ("data", "Planilha_paper_x_topicos.csv"))
# replace "," by ""
res_topic_modeling$Paper_ID <- gsub (",","", res_topic_modeling$Paper_ID) # here it is, the "SR"
res_topic_modeling$Paper_ID <- gsub (":","", res_topic_modeling$Paper_ID) # here it is, the "SR"
# remove refs that are not in used papers
# to match these two datasets
res_topic_modeling <- res_topic_modeling [which(res_topic_modeling$Paper_ID %in% csv_used_papers$ï..PAPER_ID),]
# now the used papers
csv_used_papers <- csv_used_papers [which(csv_used_papers$ï..PAPER_ID %in%  res_topic_modeling$Paper_ID),]

# cf
# re_topic_modeling & csv_used papers are all equal...
table(res_topic_modeling$Paper_ID == csv_used_papers$ï..PAPER_ID)

# bind topics in used papers
csv_used_papers <- cbind (csv_used_papers,
                        res_topic_modeling)

# find inconsistencies in the paperID
#frst_set_inconsistency <- (data.frame(Paper_ID=csv_used_papers$Paper_ID [which(res_topic_modeling$Paper_ID %in% csv_with_doi$SR == F)]))# papers not in csvwith doi
#frst_set_inconsistency <- frst_set_inconsistency [order(frst_set_inconsistency$Paper_ID),]#order
#scd_set_inconsistency <- (data.frame(Paper_ID2=csv_with_doi$SR [which(csv_with_doi$SR %in% res_topic_modeling$Paper_ID == F)]))# papers of doi csv not in modeling output
#scd_set_inconsistency <- scd_set_inconsistency[order(scd_set_inconsistency$Paper_ID2),]
#NA_to_bind <- matrix(NA, nrow=length(scd_set_inconsistency)-length(frst_set_inconsistency),
#                     ncol=1)# some rows to equalize dims
## bind these rows to the smaller dataset
#frst_set_inconsistency<-c(frst_set_inconsistency,
#                              NA_to_bind)
## cbind these two vectors of inconsistencies
#tab_inconsistencies<-(cbind(frst_set_inconsistency,scd_set_inconsistency))
#colnames(tab_inconsistencies)<-c("used","doi")
#
## go to excel
#write.csv(tab_inconsistencies,file=here("data","inconsistencies.csv"))

# then after correct by hand in excel, replace some of detected inconsistencies (most were not solved)
csv_with_doi$SR[which(csv_with_doi$SR == "ALMEIDA AO 2015 ")]<-	"ALMEIDA AO 2015 MARINE BIODIVERSITY RECORDS"
csv_with_doi$SR[which(csv_with_doi$SR == "ALMEIDA DF 2016 ")]<-"ALMEIDA DF 2016 MARINE BIODIVERSITY RECORDS"
csv_with_doi$SR[which(csv_with_doi$SR == "AMARAL RF 2004 2004 IEEE INTERNATIONAL CONFERENCE ON ROBOTICS AND AUTOMATION VOLS 1 - 5 PROCEEDINGS")]<-"AMARAL RF 2004 2004 IEEE INTERNATIONAL CONFERENCE ON ROBOTICS AND AUTOMATION VOLS 1- 5 PROCEEDINGS"
csv_with_doi$SR[which(csv_with_doi$SR == "BARBOZA CADM 2015 ")]<-"BARBOZA CADM 2015 MARINE BIODIVERSITY RECORDS"
csv_with_doi$SR[which(csv_with_doi$SR == "BREVES A 2016 ")]<-"BREVES A 2016 MARINE BIODIVERSITY RECORDS"
csv_with_doi$SR[which(csv_with_doi$SR == "BUENO LS 2015 ")]<-"BUENO LS 2015 MARINE BIODIVERSITY RECORDS"
csv_with_doi$SR[which(csv_with_doi$SR == "CARVALHO FILHO A 2013 NEOTROP ICHTHYOL")]<-	"CARVALHO-FILHO A 2013 NEOTROP ICHTHYOL"
csv_with_doi$SR[which(csv_with_doi$SR == "CASTELLO-BRANCO C 2014 ")]<-"CASTELLO-BRANCO C 2014 MARINE BIODIVERSITY RECORDS"
csv_with_doi$SR[which(csv_with_doi$SR == "CORDEIRO RTS 2012 ")]<-"CORDEIRO RTS 2012 MARINE BIODIVERSITY RECORDS"
csv_with_doi$SR[which(csv_with_doi$SR == "DA ROCHA MIRANDA V 2014 ")]<-"DAROCHAMIRANDA V 2014 MARINE BIODIVERSITY RECORDS"
csv_with_doi$SR[which(csv_with_doi$SR == "DA SILVA COSTA MM 2014 ")]<-"DASILVACOSTA MM 2014 MARINE BIODIVERSITY RECORDS"
csv_with_doi$SR[which(csv_with_doi$SR == "DE ALMEIDA ACS 2015 ")]<-"DEALMEIDA ACS 2015 MARINE BIODIVERSITY RECORDS"
csv_with_doi$SR[which(csv_with_doi$SR == "DE CASTRO MELO PAM 2012 ")]<-"DECASTROMELO PAM 2012 MARINE BIODIVERSITY RECORDS"
csv_with_doi$SR[which(csv_with_doi$SR == "DE PAIVA BARROS-ALVES S 2016 ")]<-"DEPAIVABARROS-ALVES S 2016 MARINE BIODIVERSITY RECORDS"
csv_with_doi$SR[which(csv_with_doi$SR == "DIAS RM 2015 ")]<-"DIAS RM 2015 MARINE BIODIVERSITY RECORDS"
csv_with_doi$SR[which(csv_with_doi$SR == "DIAS RM 2015 -a")]<-"DIAS RM 2015 MARINE BIODIVERSITY RECORDS-a"
csv_with_doi$SR[which(csv_with_doi$SR == "DIAS TLP 2015 ")]<-"DIAS TLP 2015 MARINE BIODIVERSITY RECORDS"
csv_with_doi$SR[which(csv_with_doi$SR == "DOS SANTOS KLH A 2013 ")]<-"DOSSANTOSKLâˆšÃ®H A 2013 MARINE BIODIVERSITY RECORDS"
csv_with_doi$SR[which(csv_with_doi$SR == "DUARTE J 2014 ")]<-"DUARTE J 2014 MARINE BIODIVERSITY RECORDS"
csv_with_doi$SR[which(csv_with_doi$SR == "GALVO FILHO HC 2015 ")]<- "GALVâˆšÃ‰O FILHO HC 2015 MARINE BIODIVERSITY RECORDS"
csv_with_doi$SR[which(csv_with_doi$SR == "GIGLIO VJ 2014 ")]<- "GIGLIO VJ 2014 MARINE BIODIVERSITY RECORDS"
csv_with_doi$SR[which(csv_with_doi$SR == "GIRALDES BW 2016 ")]<- "GIRALDES BW 2016 MARINE BIODIVERSITY RECORDS"
csv_with_doi$SR[which(csv_with_doi$SR == "LIMA MLF 2011 ")]<-	"LIMA MLF 2011 MARINE BIODIVERSITY RECORDS"
csv_with_doi$SR[which(csv_with_doi$SR == "LOMNACO C 2011 ")]<-	"LOMâˆšÃ®NACO C 2011 MARINE BIODIVERSITY RECORDS"
csv_with_doi$SR[which(csv_with_doi$SR == "LONGO GO 2012 ")]<-	"LONGO GO 2012 MARINE BIODIVERSITY RECORDS"
csv_with_doi$SR[which(csv_with_doi$SR == "MANTELATTO MC 2016 ")]<-	"MANTELATTO MC 2016 MARINE BIODIVERSITY RECORDS"
csv_with_doi$SR[which(csv_with_doi$SR == "MARTINS L 2012 ")]<-	"MARTINS L 2012 MARINE BIODIVERSITY RECORDS"
csv_with_doi$SR[which(csv_with_doi$SR == "MEDEIROS DV 2011 ")]<-	"MEDEIROS DV 2011 MARINE BIODIVERSITY RECORDS"
csv_with_doi$SR[which(csv_with_doi$SR == "MENDES TC 2011 ")]<-	"MENDES TC 2011 MARINE BIODIVERSITY RECORDS"
csv_with_doi$SR[which(csv_with_doi$SR == "MENDONA P 2011 ")]<-	"MENDONâˆšÃ¡A P 2011 MARINE BIODIVERSITY RECORDS"
csv_with_doi$SR[which(csv_with_doi$SR == "MIRANDA RJ 2016 ")]<-	"MIRANDA RJ 2016 MARINE BIODIVERSITY RECORDS"
csv_with_doi$SR[which(csv_with_doi$SR == "MIYASHITA LK 2014 ")]<-	"MIYASHITA LK 2014 MARINE BIODIVERSITY RECORDS"
csv_with_doi$SR[which(csv_with_doi$SR == "MONTEIRO-NETO C 2013 ")]<-	"MONTEIRO-NETO C 2013 MARINE BIODIVERSITY RECORDS"
csv_with_doi$SR[which(csv_with_doi$SR == "MOREIRA ALP 2014 ")]<-	"MOREIRA ALP 2014 MARINE BIODIVERSITY RECORDS"
csv_with_doi$SR[which(csv_with_doi$SR == "OLIVEIRA MV 2015 ")]<-	"OLIVEIRA MV 2015 MARINE BIODIVERSITY RECORDS"
csv_with_doi$SR[which(csv_with_doi$SR == "OLIVEIRA MV 2015 -a")]<-	"OLIVEIRA MV 2015 MARINE BIODIVERSITY RECORDS-a"
csv_with_doi$SR[which(csv_with_doi$SR == "PADULA V 2012 ")]<-	"PADULA V 2012 MARINE BIODIVERSITY RECORDS"
csv_with_doi$SR[which(csv_with_doi$SR == "PEREIRA PHC 2011 ")]<-	"PEREIRA PHC 2011 MARINE BIODIVERSITY RECORDS"
csv_with_doi$SR[which(csv_with_doi$SR == "PEREIRA PHC 2012 ")]<-	"PEREIRA PHC 2012 J MAR BIOL ASSOC UK"
csv_with_doi$SR[which(csv_with_doi$SR == "REIS-FILHO JA 2014 ")]<-	"REIS-FILHO JA 2014 MARINE BIODIVERSITY RECORDS"
csv_with_doi$SR[which(csv_with_doi$SR == "SAMPAIO CLS 2015 ")]<-	"SAMPAIO CLS 2015 MARINE BIODIVERSITY RECORDS"
csv_with_doi$SR[which(csv_with_doi$SR == "SAMPAIO CLS 2017 ")]<-	"SAMPAIO CLS 2017 MARINE BIODIVERSITY RECORDS"
csv_with_doi$SR[which(csv_with_doi$SR == "SANTANDER-NETO J 2011 ")]<-	"SANTANDER-NETO J 2011 MARINE BIODIVERSITY RECORDS"
csv_with_doi$SR[which(csv_with_doi$SR == "SISSINI MN 2014 ")]<-	"SISSINI MN 2014 MARINE BIODIVERSITY RECORDS"
csv_with_doi$SR[which(csv_with_doi$SR == "SKINNER LF 2013 ")]<-	"SKINNER LF 2013 MARINE BIODIVERSITY RECORDS"
csv_with_doi$SR[which(csv_with_doi$SR == "SOUZA AT 2011 ")]<-	"SOUZA AT 2011 MARINE BIODIVERSITY RECORDS"
csv_with_doi$SR[which(csv_with_doi$SR == "SPIER D 2012 ")]<-	"SPIER D 2012 MARINE BIODIVERSITY RECORDS"
csv_with_doi$SR[which(csv_with_doi$SR == "VERHOEF LGW 2006 PROCEEDINGS OF THE THIRD INTERNATIONAL SYMPOSIUM ON RESTORATION WORLD HERITAGE SITE OLINDA IN BRAZIL PROPOSALS FOR INTERVENTION")]<-	"	VERHOEF LGW 2006 PROCEEDINGS OF THE THIRD INTERNATIONAL SYMPOSIUM ON RESTORATION WORLD HERITAGE SITE OLINDA IN BRAZIL PROPOSALS FOR INTERVENTION"

# checking names
csv_used_papers[grep("VERHOEF", csv_used_papers$Paper_ID),]
csv_with_doi$SR[grep("VERHOEF", csv_with_doi$SR)]
csv_with_doi$X[grep("VERHOEF", csv_with_doi$X)]

table(is.na(csv_with_doi$DI [match(csv_used_papers$ï..PAPER_ID,gsub(",","",csv_with_doi$X))]))
table(is.na(csv_with_doi$DI [match(csv_used_papers$SR,gsub(",","",csv_with_doi$SR))]))

# match DOIs and bind
csv_used_papers <- cbind(csv_used_papers, 
                         used_dois = csv_with_doi$DI [match(csv_used_papers$SR,csv_with_doi$SR)])

table(csv_used_papers$SR %in% csv_with_doi$SR) # the number of papers usedm with DOI
table(is.na(csv_used_papers$used_dois)) # the number of papers lacking DOI

# used DOIS
#used_dois <- csv_with_doi$DI [which(csv_with_doi$X %in% csv_used_papers$ï..PAPER_ID)]
csv_used_papers$used_dois <- paste0("http://dx.doi.org/", # standardize DOI format
                                    gsub("http://dx.doi.org/","",
                                         csv_used_papers$used_dois))

# subset extracted text
text_to_save <- text_to_save [which(names(text_to_save) %in% csv_used_papers$used_dois)] # text within used data set

# successfull extraction of text
length(text_to_save) # all DOIs
table(unlist(lapply(text_to_save,class))) # successful extraction of some text(=="character")


# --------------------------------------------------------- #
# Run extraction of coordinates, based on saved texts
# dxdoi
#resu_dxdoi<- lapply(text_to_save, function (i) 
#
#  tryCatch ( # try to overcome errors
#    convertLongLat (i),
#      error = function (e)
#      return (e))
#
#  )
#
##set names
#names(resu_dxdoi)<-names(text_to_save)
## save
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

# range of coord values
range(lapply (sucess_extract_coords, function (i) i [,"Lat"]),na.rm=T)
range(lapply (sucess_extract_coords, function (i) i [,"Long"]),na.rm=T)

# list to df to use in maps
resu_set_to_map <-data.frame(do.call(rbind,sucess_extract_coords))
# remove NAs
resu_set_to_map <- resu_set_to_map[is.na(resu_set_to_map$Long) !=T,]
colnames(resu_set_to_map)<-c("decimallongitude","decimallatitude")# edit colnames

# mapping
maps::map(database="world",col="gray")
points(resu_set_to_map,
       pch=3)
# unique(gsub (".extracted_coords.*","",rownames(resu_set_to_map)))

# ----------------------------------------------------------- #
# now finding latlong in decimal degree format
# also implements coordinate cleaning

#findtLongLat_dxdoi<- lapply(text_to_save, function (i) 
# 
#  tryCatch ( # try to overcome errors
#    findtLongLat (i),
#    error = function (e)
#      return (e))
#  
#)
#
##set names
#names(findtLongLat_dxdoi)<-names(text_to_save)
## save
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

# range of coords
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
# table(unique(gsub (".extracted_coords.*","",rownames(resu_set_to_map))) %in% unique(gsub (".extracted_coords.*","",rownames(resu_set_to_map_latlong))))

# ---------------------------------------------------------------- # 
# the complete set of coordinates
# unifying extracted coordinates in different formats
complet_set_of_coordinates_df <- rbind (resu_set_to_map,
                                       resu_set_to_map_latlong)

# ------------------------------------- #
# load South America shapefile
# for mapping, cropping, etc
southAme<- readOGR(dsn= here("data","South_America"),encoding="latin1", 
                   layer="South_America")

# load ZEE for mapping, cropping, etc
ZEE <- readOGR(dsn= here("data","BaseLines_2"),encoding="latin1",use_iconv = T, 
                   layer="BR-BaseLine_total2019")

# transform ZEE in the same proj as south america
ZEE<-spTransform(ZEE, crs(southAme))

# spatial line between CVT and land
library(sp)
y <- c(-19.711404,-20.798461,-20.737267)
x <- c(-38.321404,-33.140530,-29.001198)
CVT <- SpatialLines(list(Lines(Line(cbind(x,y)), ID="a")))
crs(CVT)<- crs(southAme)

# create a buffer of 2 degrees around the ZEE line 
ZEE_buffer <- (buffer (ZEE,width=2))
CVT_buffer <- (buffer (CVT,width=2))

# buffer 
require(maptools)
ZEE_buffer<-(gUnion (CVT_buffer,ZEE_buffer))

# check
par(mar=c(0,0,0,0))
plot(southAme,border="gray")
plot(ZEE,add=T,lwd=1)
plot(ZEE_buffer,add=T,border="red")
points(complet_set_of_coordinates_df,add=T)

# -------------------------
# create raster using extracted coordinates
# help here : https://nceas.github.io/oss-lessons/spatial-data-gis-law/4-tues-spatial-analysis-in-r.html

# convert the data frame into a spatial coordinates object
complet_set_of_coordinates_sp <- complet_set_of_coordinates_df
coordinates(complet_set_of_coordinates_sp) <- ~decimallongitude + decimallatitude # as coord
crs (complet_set_of_coordinates_sp)<- crs(ZEE_buffer)# adjust crs

# check which points are in the ZEE
ZEE_points <- extract(ZEE_buffer,
                      complet_set_of_coordinates_sp)

# find papers DOI
paper_doi <- gsub (".extracted_coords.*","",
                   rownames(complet_set_of_coordinates_df))

# cbind
complet_set_of_coordinates_DOI_df <- cbind(complet_set_of_coordinates_df,
                                        DOI=paper_doi)
# subset based on the ZEE buffer
complet_set_of_coordinates_DOI_df <- (complet_set_of_coordinates_DOI_df[is.na(ZEE_points$poly.ID)!=T,])
# round these coordinates
complet_set_of_coordinates_DOI_df$decimallongitude <- round (complet_set_of_coordinates_DOI_df$decimallongitude,3)
complet_set_of_coordinates_DOI_df$decimallatitude <- round (complet_set_of_coordinates_DOI_df$decimallatitude,3)

# unique(gsub (".extracted_coords.*","",rownames(complet_set_of_coordinates_DOI_df)))

# check points on the map
plot(southAme,border="gray")
plot(ZEE,add=T,col="black",lwd=1)
plot(ZEE_buffer,add=T,lty=2,col=rgb(0.1,0.1,1,alpha=0.1))
# plot the complete dataset
#points(complet_set_of_coordinates_sp,
#	pch=19, 
#	col=rgb(0.1,0.1,0.8,alpha=0.2))
# plot after filtering based on ZEE
points(complet_set_of_coordinates_DOI_df,pch=19,
	col = rgb(0.5,0.8,0.1,alpha=0.4))

#-------------------------------------------
# solving minor issues

## coordinate in the middle of Paraiba,
## study in Mamanguape River Estuary (6°43′02”S e 35°67′46”W)
# study: http://dx.doi.org/10.1007/s10641-018-0790-7
# the authors report "Mamanguape River Estuary (6°43′02”S e 35°67′46”W)"
# it likely should be 35°57′46”W
# thus, we replaced the coordinates by searching the place in GOogle Earth
complet_set_of_coordinates_DOI_df [grep("http://dx.doi.org/10.1007/s10641-018-0790-7",
                                    rownames(complet_set_of_coordinates_DOI_df)),"decimallongitude"]<- -34.937
complet_set_of_coordinates_DOI_df [grep("http://dx.doi.org/10.1007/s10641-018-0790-7",
                                    rownames(complet_set_of_coordinates_DOI_df)),"decimallatitude"]<-  -6.773

# the other issue is here:
# http://dx.doi.org/10.1017/S0025315410000858
# study performed aloing the alagoas coast
# thus I moved the longitude toward east
# it was -36.167, now it is -35.07
complet_set_of_coordinates_DOI_df [grep("http://dx.doi.org/10.1017/S0025315410000858",rownames(complet_set_of_coordinates_DOI_df)),"decimallongitude"]<-  -35.07

#matched_data_coordinates_mpas[which(matched_data_coordinates_mpas$decimallatitude > -10 & 
#                              matched_data_coordinates_mpas$decimallatitude < -5),c("decimallatitude","decimallongitude")]

# plot again
plot(southAme,border="gray")
plot(ZEE,add=T,col="black",lwd=1)
plot(ZEE_buffer,add=T,lty=2,col=rgb(0.1,0.1,1,alpha=0.1))
# plot the complete dataset
#points(complet_set_of_coordinates_sp,
#	pch=19, 
#	col=rgb(0.1,0.1,0.8,alpha=0.2))
# plot after filtering based on ZEE
points(complet_set_of_coordinates_DOI_df,pch=19,
       col = rgb(0.5,0.8,0.1,alpha=0.4))

# Good!

# -------------------------------------------------
# adding topic information
# match between articles' data set and coordinate information

match_coords_doi <- (csv_used_papers [match (complet_set_of_coordinates_DOI_df$DOI, 
                                          csv_used_papers$used_dois),]) # match DOIs
# bind coordinates
matched_data<-cbind(complet_set_of_coordinates_DOI_df,
                    match_coords_doi)  # create an unified df

# now we can use coordinate information to check superimposition on BR MPAs
MPAs<- readOGR(dsn=here("data", "MPAs"),
               encoding="utf8",use_iconv = T, 
        layer="cnuc_2021_02")

# coordinates to extract
coord_extract <- matched_data[c("decimallatitude","decimallongitude")]
coordinates(coord_extract) <- ~decimallongitude + decimallatitude
crs(coord_extract) <- crs(MPAs)

# over these data on MPAs
pts_mpas <- (over(coord_extract,MPAs)$nome_uc)

# bind the MPA to the dataset
matched_data_coordinates_mpas <- cbind(matched_data,
                                      pts_mpas)

# plot
plot(southAme,border="gray")
plot(ZEE,add=T,col="black",lwd=1)
plot(crop (MPAs,ZEE_buffer) ,add=T,border="blue")
plot(ZEE_buffer,add=T,border="red")
points(coord_extract,pch=3,col=rgb(0.1,0.1,0.1,alpha=0.5))
legend ("topright",legend = c("ZEE","MPAs","Sites"),
        pch=c(1,1,3),col=c("red","blue","black"))
#

# ----------------------------------------------------------- #
## binding historical data  (before 2000) # no DOI
# load
historical_Data <- read.csv (here("data", "HistoricalData_60s_90s", "HistoricalData.csv"))
# round to avoid the double count of spatially close sites 
historical_Data$Latitude <- round(historical_Data$Latitude,3)
historical_Data$Longitude <- round(historical_Data$Longitude,3)
# rm duplicated
#historical_Data<-(historical_Data [!duplicated(historical_Data$Latitude),])
# data to bind
historical_Data<-historical_Data[,c("PAPER_ID", "Longitude","Latitude")] 
# change colnames
colnames(historical_Data)[2:3] <- c("decimallongitude", "decimallatitude")
# rm NA
historical_Data <- historical_Data [is.na(historical_Data$decimallatitude) != T,]
# check match of id
historical_Data$PAPER_ID[which(historical_Data$PAPER_ID %in% res_topic_modeling$Paper_ID ==F)] # no problem, this is a duplicate study

# matching based on ID (not DOI as before)
topics_historical <- (res_topic_modeling [match(historical_Data$PAPER_ID,res_topic_modeling$Paper_ID),])
# topics_historical$Paper_ID == historical_Data$PAPER_ID
# DOI (actually none)
historical_Data$DOI <- NA
# year of the paper
historical_Data$Year <- csv_used_papers [match (historical_Data$PAPER_ID, csv_used_papers$Paper_ID), "Year"]

# cbind topics
historical_Data <- cbind (historical_Data,
                          topics_historical)

# cbind MPAs
# coordinates to extract
coord_extract_historical <- historical_Data[c("decimallatitude","decimallongitude")]
coordinates(coord_extract_historical) <- ~decimallongitude + decimallatitude
crs(coord_extract_historical) <- crs(MPAs)

# check which points are in the ZEE
ZEE_points_historical <- extract(ZEE_buffer,
                                 coord_extract_historical)

# over these data on MPAs
pts_mpas_historical <- (over(coord_extract_historical,
                             MPAs)$nome_uc)

# bind the MPA to the dataset
historical_Data <- cbind(historical_Data,
                         pts_mpas=pts_mpas_historical)

# subset based on the ZEE buffer
historical_Data <- (historical_Data[is.na(ZEE_points_historical$poly.ID)!=T,])

# now lets organize the extracted (based on text) and historical dataset (extracted by hand)
# matching cols
geo_extracted_function <- (matched_data_coordinates_mpas[,colnames(matched_data_coordinates_mpas) %in% colnames(historical_Data)])
geo_historical <- (historical_Data[,colnames(historical_Data) %in% colnames(geo_extracted_function)])

# match order
historical_extracted_data <- rbind (data.frame (geo_extracted_function,data="function"),
                                    data.frame (geo_historical, data="historical"))

# ------------------------------------------------------------# 
# add data of corresponding author's institution
institution <- read.csv(here ("data","instituicoes_final.csv"),sep=",")

# correspond and match data
matched_data_coordinates_mpas_institutions <- cbind (historical_extracted_data, 
                                                     (institution [match(historical_extracted_data$Paper_ID,
                                                                         institution$PAPER_ID),c("PAPER_ID","Institution.corresponnding")])
)

# adjust some inconsistencies
matched_data_coordinates_mpas_institutions$Institution.corresponnding[which(matched_data_coordinates_mpas_institutions$Institution.corresponnding == "FED UNIV RIO GRANDE")] <-  "UNIV FED RIO GRANDE FURG"
matched_data_coordinates_mpas_institutions$Institution.corresponnding[which(matched_data_coordinates_mpas_institutions$Institution.corresponnding == "UNIV FED RIO GRANDE")] <-  "UNIV FED RIO GRANDE FURG"
matched_data_coordinates_mpas_institutions$Institution.corresponnding[which(matched_data_coordinates_mpas_institutions$Institution.corresponnding == "UNIV ESTADO RIO DE JANEIRO")] <-  "UNIV ESTADO RIO DE JANEIRO UERJ"
matched_data_coordinates_mpas_institutions$Institution.corresponnding[which(matched_data_coordinates_mpas_institutions$Institution.corresponnding == "UNIV ESTADUAL CAMPINAS")] <-  "UNIV ESTADUAL CAMPINAS UNICAMP"
matched_data_coordinates_mpas_institutions$Institution.corresponnding[which(matched_data_coordinates_mpas_institutions$Institution.corresponnding == "UNIV ESTADUAL PAULISTA")] <-  "UNIV ESTADUAL PAULISTA UNESP"
matched_data_coordinates_mpas_institutions$Institution.corresponnding[which(matched_data_coordinates_mpas_institutions$Institution.corresponnding == "UNIV NORTH RIO DE JANEIRO STATE")] <-  "UNIV NORTH RIO DE JANEIRO"
matched_data_coordinates_mpas_institutions$Institution.corresponnding[which(matched_data_coordinates_mpas_institutions$Institution.corresponnding == "UNIV FED SAO PAULO")] <-  "UNIV FED SAO PAULO UNIFESP"
matched_data_coordinates_mpas_institutions$Institution.corresponnding[which(matched_data_coordinates_mpas_institutions$Institution.corresponnding == "UNIV FED PERNAMBUCO")] <-  "UNIV FED PERNAMBUCO UFPE"
matched_data_coordinates_mpas_institutions$Institution.corresponnding[which(matched_data_coordinates_mpas_institutions$Institution.corresponnding == "UNIV FED RIO GRANDE DO NORTE")] <-  "UNIV FED RIO GRANDE NORTE UFRN"
matched_data_coordinates_mpas_institutions$Institution.corresponnding[which(matched_data_coordinates_mpas_institutions$Institution.corresponnding == "UNIV FED RURAL PERNAMBUCO")] <-  "UNIV FED RURAL PERNAMBUCO UFRPE"
matched_data_coordinates_mpas_institutions$Institution.corresponnding[which(matched_data_coordinates_mpas_institutions$Institution.corresponnding == "INST ESTUDOS MAR ALMIRANTE PAULO MOREIRA")] <-  "INST ESTUDOS MAR ALMIRANTE PAULO MOREIRA IEAPM"
matched_data_coordinates_mpas_institutions$Institution.corresponnding[which(matched_data_coordinates_mpas_institutions$Institution.corresponnding == "RIO DE JANEIRO BOT GARDEN RES INST")] <-  "INST PESQUISAS JARDIM BOT RIO DE JANEIRO"
matched_data_coordinates_mpas_institutions$Institution.corresponnding[which(matched_data_coordinates_mpas_institutions$Institution.corresponnding == "UNIV FED SANTA CATARINA")] <-  "UNIV FED SANTA CATARINA UFSC"

# issues about the corresponding author
matched_data_coordinates_mpas_institutions$Institution.corresponnding[which(matched_data_coordinates_mpas_institutions$Institution.corresponnding == "CEL (CORRESPONDING AUTHOR)")] <-  "UNIV FED FLUMINENSE"
matched_data_coordinates_mpas_institutions$Institution.corresponnding[which(matched_data_coordinates_mpas_institutions$Institution.corresponnding == "LC (CORRESPONDING AUTHOR)")] <-  "UNIV FED RURAL PERNAMBUCO UFRPE"
matched_data_coordinates_mpas_institutions$Institution.corresponnding[which(matched_data_coordinates_mpas_institutions$Institution.corresponnding == "LAB ECHINODERMATA")] <-  "UNIV FED RIO DE JANEIRO"
matched_data_coordinates_mpas_institutions$Institution.corresponnding[which(matched_data_coordinates_mpas_institutions$Institution.corresponnding == "H (CORRESPONDING AUTHOR)")] <-  "HANOVER COLLEGE"
matched_data_coordinates_mpas_institutions$Institution.corresponnding[which(matched_data_coordinates_mpas_institutions$Institution.corresponnding == "MICROBIOL LAB")] <-  "UNIV FED RIO DE JANEIRO"
matched_data_coordinates_mpas_institutions$Institution.corresponnding[which(matched_data_coordinates_mpas_institutions$Institution.corresponnding == "S-N CIDADE UNIV")] <-  "UNIV FED PERNAMBUCO"
matched_data_coordinates_mpas_institutions$Institution.corresponnding[which(matched_data_coordinates_mpas_institutions$Institution.corresponnding == "J (CORRESPONDING AUTHOR)")] <-  "UNIV SAO PAULO"
matched_data_coordinates_mpas_institutions$Institution.corresponnding[which(matched_data_coordinates_mpas_institutions$Institution.corresponnding == "INST PESQUISAS")] <-  "INST PESQUISAS JARDIM BOT RIO DE JANEIRO"
matched_data_coordinates_mpas_institutions$Institution.corresponnding[which(matched_data_coordinates_mpas_institutions$Institution.corresponnding == "AJB (CORRESPONDING AUTHOR)")] <-  "FDN PRO TAMAR"
matched_data_coordinates_mpas_institutions$Institution.corresponnding[which(matched_data_coordinates_mpas_institutions$Institution.corresponnding == "M (CORRESPONDING AUTHOR)")] <-  "UNIV FED ABC"
matched_data_coordinates_mpas_institutions$Institution.corresponnding[which(matched_data_coordinates_mpas_institutions$Institution.corresponnding == "SR (CORRESPONDING AUTHOR)")] <-  "UNIV FED SANTA CATARINA UFSC"
matched_data_coordinates_mpas_institutions$Institution.corresponnding[which(matched_data_coordinates_mpas_institutions$Institution.corresponnding == "SR (CORRESPONDING AUTHOR)")] <-  "UNIV FED SANTA CATARINA UFSC"
matched_data_coordinates_mpas_institutions$Institution.corresponnding[which(matched_data_coordinates_mpas_institutions$Institution.corresponnding == "A (CORRESPONDING AUTHOR)")] <-  "UNIV FED BAHIA"
matched_data_coordinates_mpas_institutions$Institution.corresponnding[which(matched_data_coordinates_mpas_institutions$Institution.corresponnding == "CW (CORRESPONDING AUTHOR)")] <-  "UNIV FED PARANA"
matched_data_coordinates_mpas_institutions$Institution.corresponnding[which(matched_data_coordinates_mpas_institutions$Institution.corresponnding == "CTR CIENCIAS SAUDE")] <-  "UNIV FED RIO DE JANEIRO"
matched_data_coordinates_mpas_institutions$Institution.corresponnding[which(matched_data_coordinates_mpas_institutions$Institution.corresponnding == "FDN UNIV RIO GRANDE")] <-  "UNIV FED RIO GRANDE FURG"
matched_data_coordinates_mpas_institutions$Institution.corresponnding[which(matched_data_coordinates_mpas_institutions$Institution.corresponnding == "INST BIOL")] <-  "UNIV FED RIO DE JANEIRO"
matched_data_coordinates_mpas_institutions$Institution.corresponnding[which(matched_data_coordinates_mpas_institutions$Institution.corresponnding == "INST BOT")] <-  "JARDIM BOT SAO PAULO"
matched_data_coordinates_mpas_institutions$Institution.corresponnding[which(matched_data_coordinates_mpas_institutions$Institution.corresponnding == "INST BOTAN")] <-  "JARDIM BOT SAO PAULO"
matched_data_coordinates_mpas_institutions$Institution.corresponnding[which(matched_data_coordinates_mpas_institutions$Institution.corresponnding == "DFM (CORRESPONDING AUTHOR)")] <-  "INST NACL PESQUISAS ESPACIAIS"
matched_data_coordinates_mpas_institutions$Institution.corresponnding[which(matched_data_coordinates_mpas_institutions$Institution.corresponnding == "TB (CORRESPONDING AUTHOR)")] <-  "CONSERVAT INT"
matched_data_coordinates_mpas_institutions$Institution.corresponnding[which(matched_data_coordinates_mpas_institutions$Institution.corresponnding == "CAMPUS UNIV ONDINA")] <-  "UNIV FED BAHIA"
matched_data_coordinates_mpas_institutions$Institution.corresponnding[which(matched_data_coordinates_mpas_institutions$Institution.corresponnding == "INST MICROBIOL PROF PAULO DE GOES")] <-  "UNIV FED RIO DE JANEIRO"
matched_data_coordinates_mpas_institutions$Institution.corresponnding[which(matched_data_coordinates_mpas_institutions$Institution.corresponnding == "LAB VERTEBRADOS AQUAT")] <-  "UNIV FED ESPIRITO SANTO"
matched_data_coordinates_mpas_institutions$Institution.corresponnding[which(matched_data_coordinates_mpas_institutions$Institution.corresponnding == "INST GEOCIENCIAS")] <-  "UNIV FED BAHIA"
#matched_data_coordinates_mpas_institutions[which(matched_data_coordinates_mpas_institutions$Institution.corresponnding == "INST GEOCIENCIAS"),]


# define which institutions are from south, north, northeastern and southeastern
Region <- (matched_data_coordinates_mpas_institutions$Institution.corresponnding)
# SOUTHEASTERN
Region [grep("RIO DE JANEIRO", Region)] <- "Southeastern"
Region [grep("SAO PAULO", Region)] <- "Southeastern"
Region [grep("ESPIRITO SANTO", Region)] <- "Southeastern"
Region [grep("FLUMINENSE", Region)] <- "Southeastern"
Region [grep("PAULISTA", Region)] <- "Southeastern"
Region [grep("CAMPINAS", Region)] <- "Southeastern"
Region [grep("INST ESTUDOS MAR ALMIRANTE PAULO MOREIRA IEAPM", Region)] <- "Southeastern"
Region [grep("JUIZ DE FORA", Region)] <- "Southeastern"
Region [grep("UBERLANDIA", Region)] <- "Southeastern"
Region [grep("UBERLANDIA", Region)] <- "Southeastern"
Region [grep("UNIV FED ABC", Region)] <- "Southeastern"
Region [grep("INST LAJE VIVA", Region)] <- "Southeastern"
Region [grep("UNIV FED SAO CARLOS", Region)] <- "Southeastern"
Region [grep("INST NACL PESQUISAS ESPACIAIS", Region)] <- "Southeastern"
Region [grep("UNIV SANTA URSULA", Region)] <- "Southeastern"

#NORTHEASTERN
Region [grep("ALAGOAS", Region)] <- "Northeastern"
Region [grep("RIO GRANDE DO NORTE", Region)] <- "Northeastern"
Region [grep("BAHIA", Region)] <- "Northeastern"
Region [grep("CEARA", Region)] <- "Northeastern"
Region [grep("PERNAMBUCO", Region)] <- "Northeastern"
Region [grep("PARAIBA", Region)] <- "Northeastern"
Region [grep("UNIV FED RURAL SEMI ARIDO", Region)] <- "Northeastern"
Region [grep("FDN PRO TAMAR", Region)] <- "Northeastern"
Region [grep("UNIV ESTADUAL SANTA CRUZ", Region)] <- "Northeastern"

# SOUTH
Region [grep("SANTA CATARINA", Region)] <- "South"
Region [grep("UNIV REGIAO JOINVILLE", Region)] <- "South"
Region [grep("RIO GRANDE DO SUL", Region)] <- "South"
Region [grep("RIO GRANDE", Region)] <- "South"
Region [grep("PARANA", Region)] <- "South"

# NORTH
Region [grep("PARA", Region)] <- "North"

# abroad
Region <- ifelse (Region %in% c("North","South","Northeastern","Southeastern") == F,
        "Foreign",
        Region)

# bind 
matched_data_coordinates_mpas_institutions$Region <- Region

# save
save (matched_data_coordinates_mpas_institutions,
	file=  here ("output", "spatial_data_topic_modeling.RData"))

# also in csv
write.csv (matched_data_coordinates_mpas_institutions,
	file= here ("output", "spatial_data_topic_modeling.csv"))


# end

