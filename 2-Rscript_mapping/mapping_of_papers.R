# ---------------------------------- #


# load (or install) needed packages


source ("Rscript/packages.R")

# load data for topic-modeling data for mapping
# load (here("output", "spatial_data_topic_modeling.RData"))
matched_data_coordinates_mpas_institutions <- read.csv(here ("data","spatial_data_topic_modeling_3.csv"))

# remove some papers that are mistakes
matched_data_coordinates_mpas_institutions<-matched_data_coordinates_mpas_institutions [-grep("SIMONE LUIZ RICARDO L 2012", # mollusk from Caatinga
                                                  matched_data_coordinates_mpas_institutions$Paper_ID),]


# adjusting coordinates (gather costal coordinates from provided range)
matched_data_coordinates_mpas_institutions [grep ("BEZERRA IM, 2021, FISH RES",
                                                  matched_data_coordinates_mpas_institutions$Paper_ID),"decimallongitude"] <- -39.38
matched_data_coordinates_mpas_institutions [grep ("BEZERRA IM, 2021, FISH RES",
                                                  matched_data_coordinates_mpas_institutions$Paper_ID),"decimallatitude"] <- -21.17

# number of coordinates per period
#table(matched_data_coordinates_mpas_institutions$data)
# number of papres
length(unique(matched_data_coordinates_mpas_institutions$Paper_ID))

#matched_data_coordinates_mpas_institutions <- matched_data_coordinates_mpas_institutions[which(matched_data_coordinates_mpas_institutions$data == "function"),]
# load south america shapefile
# for mapping, cropping, etc
southAme<- readOGR(dsn= here("data","South_America"),encoding="latin1", 
                   layer="South_America")

# load the coordinates of the BR capitals
capitals <- read.csv (file = here ("data", "capitals.csv"),
                      sep=";",
                      encoding = "latin1")

# convert the data frame into a spatial coordinates object
complet_set_of_coordinates_sp <- matched_data_coordinates_mpas_institutions[,c("decimallongitude", "decimallatitude")]
coordinates(complet_set_of_coordinates_sp) <- ~decimallongitude + decimallatitude # as coord
crs (complet_set_of_coordinates_sp)<- "+proj=longlat +datum=WGS84 +no_defs" # set crs

# create a grid for mapping
# based on the extent of extracted data
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
crs(grd_raster) <-crs(complet_set_of_coordinates_sp)

# -------------------------------------------------
# spatial distribution of studies and topics

# categories of  year  (each 5 yrs)
matched_data_coordinates_mpas_institutions$YearCat  <- sapply(matched_data_coordinates_mpas_institutions$Year, function(x) {
  
                                            if (x<=2005) {"<=2005"} 
                                            else if (x>=2006&x<=2010) {"2006-2010"} 
                                            else if (x>=2011&x<=2015) {"2011-2015"} 
                                            else if (x>=2016&x<=2022) {"2016-2022"}}
              )

# table(matched_data_coordinates_mpas_institutions$YearCat)
data_to_map_year <- unique(matched_data_coordinates_mpas_institutions$YearCat)
complet_set_of_coordinates_DOI_count <- lapply (data_to_map_year, function (i) {
  # subsetting based on DOI (also rm this column )
  data_DOI<-matched_data_coordinates_mpas_institutions [which(matched_data_coordinates_mpas_institutions$YearCat %in% i),
                          c("decimallongitude","decimallatitude")]
  # aggregate
  #data_DOI<-aggregate(data_DOI,by=list(rep(1,nrow(data_DOI))), FUN=mean)
  # convert  data into spatial coordinates
  coordinates(data_DOI) <- ~decimallongitude + decimallatitude
  
  # rasterize to count the number of papers per cell
  overlap_grid_dois <- rasterize(data_DOI,
                                 grd_raster,
                                 fun="count")
  #clamp
  overlap_grid_dois<-clamp(overlap_grid_dois, 
                           lower=0,
                           useValues=T)
  
  
})

# stack rasters from the several papers
stack_DOIS <- (stack(complet_set_of_coordinates_DOI_count))
# sum the number of papers per cell
rs1 <- calc(stack_DOIS, sum,na.rm=T)
rs1_total<-clamp(rs1, lower=0, useValues=T)

# plot - number of sites per cell
plot2 <- gplot(rs1_total) +
  geom_tile(aes(x=x, y=y, fill=value), alpha=1) + 
  coord_fixed (xlim = c( -60, -25), 
               ylim = c(-40, 10), ratio = 1) +
  scale_fill_viridis(option="magma",direction=1,begin=0,
                     breaks = seq (range(values(rs1_total),na.rm=T)[1],
                                   range(values(rs1_total),na.rm=T)[2],
                                   30),
                     limits=c(range(values(rs1_total),na.rm=T)[1]+1,
                              range(values(rs1_total),na.rm=T)[2]),
                     na.value=NA,
                     name="Count") +
  ggtitle ("A) 'Cold' and 'hot' regions") + 
  theme_classic() +
  theme (legend.position = "top",
         legend.direction = "horizontal") + 
  xlab("Longitude")+
  ylab("Latitude")

# add south america map
plot2_total <- plot2 + geom_polygon(data=southAme, 
                     aes(x=long, y=lat, group=group),
                     size = 0.1, fill="gray60", 
                     colour="gray75",alpha=0.1) + 
  xlab("Longitude") + ylab("Latitude")

# add BR capitals
plot2_total <- plot2_total + #geom_point(data=capitals,aes (x=LONG,y=LAT))+
  #coord_quickmap() +
  geom_label_repel(data=capitals,aes(x=LONG,
                                     y=LAT,
                                     label = Capital), 
                   size = 2,
                   direction = "x",
                   nudge_y      = 0,
                   nudge_x      = -10,
                   max.time = 1, max.iter = 1e5, # stop after 1 second, or after 100,000 iterations
                   box.padding = -0.3)




# distribution of sites (gain of cells) per period of five years
# transforming >0 in 1

x <- clamp(stack_DOIS[[4]], lower=1, upper=1, useValues=T)
x1 <- clamp(stack_DOIS[[3]], lower=1, upper=1, useValues=T)
x2 <- clamp(stack_DOIS[[2]], lower=1, upper=1, useValues=T)
x3 <- clamp(stack_DOIS[[1]], lower=1,  upper=1, useValues=T)

# before 2005
y1 <- gplot(x) +
  geom_tile(aes(x=x, y=y, fill=value), alpha=1) + 
  coord_fixed (xlim = c( -57, -25), 
               ylim = c(-40, 10), ratio = 1) +
  scale_fill_gradientn(colors="#87c450",na.value = "white") + 
  ggtitle ("Sites studied until 2005") + 
  theme_classic() + geom_polygon(data=southAme, 
                                 aes(x=long, y=lat, group=group),
                                 size = 0.1, fill="gray60", 
                                 colour="gray75",alpha=0.1) + 
  xlab("Longitude") + ylab("Latitude") + theme(legend.position = "none",
                                               axis.title = element_blank(),
                                               axis.text = element_blank(),
                                               axis.line = element_blank(),
                                               axis.ticks = element_blank(),
                                               plot.title = element_text(size=7))

# 2006-2010
cinco_dez <- merge(stack_DOIS[[3]],stack_DOIS[[4]])
soma_cinco_dez1 <- clamp(cinco_dez, lower=0,upper=1,  useValues=T)
soma_cinco_dez <- sum(soma_cinco_dez1, x,na.rm=T)
cols <- c("0" = "white", "1" = "#2087af", "2" = "#87c450")

y2<- gplot(soma_cinco_dez) +
  geom_tile(aes(x=x, y=y, fill=value), alpha=1) + 
  coord_fixed (xlim = c( -57, -25), 
               ylim = c(-40, 10), ratio = 1) +
  scale_fill_gradientn(colors=cols,na.value="white") + 
  ggtitle ("Gain between 2006 and 2010") + 
  theme_classic() + geom_polygon(data=southAme, 
                                 aes(x=long, y=lat, group=group),
                                 size = 0.1, fill="gray60", 
                                 colour="gray75",alpha=0.1) + 
  xlab("Longitude") + ylab("Latitude")+ theme(legend.position = "none",
                                              axis.title = element_blank(),
                                              axis.text = element_blank(),
                                              axis.line = element_blank(),
                                              axis.ticks = element_blank(),
                                              plot.title = element_text(size=7))


# 2011-2015
cinco_dez_quinze <- merge(cinco_dez,stack_DOIS[[2]])
soma_cinco_dez_quinze1 <- clamp(cinco_dez_quinze, lower=0,upper=1,  useValues=T)
soma_cinco_dez_quinze <- sum(soma_cinco_dez_quinze1, soma_cinco_dez1,na.rm=T)

y3<-gplot(soma_cinco_dez_quinze) +
  geom_tile(aes(x=x, y=y, fill=value), alpha=1) + 
  coord_fixed (xlim = c( -57, -25), 
               ylim = c(-40, 10), ratio = 1) +
  scale_fill_gradientn(colors=cols) + 
  ggtitle ("Gain between 2011 and 2015") + 
  theme_classic() + geom_polygon(data=southAme, 
                                 aes(x=long, y=lat, group=group),
                                 size = 0.1, fill="gray60", 
                                 colour="gray75",alpha=0.1) + 
  xlab("Longitude") + ylab("Latitude")+ theme(legend.position = "none",
                                              axis.title = element_blank(),
                                              axis.text = element_blank(),
                                              axis.line = element_blank(),
                                              axis.ticks = element_blank(),
                                              plot.title = element_text(size=7))


## 2016-2021

cinco_dez_quinze_vinte <- merge( cinco_dez_quinze,stack_DOIS[[1]])
soma_cinco_dez_quinze_vinte1 <- clamp(cinco_dez_quinze_vinte, lower=0,upper=1,  useValues=T)
soma_cinco_dez_quinze_vinte <- sum(soma_cinco_dez_quinze_vinte1, soma_cinco_dez_quinze1,na.rm=T)

y4<-gplot(soma_cinco_dez_quinze_vinte) +
  geom_tile(aes(x=x, y=y, fill=value), alpha=1) + 
  coord_fixed (xlim = c( -57, -25), 
               ylim = c(-40, 10), ratio = 1) +
  scale_fill_gradientn(colors=cols) + 
  ggtitle ("Gain between 2016 and 2022") + 
  theme_classic() + geom_polygon(data=southAme, 
                                 aes(x=long, y=lat, group=group),
                                 size = 0.1, fill="gray60", 
                                 colour="gray75",alpha=0.1) + 
  xlab("Longitude") + ylab("Latitude") + theme(legend.position = "none",
                                               axis.title = element_blank(),
                                               axis.text = element_blank(),
                                               axis.line = element_blank(),
                                               axis.ticks = element_blank(),
                                               plot.title = element_text(size=7))

# -------------------------------------------
# predominance of different topics

# load the list of topics
topic_names <- read.csv (here("data", "topic_names.csv"),sep=";")
# load topic modeling output
topic_modeling_output <- read.csv (here ("data", "M2.csv"),sep=",")
colnames(topic_modeling_output)[-1]<-topic_names$topic


# match datasets
# dimensions are different but the datasets match
table(unique(matched_data_coordinates_mpas_institutions$Paper_ID) %in% unique(topic_modeling_output$Paper_ID))

matched_data_coordinates_mpas_institutions <-  (merge (
  
        matched_data_coordinates_mpas_institutions,
         topic_modeling_output,
        by="Paper_ID"))


# all topics
# where more topics are studied
# thrshold to cover a topic
tp <- 0.1

# get the set of coordinates for each topic
complet_set_topics <- lapply (topic_names$topic, function (i) {

    # subsetting based on DOI (also rm this column )
  col_to_sel<-which(colnames(matched_data_coordinates_mpas_institutions) %in% i)
  data_DOI<-matched_data_coordinates_mpas_institutions [, c("decimallongitude",
                                               "decimallatitude",
                                               colnames(matched_data_coordinates_mpas_institutions)[col_to_sel])] # coordinates
  # subset based on topic cover threshold 
  data_DOI <- data_DOI[which(data_DOI[,3] >= tp),]# the third column always have the probabilities
  
  # aggregate
  #data_DOI<-aggregate(data_DOI,by=list(rep(1,nrow(data_DOI))), FUN=mean)
  # convert  data into spatial coordinates
  coordinates(data_DOI) <- ~decimallongitude + decimallatitude
  
  # rasterize to count the number of papers per cell
  overlap_grid_dois <- rasterize(data_DOI,
                                 grd_raster,
                                 fun="count")
  #clamp
  overlap_grid_dois<-clamp(overlap_grid_dois, 
                           lower=0,
                           useValues=T)
  
  
})

# stack rasters from the several topics
stack_topics <- (stack(complet_set_topics))
stack_topics<-stack_topics [[-grep("ID.",names(stack_topics))]]# remove IDs

# sum the number of sites per cell and topic
rs1_topics <- calc(stack_topics, sum,na.rm=T)
rs1_topics_clamp <-clamp(rs1_topics, 
                         lower=0, 
                         useValues=T)

# number of unique topics per cell
topics_per_cell<-(clamp(stack_topics,lower=0,upper=1))
topics_per_cell <- calc(topics_per_cell, sum,na.rm=T)
topics_per_cell<- topics_per_cell/length(complet_set_topics) #proportion of topics covered

# get the number of sites per topic (low values mean that a given cell have many studies that studied a few topics,
# whereas a high values indicate many sites for the study of many different topics
# sites_per_topic <- topics_per_cell/rs1_topics_clamp

# plot - number of topics per cell
plot_top <- gplot(topics_per_cell) +
  geom_tile(aes(x=x, y=y, fill=value), alpha=1) + 
  coord_fixed (xlim = c( -57, -25), 
               ylim = c(-40, 10), ratio = 1) +
  scale_fill_viridis(option="magma",direction=-1,begin=0,
                     breaks = seq (range(values(topics_per_cell),na.rm=T)[1],
                                   1,#range(values(topics_per_cell),na.rm=T)[2]
                                   0.2),
                     limits=c(range(values(topics_per_cell),na.rm=T)[1]+0.00001,#+1
                              1),#range(values(topics_per_cell),na.rm=T)[2]),
                     na.value=NA) +
  ggtitle ("C) Proportion of topics covered, per cell") + 
  theme_classic() 

# add south america map
plot_top <- plot_top + geom_polygon(data=southAme, 
                                    aes(x=long, y=lat, group=group),
                                    size = 0.1, fill="gray60", 
                                    colour="gray75",alpha=0.1) + 
  xlab("Longitude") + ylab("Latitude")+
  labs(fill='Proportion') + 
  theme(legend.position = "top",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank())

# number of topics per cell
plot_top


# there's a high correlation between the number of studies and topics
cor (values(rs1_total),
     values(topics_per_cell))


# arrange
require(ggpubr)
arranged_plot <-grid.arrange(plot2_total,
                             y1,y2,y3,y4,
                             plot_top,
                             ncol=8,nrow=6,
                             layout_matrix = rbind (c(1,1,1,2,3,6,6,6),
                                                    c(1,1,1,2,3,6,6,6),
                                                    c(1,1,1,2,3,6,6,6),
                                                    c(1,1,1,4,5,6,6,6),
                                                    c(1,1,1,4,5,6,6,6),
                                                    c(1,1,1,4,5,6,6,6)))

# annotate
arranged_plot <- annotate_figure(arranged_plot,
                top = text_grob("B) Spatiotemporal trends", 
                                color = "black",  size = 14))

pdf(here("output","figs","map1.pdf"),width=12,heigh=6)
arranged_plot
dev.off()

# -----------------------------------------

# select topics to show in the map



# the 10 more frequent
selected_topics <- colSums(matched_data_coordinates_mpas_institutions[,7:31])[order(colSums(matched_data_coordinates_mpas_institutions[,7:31]),decreasing = T)]
n_sel_topics<- 8 # the number of selected topics 
selected_topics <- names(selected_topics)[1:n_sel_topics]
selected_topics <- gsub (" ", ".", selected_topics)

# subset the dataset of rasters
stack_topics_sub <- stack_topics[[which(names(stack_topics) %in% selected_topics)]]# rm IDs
stack_topics_sub<-stack_topics_sub[[match(selected_topics,names(stack_topics_sub))]]# match names of most frequent

# check the ranges
range(values(stack_topics_sub[[8]]),na.rm=T)

# taxonomy

maps_topics <- lapply (seq (1,dim(stack_topics_sub)[3]), function (i) {

  to_map <- clamp(stack_topics_sub[[i]], 
                  lower=0, 
                  useValues=T)
  # 
  plot_topic <- gplot(to_map) +
    geom_tile(aes(x=x, y=y, fill=value), alpha=1) + 
    coord_fixed (xlim = c( -57, -25), 
               ylim = c(-40, 10), ratio = 1) +
    scale_fill_viridis(option="magma", direction=-1,begin=0,
                     breaks = seq (range(values(stack_topics_sub[[i]]),na.rm=T)[1], # legend always the same
                                   range(values(stack_topics_sub[[i]]),na.rm=T)[2],# taxonomy because it was the most frequent topic
                                   5),
                     limits=c(range(values(stack_topics_sub[[8]]),na.rm=T)[1]+1,
                              range(values(stack_topics_sub[[8]]),na.rm=T)[2]),
                     na.value=NA) +
  ggtitle (names(stack_topics_sub)[i]) + 
  theme_classic() 
  # add south america map
  plot_topic <- plot_topic + geom_polygon(data=southAme, 
                                    aes(x=long, y=lat, group=group),
                                    size = 0.1, fill="gray60", 
                                    colour="gray75",alpha=0.1) + 
  xlab("Longitude") + ylab("Latitude")+
  labs(fill='Count') + theme(title = element_text(size=8),
                             axis.text = element_blank(),
                             axis.title = element_blank(),
                             axis.ticks = element_blank(),
                             axis.line = element_blank())
  ;
  plot_topic
})

# arrange maps
pdf(here("output", "figs","map2.pdf"),height=10,width=12)

grid.arrange (maps_topics[[1]]+theme(legend.position = "none"),
              maps_topics[[2]]+theme(legend.position = "none"),
              maps_topics[[3]]+theme(legend.position = "none"),
              maps_topics[[4]]+theme(legend.position = "none"),
              maps_topics[[5]]+theme(legend.position = "none"),
              maps_topics[[6]]+theme(legend.position = "none"),
              maps_topics[[7]]+theme(legend.position = "none"),
              maps_topics[[8]]+theme(legend.position = c(0.9, 0.25), 
                                     legend.key.width=unit(0.55,"cm"),
                                     legend.key.height=unit(0.45,"cm"),
                                     legend.key.size = unit(0.4,"cm"),
                                     legend.text = element_text(color = "black", size = 7),
                                     strip.text = element_text(size=5),
                                     legend.title = element_text (size=7)),
              
              ncol=4,nrow=2,
              layout_matrix = cbind (c(1,1,1,5,5,5),
                                     c(1,1,1,5,5,5),
                                     c(2,2,2,6,6,6),
                                     c(2,2,2,6,6,6),
                                     c(3,3,3,7,7,7),
                                     c(3,3,3,7,7,7),
                                     c(4,4,4,8,8,8),
                                     c(4,4,4,8,8,8)
                                     ))


dev.off()
  
# ----------------------

# number of papers per cell
# unique DOIs
list_dois <- unique(matched_data_coordinates_mpas_institutions$DOI)

# subsetting per DOI - apply to the complete list
complet_set_of_coordinates_DOI_count <- lapply (list_dois, function (i) {
  # subsetting based on DOI (also rm this column )
  data_DOI<-matched_data_coordinates_mpas_institutions [which(matched_data_coordinates_mpas_institutions$DOI %in% i),c("decimallongitude","decimallatitude")]
                                              
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
                           useValues=T)
  
  
})

# stack rasters from the several papers
stack_DOIS <- (stack(complet_set_of_coordinates_DOI_count))
# sum the number of papers per cell
rs1_np <- calc(stack_DOIS, sum,na.rm=T)
#rs1<-clamp(rs1, lower=1, useValues=FALSE)

# plot - number of papers

plot2 <- gplot(rs1_np) +
  geom_tile(aes(x=x, y=y, fill=value), alpha=1) + 
  coord_fixed (xlim = c( -57, -25), 
               ylim = c(-40, 10), ratio = 1) +
  scale_fill_viridis(option="magma",direction=-1,begin=0,
                     breaks = seq (range(values(rs1_np),na.rm=T)[1],
                                   range(values(rs1_np),na.rm=T)[2],
                                   8),
                     limits=c(range(values(rs1_np),na.rm=T)[1]+1,
                              range(values(rs1_np),na.rm=T)[2]),
                     na.value=NA) +
  ggtitle ("Number of articles per cell") + 
  theme_classic() 

plot2 <- plot2 + geom_polygon(data=southAme, 
                     aes(x=long, y=lat, group=group),
                     size = 0.1, fill="gray60", 
                     colour="gray75",alpha=0.1) + 
  xlab("Longitude") + ylab("Latitude")+
  labs(fill='# articles') +
  theme (legend.position = c(0.9, 0.22))


pdf(here("output", "figs","#articles.pdf"),height=8,width=10)
plot2
dev.off()


# end of the script