
library(sf)
library(ggplot2)
library(cowplot)
library(ggspatial)
library(raster)
library(ggrepel)
library(ggnewscale) 
library(rnaturalearth)                                   #Continentes
library(rnaturalearthdata) 
library(data.table)

### para cargar y trazar datos de RGBIF

occurrence <- fread("mindoro_occurrence.txt")
occurrence
View(occurrence)
names(occurrence)
occurrence_select <- occurrence %>%
  dplyr::select(date, stateProvince, decimalLatitude, decimalLongitude, kingdom, phylum, class, order, family, genus, taxonRank, hasCoordinate, species)

occurrence_select 
View(occurrence_select)

occurrence_sp <- SpatialPoints(cbind(occurrence_select$decimalLongitude, occurrence_select$decimalLatitude), proj4string = CRS("+init=epsg:4326"))

occurrence_sp
View(occurrence_sp)

occurrence_sp <- data.frame(occurrence_select)
View(occurrence_sp)

occurrence_map <- ggplot()+
  geom_point(data= occurrence_sp, aes(x=decimalLongitude, y=decimalLatitude, color=phylum),size=1) + scale_color_viridis_d(name="Phylum") +
  coord_sf(xlim = c(120.2819, 121.7367 ), ylim = c(12.09474 , 13.77188)) +
  theme_bw() +
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  scale_x_continuous(name=expression(paste("Longitude (",degree,")"))) +
  scale_y_continuous(name=expression(paste("Latitude (",degree,")")))

occurrence_map

### 1. Quiero superponer el occurrence_sp al mapa de elevación
Filipinas    <- getData('GADM', country='PHL', level=1) %>% st_as_sf()

my_bbox <- c(xmin = min(120.291657), xmax = max(121.739758),  ymin = min(12.103986),  ymax = max(13.761049))
my_bbox.m <- matrix(c(my_bbox['xmin'], my_bbox['xmin'], my_bbox['xmax'], my_bbox['xmax'], my_bbox['xmin'],  my_bbox['ymax'], my_bbox['ymin'], my_bbox['ymin'], my_bbox['ymax'], my_bbox['ymax']), ncol = 2)

my_bbox.sf <- st_geometry(st_polygon(x = list(my_bbox.m)))
st_crs(my_bbox.sf) <- 4326

my_bb<-  my_bbox.sf %>% st_transform(crs = 32632) %>%
  st_transform(crs = 4326)

Zona<- st_transform(my_bb,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
world        <- ne_countries(scale= "small", returnclass = "sf") # Continentes del mundo
Zona_box = st_intersection( Filipinas, Zona)

library(elevatr)
elev = get_elev_raster(Zona_box  , z=12)
plot(elev)
Poligo_alt    <- crop(elev, Zona_box )                           #
Poligo_alt   <- Poligo_alt <- mask(Poligo_alt, Zona_box )
plot(Poligo_alt)

slopee    = terrain(Poligo_alt  , opt = "slope")
aspecte    = terrain(Poligo_alt, opt = "aspect")
hille     = hillShade(slopee, aspecte, angle = 40, direction = 270)

hill.p        <-  rasterToPoints(hille)
hill.pa_      <-  data.frame(hill.p)

colores<- c("#015B26", "#3F8433", "#8BAB56", "#CFCF7D", "#C49450", "#AA5B32", "#872921")
cortes <- c(200, 500,1000,2000,3000,4000,5000, 6500)


Geo_data       <-  rasterToPoints(Poligo_alt)
Geo_data_frame <-  data.frame(Geo_data)
colnames(Geo_data_frame) <- c("x","y", "alt")


Macro=ggplot()+
  geom_sf(data = Filipinas, fill="gray")+
  geom_sf(data = world, fill="gray90")+
  geom_sf(data = Zona, fill="red", size=0.05)+
  geom_sf_text(data =world, aes(label=sovereignt) , size =2)+
  coord_sf(xlim = c(100, 140 ), ylim = c(-12 ,40)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        
        panel.background = element_rect(fill = "#a2d2ff"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))

Pais= ggplot()+
  geom_sf(data = Filipinas, fill="gray")+
  geom_sf(data = Zona, fill="red", size=0.05)+
  coord_sf(xlim = c(116.9283, 128), ylim = c(4.58694 , 21.07014)) +
  geom_sf_text(data =world, aes(label=sovereignt) , size =2)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        
        panel.background = element_rect(fill = "#a2d2ff"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))

Mapa = ggplot()+
  geom_sf(data = Filipinas,  fill=NA, color="gray30", size=0.8)+
  geom_point(data= occurrence_sp, aes(x=decimalLongitude, y=decimalLatitude, color=phylum),size=1) + scale_color_viridis_d(name="Phylum") +
  coord_sf(xlim = c(120.2819, 121.7367 ), ylim = c(12.09474 , 13.77188)) +
  
  theme(legend.text = element_text(size=8,face = c(rep("italic", 5), rep("plain", 5))),
        legend.title = element_text(size=8, family="serif", face='bold',hjust=0.5),
        axis.text.y  = element_text(angle = 90,face="bold", color="black",family="serif",size=11),
        axis.text.x  = element_text(face="bold", color="black", size=11,family="serif"),
        panel.background = element_rect(fill = "#a2d2ff"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        
  )+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  annotation_north_arrow(location="tr",which_north="true",style=north_arrow_fancy_orienteering ())+
  scale_x_continuous(name=expression(paste("Longitude (",degree,")"))) +
  scale_y_continuous(name=expression(paste("Latitude (",degree,")")))+
  annotate(geom = "text", x = 120.4, y = 12.5, hjust = 0, vjust = 1,  label = "Mindoro Strait",size = 3, family="serif", color =  "blue",  fontface="italic", face = "bold") +
  annotate(geom = "text", x = 120.3, y = 13.3, hjust = 0, vjust = 1,  label = "Paluan Bay",size = 3, family="serif", color =  "blue",  fontface="italic", face = "bold") +
  annotate(geom = "text", x = 121, y = 13.5, hjust = 0, vjust = 1,  label = "Verde Island Passage",size = 3, family="serif", color =  "blue",  fontface="italic", face = "bold") +
  annotate(geom = "text", x = 121.6, y = 12.4, hjust = 0, vjust = 1,  label = "Tablas Strait",size = 3, family="serif", color =  "blue",  fontface="italic", face = "bold") +
  annotate(geom = "text", x = 121, y = 12.8, hjust = 0, vjust = 1,  label = "OCCIDENTAL \nMINDORO",size = 4, family="serif", color =  "#3a5a40",  face = "bold")+
  annotate(geom = "text", x = 121.3, y = 12.8, hjust = 0, vjust = 1,  label = "ORIENTAL \nMINDORO",size = 4, family="serif", color =  "#3a5a40",   face = "bold")+
  guides(color = guide_legend(nrow = 11, ncol=4))

library(ggpubr)
legend <- get_legend(Mapa)

Mapa_final= Mapa + theme(legend.position = "none")


Mapa_eleva= ggplot()+
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+ scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_raster(data = Geo_data_frame  ,aes(x,y, fill = alt), alpha=0.6)+
  scale_fill_gradientn(colours = colores,
                       breaks = cortes ,
                       na.value = 'white',
                       labels = c("[menor a - 270] ","[270 - 499]", "[500 - 999]", "[1000 - 1999]", "[2000 - 2999]",
                                  "[3000 - 3999]", "[4000 - 4999]", "[5000 -6500]"),
                       name='Elevacion \n(msnm)')+
  geom_sf(data = Filipinas,  fill=NA, color="gray30", size=0.8)+
  geom_point(data= occurrence_sp, aes(x=decimalLongitude, y=decimalLatitude, color=phylum),size=1, show.legend = F) + 
  scale_color_viridis_d(name="Phylum") +
  coord_sf(xlim = c(120.2819, 121.7367 ), ylim = c(12.09474 , 13.77188)) +
  
  theme(legend.text = element_text(size=8,face = c(rep("italic", 5), rep("plain", 5))),
        legend.title = element_text(size=8, family="serif", face='bold',hjust=0.5),
        axis.text.y  = element_text(angle = 90,face="bold", color="black",family="serif",size=11),
        axis.text.x  = element_text(face="bold", color="black", size=11,family="serif"),
        panel.background = element_rect(fill = "#a2d2ff"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
  )+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  annotation_north_arrow(location="tr",which_north="true",style=north_arrow_fancy_orienteering ())+
  scale_x_continuous(name=expression(paste("Longitude (",degree,")"))) +
  scale_y_continuous(name=expression(paste("Latitude (",degree,")")))+
  annotate(geom = "text", x = 120.4, y = 12.5, hjust = 0, vjust = 1,  label = "Mindoro Strait",size = 3, family="serif", color =  "blue",  fontface="italic", face = "bold") +
  annotate(geom = "text", x = 120.3, y = 13.3, hjust = 0, vjust = 1,  label = "Paluan Bay",size = 3, family="serif", color =  "blue",  fontface="italic", face = "bold") +
  annotate(geom = "text", x = 121, y = 13.5, hjust = 0, vjust = 1,  label = "Verde Island Passage",size = 3, family="serif", color =  "blue",  fontface="italic", face = "bold") +
  annotate(geom = "text", x = 121.6, y = 12.4, hjust = 0, vjust = 1,  label = "Tablas Strait",size = 3, family="serif", color =  "blue",  fontface="italic", face = "bold") +
  annotate(geom = "text", x = 121, y = 12.8, hjust = 0, vjust = 1,  label = "OCCIDENTAL \nMINDORO",size = 4, family="serif", color =  "#3a5a40",  face = "bold")+
  annotate(geom = "text", x = 121.3, y = 12.8, hjust = 0, vjust = 1,  label = "ORIENTAL \nMINDORO",size = 4, family="serif", color =  "#3a5a40",   face = "bold")+
  guides(color = guide_legend(nrow = 11, ncol=4))+
  guides(fill = guide_legend(
    title = " msnm.",
    direction = "horizontal",
    nrow = 1,
    keywidth = 1.75,
    keyheight = 0.5,
    label.position = "bottom",
    title.position = "right",
    override.aes = list(alpha = 1)
  ))

legend_ele <- get_legend(Mapa_eleva)

Mapa_final_ele= Mapa_eleva + theme(legend.position = "none")

library(cowplot)
Expo = ggdraw() +
  coord_equal(xlim = c(0, 29), ylim = c(0, 21), expand = FALSE) +
  draw_plot(Macro, width = 4, height = 6,x = 19.5, y = 15)+
  draw_plot(Pais, width = 4, height = 6,x = 23.6, y = 15)+
  
  draw_plot(legend , width = 5, height = 5,x = 22, y = 5)+
  draw_plot(legend_ele , width = 6, height = 3,x = 21, y = 12)+
  
  draw_plot(Mapa_final_ele, width = 21, height = 21,x = 0, y = 0)+
  theme(panel.background = element_rect(fill = "white"))


ggsave(plot=Expo ,"Mapa de dispersion Filipinas.png",units = "cm",width = 29, #X
       height = 21, #Y
       dpi=1200)












### 2. Quiero contar el número total de especies para < 0m, 0-10m, 10-20m, 20-50m, 50-100m, 100-200m, 200-500m, 500-1000m, > 1000m

occurrence <- fread("mindoro_occurrence.txt")

occurrence_select <- occurrence %>%
  dplyr::select(date, stateProvince, decimalLatitude, decimalLongitude, kingdom, phylum, class, order, family, genus, taxonRank, hasCoordinate, species)
occurrence_select 
occurrence_sp <- SpatialPoints(cbind(occurrence_select$decimalLongitude, occurrence_select$decimalLatitude), proj4string = CRS("+init=epsg:4326"))
occurrence_sp
occurrence_sp <- data.frame(occurrence_select)

class(occurrence_sp)
colnames(occurrence_sp) <- c("date","stateProvince", "y", "x", "kingdom", "phylum", "class", "order", "family", "genus", "taxonRank", "hasCoordinate", "species")


Filipinas    <- getData('GADM', country='PHL', level=1) %>% st_as_sf()

my_bbox <- c(xmin = min(120.291657), xmax = max(121.739758),  ymin = min(12.103986),  ymax = max(13.761049))
my_bbox.m <- matrix(c(my_bbox['xmin'], my_bbox['xmin'], my_bbox['xmax'], my_bbox['xmax'], my_bbox['xmin'],  my_bbox['ymax'], my_bbox['ymin'], my_bbox['ymin'], my_bbox['ymax'], my_bbox['ymax']), ncol = 2)

my_bbox.sf <- st_geometry(st_polygon(x = list(my_bbox.m)))
st_crs(my_bbox.sf) <- 4326

my_bb<-  my_bbox.sf %>% st_transform(crs = 32632) %>%
  st_transform(crs = 4326)

Zona<- st_transform(my_bb,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
world        <- ne_countries(scale= "small", returnclass = "sf") # Continentes del mundo
Zona_box = st_intersection( Filipinas, Zona)


library(elevatr)
elev = get_elev_raster(Zona_box  , z=12)
Poligo_alt    <- crop(elev, Zona_box )                           #
Poligo_alt   <- Poligo_alt <- mask(Poligo_alt, Zona_box )
plot(Poligo_alt)

Geo_data       <-  rasterToPoints(Poligo_alt)
Geo_data_frame <-  data.frame(Geo_data)
colnames(Geo_data_frame) <- c("x","y", "alt")


library(dplyr)

SPP_10m = Geo_data_frame%>% subset(alt<= 10 & alt> 0)  
SPP_20m = Geo_data_frame%>% subset(alt<= 20 & alt> 10) 
SPP_50m = Geo_data_frame%>% subset(alt<= 50 & alt> 20) 
SPP_100m = Geo_data_frame%>% subset(alt<= 100 & alt> 50) 
SPP_200m = Geo_data_frame%>% subset(alt<= 200 & alt> 100) 
SPP_500m = Geo_data_frame%>% subset(alt<= 500 & alt> 200) 
SPP_1000m = Geo_data_frame%>% subset(alt<= 1000 & alt> 500) 
SPP_10000m = Geo_data_frame%>% subset(alt<= 9000 & alt> 1000) 

occurrence_sp_SF <- occurrence_sp %>% st_as_sf(coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84 +no_defs")
Altitud_10m <- SPP_10m        %>% st_as_sf(coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84 +no_defs")

Spp_10m_filtro = st_intersection(occurrence_sp_SF, Altitud_10m)

ggplot()+
  geom_sf(data = occurrence_sp_SF)







