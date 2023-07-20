# Library

library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(DT) # datatable

options(scipen = 99) # me-non-aktifkan scientific notation
library(dplyr) # koleksi beberapa package R
library(plotly) # plot interaktif
library(glue) # setting tooltip
library(scales) # mengatur skala pada plot
library(lubridate) #date format
library(readxl) #read excel
library(tidyr) #wide to long format
library(leaflet) #mapchart
#library(png)
#library(ggpubr)

library(sf)
library(tableHTML)
#library(raster)
#library(RColorBrewer)
#library(leaflegend)


# Read Data Upah Minimum Provinsi

ump1 <- read_excel("datasets/Upah Minimum Provinsi 2021-2022.xlsx") #2021-2022
ump2 <- read_excel("datasets/Upah Minimum Regional_Propinsi.xlsx")[-c(1,37:40),] #2018-2020
ump3 <- read_excel("datasets/Upah Minimum Regional_Propinsi (1).xlsx")[-c(1,37:40),] #2014-2017
ump4 <- read_excel("datasets/Upah Minimum Regional_Propinsi (2).xlsx")[-c(1,37:40),] #2011-2013
ump5 <- read_excel("datasets/Upah Minimum Regional_Propinsi (3).xlsx")[-c(1,37:40),] #2008-2010
ump6 <- read_excel("datasets/Upah Minimum Regional_Propinsi (4).xlsx")[-c(1,37:40),] #2005-2007
ump7 <- read_excel("datasets/Upah Minimum Regional_Propinsi (5).xlsx")[-c(1,37:40),] #2002-2004

#merge
ump <- cbind.data.frame(ump7,ump6[,-1],ump5[,-1],ump4[,-1],ump3[,-1],ump2[,-1],ump1[,-1])
rm(ump1,ump2,ump3,ump4,ump5,ump6,ump7)

#memberi nama kolom
v <- c()
for (i in c(2002:2022)){
  v <- c(v,paste("ump.",i,sep=""))
}

colnames(ump)[1] <- "provinsi"
colnames(ump)[2:22] <- as.character(v)

for (i in 2:22){
  ump[,i] <- as.integer(ump[,i])
}


#wide format to long format data
ump.df <- pivot_longer(
  data = ump,
  cols = starts_with("ump."),
  names_to = "tahun",
  names_prefix = "ump.",
  values_to = "ump"
)

glimpse(ump.df)

ump.df <- ump.df %>%
  mutate(provinsi = as.factor(provinsi)) %>%
  mutate(tahun = as.integer(tahun))

#save dataset
write.csv(ump,"datasets/ump.csv",row.names = FALSE)
write.csv(ump.df,"datasets/ump.df.csv",row.names = FALSE)


## Read Data Upah Pekerja per Jam
upah1 <- read_excel("datasets/Upah Rata - Rata Per Jam Pekerja Menurut Provinsi.xlsx")[-c(1,37:40),] #2020-2022
upah2 <- read_excel("datasets/Upah Rata - Rata Per Jam Pekerja Menurut Provinsi (1).xlsx")[-c(1,37:40),] #2017-2019
upah3 <- read_excel("datasets/Upah Rata - Rata Per Jam Pekerja Menurut Provinsi (2).xlsx")[-c(1,37:40),] #2014-2016


#merge
upah <- cbind.data.frame(upah3,upah2[,-1],upah1[,-1])
rm(upah1,upah2,upah3)

#memberi nama kolom
w <- c()
for (i in c(2015:2022)){
  w <- c(w,paste("upah.",i,sep=""))
}

colnames(upah)[1] <- c("provinsi")
colnames(upah)[2:9] <- w

for (i in 2:9){
  upah[,i] <- as.integer(upah[,i])
}

#wide format to long format data
upah.df <- pivot_longer(
  data = upah,
  cols = starts_with("upah."),
  names_to = "tahun",
  names_prefix = "upah.",
  values_to = "upah"
)

glimpse(upah.df)

upah.df <- upah.df %>%
  mutate(provinsi = as.factor(provinsi)) %>%
  mutate(tahun = as.integer(tahun))

#save dataset
write.csv(upah,"datasets/upah.csv",row.names = FALSE)
write.csv(upah.df,"datasets/upah.df.csv",row.names = FALSE)

#Read Data Rata-rata Pengeluaran Per Kapita
peng.total <- read_excel("datasets/Rata-rata Pengeluaran Per Kapita.xls")[-c(1:3,39:45),] #2011-2022 total
peng.desa <- read_excel("datasets/Rata-Rata Pengeluaran Per Kapita Perdesaan.xls")[-c(1:3,39:45),] #2011-2022 perdesaan
peng.kota <- read_excel("datasets/Rata-Rata Pengeluaran Per Kapita Perkotaan.xls")[-c(1:3,39:45),] #2011-2022 perkotaan

peng <- cbind.data.frame(peng.desa,peng.kota[,-1],peng.total[,-1])
rm(peng.total,peng.desa,peng.kota)

#memberi nama kolom 
x <- c()
for (i in c("perdesaan","perkotaan")){
  for (j in c("makanan","nonmakanan","total")){
    for (k in 2007:2022){
      x <- c(x,paste("peng.",i,".",j,".",k,sep=""))
    }
  }
}

for (i in c("makanan","nonmakanan","total")){
  for (j in 2011:2022){
    x <- c(x,paste("peng.perdesaanperkotaan.",i,".",j,sep=""))
  }
}

colnames(peng)[1] <- c("provinsi")
colnames(peng)[2:133] <- x

for (i in 2:133){
  peng[,i] <- as.integer(peng[,i])
}

#wide format to long format data
peng.df <- pivot_longer(
  data = peng,
  cols = starts_with("peng."),
  names_to = c("daerah","jenis","tahun"),
  #names_prefix = "peng.",
  #names_sep = ".",
  names_pattern = 'peng.([A-Za-z]+).([A-Za-z]+).([0-9]+)',
  values_to = "peng"
)

glimpse(peng.df)

peng.df <- peng.df %>%
  mutate(provinsi = as.factor(provinsi)) %>%
  mutate(daerah = as.factor(toupper(daerah))) %>%
  mutate(jenis = as.factor(toupper(jenis))) %>%
  mutate(tahun = as.integer(tahun))

#simpan dataset
write.csv(peng,"datasets/peng.csv",row.names = FALSE)
write.csv(peng.df,"datasets/peng.df.csv",row.names = FALSE)


#Read Data GARIS KEMISKINAN PER KAPITA
#2013-2022 TOTAL
gk1 <- read_excel("datasets/Garis Kemiskinan (Rupiah_Kapita_Bulan) Menurut Provinsi dan Daerah .xlsx")[-c(1:3,39:45),-c(4,7,10,13,16,19)] #2021-2022
gk2 <- read_excel("datasets/Garis Kemiskinan (Rupiah_Kapita_Bulan) Menurut Provinsi dan Daerah  (1).xlsx")[-c(1:3,39:45),-c(4,7,10,13,16,19)] #2019-2020
gk3 <- read_excel("datasets/Garis Kemiskinan (Rupiah_Kapita_Bulan) Menurut Provinsi dan Daerah  (2).xlsx")[-c(1:3,39:45),-c(4,7,10,13,16,19)] #2017-2018
gk4 <- read_excel("datasets/Garis Kemiskinan (Rupiah_Kapita_Bulan) Menurut Provinsi dan Daerah  (3).xlsx")[-c(1:3,39:45),-c(4,7,10,13,16,19)] #2015-2016
gk5 <- read_excel("datasets/Garis Kemiskinan (Rupiah_Kapita_Bulan) Menurut Provinsi dan Daerah  (4).xlsx")[-c(1:3,39:45),-c(4,7,10,13,16,19)] #2013-2014

#merge
gk <- cbind.data.frame(gk5,gk4[,-1],gk3[,-1],gk2[,-1],gk1[-1])
rm(gk1,gk2,gk3,gk4,gk5)

#memberi nama kolom
y <- c()
y.daerah <- rep(c("perkotaan","perdesaan","perdesaanperkotaan"),5,each=4)
y.tahun <- c(rep(2013:2014,3,each=2),
             rep(2015:2016,3,each=2),
             rep(2017:2018,3,each=2),
             rep(2019:2020,3,each=2),
             rep(2021:2022,3,each=2))
y.semester <- rep(c("maret","september"),30)

for (i in 1:60){
  y <- c(y,paste("gk.total.",y.daerah[i],".",y.tahun[i],".",y.semester[i],sep=""))     
}

colnames(gk)[1] <- c("provinsi")
colnames(gk)[2:61] <- y

# 2015-2022 MAKANAN
gkm1 <- read_excel("datasets/Garis Kemiskinan Makanan (Rupiah_Kapita_Bulan) Menurut Provinsi dan Daerah.xlsx")[-c(1:3,39:45),
                                                                                                               -c(4,7,10,13,16,19)] #2021-2022
gkm2 <- read_excel("datasets/Garis Kemiskinan Makanan (Rupiah_Kapita_Bulan) Menurut Provinsi dan Daerah (1).xlsx")[-c(1:3,39:45),
                                                                                                                   -c(4,7,10,13,16,19)] #2019-2020
gkm3 <- read_excel("datasets/Garis Kemiskinan Makanan (Rupiah_Kapita_Bulan) Menurut Provinsi dan Daerah (2).xlsx")[-c(1:3,39:45),
                                                                                                                   -c(4,7,10,13,16,19)] #2017-2018
gkm4 <- read_excel("datasets/Garis Kemiskinan Makanan (Rupiah_Kapita_Bulan) Menurut Provinsi dan Daerah (3).xlsx")[-c(1:3,39:45),
                                                                                                                   -c(4,7,10,13,16,19)] #2015-2016

#merge
gkm <- cbind.data.frame(gkm4,gkm3[,-1],gkm2[,-1],gkm1[-1])
rm(gkm1,gkm2,gkm3,gkm4)

#memberi nama kolom
y1 <- c()
y1.daerah <- rep(c("perkotaan","perdesaan","perdesaanperkotaan"),4,each=4)
y1.tahun <- c(rep(2015:2016,3,each=2),
              rep(2017:2018,3,each=2),
              rep(2019:2020,3,each=2),
              rep(2021:2022,3,each=2))
y1.semester <- rep(c("maret","september"),24)

for (i in 1:48){
  y1 <- c(y1,paste("gk.makanan.",y1.daerah[i],".",y1.tahun[i],".",y1.semester[i],sep=""))     
}

colnames(gkm)[1] <- c("provinsi")
colnames(gkm)[2:49] <- y1


# 2015-2022 BUKAN MAKANAN
gknm1 <- read_excel("datasets/Garis Kemiskinan Non-Makanan (Rupiah_Kapita_Bulan) Menurut Provinsi dan Daerah.xlsx")[-c(1:3,39:45),
                                                                                                                    -c(4,7,10,13,16,19)] #2021-2022
gknm2 <- read_excel("datasets/Garis Kemiskinan Non-Makanan (Rupiah_Kapita_Bulan) Menurut Provinsi dan Daerah (1).xlsx")[-c(1:3,39:45),
                                                                                                                        -c(4,7,10,13,16,19)] #2019-2020
gknm3 <- read_excel("datasets/Garis Kemiskinan Non-Makanan (Rupiah_Kapita_Bulan) Menurut Provinsi dan Daerah (2).xlsx")[-c(1:3,39:45),
                                                                                                                        -c(4,7,10,13,16,19)] #2017-2018
gknm4 <- read_excel("datasets/Garis Kemiskinan Non-Makanan (Rupiah_Kapita_Bulan) Menurut Provinsi dan Daerah (3).xlsx")[-c(1:3,39:45),
                                                                                                                        -c(4,7,10,13,16,19)] #2015-2016

#merge
gknm <- cbind.data.frame(gknm4,gknm3[,-1],gknm2[,-1],gknm1[-1])
rm(gknm1,gknm2,gknm3,gknm4)

#memberi nama kolom
y2 <- c()
y1.daerah <- rep(c("perkotaan","perdesaan","perdesaanperkotaan"),4,each=4)
y1.tahun <- c(rep(2015:2016,3,each=2),
              rep(2017:2018,3,each=2),
              rep(2019:2020,3,each=2),
              rep(2021:2022,3,each=2))
y1.semester <- rep(c("maret","september"),24)

for (i in 1:48){
  y2 <- c(y2,paste("gk.nonmakanan.",y1.daerah[i],".",y1.tahun[i],".",y1.semester[i],sep=""))     
}

colnames(gknm)[1] <- c("provinsi")
colnames(gknm)[2:49] <- y2

#merge
gk <- cbind.data.frame(gkm,gknm[,-1],gk[,-1])


for (i in 2:157){
  gk[,i] <- as.integer(gk[,i])
}


#wide format to long format data
gk.df <- pivot_longer(
  data = gk,
  cols = starts_with("gk."),
  names_to = c("jenis","daerah","tahun","periode"),
  names_pattern = 'gk.([A-Za-z]+).([A-Za-z]+).([0-9]+).([A-Za-z]+)',
  values_to = "gk"
)

glimpse(gk.df)

gk.df <- gk.df %>%
  mutate(provinsi = as.factor(provinsi)) %>%
  mutate(daerah = as.factor(toupper(daerah))) %>%
  mutate(jenis = as.factor(toupper(jenis))) %>%
  mutate(periode = as.factor(toupper(periode))) %>%
  mutate(tahun = as.integer(tahun))

#simpan dataset
write.csv(gk,"datasets/gk.csv",row.names = FALSE)
write.csv(gk.df,"datasets/gk.df.csv",row.names = FALSE)

# #import data .shp file
# library("rgdal")
# #import .shp data
# idn_shape <- readOGR(dsn = path.expand("assets/IDN_adm_shp"), layer="IDN_adm1")
# idn_shape$NAME_1 <- toupper(idn_shape$NAME_1)
# idn_shape$NAME_1[c(3,7,8,18,34)] <- c("KEP. BANGKA BELITUNG", "PAPUA BARAT", "DKI JAKARTA", "KEP. RIAU", "DI YOGYAKARTA")
# #format data
# idn_shape_df <- fortify(idn_shape)
# 
# dict_prov_id = data.frame(idn_shape$ID_1, idn_shape$NAME_1)
# colnames(dict_prov_id) <- c("id", "provinsi") #rename the column name
# idn_shape_df$id <- as.integer(idn_shape_df$id) + 1 #transform id value to be similar with id on idn_shape table due to different initial index
# idn_shape_df$id <- with(dict_prov_id, provinsi[match(idn_shape_df$id, id)])
# colnames(idn_shape_df)[6] <- "provinsi"
# 
# 
# #need this to remove background and outline of graph
# ditch_the_axes <- theme(
#   axis.text = element_blank(),
#   axis.line = element_blank(),
#   axis.ticks = element_blank(),
#   panel.border = element_blank(),
#   panel.grid = element_blank(),
#   axis.title = element_blank()
# )

#-------

#map_ina <- readRDS("assets/IDN_adm1.rds")
#
#map_ina@data$NAME_1 <- toupper(map_ina@data$NAME_1)
#map_ina@data$NAME_1[c(3,7,8,18,34)] <- c("KEP. BANGKA BELITUNG", "PAPUA BARAT", "DKI JAKARTA", "KEP. RIAU", "DI YOGYAKARTA")
#colnames(map_ina@data)[6] <- "provinsi"

map_indo <- read_sf("assets/indonesia.geojson")
map_indo$state <- toupper(map_indo$state)
map_indo$state[c(3,5,11,27)] <- c("DI YOGYAKARTA","KEP. BANGKA BELITUNG","KEP. RIAU","DKI JAKARTA")
colnames(map_indo)[5] <- "provinsi"

### prepare label
#mytext <- paste(map_ina@data$provinsi) %>%
#  lapply(htmltools::HTML)
#
### prepare pop-up
#popup_shape <- paste("<h3><b>", map_ina@data$provinsi, "</b></h3>", 
#                     "Status: ", map_ina@data$Status, "<br>", 
#                     "Ecological Footprint: ", map_ina@data$Total.Ecological.Footprint, " gha <br>",
#                     "Biocapacity: ", map_ina@data$Total.Biocapacity, " gha <br>",
#                     "HDI: ", map_ina@data$HDI, "<br>",
#                     "GDP per Capita: ", "$", map_ina@data$GDP.per.Capita, "<br>", 
#                     sep="")