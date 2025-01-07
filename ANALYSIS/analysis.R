packages <-c('dplyr','magrittr','tidyverse','corrr',
             'ggplot2','ggcorrplot','ggpubr','PerformanceAnalytics','ggtext','sf',
             'ResourceSelection')
for(p in packages) if(p %in% rownames(installed.packages()) == F) { install.packages(p) }
for(p in packages) suppressPackageStartupMessages(library(p,quietly=T,character.only=T))

setwd("C:/Users/sfield3/OneDrive - University of Wyoming/RESEARCH/PROJECT/CLIMATE AND PEACE")
theme_set(theme_bw())


################################################################################
### Step 1: Read in Data ###
################################################################################
epi <- read.csv("https://raw.githubusercontent.com/sfield2/Peace_EnvironmentalSustainability_Paradox/refs/heads/main/DATA/EPI_2010_2022.csv",header=T,fileEncoding = 'UTF-8-BOM')%>%
  select(c("country","year","EPI.new","EPI.cc"))%>%
  setNames(c("country_name","year","epi_score","epi_cc_score"))

gpi <- read.csv("https://raw.githubusercontent.com/sfield2/Peace_EnvironmentalSustainability_Paradox/refs/heads/main/DATA/GPI_2008_2024.csv",header=T,fileEncoding = 'UTF-8-BOM')%>%
  select(c("country","year","Overall.Score"))%>%
  setNames(c("country_name","year","gpi_score"))

ppi <- read.csv("https://raw.githubusercontent.com/sfield2/Peace_EnvironmentalSustainability_Paradox/refs/heads/main/DATA/PPI_2009_2022.csv",header=T,fileEncoding = 'UTF-8-BOM')%>%
  select(c("Country","Year","PPI.Overall.Score"))%>%
  setNames(c("country_name","year","ppi_score"))

nd_vuln <- read.csv("https://raw.githubusercontent.com/sfield2/Peace_EnvironmentalSustainability_Paradox/refs/heads/main/DATA/ND_Vulnerability_1995_2022.csv",header=T,fileEncoding = 'UTF-8-BOM')%>%
  select(c("Name","X2010","X2011","X2012","X2013","X2014","X2015","X2016",
           "X2017","X2018","X2019","X2020","X2021","X2022"))%>%
  setNames(c("country_name","2010","2011","2012", "2013", "2014","2015", "2016", 
             "2017","2018", "2019", "2020","2021","2022"))%>%
  gather(.,"year","nd_vuln_score",-country_name)

gf_ecofoot <- read.csv("https://raw.githubusercontent.com/sfield2/Peace_EnvironmentalSustainability_Paradox/refs/heads/main/DATA/GF_ECOFOOT_PERCAPITA_2010_2022.csv",header=T,fileEncoding = 'UTF-8-BOM')%>%
  select(c("Country.Name","year","Total_ecofoot"))%>%
  setNames(c("country_name","year","ecofoot_score"))%>%
  mutate(ecofoot_score = as.numeric(ecofoot_score))

mat_foot <- read.csv("https://raw.githubusercontent.com/sfield2/Peace_EnvironmentalSustainability_Paradox/refs/heads/main/DATA/MAT_FOOT_1974_2024.csv",header=T,fileEncoding = 'UTF-8-BOM')%>%
  select(c("Country","Category","X2008","X2009","X2010","X2011","X2012","X2013","X2014","X2015","X2016",
           "X2017","X2018","X2019","X2020","X2021","X2022"))%>%
  setNames(c("country_name","material_category","2008","2009","2010","2011","2012", "2013", "2014","2015", "2016", 
             "2017","2018", "2019", "2020","2021","2022"))%>%
  gather(.,"year","value",-c("country_name","material_category"))

ghg <- read.csv("https://raw.githubusercontent.com/sfield2/Peace_EnvironmentalSustainability_Paradox/refs/heads/main/DATA/co2_ghg.csv",header=T,fileEncoding = 'UTF-8-BOM')%>%
  select(c("country","year","co2_per_capita"))%>%
  setNames(c("country_name","year","ghg"))%>%
  mutate(ghg = as.numeric(ghg))%>%
  filter(complete.cases(.))



################################################################################
### Step 1: Measure cumulative material footprint and calculate per capita ###
################################################################################
# step 1.1: read in population dataset
population <- read.csv("./DATA/DATA FOR ANALYSIS/POPULATIONS.csv",header=T,fileEncoding = 'UTF-8-BOM')%>%
  select(c("Location","Time","TPopulation1Jan"))%>%
  setNames(c("country_name","year","population_thousands"))%>%
  mutate(population = population_thousands * 1000)%>%
  select(c("country_name","year","population"))

# step 1.2: run calculations
mat_foot <- mat_foot%>%
  group_by(country_name,year)%>%
  summarize(mf_score = sum(value))%>%
  merge(.,population,by=c("country_name","year"))%>%
  mutate(mf_score = (mf_score/1102311310)/population)%>% ## 1102311310 in 1 gigaton
  select(-c("population"))

################################################################################
### Step 2: Interpolate missing years for epi 
################################################################################
epi <- epi[,1:3]%>%
  spread(year,epi_score)%>%
  set_colnames(c("country_name","twoten","twotwelve","twofourteen","twosixteen",
                 "twoeighteen","twotwenty","twotwentytwo"))%>%
  mutate(twoeleven = (twoten + twotwelve)/2)%>%
  mutate(twothirteen = (twotwelve + twofourteen)/2)%>%
  mutate(twofifteen = (twofourteen + twosixteen)/2)%>%
  mutate(twoseventeen = (twosixteen + twoeighteen)/2)%>%
  mutate(twonineteen = (twoeighteen + twotwenty)/2)%>%
  mutate(twotwentyone = (twotwenty + twotwentytwo)/2)%>%
  setNames(c("country_name","2010","2012","2014","2016",
             "2018","2020","2022","2011","2013","2015",
             "2017","2019","2021"))%>%
  gather(year,epi_score,-country_name)




################################################################################
### Step 3: Build full dataset ###
################################################################################
full <- merge(epi,ghg,by=c("country_name","year"))%>%
  merge(.,gpi,by=c("country_name","year"))%>%
  merge(.,ppi,by=c("country_name","year"))%>%
  merge(.,nd_vuln,by=c("country_name","year"))%>%
  merge(.,gf_ecofoot,by=c("country_name","year"))%>%
  merge(.,mat_foot,by=c("country_name","year"))

rm(epi,gpi,nd_vuln,ppi,gf_ecofoot,mat_foot,ghg,population)


## Note:Identify number of years each country is represented
count(unique(full,vars = c(country_name, year)),vars=country_name)

################################################################################
### Step 4: normalize data  ###
################################################################################
full_scale <- full%>%
  mutate_at(c("epi_score","ghg","gpi_score","ppi_score",
              "nd_vuln_score","ecofoot_score",
              "mf_score"), ~(scale(.) %>% as.vector))


################################################################################
### Step 5: merge with shapefile data for mapping  ###
################################################################################
full_mapping <- full_scale%>%
  mutate(environmental_sustainability = ghg+ecofoot_score+mf_score)%>%
  mutate(peace = nd_vuln_score+ppi_score+gpi_score)%>%
  select(c("country_name","year","environmental_sustainability","peace"))%>%
  subset(.,year==c("2022"))%>%
  mutate(peace=peace*-1)%>%
  mutate(tot=environmental_sustainability+peace)%>%
  mutate(tot_rank = rank(-tot))%>%
  setNames(c("name","year","environmental_sustainability","peace","tot","tot_rank"))


country <- sf::read_sf("./DATA/world-administrative-boundaries.shp")

country <- merge(country,full_mapping,by="name")

st_write(country,"./OUTPUT/country_scores.gpkg")

rm(full_mapping,country)


################################################################################
### Step 7: Run time-series spearman correlations for all variable pairs ###
################################################################################
years <- unique(full_scale$year)

# conduct loop that runs spearman correlatiosn for all variable pairs each year
for(i in 1:length(years)){
  
  # build dataframes
  spearman_correlations <- as.data.frame(matrix(NA,7,7))%>%
    setNames(c("EPI","GHG","GPI","PPI",
               "ND Vulnerability","Ecological Footprint","Material Footprint"))%>%
    `rownames<-`(c("EPI","GHG","GPI","PPI",
                   "ND Vulnerability","Ecological Footprint","Material Footprint"))
  spearman_correlations_p <- as.data.frame(matrix(NA,7,7))%>%
    setNames(c("EPI","GHG","GPI","PPI",
               "ND Vulnerability","Ecological Footprint","Material Footprint"))%>%
    `rownames<-`(c("EPI","GHG","GPI","PPI",
                   "ND Vulnerability","Ecological Footprint","Material Footprint"))
  
  # subset by year
  year_id <- years[i]
  yearly_scale <- subset(full_scale,year == year_id)
  
  # run correlations between all variables 
  for(j in 1:7){
    scale_var1 <- yearly_scale[,c(1,(j+2))]
    
    for(k in 1:7){
      scale_var2 <- yearly_scale[,c(1,(k+2))]
      
      df <- merge(scale_var1,scale_var2,by="country_name")
      
      spearman <- cor.test(df[,2],df[,3],method="spearman")
      spearman_correlations[j,k] <- spearman$estimate
      spearman_correlations_p[j,k] <- spearman$p.value
    }
  }
  # append year to the outputs
  spearman_correlations$year <- year_id
  spearman_correlations_p$year <- year_id
  
  # export 
  write.csv(spearman_correlations,paste0("./OUTPUT/spearman_",year_id,".csv",sep=""))
  write.csv(spearman_correlations_p,paste0("./OUTPUT/spearman_p_",year_id,".csv",sep=""))
  
  ## clean up outputs so easier to read in later for time series
  spearman_correlations <- gather(spearman_correlations,var1,spearman,-c("year"))%>%
    mutate(var2 = rep(c("EPI","GHG","GPI","PPI",
                        "ND Vulnerability","Ecological Footprint","Material Footprint"),7))%>%
    mutate(vars = paste0(pmin(var1,var2),pmax(var1,var2),sep=""))%>%
    distinct(vars,year,spearman)
  spearman_correlations <- spearman_correlations[!(spearman_correlations$vars %in% c("EPI","GHG","GPI","PPI",
                                                                                     "ND Vulnerability","Ecological Footprint","Material Footprint")),]
  
  spearman_correlations_p <- gather(spearman_correlations_p,var1,spearman_p,-c("year"))%>%
    mutate(var2 = rep(c("EPI","GHG","GPI","PPI",
                        "ND Vulnerability","Ecological Footprint","Material Footprint"),7))%>%
    mutate(vars = paste0(pmin(var1,var2),pmax(var1,var2),sep=""))%>%
    distinct(vars,year,spearman_p)
  spearman_correlations_p <- spearman_correlations_p[!(spearman_correlations_p$vars %in% c("EPI","GHG","GPI","PPI",
                                                                                           "ND Vulnerability","Ecological Footprint","Material Footprint")),]
  
  
  # assign to environment 
  assign(paste0("spearman_",year_id),spearman_correlations)
  assign(paste0("spearman_p_",year_id),spearman_correlations_p)
  
}


## step 7.2: combine outputs for summary table
spearman_full <- rbind(spearman_2010,spearman_2011,spearman_2012,spearman_2013,
                       spearman_2014,spearman_2015,spearman_2016,spearman_2017,
                       spearman_2018,spearman_2019,spearman_2020,spearman_2021,
                       spearman_2022)%>%
  spread(.,vars,spearman)


spearman_p_full_stack <- rbind(spearman_p_2010,spearman_p_2011,spearman_p_2012,spearman_p_2013,
                       spearman_p_2014,spearman_p_2015,spearman_p_2016,spearman_p_2017,
                       spearman_p_2018,spearman_p_2019,spearman_p_2020,spearman_p_2021,
                       spearman_p_2022)

spearman_full_p <-spearman_p_full_stack%>%
  mutate(spearman_p = ifelse(spearman_p>0.05, ">0.05","sig"))%>%
  spread(.,vars,spearman_p)


write.csv(spearman_full,"./OUTPUT/spearman_FULL.csv")
write.csv(spearman_full_p,"./OUTPUT/spearman_p_FULL.csv")



################################################################################
### Step 8: Run time-series pearson correlations for all variable pairs ###
################################################################################
# conduct loop that runs spearman correlatiosn for all variable pairs each year
for(i in 1:length(years)){
  pearson_correlations <- as.data.frame(matrix(NA,7,7))%>%
    setNames(c("EPI","GHG","GPI","PPI",
               "ND Vulnerability","Ecological Footprint","Material Footprint"))%>%
    `rownames<-`(c("EPI","GHG","GPI","PPI",
                   "ND Vulnerability","Ecological Footprint","Material Footprint"))
  pearson_correlations_p <- as.data.frame(matrix(NA,7,7))%>%
    setNames(c("EPI","GHG","GPI","PPI",
               "ND Vulnerability","Ecological Footprint","Material Footprint"))%>%
    `rownames<-`(c("EPI","GHG","GPI","PPI",
                   "ND Vulnerability","Ecological Footprint","Material Footprint"))
  year_id <- years[i]
  yearly_scale <- subset(full_scale,year == year_id)
  for(j in 1:7){
    scale_var1 <- yearly_scale[,c(1,(j+2))]
    
    for(k in 1:7){
      scale_var2 <- yearly_scale[,c(1,(k+2))]
      
      df <- merge(scale_var1,scale_var2,by="country_name")
      
      pearson <- cor.test(df[,2],df[,3],method="pearson")
      pearson_correlations[j,k] <- pearson$estimate
      pearson_correlations_p[j,k] <- pearson$p.value
    }
  }
  pearson_correlations$year <- year_id
  pearson_correlations_p$year <- year_id
  write.csv(pearson_correlations,paste0("./OUTPUT/pearson_",year_id,".csv",sep=""))
  write.csv(pearson_correlations_p,paste0("./OUTPUT/pearson_p_",year_id,".csv",sep=""))
  pearson_correlations <- gather(pearson_correlations,var1,pearson,-c("year"))%>%
    mutate(var2 = rep(c("EPI","GHG","GPI","PPI",
                        "ND Vulnerability","Ecological Footprint","Material Footprint"),7))%>%
    mutate(vars = paste0(pmin(var1,var2),pmax(var1,var2),sep=""))%>%
    distinct(vars,year,pearson)
  pearson_correlations <- pearson_correlations[!(pearson_correlations$vars %in% c("EPI","GHG","GPI","PPI",
                                                                                  "ND Vulnerability","Ecological Footprint","Material Footprint")),]
  pearson_correlations_p <- gather(pearson_correlations_p,var1,pearson_p,-c("year"))%>%
    mutate(var2 = rep(c("EPI","GHG","GPI","PPI",
                        "ND Vulnerability","Ecological Footprint","Material Footprint"),7))%>%
    mutate(vars = paste0(pmin(var1,var2),pmax(var1,var2),sep=""))%>%
    distinct(vars,year,pearson_p)
  pearson_correlations_p <- pearson_correlations_p[!(pearson_correlations_p$vars %in% c("EPI","GHG","GPI","PPI",
                                                                                        "ND Vulnerability","Ecological Footprint","Material Footprint")),]
  
  assign(paste0("pearson_",year_id),pearson_correlations)
  assign(paste0("pearson_p_",year_id),pearson_correlations_p)
}




## step 8.2: combine outputs for summary table
pearson_full <- rbind(pearson_2010,pearson_2011,pearson_2012,pearson_2013,
                       pearson_2014,pearson_2015,pearson_2016,pearson_2017,
                       pearson_2018,pearson_2019,pearson_2020,pearson_2021,
                       pearson_2022)%>%
  spread(.,vars,pearson)


pearson_p_full_stack <- rbind(pearson_p_2010,pearson_p_2011,pearson_p_2012,pearson_p_2013,
                         pearson_p_2014,pearson_p_2015,pearson_p_2016,pearson_p_2017,
                         pearson_p_2018,pearson_p_2019,pearson_p_2020,pearson_p_2021,
                         pearson_p_2022)


pearson_full_p <-pearson_p_full_stack%>%
  mutate(pearson_p = ifelse(pearson_p>0.05, ">0.05","sig"))%>%
  spread(.,vars,pearson_p)


write.csv(pearson_full,"./OUTPUT/pearson_FULL.csv")
write.csv(pearson_full_p,"./OUTPUT/pearson_p_FULL.csv")



## step 8.3: clean up our environment
rm(list=setdiff(ls(), c("pearson_full","spearman_full","pearson_p_full_stack","spearman_p_full_stack","full","full_scale")))




################################################################################
### Step 9: Build some plots ###
################################################################################
pearson_full_stack <- pearson_full%>%
  select(c("year","EPIND Vulnerability","EPIGPI","EPIPPI",
           "GHGND Vulnerability","GHGGPI","GHGPPI",
           "Ecological FootprintND Vulnerability","Ecological FootprintGPI","Ecological FootprintPPI",
           "Material FootprintND Vulnerability", "GPIMaterial Footprint","Material FootprintPPI"))%>%
  gather(vars,pearson_corr,-year)%>%
  merge(.,pearson_p_full_stack,by=c("year","vars"))%>%
  mutate(sigsize = ifelse(pearson_p>0.05,1,3))%>%
  mutate(year = as.numeric(year))

spearman_full_stack <- spearman_full%>%
  select(c("year","EPIND Vulnerability","EPIGPI","EPIPPI",
           "GHGND Vulnerability","GHGGPI","GHGPPI",
           "Ecological FootprintND Vulnerability","Ecological FootprintGPI","Ecological FootprintPPI",
           "Material FootprintND Vulnerability", "GPIMaterial Footprint","Material FootprintPPI"))%>%
  gather(vars,spearman_corr,-year)%>%
  merge(.,spearman_p_full_stack,by=c("year","vars"))%>%
  mutate(sigsize = ifelse(spearman_p>0.05,1,3))%>%
  mutate(year = as.numeric(year))



var_names <- c(`EPIND Vulnerability` = "EPI v. ND Vulnerability",
               `EPIGPI` = "EPI v. GPI",
               `EPIPPI` = "EPI v. PPI",
              
               `GHGND Vulnerability` = "GHG v. ND Vulnerability",
               `GHGGPI` = "GHG v. GPI",
               `GHGPPI` = "GHG v. PPI",
               
               `Ecological FootprintND Vulnerability` = "Ecological Footprint v. ND Vulnerability",
               `Ecological FootprintGPI` = "Ecological Footprint v. GPI",
               `Ecological FootprintPPI` = "Ecological Footprint v. PPI",

               `Material FootprintND Vulnerability` = "Material Footprint v. ND Vulnerability",
               `GPIMaterial Footprint` = "Material Footprint v. GPI",
               `Material FootprintPPI`="Material Footprint v. PPI"
               )


pearson_full_stack$vars <- factor(pearson_full_stack$vars, 
                                  levels=c("EPIND Vulnerability","EPIGPI","EPIPPI",
                                           "GHGND Vulnerability","GHGGPI","GHGPPI",
                                           "Ecological FootprintND Vulnerability","Ecological FootprintGPI","Ecological FootprintPPI",
                                           "Material FootprintND Vulnerability", "GPIMaterial Footprint","Material FootprintPPI"))

spearman_full_stack$vars <- factor(spearman_full_stack$vars, 
                                   levels=c("EPIND Vulnerability","EPIGPI","EPIPPI",
                                            "GHGND Vulnerability","GHGGPI","GHGPPI",
                                            "Ecological FootprintND Vulnerability","Ecological FootprintGPI","Ecological FootprintPPI",
                                            "Material FootprintND Vulnerability", "GPIMaterial Footprint","Material FootprintPPI"))



################################################################################
### Step 9.2: Chart showing EPI vs ND Vulnerability, GPI, and PPI ###
p1<-ggplot(full_scale)+
  geom_smooth(aes(epi_score,nd_vuln_score),col="#204645",alpha=0.45,size=1,method="lm",se=F)+
  geom_point(aes(epi_score,nd_vuln_score),col="#73B38D",alpha=0.75,size=3)+
  facet_wrap(~year,ncol=2)+
  labs(y="ND Vulnerability",
       x="EPI")+
  # scale_x_continuous(limits=c(-4,2))+
  # scale_y_continuous(limits=c(-2,2))+
  theme(legend.position="none",
        strip.text.x = element_text(size = 24, colour = "black"),
        strip.text.y = element_text(size = 24, colour = "black",angle=360),
        strip.background = element_rect(fill="white"),
        panel.spacing = unit(1, "lines"),
        axis.text=element_text(size=24,color="black"),
        axis.title=element_text(size=26,color="black",face="bold"),
        title=element_text(size=24,color="black"),
        axis.ticks.length=unit(.25, "cm"))

p2<-ggplot(full_scale)+
  geom_smooth(aes(epi_score,gpi_score),col="#204645",alpha=0.45,size=1,method="lm",se=F)+
  geom_point(aes(epi_score,gpi_score),col="#73B38D",alpha=0.75,size=3)+
  facet_wrap(~year,ncol=2)+
  labs(y="GPI",
       x="EPI")+
  # scale_x_continuous(limits=c(-4,2))+
  # scale_y_continuous(limits=c(-2,2))+
  theme(legend.position="none",
        strip.text.x = element_text(size = 24, colour = "black"),
        strip.text.y = element_text(size = 24, colour = "black",angle=360),
        strip.background = element_rect(fill="white"),
        panel.spacing = unit(1, "lines"),
        axis.text=element_text(size=24,color="black"),
        axis.title=element_text(size=26,color="black",face="bold"),
        title=element_text(size=24,color="black"),
        axis.ticks.length=unit(.25, "cm"))


p3<-ggplot(full_scale)+
  geom_smooth(aes(epi_score,ppi_score),col="#204645",alpha=0.45,size=1,method="lm",se=F)+
  geom_point(aes(epi_score,ppi_score),col="#73B38D",alpha=0.75,size=3)+
  facet_wrap(~year,ncol=2)+
  labs(y="PPI",
       x="EPI")+
  # scale_x_continuous(limits=c(-4,2))+
  # scale_y_continuous(limits=c(-2,2))+
  theme(legend.position="none",
        strip.text.x = element_text(size = 24, colour = "black"),
        strip.text.y = element_text(size = 24, colour = "black",angle=360),
        strip.background = element_rect(fill="white"),
        panel.spacing = unit(1, "lines"),
        axis.text=element_text(size=24,color="black"),
        axis.title=element_text(size=26,color="black",face="bold"),
        title=element_text(size=24,color="black"),
        axis.ticks.length=unit(.25, "cm"))



fig_1<-ggarrange(p1,p2,p3,
                 ncol=3, nrow=1,align="hv")



png('./FIGURES/Current/EPI v. ND vuln, GPI, PPI.png',height=1400,width=2200)
fig_1
dev.off()





################################################################################
### Step 9.3: Chart showing Per Capita GHG Emmisions vs ND Vulnerability, GPI, and PPI ###

p4<-ggplot(full_scale)+
  geom_smooth(aes(ghg,nd_vuln_score),col="#204645",alpha=0.45,size=1,method="lm",se=F)+
  geom_point(aes(ghg,nd_vuln_score),col="#73B38D",alpha=0.75,size=3)+
  facet_wrap(~year,ncol=2)+
  labs(y="ND Vulnerability",
       x="GHG (CO2) per capita")+
  # scale_x_continuous(limits=c(-4,2))+
  # scale_y_continuous(limits=c(-2,2))+
  theme(legend.position="none",
        strip.text.x = element_text(size = 24, colour = "black"),
        strip.text.y = element_text(size = 24, colour = "black",angle=360),
        strip.background = element_rect(fill="white"),
        panel.spacing = unit(1, "lines"),
        axis.text=element_text(size=24,color="black"),
        axis.title=element_text(size=26,color="black",face="bold"),
        title=element_text(size=24,color="black"),
        axis.ticks.length=unit(.25, "cm"))

p5<-ggplot(full_scale)+
  geom_smooth(aes(ghg,gpi_score),col="#204645",alpha=0.45,size=1,method="lm",se=F)+
  geom_point(aes(ghg,gpi_score),col="#73B38D",alpha=0.75,size=3)+
  facet_wrap(~year,ncol=2)+
  labs(y="GPI",
       x="GHG (CO2) per capita")+
  # scale_x_continuous(limits=c(-4,2))+
  # scale_y_continuous(limits=c(-2,2))+
  theme(legend.position="none",
        strip.text.x = element_text(size = 24, colour = "black"),
        strip.text.y = element_text(size = 24, colour = "black",angle=360),
        strip.background = element_rect(fill="white"),
        panel.spacing = unit(1, "lines"),
        axis.text=element_text(size=24,color="black"),
        axis.title=element_text(size=26,color="black",face="bold"),
        title=element_text(size=24,color="black"),
        axis.ticks.length=unit(.25, "cm"))


p6<-ggplot(full_scale)+
  geom_smooth(aes(ghg,ppi_score),col="#204645",alpha=0.45,size=1,method="lm",se=F)+
  geom_point(aes(ghg,ppi_score),col="#73B38D",alpha=0.75,size=3)+
  facet_wrap(~year,ncol=2)+
  labs(y="PPI",
       x="GHG (CO2) per capita")+
  # scale_x_continuous(limits=c(-4,2))+
  # scale_y_continuous(limits=c(-2,2))+
  theme(legend.position="none",
        strip.text.x = element_text(size = 24, colour = "black"),
        strip.text.y = element_text(size = 24, colour = "black",angle=360),
        strip.background = element_rect(fill="white"),
        panel.spacing = unit(1, "lines"),
        axis.text=element_text(size=24,color="black"),
        axis.title=element_text(size=26,color="black",face="bold"),
        title=element_text(size=24,color="black"),
        axis.ticks.length=unit(.25, "cm"))



fig_2<-ggarrange(p4,p5,p6,
                 ncol=3, nrow=1,align="hv")



png('./FIGURES/Current/GHG v. ND vuln, GPI, PPI.png',height=1400,width=2200)
fig_2
dev.off()








################################################################################
### Step 9.4: Chart showing Per Capita Ecological Footprint vs ND Vulnerability, GPI, and PPI ###
p7<-ggplot(full_scale)+
  geom_smooth(aes(ecofoot_score,nd_vuln_score),col="#204645",alpha=0.45,size=1,method="lm",se=F)+
  geom_point(aes(ecofoot_score,nd_vuln_score),col="#73B38D",alpha=0.75,size=3)+
  facet_wrap(~year,ncol=2)+
  labs(y="ND Vulnerability",
       x="Ecological Footprint (per capita)")+
  # scale_x_continuous(limits=c(-4,2))+
  # scale_y_continuous(limits=c(-2,2))+
  theme(legend.position="none",
        strip.text.x = element_text(size = 24, colour = "black"),
        strip.text.y = element_text(size = 24, colour = "black",angle=360),
        strip.background = element_rect(fill="white"),
        panel.spacing = unit(1, "lines"),
        axis.text=element_text(size=24,color="black"),
        axis.title=element_text(size=26,color="black",face="bold"),
        title=element_text(size=24,color="black"),
        axis.ticks.length=unit(.25, "cm"))

p8<-ggplot(full_scale)+
  geom_smooth(aes(ecofoot_score,gpi_score),col="#204645",alpha=0.45,size=1,method="lm",se=F)+
  geom_point(aes(ecofoot_score,gpi_score),col="#73B38D",alpha=0.75,size=3)+
  facet_wrap(~year,ncol=2)+
  labs(y="GPI",
       x="Ecological Footprint (per capita)")+
  # scale_x_continuous(limits=c(-4,2))+
  # scale_y_continuous(limits=c(-2,2))+
  theme(legend.position="none",
        strip.text.x = element_text(size = 24, colour = "black"),
        strip.text.y = element_text(size = 24, colour = "black",angle=360),
        strip.background = element_rect(fill="white"),
        panel.spacing = unit(1, "lines"),
        axis.text=element_text(size=24,color="black"),
        axis.title=element_text(size=26,color="black",face="bold"),
        title=element_text(size=24,color="black"),
        axis.ticks.length=unit(.25, "cm"))


p9<-ggplot(full_scale)+
  geom_smooth(aes(ecofoot_score,ppi_score),col="#204645",alpha=0.45,size=1,method="lm",se=F)+
  geom_point(aes(ecofoot_score,ppi_score),col="#73B38D",alpha=0.75,size=3)+
  facet_wrap(~year,ncol=2)+
  labs(y="PPI",
       x="Ecological Footprint (per capita)")+
  # scale_x_continuous(limits=c(-4,2))+
  # scale_y_continuous(limits=c(-2,2))+
  theme(legend.position="none",
        strip.text.x = element_text(size = 24, colour = "black"),
        strip.text.y = element_text(size = 24, colour = "black",angle=360),
        strip.background = element_rect(fill="white"),
        panel.spacing = unit(1, "lines"),
        axis.text=element_text(size=24,color="black"),
        axis.title=element_text(size=26,color="black",face="bold"),
        title=element_text(size=24,color="black"),
        axis.ticks.length=unit(.25, "cm"))



fig_3<-ggarrange(p7,p8,p9,
                 ncol=3, nrow=1,align="hv")



png('./FIGURES/Current/Ecological Footprint (per capita) v. ND vuln, GPI, PPI.png',height=1400,width=2200)
fig_3
dev.off()




################################################################################
### Step 9.5: Chart showing Per Capita Material Footprint vs ND Vulnerability, GPI, and PPI ###

p10<-ggplot(full_scale)+
  geom_smooth(aes(mf_score,nd_vuln_score),col="#204645",alpha=0.45,size=1,method="lm",se=F)+
  geom_point(aes(mf_score,nd_vuln_score),col="#73B38D",alpha=0.75,size=3)+
  facet_wrap(~year,ncol=2)+
  labs(y="ND Vulnerability",
       x="Material Footprint (per capita)")+
  # scale_x_continuous(limits=c(-4,2))+
  # scale_y_continuous(limits=c(-2,2))+
  theme(legend.position="none",
        strip.text.x = element_text(size = 24, colour = "black"),
        strip.text.y = element_text(size = 24, colour = "black",angle=360),
        strip.background = element_rect(fill="white"),
        panel.spacing = unit(1, "lines"),
        axis.text=element_text(size=24,color="black"),
        axis.title=element_text(size=26,color="black",face="bold"),
        title=element_text(size=24,color="black"),
        axis.ticks.length=unit(.25, "cm"))

p11<-ggplot(full_scale)+
  geom_smooth(aes(mf_score,gpi_score),col="#204645",alpha=0.45,size=1,method="lm",se=F)+
  geom_point(aes(mf_score,gpi_score),col="#73B38D",alpha=0.75,size=3)+
  facet_wrap(~year,ncol=2)+
  labs(y="GPI",
       x="Material Footprint (per capita)")+
  # scale_x_continuous(limits=c(-4,2))+
  # scale_y_continuous(limits=c(-2,2))+
  theme(legend.position="none",
        strip.text.x = element_text(size = 24, colour = "black"),
        strip.text.y = element_text(size = 24, colour = "black",angle=360),
        strip.background = element_rect(fill="white"),
        panel.spacing = unit(1, "lines"),
        axis.text=element_text(size=24,color="black"),
        axis.title=element_text(size=26,color="black",face="bold"),
        title=element_text(size=24,color="black"),
        axis.ticks.length=unit(.25, "cm"))


p12<-ggplot(full_scale)+
  geom_smooth(aes(mf_score,ppi_score),col="#204645",alpha=0.45,size=1,method="lm",se=F)+
  geom_point(aes(mf_score,ppi_score),col="#73B38D",alpha=0.75,size=3)+
  facet_wrap(~year,ncol=2)+
  labs(y="PPI",
       x="Material Footprint (per capita)")+
  # scale_x_continuous(limits=c(-4,2))+
  # scale_y_continuous(limits=c(-2,2))+
  theme(legend.position="none",
        strip.text.x = element_text(size = 24, colour = "black"),
        strip.text.y = element_text(size = 24, colour = "black",angle=360),
        strip.background = element_rect(fill="white"),
        panel.spacing = unit(1, "lines"),
        axis.text=element_text(size=24,color="black"),
        axis.title=element_text(size=26,color="black",face="bold"),
        title=element_text(size=24,color="black"),
        axis.ticks.length=unit(.25, "cm"))



fig_4<-ggarrange(p10,p11,p12,
                 ncol=3, nrow=1,align="hv")



png('./FIGURES/Current/Material Footprint (per capita) v. ND vuln, GPI, PPI.png',height=1400,width=2200)
fig_4
dev.off()



png('./FIGURES/correlation_results_across_time.png',height=1200,width=1500)
ggplot()+
  geom_line(data=pearson_full_stack,aes(year,pearson_corr,group=vars),col="#204645",alpha=0.65,size=1.5)+
  geom_point(data=pearson_full_stack,aes(year,pearson_corr,group=vars,size=sigsize),col="#204645")+
  geom_line(data=spearman_full_stack,aes(year,spearman_corr,group=vars),col="#73B38D",alpha=0.65,size=1.5)+
  geom_point(data=spearman_full_stack,aes(year,spearman_corr,group=vars,size=sigsize),col="#73B38D")+
  facet_wrap(~vars,ncol=3, labeller = as_labeller(var_names))+
  scale_x_continuous(limits=c(2010,2022),breaks=seq(2010,2022,by=4))+
  scale_y_continuous(limits=c(-1,0.5),breaks=seq(-1,0.5,by=0.5))+
  scale_color_manual(values = c("white","#73B38D","white","#204645"))+
  labs(x="Spearman Correlation (green), Pearson Correlation (blue)",
       y="",
       caption = "Point = Significant Values at <0.05,
       No Point = Significant Values at >0.05" )+
  theme(legend.position="none",
        strip.text.x = element_text(size = 24, colour = "black"),
        strip.text.y = element_text(size = 24, colour = "black",angle=360),
        strip.background = element_rect(fill="white"),
        panel.spacing = unit(1, "lines"),
        axis.text=element_text(size=24,color="black"),
        axis.title=element_text(size=26,color="black",face="bold"),
        title=element_text(size=24,color="black"),
        axis.ticks.length=unit(.25, "cm"),
        plot.caption = element_textbox_simple(
          margin = margin(t = 12), size = 20
        ))
dev.off()



## clean up environment
rm(list=ls())

################################################################################
### Step 10: Generalized Linear Models
################################################################################
gpi <- read.csv("./DATA/DATA FOR ANALYSIS/GPI_2008_2024.csv",header=T,fileEncoding = 'UTF-8-BOM')%>%
  select(c("country","year","External.Conflicts.Fought","Internal.Conflicts.Fought"))%>%
  setNames(c("country_name","year","external_score","internal_score"))

gf_ecofoot <- read.csv("./DATA/DATA FOR ANALYSIS/GF_ECOFOOT_PERCAPITA_2010_2022.csv",header=T,fileEncoding = 'UTF-8-BOM')%>%
  select(c("Country.Name","year","Total_ecofoot"))%>%
  setNames(c("country_name","year","ecofoot_score"))%>%
  mutate(ecofoot_score = as.numeric(ecofoot_score))

mat_foot <- read.csv("./DATA/DATA FOR ANALYSIS/MAT_FOOT_1974_2024.csv",header=T,fileEncoding = 'UTF-8-BOM')%>%
  select(c("Country","Category","X2008","X2009","X2010","X2011","X2012","X2013","X2014","X2015","X2016",
           "X2017","X2018","X2019","X2020","X2021","X2022"))%>%
  setNames(c("country_name","material_category","2008","2009","2010","2011","2012", "2013", "2014","2015", "2016", 
             "2017","2018", "2019", "2020","2021","2022"))%>%
  gather(.,"year","value",-c("country_name","material_category"))

ghg <- read.csv("./DATA/DATA FOR ANALYSIS/co2_ghg.csv",header=T,fileEncoding = 'UTF-8-BOM')%>%
  select(c("country","year","co2_per_capita"))%>%
  setNames(c("country_name","year","ghg"))%>%
  mutate(ghg = as.numeric(ghg))%>%
  filter(complete.cases(.))

# step 10.1: read in population dataset
population <- read.csv("./DATA/DATA FOR ANALYSIS/POPULATIONS.csv",header=T,fileEncoding = 'UTF-8-BOM')%>%
  select(c("Location","Time","TPopulation1Jan"))%>%
  setNames(c("country_name","year","population_thousands"))%>%
  mutate(population = population_thousands * 1000)%>%
  select(c("country_name","year","population"))

# step 10.2: rerun calculations for material footprint
mat_foot <- mat_foot%>%
  group_by(country_name,year)%>%
  summarize(mf_score = sum(value))%>%
  merge(.,population,by=c("country_name","year"))%>%
  mutate(mf_score = (mf_score/1102311310)/population)%>% ## 1102311310 in 1 gigaton
  select(-c("population"))

# step 10.3: bin categorical variables
gpi<-gpi%>%
  mutate(external_score = ifelse(external_score == 1,0,1))%>%
  mutate(internal_score = ifelse(internal_score == 1,0,1))

# step 10.4: create full dataset
full <- merge(gf_ecofoot,ghg,by=c("country_name","year"))%>%
  merge(.,gpi,by=c("country_name","year"))%>%
  merge(.,mat_foot,by=c("country_name","year"))

# step 10.5: normalize continuous variables
full_scale <- full%>%
  mutate_at(c("ghg","ecofoot_score",
              "mf_score"), ~(scale(.) %>% as.vector))

# step 10.6: clean environment
rm(gf_ecofoot,mat_foot,population,ghg,gpi)


# step 10.8: build glms
years <- unique(full_scale$year)

for(i in 1:length(years)){
  regressions <- as.data.frame(matrix(NA,3,12))%>%
    setNames(c("internal_coef","internal_coef_p","internal_pseudoR","internal_AIC","internal_hoslem","internal_hoslem_p",
               "external_coef","external_coef_p","external_pseudoR","external_AIC","external_hoslem","external_hoslem_p"))%>%
    `rownames<-`(c("GHG", "Ecological Footprint","Material Footprint"))
  
  # yearly_scale <- full_scale
  # subset by year
  year_id <- years[i]
  yearly_scale <- subset(full_scale,year == year_id)
  
  #############################################################
  ### RUN REGRESSIONS BETWEEN CONDITIONS AND INTERNAL CONFLICT
  #############################################################
  
  ## ghg glm (how does ghg predict internal conflict occurence)
  #coefficients and p 
  model <- glm(internal_score~ghg+year,family=binomial(link='logit'),data=yearly_scale)
  model_list <- summary(model)
  coef <-coef(summary(model))[,1]
  p <- coef(summary(model))[,4]
  
  # aic
  aic <- model_list[5]
  
  # pseudo r
  pseudo_r <- (1 - (as.numeric(model_list[4])/as.numeric(model_list[8]))) # 1- (residual devicance/null deviance)
  
  #hosmer lemshow test
  hl_test <- hoslem.test(model$y, fitted(model))

  # assign values to correct location
  regressions[c("GHG"),c("internal_coef")] <- coef[2]
  regressions[c("GHG"),c("internal_coef_p")] <- p[2]
  regressions[c("GHG"),c("internal_AIC")] <- aic
  regressions[c("GHG"),c("internal_pseudoR")] <- pseudo_r
  regressions[c("GHG"),c("internal_hoslem")] <- as.numeric(hl_test[1])
  regressions[c("GHG"),c("internal_hoslem_p")] <- as.numeric(hl_test[3])


  

  
  
  ## material footprint
  model <- glm(internal_score~mf_score+year,family=binomial(link='logit'),data=yearly_scale)
  model_list <- summary(model)
  coef <-coef(summary(model))[,1]
  p <- coef(summary(model))[,4]
  
  # aic
  aic <- model_list[5]
  
  # pseudo r
  pseudo_r <- (1 - (as.numeric(model_list[4])/as.numeric(model_list[8]))) # 1- (residual devicance/null deviance)
  
  #hosmer lemshow test
  hl_test <- hoslem.test(model$y, fitted(model))
  
  # assign values to correct location
  regressions[c("Material Footprint"),c("internal_coef")] <- coef[2]
  regressions[c("Material Footprint"),c("internal_coef_p")] <- p[2]
  regressions[c("Material Footprint"),c("internal_AIC")] <- aic
  regressions[c("Material Footprint"),c("internal_pseudoR")] <- pseudo_r
  regressions[c("Material Footprint"),c("internal_hoslem")] <- as.numeric(hl_test[1])
  regressions[c("Material Footprint"),c("internal_hoslem_p")] <- as.numeric(hl_test[3])
  
  
  
  ## ecological footprint
  model <- glm(internal_score~ecofoot_score+year,family=binomial(link='logit'),data=yearly_scale)
  model_list <- summary(model)
  coef <-coef(summary(model))[,1]
  p <- coef(summary(model))[,4]
  
  # aic
  aic <- model_list[5]
  
  # pseudo r
  pseudo_r <- (1 - (as.numeric(model_list[4])/as.numeric(model_list[8]))) # 1- (residual devicance/null deviance)
  
  #hosmer lemshow test
  hl_test <- hoslem.test(model$y, fitted(model))
  
  # assign values to correct location
  regressions[c("Ecological Footprint"),c("internal_coef")] <- coef[2]
  regressions[c("Ecological Footprint"),c("internal_coef_p")] <- p[2]
  regressions[c("Ecological Footprint"),c("internal_AIC")] <- aic
  regressions[c("Ecological Footprint"),c("internal_pseudoR")] <- pseudo_r
  regressions[c("Ecological Footprint"),c("internal_hoslem")] <- as.numeric(hl_test[1])
  regressions[c("Ecological Footprint"),c("internal_hoslem_p")] <- as.numeric(hl_test[3])
  
  
  
  
  #############################################################
  ### RUN REGRESSIONS BETWEEN CONDITIONS AND EXTERNAL CONFLICT
  #############################################################
  
  ## ghg glm (how does ghg predict internal conflict occurence)
  #coefficients and p 
  model <- glm(external_score~ghg+year,family=binomial(link='logit'),data=yearly_scale)
  model_list <- summary(model)
  coef <-coef(summary(model))[,1]
  p <- coef(summary(model))[,4]
  
  # aic
  aic <- model_list[5]
  
  # pseudo r
  pseudo_r <- (1 - (as.numeric(model_list[4])/as.numeric(model_list[8]))) # 1- (residual devicance/null deviance)
  
  #hosmer lemshow test
  hl_test <- hoslem.test(model$y, fitted(model))
  
  # assign values to correct location
  regressions[c("GHG"),c("external_coef")] <- coef[2]
  regressions[c("GHG"),c("external_coef_p")] <- p[2]
  regressions[c("GHG"),c("external_AIC")] <- aic
  regressions[c("GHG"),c("external_pseudoR")] <- pseudo_r
  regressions[c("GHG"),c("external_hoslem")] <- as.numeric(hl_test[1])
  regressions[c("GHG"),c("external_hoslem_p")] <- as.numeric(hl_test[3])
  
  
  
  
  
  
  ## material footprint
  model <- glm(external_score~mf_score+year,family=binomial(link='logit'),data=yearly_scale)
  model_list <- summary(model)
  coef <-coef(summary(model))[,1]
  p <- coef(summary(model))[,4]
  
  # aic
  aic <- model_list[5]
  
  # pseudo r
  pseudo_r <- (1 - (as.numeric(model_list[4])/as.numeric(model_list[8]))) # 1- (residual devicance/null deviance)
  
  #hosmer lemshow test
  hl_test <- hoslem.test(model$y, fitted(model))
  
  # assign values to correct location
  regressions[c("Material Footprint"),c("external_coef")] <- coef[2]
  regressions[c("Material Footprint"),c("external_coef_p")] <- p[2]
  regressions[c("Material Footprint"),c("external_AIC")] <- aic
  regressions[c("Material Footprint"),c("external_pseudoR")] <- pseudo_r
  regressions[c("Material Footprint"),c("external_hoslem")] <- as.numeric(hl_test[1])
  regressions[c("Material Footprint"),c("external_hoslem_p")] <- as.numeric(hl_test[3])
  
  
  
  ## ecological footprint
  model <- glm(external_score~ecofoot_score+year,family=binomial(link='logit'),data=yearly_scale)
  model_list <- summary(model)
  coef <-coef(summary(model))[,1]
  p <- coef(summary(model))[,4]
  
  # aic
  aic <- model_list[5]
  
  # pseudo r
  pseudo_r <- (1 - (as.numeric(model_list[4])/as.numeric(model_list[8]))) # 1- (residual devicance/null deviance)
  
  #hosmer lemshow test
  hl_test <- hoslem.test(model$y, fitted(model))
  
  # assign values to correct location
  regressions[c("Ecological Footprint"),c("external_coef")] <- coef[2]
  regressions[c("Ecological Footprint"),c("external_coef_p")] <- p[2]
  regressions[c("Ecological Footprint"),c("external_AIC")] <- aic
  regressions[c("Ecological Footprint"),c("external_pseudoR")] <- pseudo_r
  regressions[c("Ecological Footprint"),c("external_hoslem")] <- as.numeric(hl_test[1])
  regressions[c("Ecological Footprint"),c("external_hoslem_p")] <- as.numeric(hl_test[3])
  
  
  
  
  regressions$year <- year_id
  
  assign(paste0("regressions_",year_id),regressions)
}




glms_full <- rbind(regressions_2010,regressions_2011,regressions_2012,regressions_2013,
                   regressions_2014,regressions_2015,regressions_2016,regressions_2017,
                   regressions_2018,regressions_2019,regressions_2020,regressions_2021,
                   regressions_2022)
  mutate(internal_coef_p = ifelse(internal_coef_p >0.05, ">0.05","sig"))%>%
  mutate(external_coef_p = ifelse(external_coef_p >0.05, ">0.05","sig"))%>%
  mutate(internal_hoslem_p = ifelse(internal_hoslem_p >0.05, ">0.05","sig"))%>%
  mutate(external_hoslem_p = ifelse(external_hoslem_p >0.05, ">0.05","sig"))


write.csv(regressions,"./OUTPUT/logisticregressions_yearscollapse_yearspredictor.csv")





