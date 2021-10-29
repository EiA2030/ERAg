## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----packages,include=F,eval=T,echo=T-----------------------------------------
  require(ERAg)
  require(sp)
  require(rgeos)
  require(ggplot2)
  require(rworldmap)
  require(rworldxtra)
  require(sf)
  require(ggnewscale)
  require(terra)
  require(RColorBrewer)
  require(rasterVis)


## ----Access ERA, echo=T-------------------------------------------------------
knitr::kable(head(ERA.Compiled[,1:8], 5))

## ----ERA Fields, echo=T-------------------------------------------------------
knitr::kable(head(ERACompiledFields[c(1,7,10,19,50),c("Field.Name","Description")]))

## ----Bibliographic, echo=T, fig.align='center', fig.asp=1, fig.width=6, warning=FALSE----
knitr::kable(head(unique(ERA.Compiled[,list(Code,Author,Date,Journal,DOI,DataLoc)]), 5))

# Plot no. studies x theme x year

# Subset data
PrxThxYe<-ERA.Compiled[!is.na(Date),list(Theme,Code,Date)]
# Split Theme on "-" delim (multiple themes can be present in an experimental treatment)
ThemeSplit<-strsplit(PrxThxYe[,Theme],"-")
# Count no. themes per observation & replicate each row by N
N<-rep(1:nrow(PrxThxYe),lapply(ThemeSplit,length))
# Do replication and add split theme back
PrxThxYe<-PrxThxYe[N][,Theme:=unlist(ThemeSplit)]
# Calculate no. studies per theme per year   
PrxThxYe<-PrxThxYe[,list(N.Studies=length(unique(Code))),by=list(Theme,Date)]
# Subset to fewer themes to simplify plot
PrxThxYe<-PrxThxYe[Theme %in% c("Nutrient Management","Agroforestry","Postharvest","Soil Management")]
# Order on publication year
PrxThxYe<-PrxThxYe[, Date:=as.numeric(Date)][order(Date)]
# Get cumulative sum by theme by year
PrxThxYe[,Cum.Sum := cumsum(N.Studies), by=list(Theme)]

# Plot
ggplot(PrxThxYe,aes(x=Date,y=Cum.Sum,col=Theme))+
  geom_line(alpha=1,size=1)+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(title="Cumulative number of studies reporting practice themes over time",
       x= "Publication Year",
       y = "No. Studies")




## ----Location, echo=T---------------------------------------------------------
knitr::kable(head(unique(ERA.Compiled[,list(Country,Site.ID,Latitude,Longitude,Buffer)]), 5))

