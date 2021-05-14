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
knitr::kable(head(unique(ERA.Compiled[,list(Code,Author,Date,Journal,DataLoc)]), 5))

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

## ----Buffers, echo=T,fig.width=7,fig.height=6---------------------------------
SiteBuffers<-Pbuffer(Data=ERA.Compiled, ID = NA, Projected = F)
plot(SiteBuffers)

## ----Filter Location, echo=T,eval=F-------------------------------------------
#  # Filter dataset to sites with spatial uncertainty radius of less than 5km
#  ERA.Compiled<-ERA.Compiled[Buffer<5000]

## ----Hexplot, echo=T,eval=T,fig.width=7,fig.height=6,warning=F----------------
ERAHexPlot(Data=ERA.Compiled[Buffer<10000],Low = "grey10",Mid = "grey80",High = "black",Point.Col = "yellow",Do.Log="Yes",Showpoints="Yes",ALevel=NA)

## ----Alphaplot, echo=T,eval=T,fig.width=7,fig.asp=1,warning=F-----------------

ERAAlphaPlot(Data = ERA.Compiled,
            Background = NA,
            Background.Labs = NA,
            Background.Cols = NA,
            Background.Title = NA,
            alpha.bandwidth = 4,
            Showpoints = T,
            Low = "black",
            Mid = "grey30",
            High = "white",
            Point.Col = "Black",
            ALevel = NA)


## ----Ag Sites, echo=T---------------------------------------------------------

Agg.Sites<-unique(ERA.Compiled[grep("[.][.]",Site.ID),
                               list(Site.ID,Latitude,Longitude,Buffer,Version)])

knitr::kable(head(Agg.Sites,5))

## ----Show ERA Concepts, echo=T------------------------------------------------
ERAConcepts

## ----Names vs. Codes, echo=T--------------------------------------------------
knitr::kable(head(unique(ERA.Compiled[,list(Out.SubInd,Out.SubInd.Code,PrName,PrName.Code,Product.Simple,Product.Simple.Code)]), 5))

## ----Show Practice Codes, echo=T----------------------------------------------
knitr::kable(head(PracticeCodes[,1:6], 5))

## ----Demonstrate Set Differences, echo=T--------------------------------------
T.Cols<-paste0("T",1:13)
C.Cols<-paste0("C",1:13)

T.Cols<-unique(unlist(ERA.Compiled[99,..T.Cols]))
C.Cols<-unique(unlist(ERA.Compiled[99,..C.Cols]))

T.Cols
C.Cols

# Remove blanks and h-codes
T.Cols<-T.Cols[!(T.Cols==""|grepl("h",T.Cols)) ]
C.Cols<-C.Cols[!(C.Cols==""|grepl("h",C.Cols)) ]

T.Cols
C.Cols

# Experimental practices in experimental treatment but not the control treatment (plist column)
T.Cols[!T.Cols %in% C.Cols]

# Base practices in both experimental and control treatments (base.list column)
T.Cols[T.Cols %in% C.Cols]

## ----ERA Show Practice Codes, echo=T------------------------------------------
knitr::kable(ERA.Compiled[99,list(plist,base.list,SubPrName,SubPrName.Code,SubPrName.Base,SubPrName.Base.Code)])

## ----Show Outcome Codes, echo=T-----------------------------------------------
knitr::kable(head(OutcomeCodes[,1:6], 5))

## ----ERA Show Outcomes, echo=T------------------------------------------------
knitr::kable(head(unique(ERA.Compiled[!is.na(Units),list(Outcode,Units,Out.SubInd,Out.SubInd.Code,Out.Ind,Out.Ind.Code,Out.Pillar,Out.Pillar.Code)]),5))

## ----Show EU Codes, echo=T----------------------------------------------------
knitr::kable(EUCodes[c(15,40,60,80,100),c(1,2,4,8,9)])

## ----Climate Published, echo=T, warning=FALSE, fig.width=5, fig.align='center', fig.asp=1----
knitr::kable(head(unique(ERA.Compiled[!(is.na(MAT)|is.na(MAP)|is.na(TSP)),list(Code,Country,Site.Key,MAT,MAP,MSP,TAP,TSP)]), 5))

# Make sure Mean.Annual.Precip variable is numeric
# Average Mean.Annual.Precip for unique spatial locations
# Select Mean.Annual.Precip variable from data.table

MAP<-ERA.Compiled[,MAP:=as.numeric(MAP)
                  ][!is.na(MAP),list(MAP=mean(MAP)),by=Site.Key
                    ][,MAP] 

hist(MAP,main="Reported MAP Histogram",xlab="Mean annual precipitation (mm)")


## ----Climate Derived, echo=T, warning=FALSE,fig.width=5,fig.align='center',fig.asp=1----
knitr::kable(head(unique(ERA.Compiled[!(is.na(MAT)|is.na(MAP)),list(Code,Country,Site.Key,MAT,Mean.Annual.Temp,MAP,Mean.Annual.Precip)]), 5))

Mean.Annual.Precip<-ERA.Compiled[,Mean.Annual.Precip:=as.numeric(Mean.Annual.Precip)
                                 ][!is.na(Mean.Annual.Precip),list(Mean.Annual.Precip=mean(Mean.Annual.Precip)),
                                   by=Site.Key
                                   ][Mean.Annual.Precip>0,Mean.Annual.Precip] 

# In the line above we filter out any negative CHIRPs values (there's a bug we need to fix)

hist(Mean.Annual.Precip,main="CHIRPS 2.0 MAP Histogram",xlab="Mean annual precipitation (mm)")


## ----Soil Published, echo=T, warning=FALSE------------------------------------
knitr::kable(head(unique(ERA.Compiled[!(is.na(SOC)|is.na(Soil.pH)|is.na(Soil.Texture)),
                                      list(Code,Site.Key,Soil.Texture,SOC,SOC.Unit,SOC.Depth,Soil.pH,Soil.pH.Method)]), 5))

## ----SoilGrids, echo=T, warning=FALSE-----------------------------------------
# Lets look at Cation Exchange Capacity of soil (variable name = CECSOL) at 5-15cm (sl2)
 Cols<-c("Site.Key",grep("CECSOL_M_sl2",colnames(ERA_SoilGrids18),value=T))
 knitr::kable(head(ERA_SoilGrids18[,Cols],5))
# There are a lot of SoilGrids variables in this dataset:
  length(colnames(ERA_SoilGrids18))

## ----Elevation, echo=T, warning=FALSE,fig.width=5,fig.align='center',fig.asp=1----

Elevation<-ERA.Compiled[,Elevation:=as.numeric(Elevation)
                        ][!is.na(Elevation),list(Elevation=mean(Elevation)),
                          by=Site.Key][,Elevation]
hist(Elevation,main="Elevation",xlab="Reported Elevation (m)")

# We have checked the site at ~4000m it is a valid observation!


## ----Physical, echo=T, warning=FALSE------------------------------------------
 knitr::kable(head(ERA_Physical[,4:10],5))

## ----AEZs, echo=T, warning=FALSE,fig.width=5,fig.align='center',fig.height=7----

AEZ16Simple<-table(unique(ERA.Compiled[!is.na(AEZ16simple),list(Site.Key,AEZ16simple)])[,AEZ16simple])

barplot(sort(AEZ16Simple), las=2,col = brewer.pal(8, "Set2"),ylab="No. Studies")

## ----BioClim, include = T, eval = T,echo=T------------------------------------
   knitr::kable(ERA_BioClim[1:5,1:5])

## ----Landuse, include = T, eval = T,echo=T------------------------------------
   knitr::kable(ERA_CCI_LC_15[1:5,1:15])

## ----Landuse Fields, include = T, eval = T,echo=T-----------------------------
   knitr::kable(ERA_CCI_LC_15_Fields[1:5,])

## ----Other Datasets, echo=T, warning=FALSE------------------------------------
 knitr::kable(ERA_Other_Linked_Data_Fields[-(1:4),c("Name","Description")])

