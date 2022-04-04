#' The compiled ERA dataset (version = Comb 2022.04.04 2022.03.23)
#' \describe{
#'   \item{Index}{unique row identity}
#'   \item{Code}{unique publication identity}
#'   \item{Author}{primary author of the publication}
#'   \item{Date}{the year of publication}
#'   \item{Journal}{abbreviation of the journal name}
#'   \item{DOI}{doi (or url if no doi available) of article. DOI = digital object identifier}
#'   \item{Elevation}{the elevation in meters (m). Provide the mid-point if a range is given}
#'   \item{Country}{country name}
#'   \item{ISO.3166-1.alpha-3}{Standardized country name using ISO.3166-1.alpha-3. This field should be auto filled using the country selected and data in the Levels Tab}
#'   \item{Site.Type}{one of the following: Farm, Station, Greenhouse, Survey, or Lab. Farm research is conducted in a farmers field and can be managed by the farmer or researcher. Station research is conducted in a controlled setting of research station, university or school. Survey research is conducted via interviews that yields quantitative data based on testimonial. Greenhouse is conducted in a greenhouse and is only relevant for studies of greenhouse gas emissions data. Lab studies can include fisheries feeding trials conducted at a small scale in university research labs}
#'   \item{Site.ID}{free text name to identify the site. Include institution/station name and location or village/town in county/district, but not the country}
#'   \item{MAT}{reported mean annual temperature in degrees C. If a range is given, average the values and enter the average. If mean highs and lows are given, they can be averaged to estimate the mean annual temperature}
#'   \item{MAP}{mean annual precipitation (mm) for the location. This should be a long-term average for more than 10 years of data}
#'   \item{TAP}{total annual precipitation (mm) for the location in the year corresponding to the outcome data}
#'   \item{MSP}{mean seasonal precipitation (mm) for the location in the season corresponding to the outcome data. This should be a long-term average from more than 10 years of data}
#'   \item{TSP}{total seasonal precipitation (mm) for the location in the season corresponding to the outcome data}
#'   \item{Soil.Type}{list the given soil classification/name in lowercase letters. If more than one soil type is given (within the same taxonomic system) enter both separated by a period}
#'   \item{Soil.Classification}{list the taxonomic system used to classify the soil. Choose from (FAO, USDA, other)}
#'   \item{Soil.Texture}{list the soil texture provided in the text or in tables in lowercase letters. If no texture is named, but % of sand, silt and clay are given, use the soil texture triangle to estimate soil texture (https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/survey/?cid=nrcs142p2_054167)}
#'   \item{SOC}{soil organic carbon value derived during site characterization}
#'   \item{SOC.Unit}{units provided for soil organic carbon. Usually either g/kg or % (also can be density kg/m3 or stocks t/ha)}
#'   \item{SOC.Depth}{concatenate min (top) and max (bottom) depths user entered with a "-", e.g. 0-10}
#'   \item{Soil.pH}{soil pH value derived during site characterization}
#'   \item{Soil.pH.Method}{method used to calculate soil pH}
#'   \item{Plant.Start}{starting of planting period (dd.mm.yyyy)}
#'   \item{Plant.End}{end of planting period (dd.mm.yyyy)}
#'   \item{Harvest.Start}{starting of harvest period (dd.mm.yyyy)}
#'   \item{Harvest.End}{end of harvest period (dd.mm.yyyy)}
#'   \item{Rep}{the number of replicates for the treatments}
#'   \item{Plot.Size}{the size of the smallest treatment plot unit in m2}
#'   \item{CID}{a unique identifier (within study) describing a set of management practices that occurred in the control which is used to identify the treatment combinations tested, to allow comparison across outcomes in a paper. A default system of T1, T2, T3, etc. can be used. CID should describe the treatment used as the control for this observation}
#'   \item{C.Descrip}{(superceded by C.Descrip.Clean column) Free text description of the control treatment}
#'   \item{C.NI}{amount of inorganic (chemical) nitrogen applied to control plots in kg/ha}
#'   \item{C.NO}{amount of nitrogen in organic materials (manures or compost, but not residues) applied to control plots, preferably in kg/ha}
#'   \item{TID}{a unique identifier (within study) describing a set of management practices that occurred in treatment which is used to identify the treatment combinations tested, to allow comparison across outcomes in a paper. A default system of T1, T2, T3, etc. can be used. TID should describe the treatment used as the control for this observation}
#'   \item{T.Descrip}{(superceded by T.Descrip.Clean column) Free text description of the CSA treatment}
#'   \item{T.NI}{amount of inorganic (chemical) nitrogen (N) applied to CSA treatment in kg/ha}
#'   \item{T.NO}{amount of nitrogen (N) in organic materials (manures or compost) applied to CSA treatment, preferably in kg/ha}
#'   \item{Diversity}{name of species in intercrop or rotations including the crop. Dash(-) indicates intercrop and slash (/) indicates a rotation}
#'   \item{Variety}{commercial name of the crop or animal variety}
#'   \item{Tree}{Name of tree species used in agroforestry experiments, can be both as an intercrop or as an agroforestry mulch}
#'   \item{Duration}{length of time the experiment has been ongoing up to the date the data are being reported for in years. In bimodal rainfall systems with multiple growing seasons, seasons are identified by fractions of the year, 0.5, etc. If outcomes are reported in a unimodal rainfall system with one growing season per year, the duration can be equivalent to one year}
#'   \item{M.Year}{the calendar year the measurement was taken. Seasons in multi-season systems are identified by decimals .1 or .2 for the first and second season respectively. When the measurements are average values for more than one year create a decimal for the years covered}
#'   \item{M.Year.Start}{first year the measurement was taken (YYYY). For yield or similar outcomes this should relate to the harvest year (rather than the planting year)}
#'   \item{M.Year.End}{this only required for temporally aggregated observations (e.g. a result that has been averaged from crop yields reported from 2000,2001 and 2002)}
#'   \item{Season.Start}{this only required in areas where there are mutiple growing seasons in a year (e.g., areas with bimodal rainfall or irrigated cropping in dry seasons of a unimodal system)}
#'   \item{Season.End}{this only required for temporally aggregated observations in areas with more than one growing season (e.g. a result that has been averaged from crop yields reported from 2000 season 1,2001 season 1 and 2002 season 1)}
#'   \item{EU}{the experimental unit (EU) for which experiment outcomes are reported. This usually a crop or animal product. For cookstove papers it may be a cookstove. The EU for soil outcomes should be the crop(s) grown on the soil unless no crops are grown at all. Letters in the code should be lowercase. If the outcome refers to multiple experimental units (e.g. soil outcomes during an intercropping experiment) select all experimental units separated by a period (see EU2 tab)}
#'   \item{Outcode}{the productivity, resilience, or mitigation outcome indicator that data are being reported on (see OUTCOMES2 tab)}
#'   \item{Units}{free text description of the units of the data being reported without special characters or formatting}
#'   \item{MeanC}{the value that corresponds to the control treatment and outcome. Please standard yields to t/ha where appropropriate, keeping as many decimal places as there is data for}
#'   \item{MeanT}{the value that corresponds to the CSA treatment and outcome. Please standard yields to kg/ha where appropropriate, keeping as many decimal places as there is data for}
#'   \item{Upper}{the depth for which the MeanC and MeanT correspond for soil properties in cm (lowest depth value = upper, so if you have a depth of 0-20 cm the lowest value = 20)}
#'   \item{Lower}{the depth for which the MeanC and MeanT correspond for soil properties in cm (highest depth value = lower, so if you have a depth of 0-20 cm the highest value = 0)}
#'   \item{DataLoc}{the location in the paper where the data can be found. This should be three letters (fig, tab, or txt) in lowercase and a number (for figures and tables) to signify figure, table or text, respectively}
#'   \item{USD2010.C}{monetary outcomes converted to USD equivalent in 2010 using World Bank CPI figures and exchange rates (see Currency Tab). There are no reliable data for Zimbabwe}
#'   \item{USD2010.T}{monetary outcomes converted to USD equivalent in 2010 using World Bank CPI figures and exchange rates (see Currency Tab). There are no reliable data for Zimbabwe}
#'   \item{MeanFlip}{is a negative result considered better than a positive result (i.e. MeanT/MeanC <1 = good)? This is indexed in using the Outcode field in the compednium and the `Negative Values` field in the OUTCOMES tab}
#'   \item{T.Feed.Source}{where were livestock diets in the treatment practice(s) sourced from?}
#'   \item{C.Feed.Source}{Where were livestock diets in the control practice(s) sourced from?}
#'   \item{Species}{the tree species for which the experiment outcome are reported}
#'   \item{Partial.Outcome.Name}{free text to describe the partial outcome}
#'   \item{Partial.Outcome.Code}{ERA practice codes that correspond to the partial outcome}
#'   \item{Analysis.Function}{Describes the logical process used to compare control to treatment (for developers)}
#'   \item{Version}{year in which the ERA data entry took place. Either 2018 or 2020}
#'   \item{Mulch.Code}{ERA mulch code based on the nitrogen fixing properties specified in Nfix. NA for animal products}
#'   \item{Irrigation.C}{indicates the presence of irrigation in the control treatment}
#'   \item{Irrigation.T}{indicates the presence of irrigation in the experimental treatment}
#'   \item{Irrigation.Meth.C}{method of irrigation used in the control treatment. Cell will be blank if Irrigation.C is FALSE}
#'   \item{Irrigation.Meth.T}{method of irrigation used in the experimental treatment. Cell will be blank if Irrigation.T is FALSE}
#'   \item{C1:C13}{codes corresponding to all practices used on the control treatment described in C.Descrip. Only one code is placed in each column (see subpractices column of PRACTICES2 tab)}
#'   \item{T1:T13}{codes corresponding to all practices used on the CSA treatment described in T.Descrip. Only one code is placed in each column (see subpractices column of PRACTICES2 tab)}
#'   \item{ID}{study number}
#'   \item{obs_count}{number of observations}
#'   \item{out_count}{number of outcomes reported per study}
#'   \item{Lat}{latitude of study location in degrees or decimal degrees, the latter recorded to same accuracy as in publication (or estimated from googlemaps where locations were found from site names/descriptions). For decimal degrees the sign is not recorded here, but in the LatH column instead (all values should be positive)}
#'   \item{Lon}{longitude of study location in decimal degrees, recorded to same accuracy as in publication (or estimated from googlemaps where locations were found from site names or descriptions). For decimal degrees the sign is not recorded here, but in the LatH column instead. All values should be positive.}
#'   \item{yi}{the natural log of the response ratio log(MeanT/MeanC)}
#'   \item{pc}{pc=proportional change.It is a measure of the ratio of experimental to control outcomes}
#'   \item{plist}{experimental ERA practices that are in the experimental, but not the control treatment }
#'   \item{base.list}{base practices that are shared between experimental and control treatments}
#'   \item{EUlist}{the product code OutcomeCodes$Code for the observation using a - delim for multiple products}
#'   \item{Out.Pillar}{outcome pillar name}
#'   \item{Out.SubPillar}{outcome subpillar name}
#'   \item{Out.Ind}{outcome indicator name}
#'   \item{Out.SubInd}{outcome subindicator name}
#'   \item{Out.SubInd}{outcome subindicator name simplified}
#'   \item{Out.Pillar.Code}{outcome pillar name code}
#'   \item{Out.SubPillar.Code}{outcome subpillar name code}
#'   \item{Out.Ind.Code}{outcome indicator name code}
#'   \item{Out.SubInd.Code}{outcome subindicator name code}
#'   \item{SubPrName}{experimental subpractice name (note that this corresponds to the Subpractice.S field in the PracticeCodes)}
#'   \item{PrName}{experimental practice name}
#'   \item{Theme}{experimental practice theme name}
#'   \item{SubPrName.Code}{experimental subpractice name code}
#'   \item{PrName.Code}{experimental practice name code}
#'   \item{Theme.Code}{experimental practice theme name code}
#'   \item{SubPrName.Base}{base practice name (note that this corresponds to the Subpractice.S field in the PracticeCodes)}
#'   \item{PrName.Base}{base practice name}
#'   \item{Theme.Base}{base practice theme name}
#'   \item{SubPrName.Base.Code}{base subpractice name code}
#'   \item{PrName.Base.Code}{base practice name code}
#'   \item{Theme.Base.Code}{base practice theme name code}
#'   \item{Product}{the full name of the product, including aspect of the component such as grain, meat or milk}
#'   \item{Product.Type}{product type name}
#'   \item{Product.Subtype}{product subtype name}
#'   \item{Product.Simple}{simplified product name, excluding any aspects of component}
#'   \item{Product.Type.Code}{product type name code}
#'   \item{Product.Subtype.Code}{product subtype name code}
#'   \item{Product.Simple.Code}{simplified product name code}
#'   \item{Latitude}{latitude of study location in degrees or decimal degrees}
#'   \item{Longitude}{longitude of study location in degrees or decimal degrees}
#'   \item{Buffer}{manual estimation (e.g. from measuring on google earth) in m of the spatial uncertainty for a point location. The site should within an X m radius (the buffer.manual amount) of the point location specified. Unit is meters}
#'   \item{Site.Key}{longitude, latitude and buffer for the site}
#'   \item{AEZ16simple}{simplified version of AEZ16 removing the tropic/subtropic classification}
#'   \item{AEZ16}{AEZs for Africa South of the Sahara (SSA) based on the methodology developed by FAO and IIASA, using the scheme with 16 classes}
#'   \item{AEZ5}{AEZs Africa South of the Sahara (SSA) based on the methodology developed by FAO and IIASA, using the scheme with 5 classes}
#'   \item{Mean.Annual.Precip}{mean annual precipitation (mm) derived from the CHIRPS 2.0 dataset API at 0.05&deg resolution}
#'   \item{Mean.Annual.Temp}{mean annual temperature (C) derived from the NASA POWER dataset API at 0.5&deg resolution}
#'   \item{CLY}{weight percentage of clay particles (<0.0002 mm)}
#'   \item{SLT}{weight percentage of silt particles (0.0002-0.05 mm)}
#'   \item{SND}{weight percentage of the sand particles (0.05-2 mm)}
#'  ...
#' }
#' @source ERA Project Team (2021)
"ERA.Compiled"
#'
#' Practice Codes (version 31.01.2022)
#'
#' \describe{The organizational hierarchy of improved farming practice concepts considered in ERA and their descriptions.
#'   \item{Code}{unique alpha numeric code}
#'   \item{Theme}{concepts in the highest level in the practice hierarchy}
#'   \item{Theme.Code}{currently a duplicate of theme}
#'   \item{Practice}{concepts in the intermediate level of the practice hierarchy. Practice is nested under Theme.}
#'   \item{Practice.Code}{short code corresponding to the practice name for use in plotting}
#'   \item{Subpractice}{concepts in the lowest and most detailed level of the practice hierarchy. Subpractice is nested under practice.}
#'   \item{Subpractice.Code}{short code corresponding to the subpractice name for use in plotting}
#'   \item{Subpractice.S}{shorter name for subpractice}
#'   \item{Subpractice.Suffix}{suffix for the subpractice}
#'   \item{Definition}{description of Subpractice}
#'   \item{Notes}{free text used to clarify the defition of the practice}
#'   ...
#' }
#' @source ERA Project Team (2021)
"PracticeCodes"
#'
#' Outcome Codes (Version 03.05.2021")
#'
#' \describe{The organizational hierarchy of outcomes considered in ERA and their descriptions.
#'   \item{Code}{unique alpha numeric code}
#'   \item{Pillar}{concepts in the highest level in the outcome hierarchy}
#'   \item{Pillar.Code}{currently a duplicate of Pillar}
#'   \item{Subpillar}{concepts in the intermediate level of the outcome hierarchy. Subpillar is nested under Pillar}
#'   \item{Subpillar.Code}{short code corresponding to the Subpillar name for use in plotting}
#'   \item{Indicator}{concepts in the intermediate level of the outcome hierarchy. Indicator is nested under Subpillar}
#'   \item{Indicator.Code}{short code corresponding to the Indicator name for use in plotting}
#'   \item{Subindicator}{concepts in the lowest and most detailed level of the outcome hierarchy. Subindicator is nested under Indicator}
#'   \item{Subindicator.Short}{shorter name for the Subindicator}
#'   \item{Subindicator.Code}{short code corresponding to the Subindicator name for use in plotting}
#'   \item{Definition}{description of pthe Subindicator}
#'   \item{Notes}{free text used to clarify the defition of the Subindicator}
#'   \item{Example.units}{example of units that are collecting for that Subindicator}
#'   \item{Original.Outcome}{currently NA}
#'   \item{Negative.Values}{indicates whether the outcome can have negative values; N- No and Y-Yes}
#'   \item{Sign}{indicates whether the outcome is positive or negative}
#'   \item{TC.Ratio}{this is the ratio between the outcome for the treatment and control. TC = TreatmeantControl}
#'   ...
#' }
#' @source ERA Project Team (2021)
"OutcomeCodes"
#'
#' EU Codes (28.10.2021)
#'
#' \describe{The organizational hierarchy of experimental units considered in ERA and their descriptions.
#'   \item{EU}{unique alpha numeric code}
#'   \item{Product.Type}{concepts in the highest level in the experimental unit hierarchy}
#'   \item{Product.Type.Code}{currently a duplicate of Product.Type}
#'   \item{Product.Subtype}{concepts in the intermediate level of the EU hierarchy. Product.Subtype is nested under Product.Type}
#'   \item{Product.Subtype.Code}{short code corresponding to the Product.Subtype name for use in plotting}
#'   \item{Product.Subtype.Code.Check}{currently all TRUE}
#'   \item{Product}{concepts in the lowest and most detailed level of the outcome hierarchy.Product is nested under Product.Subtype}
#'   \item{Product.Simple}{shorter and cleaner name for the Product}
#'   \item{Product.Simple.Code}{short code corresponding to the Product.Simple name for use in plotting}
#'   \item{Product.Code.Check}{currently all TRUE}
#'   \item{Component}{the component of the Product that the experiment is collecting data on}
#'   \item{Latin.Name}{the scientific name for the Product.Simple}
#'   \item{Notes}{free text used to clarify any details of the Product and Product.Simple}
#'   \item{Nfix}{used to identify whether the product is a nitrogen fixing plant. NA for animal products}
#'   \item{Tree}{used to identify whether the product is a tree. NA for animal products}
#'   \item{Mulched}{ERA mulch code based on the nitrogen fixing properties specified in Nfix. NA for animal products}
#'   \item{Incorp}{ERA mulch incorporated code based on the nitrogen fixing properties specified in Nfix. NA for animal products}
#'   \item{Unknown.Fate}{ERA code for unknown fate of the crop biomass. NA for animal products}
#'   \item{ECOCROP.Name}{scientific name of the product exported from FAO's crop model- Ecocrop}
#'   \item{ECOCROP.Notes}{free text to clarify andy details in ECOCROP.Name}
#'   ...
#' }
#'
#' @source ERA Project Team (2021)
"EUCodes"
#'
#' ERA_Bibliography
#'
#' \describe{The fields used to collect details of the publication used to collect ERA data.
#'   \item{CATEGORY}{the type of publication}
#'   \item{AUTHOR}{authors of the publication}
#'   \item{BOOKTITLE}{title of the book. NA if the publication is in a journal}
#'   \item{JOURNAL}{journal in which the article was published}
#'   \item{NUMBER}{journal number, which refers to how many times that periodical has been published during that year}
#'   \item{PAGES}{pages of the journal or book}
#'   \item{TITLE}{the title of the article}
#'   \item{TYPE}{type of article}
#'   \item{VOLUME}{the volumn of a journal}
#'   \item{YEAR}{the year of publication}
#'   \item{DOI}{doi of the article. DOI = Digital Object Identifier}
#'   \item{ISSN}{issn of the article. ISNN = International Standard Serial Number}
#'   \item{URL}{url of the article. The URL provides the wed address of the journal. URL = Uniform Resource Locator}
#'   \item{ERACODE}{unique data entry code}
#'   \item{KEYWORDS}{keywords used in the article}
#'   \item{ABSTRACT}{a brief summary of the publication, collected from the article}
#'   ...
#' }
#' @source ERA Project Team (2021)
"ERA_Bibliography"
#'
#' ERA_Search_Terms
#'
#'  \describe{
#'    \item{Database}{Scopus and WoS (Web of Science) are abstract and citation database of peer-reviewed literature, scientific journals, books and conference proceedings. Scopus: https://www.scopus.com/home.uri; WoS: https://www.webofscience.com/wos/alldb/basic-search}
#'    \item{Topic}{topic is the high level focal concept that the search focusses on, we have practices (list in parenthesis), outcomes (list in parenthesis) and geographic region}
#'    \item{Keywords}{keywords used in the peer-reviewed literature}
#'    \item{Date}{the year of publication}
#'    ...
#'}
#' @source ERA Project Team (2021)
"ERA_Search_Terms"
#'
#' ERA_Physical
#'
#'  \describe{
#'    \item{Latitude}{latitude of study location in decimal degrees}
#'    \item{Longitude}{longitude of study location in decimal degrees}
#'    \item{Buffer}{estimation in m of the spatial uncertainty for a point location}
#'    \item{Site.Key}{longitude, latitude and buffer for the site}
#'    \item{Country}{country in which the site is located in}
#'    \item{ISO.3166.1.alpha.3}{three-letter country codes. See: https://www.iso.org/obp/ui/#search}
#'    \item{DEMcells}{number of DEM cells within the site buffer}
#'    \item{Altitude.med}{median altitude of the site}
#'    \item{Altitude.mean}{mean altitude of the site}
#'    \item{Altitude.sd}{standard deviation of altitude at the site}
#'    \item{Altitude.max}{maximum altitude of the site}
#'    \item{Altitude.min}{minimum altitude of the site}
#'    \item{Slope.med}{median slope of the site}
#'    \item{Slope.mean}{mean slope of the site}
#'    \item{Slope.sd}{standard deviation of slope at the site}
#'    \item{Slope.max}{maximum slope of the site}
#'    \item{Slope.min}{minimum slope of the site}
#'    \item{Aspect.med}{median aspect of the site}
#'    \item{Aspect.mean}{mean aspect of the site}
#'    \item{Aspect.sd}{standard deviation of aspect at the site}
#'    ...
#'  }
#' @source Void filled Aster GDEM version 2 downloaded from http://www.viewfinderpanoramas.org/Coverage%20map%20viewfinderpanoramas_org3.htm
"ERA_Physical"
#'
#' Bioclimatic Variables
#'
#' \describe{There are  19 bioclimatic variables which are derived from the monthly temperature and rainfall values in order to generate biologically meaningful variables.
#' These are often used in species distribution modeling and related ecological modeling techniques. The bioclimatic variables represent annual
#' trends (e.g., mean annual temperature, annual precipitation) seasonality (e.g., annual range in temperature and precipitation) and extreme or
#' limiting environmental factors (e.g., temperature of the coldest and warmest month, and precipitation of the wet and dry quarters). A quarter
#' is a period of three months (1/4 of the year).
#' Bioclim data are extracted and summarized for each unique ERA location plus its buffer of spatial uncertainty.
#'   \item{bio_1}{Annual Mean Temperature}
#'   \item{bio_2}{Mean Diurnal Range (Mean of monthly (max temp - min temp))}
#'   \item{bio_3}{Isothermality (BIO2/BIO7) (* 100)}
#'   \item{bio_4}{Temperature Seasonality (standard deviation *100)}
#'   \item{bio_5}{Max Temperature of Warmest Month}
#'   \item{bio_6}{Min Temperature of Coldest Month}
#'   \item{bio_7}{Temperature Annual Range (BIO5-BIO6)}
#'   \item{bio_8}{Mean Temperature of Wettest Quarter}
#'   \item{bio_9}{Mean Temperature of Driest Quarter}
#'   \item{bio_10}{Mean Temperature of Warmest Quarter}
#'   \item{bio_11}{Mean Temperature of Coldest Quarter}
#'   \item{bio_12}{Annual Precipitation}
#'   \item{bio_13}{Precipitation of Wettest Month}
#'   \item{bio_14}{Precipitation of Driest Month}
#'   \item{bio_15}{Precipitation Seasonality (Coefficient of Variation)}
#'   \item{bio_16}{Precipitation of Wettest Quarter}
#'   \item{bio_17}{Precipitation of Driest Quarter}
#'   \item{bio_18}{Precipitation of Warmest Quarter}
#'   \item{bio_19}{Precipitation of Coldest Quarter}
#'   ...
#' }
#' @source https://www.worldclim.org/data/bioclim.html version 2.1 climate data for 1970-2000, released in January 2020.
"ERA_BioClim"
#'
#' Landcover
#'
#' \describe{
#'  The CCI-LC project delivers a new time series of 24 consistent global LC maps at 300 m spatial resolution on an annual basis from 1992 to 2015.
#'  The number of raster cells of each landcover class for the year 2015 is summed for each unique ERA location and its buffer of spatial uncertainty.
#'  A description of the landcover classes (fields) in this dataset can be found in the *`ERA_CCI_LC_15_Fields`* object.
#'   ...
#' }
#' @source CCI-LC Land Cover Maps - v2.0.7 http://maps.elie.ucl.ac.be/CCI/viewer/download.php
"ERA_CCI_LC_15"
#'
#' ISDA Soil Parameters
#'
#' \describe{
#' Using the  African Soil and Agronomy Data Cube ISDA soil data were downloaded and summarized for each unique ERA locations and its buffer of spatial
#' uncertainty. Variable information & descriptions can be found on the African Soil and Agronomy Data Cube website.
#'   \item{Median}{median value of the soil variable}
#'   \item{Mean}{mean value of the soil variable}
#'   \item{SD}{standard deviation of the soil variable}
#'   \item{Mode}{mode value of the soil variable}
#'   \item{Sum}{sum of the soil variable}
#'   \item{Ncells}{number of ISDA cells within buffer}
#'   \item{NAcells}{number of NA ISDA cells within buffer}
#'   \item{Variable}{soil variables including the soil depth. Variable include bulk density, soil pH, sand, silt and clay content etc}
#'   \item{Site.Key}{longitude, latitude and buffer for the site}
#'   ...
#' }
#' @source ISDA: https://www.isda-africa.com/isdasoil/  African Soil and Agronomy Data Cube: https://gitlab.com/openlandmap/africa-soil-and-agronomy-data-cube.
"ERA_ISDA"
#'
#' Agroecological zones
#'
#' \describe{For each of the unique ERA locations and its buffer of spatial uncertainty, the agroecological zone is determined based on one of the three
#' classification schemes
#'   \item{Site.Key}{longitude, latitude and buffer for the unique ERA location}
#'   \item{AEZ16simple}{shorter name for AEZ16}
#'   \item{AEZ16}{agroecological zone using the scheme with 16 classes}
#'   \item{AEZ5}{agroecological zone using the scheme with 5 classes}
#'   \item{Mean.Annual.Precip}{mean annual precipitation at the ERA location}
#'   \item{Mean.Annual.Temp}{mean annual precipitation at the ERA location}
#'   ...
#' }
#' @source HarvestChoice; International Food Policy Research Institute (IFPRI), 2015, "Agro-Ecological Zones for Africa South of the Sahara", https://doi.org/10.7910/DVN/M7XIUB, Harvard Dataverse, V3
"ERA_AEZ_MAP_MAT"
#'
#' SoilGrids Parameters
#'
#' \describe{
#' Using ISRIC World Soil Information, soil data were downloaded and summarized for each unique ERA locations and its buffer of spatial
#' uncertainty.
#'   \item{Latitude}{latitude of study location in decimal degrees}
#'   \item{Longitude}{longitude of study location in decimal degrees}
#'   \item{Buffer}{estimation in m of the spatial uncertainty for a point location}
#'   \item{Site.Key}{longitude, latitude and buffer for the site}
#'   \item{ACDWRB_M_ss_250m.Mean}{mean values of Acid sub-soils grade. Grade of a sub-soil being acid e.g. having a pH < 5 and low BS}
#'   \item{ACDWRB_M_ss_250m.SD}{standard deviation of Acid sub-soils grade. Grade of a sub-soil being acid e.g. having a pH < 5 and low BS}
#'   \item{ACDWRB_M_ss_250m.Quantiles}{quantiles of Acid sub-soils grade. Grade of a sub-soil being acid e.g. having a pH < 5 and low BS}
#'   \item{AWCh1_M_sl1_250m.Mean}{mean value of Available soil water capacity (volumetric fraction) for h1 (moisture potential in kPa, e.g. -10 (pF 2.0)) at a dept of 0-5cm. Unit = percent}
#'   \item{AWCh1_M_sl1_250m.SD}{standard deviation of Available soil water capacity (volumetric fraction) for h1 (moisture potential in kPa, e.g. -10 (pF 2.0)) at a dept of 0-5cm. Unit = percent}
#'   \item{AWCh1_M_sl1_250m.Quantiles}{quantiles of Available soil water capacity (volumetric fraction) for h1 (moisture potential in kPa, e.g. -10 (pF 2.0)) at a dept of 0-5cm. Unit = percent}
#'   \item{AWCh1_M_sl2_250m.Mean}{mean value of Available soil water capacity (volumetric fraction) for h1 (moisture potential in kPa, e.g. -10 (pF 2.0)) at a depth of 5-15cm. Unit = percent}
#'   \item{AWCh1_M_sl2_250m.SD}{standard deviation of Available soil water capacity (volumetric fraction) for h1 (moisture potential in kPa, e.g. -10 (pF 2.0)) at a depth of 5-15cm. Unit = percent}
#'   \item{AWCh1_M_sl2_250m.Quantiles}{quantiles of Available soil water capacity (volumetric fraction) for h1 (moisture potential in kPa, e.g. -10 (pF 2.0)) at a depth of 5-15cm. Unit = percent}
#'   \item{AWCh1_M_sl3_250m.Mean}{mean value of Available soil water capacity (volumetric fraction) for h1 (moisture potential in kPa, e.g. -10 (pF 2.0)) at a depth of 15-30cm. Unit = percent}
#'   \item{AWCh1_M_sl3_250m.SD}{standard deviation of Available soil water capacity (volumetric fraction) for h1 (moisture potential in kPa, e.g. -10 (pF 2.0)) at a depth of 15-30cm. Unit = percent}
#'   \item{AWCh1_M_sl3_250m.Quantiles}{quantiles of Available soil water capacity (volumetric fraction) for h1 (moisture potential in kPa, e.g. -10 (pF 2.0)) at a depth of 15-30cm. Unit = percent}
#'   \item{AWCh2_M_sl1_250m.Mean}{mean value of Available soil water capacity (volumetric fraction) for h2 (moisture potential in kPa, e.g. -20 (pF 2.3)) at a depth of 0-5cm. Unit = percent}
#'   \item{AWCh2_M_sl1_250m.SD}{standard deviation of Available soil water capacity (volumetric fraction) for h2 (moisture potential in kPa, e.g. -20 (pF 2.3)) at a depth of 0-5cm. Unit = percent}
#'   \item{AWCh2_M_sl1_250m.Quantiles}{quantiles of Available soil water capacity (volumetric fraction) for h2 (moisture potential in kPa, e.g. -20 (pF 2.3)) at a depth of 0-5cm. Unit = percent}
#'   \item{AWCh2_M_sl2_250m.Mean}{mean value of Available soil water capacity (volumetric fraction) for h2 (moisture potential in kPa, e.g. -20 (pF 2.3)) at a depth of 5-15cm. Unit = percent}
#'   \item{AWCh2_M_sl2_250m.SD}{standard deviation of Available soil water capacity (volumetric fraction) for h2 (moisture potential in kPa, e.g. -20 (pF 2.3)) at a depth of 5-15cm. Unit = percent}
#'   \item{AWCh2_M_sl2_250m.Quantiles}{quantiles of Available soil water capacity (volumetric fraction) for h2 (moisture potential in kPa, e.g. -20 (pF 2.3)) at a depth of 5-15cm. Unit = percent}
#'   \item{AWCh2_M_sl3_250m.Mean}{mean value of Available soil water capacity (volumetric fraction) for h2 (moisture potential in kPa, e.g. -20 (pF 2.3)) at a depth of 15-30cm. Unit = percent}
#'   \item{AWCh2_M_sl3_250m.SD}{standard deviation of}{Available soil water capacity (volumetric fraction) for h2 (moisture potential in kPa, e.g. -20 (pF 2.3)) at a depth of 15-30cm. Unit = percent}
#'   \item{AWCh2_M_sl3_250m.Quantiles}{quantiles of Available soil water capacity (volumetric fraction) for h2 (moisture potential in kPa, e.g. -20 (pF 2.3)) at a depth of 15-30cm. Unit = percent}
#'   \item{AWCh3_M_sl1_250m.Mean}{mean value of Available soil water capacity (volumetric fraction) for h3 (moisture potential in kPa, e.g. -31.6  (pF 2.5)) at a dept of 0-5cm. Unit = percent}
#'   \item{AWCh3_M_sl1_250m.SD}{standard deviation of Available soil water capacity (volumetric fraction) for h3 (moisture potential in kPa, e.g. -31.6  (pF 2.5)) at a dept of 0-5cm. Unit = percent}
#'   \item{AWCh3_M_sl1_250m.Quantiles}{quantiles of Available soil water capacity (volumetric fraction) for h3 (moisture potential in kPa, e.g. -31.6  (pF 2.5)) at a dept of 0-5cm. Unit = percent}
#'   \item{AWCh3_M_sl2_250m.Mean}{mean value of Available soil water capacity (volumetric fraction) for h3 (moisture potential in kPa, e.g. -31.6  (pF 2.5)) at a dept of 5-15cm. Unit = percent}
#'   \item{AWCh3_M_sl2_250m.SD}{standard deviation of Available soil water capacity (volumetric fraction) for h3 (moisture potential in kPa, e.g. -31.6  (pF 2.5)) at a dept of 5-15cm. Unit = percent}
#'   \item{AWCh3_M_sl2_250m.Quantiles}{quantiles of Available soil water capacity (volumetric fraction) for h3 (moisture potential in kPa, e.g. -31.6  (pF 2.5)) at a dept of 5-15cm. Unit = percent}
#'   \item{AWCh3_M_sl3_250m.Mean}{mean value of Available soil water capacity (volumetric fraction) for h3 (moisture potential in kPa, e.g. -31.6  (pF 2.5)) at a dept of 15-30cm. Unit = percent}
#'   \item{AWCh3_M_sl3_250m.SD}{standard deviation of Available soil water capacity (volumetric fraction) for h3 (moisture potential in kPa, e.g. -31.6  (pF 2.5)) at a dept of 15-30cm. Unit = percent}
#'   \item{AWCh3_M_sl3_250m.Quantiles}{quantiles of Available soil water capacity (volumetric fraction) for h3 (moisture potential in kPa, e.g. -31.6  (pF 2.5)) at a dept of 15-30cm. Unit = percent}
#'   \item{AWCtS_M_sl1_250m.Mean}{mean value of Saturated water content (volumetric fraction) for tS at a depth of 0-5cm. Unit = percent}
#'   \item{AWCtS_M_sl1_250m.SD}{standard deviation of Saturated water content (volumetric fraction) for tS at a depth of 0-5cm. Unit = percent}
#'   \item{AWCtS_M_sl1_250m.Quantiles}{quantiles of Saturated water content (volumetric fraction) for tS at a depth of 0-5cm. Unit = percent}
#'   \item{AWCtS_M_sl2_250m.Mean}{mean value of Saturated water content (volumetric fraction) for tS at a depth of 5-15cm. Unit = percent}
#'   \item{AWCtS_M_sl2_250m.SD}{standard deviation of Saturated water content (volumetric fraction) for tS at a depth of 5-15cm. Unit = percent}
#'   \item{AWCtS_M_sl2_250m.Quantiles}{quantiles of Saturated water content (volumetric fraction) for tS at a depth of 5-15cm. Unit = percent}
#'   \item{AWCtS_M_sl3_250m.Mean}{mean value of Saturated water content (volumetric fraction) for tS at a depth of 15-30cm. Unit = percent}
#'   \item{AWCtS_M_sl3_250m.SD}{standard deviation of Saturated water content (volumetric fraction) for tS at a depth of 15-30cm. Unit = percent}
#'   \item{AWCtS_M_sl3_250m.Quantiles}{quantiles of Saturated water content (volumetric fraction) for tS at a depth of 15-30cm. Unit = percent}
#'   \item{BDRICM_M_250m.Mean}{mean value of Depth to bedrock (R horizon) up to 200 cm. Unit = cm}
#'   \item{BDRICM_M_250m.SD}{standard deviation of Depth to bedrock (R horizon) up to 200 cm. Unit = cm}
#'   \item{BDRICM_M_250m.Quantiles}{quantiles of Depth to bedrock (R horizon) up to 200 cm. Unit = cm}
#'   \item{BDRLOG_M_250m.Mean}{mean value of the Probability of occurrence of R horizon. Unit = percent}
#'   \item{BDRLOG_M_250m.SD}{standard deviation of the  Probability of occurrence of R horizon. Unit = percent}
#'   \item{BDRLOG_M_250m.Quantiles}{quantile of the Probability of occurrence of R horizon. Unit = percent}
#'   \item{BDTICM_M_250m.Mean}{mean value of the Absolute depth to bedrock. Unit = cm}
#'   \item{BDTICM_M_250m.SD}{standard deviation of the Absolute depth to bedrock. Unit = cm}
#'   \item{BDTICM_M_250m.Quantiles}{quantile of the Absolute depth to bedrock. Unit = cm}
#'   \item{BLDFIE_M_sl1_250m.Mean}{mean value of Bulk density (fine earth) in kg per cubic meter at a depth of 0-5cm. Unit = kg/cubic-m}
#'   \item{BLDFIE_M_sl1_250m.SD}{standard deviation of Bulk density (fine earth) in kg per cubic meter at a depth of 0-5cm. Unit = kg/cubic-m}
#'   \item{BLDFIE_M_sl1_250m.Quantiles}{quantiles of Bulk density (fine earth) in kg per cubic meter at a depth of 0-5cm. Unit = kg/cubic-m}
#'   \item{BLDFIE_M_sl2_250m.Mean}{mean value of Bulk density (fine earth) in kg per cubic meter at a depth of 5-15cm. Unit = kg/cubic-m}
#'   \item{BLDFIE_M_sl2_250m.SD}{standard deviation of Bulk density (fine earth) in kg per cubic meter at a depth of 5-15cm. Unit = kg/cubic-m}
#'   \item{BLDFIE_M_sl2_250m.Quantiles}{quantiles of Bulk density (fine earth) in kg per cubic meter at a depth of 5-15cm. Unit = kg/cubic-m}
#'   \item{BLDFIE_M_sl3_250m.Mean}{mean value of Bulk density (fine earth) in kg per cubic meter at a depth of 15-30cm. Unit = kg/cubic-m}
#'   \item{BLDFIE_M_sl3_250m.SD}{standard deviation of Bulk density (fine earth) in kg per cubic meter at a depth of 15-30cm. Unit = kg/cubic-m}
#'   \item{BLDFIE_M_sl3_250m.Quantiles}{quantiles of Bulk density (fine earth) in kg per cubic meter at a depth of 15-30cm. Unit = kg/cubic-m}
#'   \item{CECSOL_M_sl1_250m.Mean}{mean of Cation Exchange Capacity of soil at a depth of 0-5cm. Unit = cmol/kg}
#'   \item{CECSOL_M_sl1_250m.SD}{standard deviation of Cation Exchange Capacity of soil at a depth of 0-5cm. Unit = cmol/kg}
#'   \item{CECSOL_M_sl1_250m.Quantiles}{quantiles of Cation Exchange Capacity of soil at a depth of 0-5cm. Unit = cmol/kg}
#'   \item{CECSOL_M_sl2_250m.Mean}{mean of Cation Exchange Capacity of soil at a depth of 5-15cm. Unit = cmol/kg}
#'   \item{CECSOL_M_sl2_250m.SD}{standard deviation of Cation Exchange Capacity of soil at a depth of 5-15cm. Unit = cmol/kg}
#'   \item{CECSOL_M_sl2_250m.Quantiles}{Cation Exchange Capacity of soil at a depth of 5-15cm. Unit = cmol/kg}
#'   \item{CECSOL_M_sl3_250m.Mean}{mean of Cation Exchange Capacity of soil at a depth of 15-30cm. Unit = cmol/kg}
#'   \item{CECSOL_M_sl3_250m.SD}{standard deviation of Cation Exchange Capacity of soil at a depth of 15-30cm. Unit = cmol/kg}
#'   \item{CECSOL_M_sl3_250m.Quantiles}{standard deviation of Cation Exchange Capacity of soil at a depth of 15-30cm. Unit = cmol/kg}
#'   \item{CLYPPT_M_sl1_250m.Mean}{mean of Weight percentage of the clay particles (<0.0002 mm) at a depth of 0-5cm. Unit = percent}
#'   \item{CLYPPT_M_sl1_250m.SD}{standard deviation of Weight percentage of the clay particles (<0.0002 mm) at a depth of 0-5cm. Unit = percent}
#'   \item{CLYPPT_M_sl1_250m.Quantiles}{quantiles of Weight percentage of the clay particles (<0.0002 mm) at a depth of 0-5cm. Unit = percent}
#'   \item{CLYPPT_M_sl2_250m.Mean}{mean of Weight percentage of the clay particles (<0.0002 mm) at a depth of 5-15cm. Unit = percent}
#'   \item{CLYPPT_M_sl2_250m.SD}{standard deviation of Weight percentage of the clay particles (<0.0002 mm) at a depth of 5-15cm. Unit = percent}
#'   \item{CLYPPT_M_sl2_250m.Quantiles}{quantiles of Weight percentage of the clay particles (<0.0002 mm) at a depth of 5-15cm. Unit = percent}
#'   \item{CLYPPT_M_sl3_250m.Mean}{mean of Weight percentage of the clay particles (<0.0002 mm) at a depth of 15-30cm. Unit = percent}
#'   \item{CLYPPT_M_sl3_250m.SD}{standard deviation of Weight percentage of the clay particles (<0.0002 mm) at a depth of 15-30cm. Unit = percent}
#'   \item{CLYPPT_M_sl3_250m.Quantiles}{quantiles of Weight percentage of the clay particles (<0.0002 mm) at a depth of 15-30cm. Unit = percent}
#'   \item{CRFVOL_M_sl1_250m.Mean}{mean value of Volumetric percentage of coarse fragments (>2 mm) at a depth of 0-5cm. Unit = percent}
#'   \item{CRFVOL_M_sl1_250m.SD}{standard deviation of Volumetric percentage of coarse fragments (>2 mm) at a depth of 0-5cm. Unit = percent}
#'   \item{CRFVOL_M_sl1_250m.Quantiles}{quantiles of Volumetric percentage of coarse fragments (>2 mm) at a depth of 0-5cm. Unit = percent}
#'   \item{CRFVOL_M_sl2_250m.Mean}{mean value of Volumetric percentage of coarse fragments (>2 mm) at a depth of 5-15cm. Unit = percent}
#'   \item{CRFVOL_M_sl2_250m.SD}{standard deviation of Volumetric percentage of coarse fragments (>2 mm) at a depth of 5-15cm. Unit = percent}
#'   \item{CRFVOL_M_sl2_250m.Quantiles}{quantiles of Volumetric percentage of coarse fragments (>2 mm) at a depth of 5-15cm. Unit = percent}
#'   \item{CRFVOL_M_sl3_250m.Mean}{mean value of Volumetric percentage of coarse fragments (>2 mm) at a depth of 15-30cm. Unit = percent}
#'   \item{CRFVOL_M_sl3_250m.SD}{standard deviation of Volumetric percentage of coarse fragments (>2 mm) at a depth of 15-30cm. Unit = percent}
#'   \item{CRFVOL_M_sl3_250m.Quantiles}{quantiles of Volumetric percentage of coarse fragments (>2 mm) at a depth of 15-30cm. Unit = percent}
#'   \item{gyga_af_erzd_limfactor__m_1km.Mean}{mean of rootable depth limiting soil factor for maize, mapped at 1km resolution}
#'   \item{gyga_af_erzd_limfactor__m_1km.SD}{standard deviation of rootable depth limiting soil factor for maize, mapped at 1km resolution}
#'   \item{gyga_af_erzd_limfactor__m_1km.Quantiles}{quantiles of rootable depth limiting soil factor for maize, mapped at 1km resolution}
#'   \item{OCSTHA_M_sd1_250m.Mean}{mean value of Soil organic carbon stock in tons per ha at a depth of 0-5cm. Unit = tonnes/ha}
#'   \item{OCSTHA_M_sd1_250m.SD}{standard deviation of Soil organic carbon stock in tons per ha at a depth of 0-5cm. Unit = tonnes/ha}
#'   \item{OCSTHA_M_sd1_250m.Quantiles}{quantiles of Soil organic carbon stock in tons per ha at a depth of 0-5cm. Unit = tonnes/ha}
#'   \item{OCSTHA_M_sd2_250m.Mean}{mean value of Soil organic carbon stock in tons per ha at a depth of 5-15cm. Unit = tonnes/ha}
#'   \item{OCSTHA_M_sd2_250m.SD}{standard deviation of Soil organic carbon stock in tons per ha at a depth of 5-15cm. Unit = tonnes/ha}
#'   \item{OCSTHA_M_sd2_250m.Quantiles}{quantiles of Soil organic carbon stock in tons per ha at a depth of 5-15cm. Unit = tonnes/ha}
#'   \item{OCSTHA_M_sd3_250m.Mean}{mean value of Soil organic carbon stock in tons per ha at a depth of 15-30cm. Unit = tonnes/ha}
#'   \item{OCSTHA_M_sd3_250m.SD}{standard deviation of Soil organic carbon stock in tons per ha at a depth of 15-30cm. Unit = tonnes/ha}
#'   \item{OCSTHA_M_sd3_250m.Quantiles}{quantiles of Soil organic carbon stock in tons per ha at a depth of 15-30cm. Unit = tonnes/ha}
#'   \item{ORCDRC_M_sl1_250m.Mean}{mean value of soil organic carbon content in permille at a depth of 0-5cm. Unit = g/kg}
#'   \item{ORCDRC_M_sl1_250m.SD}{standard deviation of soil organic carbon content in permille at a depth of 0-5cm. Unit = g/kg}
#'   \item{ORCDRC_M_sl1_250m.Quantiles}{quantiles of soil organic carbon content in permille at a depth of 0-5cm. Unit = g/kg}
#'   \item{ORCDRC_M_sl2_250m.Mean}{mean value of soil organic carbon content in permille at a depth of 5-15cm. Unit = g/kg}
#'   \item{ORCDRC_M_sl2_250m.SD}{standard deviation of soil organic carbon content in permille at a depth of 5-15cm. Unit = g/kg}
#'   \item{ORCDRC_M_sl2_250m.Quantiles}{quantiles of soil organic carbon content in permille at a depth of 5-15cm. Unit = g/kg}
#'   \item{ORCDRC_M_sl3_250m.Mean}{mean value of soil organic carbon content in permille at a depth of 15-30cm. Unit = g/kg}
#'   \item{ORCDRC_M_sl3_250m.SD}{standard deviation of soil organic carbon content in permille at a depth of 15-30cm. Unit = g/kg}
#'   \item{ORCDRC_M_sl3_250m.Quantiles}{quantiles of soil organic carbon content in permille at a depth of 15-30cm. Unit = g/kg}
#'   \item{PHIHOX_M_sl1_250m.Mean}{mean value of pH index measured in water solution at a depth of 0-5cm. Unit = index*10}
#'   \item{PHIHOX_M_sl1_250m.SD}{standard deviation of pH index measured in water solution at a depth of 0-5cm. Unit = index*10}
#'   \item{PHIHOX_M_sl1_250m.Quantiles}{quantiles of pH index measured in water solution at a depth of 0-5cm. Unit = index*10}
#'   \item{PHIHOX_M_sl2_250m.Mean}{mean value of pH index measured in water solution at a depth of 5-15cm. Unit = index*10}
#'   \item{PHIHOX_M_sl2_250m.SD}{pH index measured in water solution at a depth of 5-15cm. Unit = index*10}
#'   \item{PHIHOX_M_sl2_250m.Quantiles}{pH index measured in water solution at a depth of 5-15cm. Unit = index*10}
#'   \item{PHIHOX_M_sl3_250m.Mean}{mean value of pH index measured in water solution at a depth of 15-30cm. Unit = index*10}
#'   \item{PHIHOX_M_sl3_250m.SD}{pH index measured in water solution at a depth of 15-30cm. Unit = index*10}
#'   \item{PHIHOX_M_sl3_250m.Quantiles}{pH index measured in water solution at a depth of 15-30cm. Unit = index*10}
#'   \item{PHIKCL_M_sl1_250m.Mean}{mean value of pH index measured in KCl solution at a depth of 0-5cm. Unit = index*10}
#'   \item{PHIKCL_M_sl1_250m.SD}{standard deviation of pH index measured in KCl solution at a depth of 0-5cm. Unit = index*10}
#'   \item{PHIKCL_M_sl1_250m.Quantiles}{quantiles of pH index measured in KCl solution at a depth of 0-5cm. Unit = index*10}
#'   \item{PHIKCL_M_sl2_250m.Mean}{mean value of pH index measured in KCl solution at a depth of 5-15cm. Unit = index*10}
#'   \item{PHIKCL_M_sl2_250m.SD}{standard deviation of pH index measured in KCl solution at a depth of 5-15cm. Unit = index*10}
#'   \item{PHIKCL_M_sl2_250m.Quantiles}{quantiles of pH index measured in KCl solution at a depth of 5-15cm. Unit = index*10}
#'   \item{PHIKCL_M_sl3_250m.Mean}{mean value of pH index measured in KCl solution at a depth of 15-30cm. Unit = index*10}
#'   \item{PHIKCL_M_sl3_250m.SD}{standard deviation of pH index measured in KCl solution at a depth of 15-30cm. Unit = index*10}
#'   \item{PHIKCL_M_sl3_250m.Quantiles}{quantiles of pH index measured in KCl solution at a depth of 15-30cm. Unit = index*10}
#'   \item{SLGWRB_250m.Mean}{mean value of Sodic soil grade based on WRB soil types and soil pH}
#'   \item{SLGWRB_250m.SD}{standard deviation of Sodic soil grade based on WRB soil types and soil pH}
#'   \item{SLGWRB_250m.Quantiles}{quantiles of Sodic soil grade based on WRB soil types and soil pH}
#'   \item{SLTPPT_M_sl1_250m.Mean}{mean value of Weight percentage of the silt particles (0.0002-0.05 mm) at a depth of 0-5cm. Unit = percent}
#'   \item{SLTPPT_M_sl1_250m.SD}{standard deviation of Weight percentage of the silt particles (0.0002-0.05 mm) at a depth of 0-5cm. Unit = percent}
#'   \item{SLTPPT_M_sl1_250m.Quantiles}{quantiles of Weight percentage of the silt particles (0.0002-0.05 mm) at a depth of 0-5cm. Unit = percent}
#'   \item{SLTPPT_M_sl2_250m.Mean}{mean value of Weight percentage of the silt particles (0.0002-0.05 mm) at a depth of 5-15cm. Unit = percent}
#'   \item{SLTPPT_M_sl2_250m.SD}{standard deviation of Weight percentage of the silt particles (0.0002-0.05 mm) at a depth of 5-15cm. Unit = percent}
#'   \item{SLTPPT_M_sl2_250m.Quantiles}{quantiles of Weight percentage of the silt particles (0.0002-0.05 mm) at a depth of 5-15cm. Unit = percent}
#'   \item{SLTPPT_M_sl3_250m.Mean}{mean value of Weight percentage of the silt particles (0.0002-0.05 mm) at a depth of 15-30cm. Unit = percent}
#'   \item{SLTPPT_M_sl3_250m.SD}{standard deviation of Weight percentage of the silt particles (0.0002-0.05 mm) at a depth of 15-30cm. Unit = percent}
#'   \item{SLTPPT_M_sl3_250m.Quantiles}{quantiles of Weight percentage of the silt particles (0.0002-0.05 mm) at a depth of 0-5cm. Unit = percent}
#'   \item{SNDPPT_M_sl1_250m.Mean}{mean value of Weight percentage of the sand particles (0.05-2 mm) at a depth of 0-5cm. Unit = percent}
#'   \item{SNDPPT_M_sl1_250m.SD}{standard deviation of Weight percentage of the sand particles (0.05-2 mm) at a depth of 0-5cm. Unit = percent}
#'   \item{SNDPPT_M_sl1_250m.Quantiles}{quantiles of Weight percentage of the sand particles (0.05-2 mm) at a depth of 0-5cm. Unit = percent}
#'   \item{SNDPPT_M_sl2_250m.Mean}{mean value of Weight percentage of the sand particles (0.05-2 mm) at a depth of 5-15cm. Unit = percen}
#'   \item{SNDPPT_M_sl2_250m.SD}{standard deviation of Weight percentage of the sand particles (0.05-2 mm) at a depth of 5-15cm. Unit = percent}
#'   \item{SNDPPT_M_sl2_250m.Quantiles}{quantiles of Weight percentage of the sand particles (0.05-2 mm) at a depth of 5-15cm. Unit = percent}
#'   \item{SNDPPT_M_sl3_250m.Mean}{mean value of Weight percentage of the sand particles (0.05-2 mm) at a depth of 15-30cm. Unit = percen}
#'   \item{SNDPPT_M_sl3_250m.SD}{standard deviation of Weight percentage of the sand particles (0.05-2 mm) at a depth of 15-30cm. Unit = percent}
#'   \item{SNDPPT_M_sl3_250m.Quantiles}{quantiles of Weight percentage of the sand particles (0.05-2 mm) at a depth of 15-30cm. Unit = percent}
#'   \item{TAXNWRB_250m.Mode}{mode of Predicted WRB 2006 subgroup classes}
#'   \item{TAXOUSDA_250m.Mode}{predicted WRB 2006 subgroup classes}
#'   \item{TEXMHT_M_sl1_250m.Mean}{mean value of Texture class (USDA system) at a depth of 0-5cm. Unit = factor}
#'   \item{TEXMHT_M_sl1_250m.SD}{standard deviation of Texture class (USDA system) at a depth of 0-5cm. Unit = factor}
#'   \item{TEXMHT_M_sl1_250m.Quantiles}{quantiles of Texture class (USDA system) at a depth of 0-5cm. Unit = factor}
#'   \item{TEXMHT_M_sl2_250m.Mean}{mean value of Texture class (USDA system) at a depth of 5-15cm. Unit = factor}
#'   \item{TEXMHT_M_sl2_250m.SD}{standard deviation of Texture class (USDA system) at a depth of 5-15cm. Unit = factor}
#'   \item{TEXMHT_M_sl2_250m.Quantiles}{quantiles of Texture class (USDA system) at a depth of 5-15cm. Unit = factor}
#'   \item{TEXMHT_M_sl3_250m.Mean}{mean value of Texture class (USDA system) at a depth of 15-30cm. Unit = factor}
#'   \item{TEXMHT_M_sl3_250m.SD}{standard deviation of Texture class (USDA system) at a depth of 15-30cm. Unit = factor}
#'   \item{TEXMHT_M_sl3_250m.Quantiles}{quantiles of Texture class (USDA system) at a depth of 15-30cm. Unit = factor}
#'   \item{WWP_M_sl1_250m.Mean}{mean value of available soil water capacity (volumetric fraction) until wilting point at a depth of 0-5cm. Unit = percent}
#'   \item{WWP_M_sl1_250m.SD}{standard deviation of available soil water capacity (volumetric fraction) until wilting point at a depth of 0-5cm. Unit = percent}
#'   \item{WWP_M_sl1_250m.Quantiles}{quantiles of available soil water capacity (volumetric fraction) until wilting point at a depth of 0-5cm. Unit = percent}
#'   \item{WWP_M_sl2_250m.Mean}{mean value of available soil water capacity (volumetric fraction) until wilting point at a depth of 5-15cm. Unit = percent}
#'   \item{WWP_M_sl2_250m.SD}{standard deviation of available soil water capacity (volumetric fraction) until wilting point at a depth of 5-15cm. Unit = percent}
#'   \item{WWP_M_sl2_250m.Quantiles}{quantiles of available soil water capacity (volumetric fraction) until wilting point at a depth of 5-15cm. Unit = percent}
#'   \item{WWP_M_sl3_250m.Mean}{mean value of available soil water capacity (volumetric fraction) until wilting point at a depth of 15-30cm. Unit = percent}
#'   \item{WWP_M_sl3_250m.SD}{standard deviation of available soil water capacity (volumetric fraction) until wilting point at a depth of 15-30cm. Unit = percent}
#'   \item{WWP_M_sl3_250m.Quantiles}{quantiles of available soil water capacity (volumetric fraction) until wilting point at a depth of 15-30cm. Unit = percent}
#'  ...
#' }
#' @source https://www.isda-africa.com/isdasoil/?location=13.812%2C-21.671%2C43.677%2C-0.051 & https://github.com/ISRICWorldSoil/SoilGrids250m/blob/master/grids/models/TAXNWRB/TAXNWRB_legend.csv & https://github.com/ISRICWorldSoil/SoilGrids250m/blob/master/grids/models/TAXOUSDA/TAXOUSDA_legend.csv
"ERA_SoilGrids18"
