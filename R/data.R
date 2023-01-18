#' The compiled ERA dataset (version = Comb 2023.01.06 2023.01.08)
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
#' EU Codes (18.01.2023)
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
