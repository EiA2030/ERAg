#' Calculate Penman-Monteith reference evapotranspiration
#'
#' This function calculates reference evapotranspiration using the ref{FAO Penman-Monteith method}{http://www.fao.org/docrep/X0490E/x0490e06.htm}.
#'
#' @param Tmin a numeric vector of daily minimum air temperatures at 2m height (celcius)
#' @param Tmax a numeric vector of daily maximum air temperatures at 2m height (celcius)
#' @param SRad a numeric vector of daily net radiation at the crop surface,  daily solar radiation (MJ m-2 day-1)
#' @param Wind a numeric vector of  mean daily wind speed at 2 m height (ms-1)
#' @param Pressure a numeric vector of mean daily atmospheric pressure (kPa)
#' @param Humid a numeric vector of daily mean relative humidity (%)
#' @param Rain a numeric vector of daily precipitation (mm). Can be set to NA if rainfall data is not supplied.
#' @param YearDay an integer vector julian day of the year
#' @param Latitude  a numeric of latitude in decimal degrees
#' @param Altitude a numeric of altitude (meters above sea level) (m)
#' @return A data.table with two fields:
#' * `ETo` = reference evapotranspiration (mm)
#' * `WBalance` = daily precipitation-ETo (mm) (if `Rain` argument supplied)
#' @export
PETcalc<-function(Tmin,Tmax,SRad,Wind,Pressure,Humid,Rain,YearDay,Latitude,Altitude){

  ### Step 1: Calculate mean temperature using min and max temperatures ####
  #The (average) daily maximum and minimum air temperatures in degrees Celsius (?C) are required. Where
  #only (average) mean daily temperatures are available, the calculations can still be executed but some underestimation
  #of ETo will probably occur due to the non-linearity of the saturation vapor pressure - temperature relationship (Allen et al. 1998).
  Tmean<-(Tmin+Tmax)/2 # Unit degrees celcius.

  ### Step 2: Set solar radiation values ####
  #The average daily net radiation expressed in megajoules per square meter per day (MJ m-2 day-1) is required. A simple
  #average of solar radiation values obtained from a weather station in the period of 24h (0:00:01 am to 11:59:59 pm) is required.
  # Units are MJ m-2 d-1. Typical values for daily global solar exposure range from 1 to 35 MJ/m2. http://www.bom.gov.au/climate/austmaps/solar-radiation-glossary.shtml
  Rs<-SRad

  ### Step 3: Set wind speeds ####
  #The average daily wind speed in meters per second (m s-1) measured at 2 m above the ground level is required. It is important to verify the height at which wind speed is
  #measured, as wind speeds measured at different heights above the soil surface differ. The wind speed measured at heights other than 2 m can be adjusted.
  WS<-Wind

  ### Step 4: Slope of saturation vapor pressure curve ####
  #For the calculation of evapotranspiration, the slope of the relationship between saturation vapor pressure and temperature, ??, is required

  delta<-(4098*(0.6108*exp((17.27*Tmean)/(Tmean+237.3))))/(Tmean+237.3)^2
  # Formula double checked.

  ### Step 5: Set Atmospheric Pressure ####
  #The atmospheric pressure, P, is the pressure exerted by the weight of the earth's atmosphere. Evaporation at high
  #altitudes is promoted due to low atmospheric pressure. This effect is, however, small and in the calculation procedures,
  #the average value for a location is sufficient. A simplification of the ideal gas law, assuming 20?C for a standard atmosphere,
  #can be employed to calculate P in kPa at a particular elevation
  Pressure<-Pressure

  ### Step 6: Psychrometric constant ####
  #The psychrometric constant relates the partial pressure of water in air to the air temperature so that vapor pressure
  #can be estimated using paired dry and wet thermometer bulb temperature readings. Another way to describe the
  #psychrometric constant is the ratio of specific heat of moist air at constant pressure (Cp) to latent heat of vaporization.
  #The specific heat at constant pressure is the amount of energy required to increase the temperature of a unit mass
  #of air by one degree at constant pressure. Its value depends on the composition of the air, i.e., on its humidity. For
  #average atmospheric conditions a Cp value of 1.013 10-3 MJ kg-1 ?C-1 can be used. As an average atmospheric pressure is
  #used for each location, the psychrometric constant is kept constant for each location depending of the altitude

  gamma<-0.000665*Pressure # Formula double checked

  ### Step 7: Delta Term (DT) (auxiliary calculation for Radiation Term) ####
  #In order to simplify the ETo calculation, several terms are calculated separated. The delta term is used to calculate the
  #Radiation Term of the overall ETo equation

  DT<-delta/(delta+gamma*(1+0.34*WS))
  # Formula double checked

  ### Step 8: Psi Term (PT) (auxiliary calculation for Wind Term) ####
  # The psi term is used to calculate the Wind Term of the overall ETo equation
  PT<-gamma/(delta+gamma*(1+0.34*WS)) # Formula double checked

  ### Step 9: Temperature Term (TT) (auxiliary calculation for Wind Term) ####
  # The temperature term is used to calculate the Wind Term of the overall ETo equation
  TT<-(900/(Tmean+273))*WS # Formula double checked

  ### Step 10: Mean saturation vapor pressure derived from air temperature(es) ####
  #As saturation vapor pressure is related to air temperature, it can be calculated from the air temperature.
  eT<-0.6108*exp((17.27*Tmean)/(Tmean+237.3))
  eTmax<-0.6108*exp((17.27*Tmax)/(Tmax+237.3))
  eTmin<-0.6108*exp((17.27*Tmin)/(Tmin+237.3))
  eS<-(eTmax+eTmin)/2
  # Formula double checked

  ### Step 11: Actual vapor pressure (ea) derived from relative humidity ####
  # In all cases assume that RH is RH mean, for AgMERRA RH is at the time of max temp, this means
  # that RH is closer to RHmin as RH increases at cooler temperatures. AgMERRA data is therefore preferred less to POWER.
  # We will use POWER data if available, and for sites with missing or questionable quality of humidity
  # data, the ea can be obtained by assuming when  the air temperature is close to Tmin, the air is
  # nearly saturated with water vapor and the relative humidity is near 100%, in other words,
  # dewpoint temperature (Tdew) is near the daily minimum temperature (Tmin).

  if(!all(is.na(Humid))){
    ea<-(Humid/100)*((eTmax+eTmin)/2)
  }else{
    ea<-eTmin
  }

  ### Step 12: The inverse relative distance Earth-Sun (dr) and solar declination (theta) ####
  # The inverse relative distance Earth-Sun, dr, and the solar declination, dr
  dr<-1+0.033*cos(((2*pi)/365)*YearDay)
  theta<-0.409*sin((((2*pi)/365)*YearDay)-1.39) # Formulae double checked

  ### Step 13: Conversion of latitude in degrees to radians (psi) ####
  #The latitude, ??, expressed in radians is positive for the northern hemisphere and negative for the southern hemisphere

  psi<-(pi/180)*Latitude # Formula double checked

  ### Step 14: Sunset hour angle (omega_s) ####
  omega_s<-acos(-tan(psi)*tan(theta))
  # Formula double checked

  ### Step 15: Extraterrestrial radiation (Ra) ####
  #The extraterrestrial radiation, Ra, for each day of the year and for different latitudes can be estimated from the solar constant, the solar declination and the time of the year.

  Ra<-((24*60)/pi)*0.082*dr*((omega_s*sin(psi)*sin(theta))+(cos(psi)*cos(theta)*sin(omega_s))) # Formula double checked

  ### Step 16: Clear sky solar radiation (Rso) ####
  Rso<-(0.75+(2*10^-5)*Altitude)*Ra
  # Formula double checked

  ### Step 17: Net solar or net shortwave radiation (Rns) ####
  Rns<-(1-0.23)*Rs #albedo or canopy reflection coefficient is 0.23 for a hypothetical grass reference crop. Formula double checked

  ### Step 18: Net outgoing long wave solar radiation (Rnl) ####
  #The rate of longwave energy emission is proportional to the absolute temperature of the surface raised to the
  #fourth power. This relation is expressed quantitatively by the Stefan-Boltzmann law. The net energy flux leaving the
  #earth's surface is, however, less than that emitted and given by the Stefan-Boltzmann law due to the absorption and
  #downward radiation from the sky. Water vapor, clouds, carbon dioxide, and dust are absorbers and emitters of
  #longwave radiation. It is thereby assumed that the concentrations of the other absorbers are constant

  Rnl<-(4.903*10^-9)*(((Tmax+273.16)^4+(Tmin+273.16)^4)/2)*(0.34-(0.14*ea^0.5))*((1.35*(Rs/Rso))-0.35) # Formula double checked

  ### Step 19: Net radiation (Rn) ####
  #The net radiation (Rn) is the difference between the incoming net shortwave radiation (Rns) and the outgoing net longwave radiation (Rnl)

  Rn <- Rns - Rnl
  #To express the net radiation (Rn) in equivalent of evaporation(mm) (Rng)
  Rng <- 0.408*Rn # Formulae double checked

  ### Final Step: Overall ETo equation ####
  #FS1. Radiation term (ETrad)
  ETrad <- DT * Rng
  #FS2. Wind term (ETwind)
  ETwind <- PT*TT*(eS-ea)
  #Final Reference Evapotranspiration Value (ETo)
  ETo <- round(ETwind + ETrad,2)

  if(!all(is.na(Rain))){
  #Precipitation-PET
  WBalance<-round(Rain-ETo,2)
  # To check that values are sensible, see Table 2 http://www.fao.org/docrep/x0490e/x0490e04.htm
    return(data.frame(ETo,WBalance))
  }else{
   return(data.frame(ETo))
  }
}
