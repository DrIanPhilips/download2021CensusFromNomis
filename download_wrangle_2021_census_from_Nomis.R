
#Script for downloading UK 2021 LSOA census data from Nomis using R

#(An LSOA is an area with about 1500 ish people in it)
#more info here:
#https://www.ons.gov.uk/methodology/geography/ukgeographies/censusgeographies/census2021geographies#:~:text=Lower%20layer%20Super%20Output%20Areas%20(LSOAs),-Lower%20layer%20Super&text=They%20comprise%20between%20400%20and,and%20household%20changes%20since%202011.


#the script downloads LSOA census data tables
# then unzips it
# then does some wrangling - I've wrangled it for a piece of work I have been doing, but
#the code could be modified to suit
# The dataframe called LSOA is then made.  Each time I read in a new table of data and wrangle it I join it to LSOA.
#This eventually becomes the Master file that I save at the end


##### load packages   ###


require(tidyverse) #for data wrangling and more
require(foreign) #for reading in excel files and stuff
require(readxl) #for reading in excel files and stuff
library(lubridate) #for dates
require(sf) #for wrangling spatial data
require(tmap) # for making maps



########## data download ####################

#At the time of me writing this script
#there are 78 neatly numbered tables
#"https://www.nomisweb.co.uk/sources/census_2021_bulk" shown on this page
#there are 3 others at the end

#each zipfile has a link like this
#https://www.nomisweb.co.uk/output/census/2021/census2021-ts001.zip
#becase Nomis have helpfully names the files we can make a loop that
#cycles through and downloads all the tables


#make a vector 001 to 0078
zipnums = sprintf('%0.3d', 1:78)


for(k in 1:length(zipnums)){

  zipnum = zipnums[k]

  #nomis have made this easy to run through a loop to download all the zips
  urlstring = paste0("https://www.nomisweb.co.uk/output/census/2021/census2021-ts",zipnum,".zip")
  deststring = paste0("data/census21/ts",zipnum,"zip")

  download.file(url = urlstring,
                destfile = deststring )


  unzip(zipfile = deststring,
        exdir = paste0("data/census21/ts",zipnum)
  )

}


# me checking things out before I made the loop above
# download.file("https://www.nomisweb.co.uk/output/census/2021/census2021-ts001.zip",destfile =  "data/census21/test.zip")
# unzip(zipfile = "data/census21/test.zip",
#       exdir = "data/census21/ts001test"
#       )





#### WRANGLE DATA THAT IT IS OF USE #########################

#this is less automated.
#I needed to look at the table headings to see what percentages I wanted
#TODO:  table format is same so could reference columns by index. but then a bit of faff with
#col names
# for those who are keen
#var <- rlang::sym(""the variable name you want as a string")
#then when you're wrangling in dplyr do stuff like this with 2 exclamation marks
#x %>% group_by(a, !!var)%>%  bla bla bla
#or use this funny fellow :=
# for example:
# df %>% rename(!!var:= `some variable name that's already in your dataframe`)


## usual resident population
resolution = "lsoa"
foldernumber = "001"
name = "URP" # not using this variable at the moment
df1 <- read_csv(paste0("data/census21/ts",foldernumber,"/census2021-ts",foldernumber,"-",resolution,".csv"))
glimpse(df1)

require(DescTools)


#you can subset in various ways if you want
#df1 <- df1 %>% filter(geography %like% "%Hartlepool%")
#df1 <- df1 %>% filter(geography %like% "Otley")


#select the columns you want
lsoa <- df1 %>% select(geography,lsoa21cd = `geography code`,household_pop = `Residence type: Lives in a household; measures: Value`,
                       communal_pop = `Residence type: Lives in a communal establishment; measures: Value`)
df1 <- df1 %>% select(-`geography code`,-geography , -date)


# The dataframe called LSOA is made above.  Each time I read in a new table of data and wrangle it I join it to LSOA.
#This eventually becomes the Master file that I save


## population  density
resolution = "lsoa"
foldernumber = "006"
name = "URP"
df1 <- read_csv(paste0("data/census21/ts",foldernumber,"/census2021-ts",foldernumber,"-",resolution,".csv"))
glimpse(df1)

df1 <- df1 %>% select(lsoa21cd = `geography code`,
                      pop_dens_perkm2 = `Population Density: Persons per square kilometre; measures: Value`)
#df1 <- df1 %>% select(-`geography code`,-geography , -date)


lsoa = left_join(lsoa,df1, by = c("lsoa21cd" = "lsoa21cd"))


## age bands
resolution = "lsoa"
foldernumber = "007a"
name = "URP"
df1 = read_csv("data/census21/census2021-ts007a/census2021-ts007a-lsoa.csv")
#df1 <- read_csv(paste0("data/census21/ts",foldernumber,"/census2021-ts",foldernumber,"-",resolution,".csv"))
glimpse(df1)


#NB here I'm wrangling into gropuings that are useful for me.  you may want to wrangle differently
df1 <- df1 %>% mutate(lsoa21cd = `geography code`)
df1 <- df1 %>% mutate(agetotal = `Age: Total`)
df1 <- df1 %>% mutate(children9U = `Age: Aged 4 years and under` + `Age: Aged 5 to 9 years` )
df1 <- df1 %>% mutate(pc9U = 100*(children9U/agetotal))
df1 <- df1 %>% mutate(pc4U = 100*(`Age: Aged 4 years and under`/agetotal))
df1 <- df1 %>% mutate(pc10_14 = 100*( `Age: Aged 10 to 14 years`/agetotal))
df1 <- df1 %>% mutate(pc1519 = 100*( `Age: Aged 15 to 19 years`/agetotal))
df1 <- df1 %>% mutate(pc19U = 100*((`Age: Aged 4 years and under` + `Age: Aged 5 to 9 years`+`Age: Aged 10 to 14 years`+`Age: Aged 15 to 19 years`)/agetotal))
df1 <- df1 %>% mutate(pc19_29 = 100*( (`Age: Aged 20 to 24 years` + `Age: Aged 25 to 29 years`)/agetotal))
df1 <- df1 %>% mutate(pc30_49 = 100*( (`Age: Aged 30 to 34 years`+ `Age: Aged 35 to 39 years`+ `Age: Aged 40 to 44 years`+ `Age: Aged 45 to 49 years`)/agetotal))
df1 <- df1 %>% mutate(pc50_65 = 100*( (`Age: Aged 50 to 54 years`+ `Age: Aged 55 to 59 years`+ `Age: Aged 60 to 64 years`)/agetotal))
df1 <- df1 %>% mutate(pc65plus = 100*( (`Age: Aged 65 to 69 years`+ `Age: Aged 70 to 74 years`+`Age: Aged 75 to 79 years`+`Age: Aged 80 to 84 years` +`Age: Aged 85 years and over`)/agetotal))
df1 <- df1 %>% select(-`geography code`,-geography , -date)

lsoa = left_join(lsoa,df1, by = c("lsoa21cd" = "lsoa21cd"))


## deprivation

resolution = "lsoa"
foldernumber = "011"
name = "URP"
#df1 = read_csv("data/census21/census2021-ts007a/census2021-ts007a-lsoa.csv")
df1 <- read_csv(paste0("data/census21/ts",foldernumber,"/census2021-ts",foldernumber,"-",resolution,".csv"))
glimpse(df1)

df1 <- df1 %>% mutate(lsoa21cd = `geography code`)
df1 <- df1 %>% mutate(pc_HH_no_deprivation_dim = 100*(`Household deprivation: Household is not deprived in any dimension; measures: Value`/`Household deprivation: Total: All households; measures: Value` ))
df1 <- df1 %>% select(-`geography code`,-geography , -date)

lsoa = left_join(lsoa,df1, by = c("lsoa21cd" = "lsoa21cd"))


## distance to work
resolution = "lsoa"
foldernumber = "058"
name = "URP"
#df1 = read_csv("data/census21/census2021-ts007a/census2021-ts007a-lsoa.csv")
df1 <- read_csv(paste0("data/census21/ts",foldernumber,"/census2021-ts",foldernumber,"-",resolution,".csv"))
glimpse(df1)

df1 <- df1 %>% mutate(lsoa21cd = `geography code`)
df1 <- df1 %>% mutate(pc_WFH = 100*(`Distance travelled to work: Works mainly from home` /`Distance travelled to work: Total: All usual residents aged 16 years and over in employment the week before the census`))
df1 <- df1 %>% mutate(pc_commuteU5km = 100*((`Distance travelled to work: Less than 2km` + `Distance travelled to work: 2km to less than 5km`) /`Distance travelled to work: Total: All usual residents aged 16 years and over in employment the week before the census`))
df1 <- df1 %>% select(-`geography code`,-geography , -date)


lsoa = left_join(lsoa,df1, by = c("lsoa21cd" = "lsoa21cd"))




## mode to work
resolution = "lsoa"
foldernumber = "061"
name = "URP"
#df1 = read_csv("data/census21/census2021-ts007a/census2021-ts007a-lsoa.csv")
df1 <- read_csv(paste0("data/census21/ts",foldernumber,"/census2021-ts",foldernumber,"-",resolution,".csv"))
glimpse(df1)

df1 <- df1 %>% mutate(lsoa21cd = `geography code`)
df1 <- df1 %>% mutate(pc_car = 100*((`Method of travel to workplace: Driving a car or van` + `Method of travel to workplace: Passenger in a car or van`) /`Method of travel to workplace: Total: All usual residents aged 16 years and over in employment the week before the census`))
df1 <- df1 %>% mutate(pc_bike = 100*(`Method of travel to workplace: Bicycle` /`Method of travel to workplace: Total: All usual residents aged 16 years and over in employment the week before the census`))
df1 <- df1 %>% select(-`geography code`,-geography , -date)


lsoa = left_join(lsoa,df1, by = c("lsoa21cd" = "lsoa21cd"))



## nssec

resolution = "lsoa"
foldernumber = "062"
name = "URP"
#df1 = read_csv("data/census21/census2021-ts007a/census2021-ts007a-lsoa.csv")
df1 <- read_csv(paste0("data/census21/ts",foldernumber,"/census2021-ts",foldernumber,"-",resolution,".csv"))
glimpse(df1)

df1 <- df1 %>% mutate(lsoa21cd = `geography code`)
df1 <- df1 %>% mutate(pc_nsec1_6 = 100*((`National Statistics Socio-economic Classification (NS-SEC): L1, L2 and L3 Higher managerial, administrative and professional occupations`+`National Statistics Socio-economic Classification (NS-SEC): L4, L5 and L6 Lower managerial, administrative and professional occupations`) /`National Statistics Socio-economic Classification (NS-SEC): Total: All usual residents aged 16 years and over`))
df1 <- df1 %>% mutate(pc_nsec_small_acc_self_emp = 100*(`National Statistics Socio-economic Classification (NS-SEC): L8 and L9 Small employers and own account workers`  /`National Statistics Socio-economic Classification (NS-SEC): Total: All usual residents aged 16 years and over`))


df1 <- df1 %>% select(-`geography code`,-geography , -date)


lsoa = left_join(lsoa,df1, by = c("lsoa21cd" = "lsoa21cd"))


## accom type

resolution = "lsoa"
foldernumber = "044"
name = "URP"
#df1 = read_csv("data/census21/census2021-ts007a/census2021-ts007a-lsoa.csv")
df1 <- read_csv(paste0("data/census21/ts",foldernumber,"/census2021-ts",foldernumber,"-",resolution,".csv"))
glimpse(df1)

df1 <- df1 %>% mutate(lsoa21cd = `geography code`)
df1 <- df1 %>% mutate(pc_detached = 100*(`Accommodation type: Detached`/`Accommodation type: Total: All households`))
df1 <- df1 %>% mutate(pc_semidetached = 100*(`Accommodation type: Semi-detached`/`Accommodation type: Total: All households`))
df1 <- df1 %>% mutate(pc_terraced = 100*(`Accommodation type: Terraced` /`Accommodation type: Total: All households`))
df1 <- df1 %>% mutate(pc_flats = 100*(`Accommodation type: In a purpose-built block of flats or tenement` /`Accommodation type: Total: All households`))
df1 <- df1 %>% mutate(pc_shared_house = 100*(`Accommodation type: Part of a converted or shared house, including bedsits` /`Accommodation type: Total: All households`))



df1 <- df1 %>% select(-`geography code`,-geography , -date)


lsoa = left_join(lsoa,df1, by = c("lsoa21cd" = "lsoa21cd"))


## car van availability


resolution = "lsoa"
foldernumber = "045"
name = "URP"
#df1 = read_csv("data/census21/census2021-ts007a/census2021-ts007a-lsoa.csv")
df1 <- read_csv(paste0("data/census21/ts",foldernumber,"/census2021-ts",foldernumber,"-",resolution,".csv"))
glimpse(df1)

df1 <- df1 %>% mutate(lsoa21cd = `geography code`)

df1 <- df1 %>% mutate(pc_no_carHH = 100*(`Number of cars or vans: No cars or vans in household`/`Number of cars or vans: Total: All households`))
df1 <- df1 %>% mutate(pc_1_carHH = 100*(`Number of cars or vans: 1 car or van in household`/`Number of cars or vans: Total: All households`))
df1 <- df1 %>% mutate(pc_2_carHH = 100*(`Number of cars or vans: 2 cars or vans in household`/`Number of cars or vans: Total: All households`))
df1 <- df1 %>% mutate(pc_3ormore_carHH = 100*(`Number of cars or vans: 3 or more cars or vans in household`/`Number of cars or vans: Total: All households`))
df1 <- df1 %>% mutate(pc_2ormore_carHH = 100*((`Number of cars or vans: 2 cars or vans in household` +`Number of cars or vans: 3 or more cars or vans in household`)/`Number of cars or vans: Total: All households`))



df1 <- df1 %>% select(-`geography code`,-geography , -date)


lsoa = left_join(lsoa,df1, by = c("lsoa21cd" = "lsoa21cd"))



#lsoa$pc_no_deprivation <- 100*(`Household deprivation: Household is not deprived in any dimension; measures: Value`/`Household deprivation: Total: All households; measures: Value`)

lsoa$pc_no_deprivation <- 100*(lsoa[42]/lsoa[41])
summary(lsoa$pc_no_deprivation)

lsoa$pc_no_deprivation <- 0
lsoa <- lsoa %>% mutate(pc_no_deprivation =  if_else(`Household deprivation: Total: All households; measures: Value` > 0,
                                                   `Household deprivation: Household is not deprived in any dimension; measures: Value`/`Household deprivation: Total: All households; measures: Value`,
                                                   0)
                        )

summary(lsoa$pc_no_deprivation)
hist(lsoa$pc_no_deprivation)


## tenure


resolution = "lsoa"
foldernumber = "054"
name = "URP"
#df1 = read_csv("data/census21/census2021-ts007a/census2021-ts007a-lsoa.csv")
df1 <- read_csv(paste0("data/census21/ts",foldernumber,"/census2021-ts",foldernumber,"-",resolution,".csv"))
glimpse(df1)

df1 <- df1 %>% mutate(lsoa21cd = `geography code`)

df1 <- df1 %>% mutate(pc_tenure_own = 100*(`Tenure of household: Owned`/`Tenure of household: Total: All households`))
df1 <- df1 %>% mutate(pc_tenure_social_rent = 100*(`Tenure of household: Social rented`/`Tenure of household: Total: All households`))
df1 <- df1 %>% mutate(pc_tenure_private_rent = 100*(`Tenure of household: Private rented`/`Tenure of household: Total: All households`))


df1 <- df1 %>% select(-`geography code`,-geography , -date)
lsoa = left_join(lsoa,df1, by = c("lsoa21cd" = "lsoa21cd"))



## Education
resolution = "lsoa"
foldernumber = "067"
name = "Education"
#df1 = read_csv("data/census21/census2021-ts007a/census2021-ts007a-lsoa.csv")
df1 <- read_csv(paste0("data/census21/ts",foldernumber,"/census2021-ts",foldernumber,"-",resolution,".csv"))
glimpse(df1)
df1 <- df1 %>% mutate(lsoa21cd = `geography code`)
df1 <- df1 %>% mutate(pc_no_qualifications = 100*(`Highest level of qualification: No qualifications`/`Highest level of qualification: Total: All usual residents aged 16 years and over`))
df1 <- df1 %>% mutate(pc_L2_quals = 100*(`Highest level of qualification: Level 2 qualifications`/`Highest level of qualification: Total: All usual residents aged 16 years and over`))
df1 <- df1 %>% mutate(pc_L3_quals = 100*(`Highest level of qualification: Level 3 qualifications`/`Highest level of qualification: Total: All usual residents aged 16 years and over`))
df1 <- df1 %>% mutate(pc_L4_quals_and_above = 100*(`Highest level of qualification: Level 4 qualifications and above`/`Highest level of qualification: Total: All usual residents aged 16 years and over`))
df1 <- df1 %>% select(-`geography code`,-geography , -date)
lsoa = left_join(lsoa,df1, by = c("lsoa21cd" = "lsoa21cd"))



## Occupation
resolution = "lsoa"
foldernumber = "063"
name = "Occupation"
#df1 = read_csv("data/census21/census2021-ts007a/census2021-ts007a-lsoa.csv")
df1 <- read_csv(paste0("data/census21/ts",foldernumber,"/census2021-ts",foldernumber,"-",resolution,".csv"))
glimpse(df1)
df1 <- df1 %>% mutate(lsoa21cd = `geography code`)

df1 <- df1 %>% mutate(pc_managers_directors_seniorofficials = 100*(`Occupation (current): 1. Managers, directors and senior officials`/`Occupation (current): Total: All usual residents aged 16 years and over in employment the week before the census`))
df1 <- df1 %>% mutate(pc_professional = 100*(`Occupation (current): 2. Professional occupations`/`Occupation (current): Total: All usual residents aged 16 years and over in employment the week before the census`))
df1 <- df1 %>% mutate(pc_associate_professional_technical = 100*(`Occupation (current): 3. Associate professional and technical occupations`/`Occupation (current): Total: All usual residents aged 16 years and over in employment the week before the census`))
df1 <- df1 %>% mutate(pc_admin_secretarial = 100*(`Occupation (current): 4. Administrative and secretarial occupations`/`Occupation (current): Total: All usual residents aged 16 years and over in employment the week before the census`))
df1 <- df1 %>% mutate(pc_skilled_trades = 100*(`Occupation (current): 5. Skilled trades occupations`/`Occupation (current): Total: All usual residents aged 16 years and over in employment the week before the census`))
df1 <- df1 %>% mutate(pc_caring_leisure_service = 100*(`Occupation (current): 6. Caring, leisure and other service occupations`/`Occupation (current): Total: All usual residents aged 16 years and over in employment the week before the census`))
df1 <- df1 %>% mutate(pc_sales_cust_service = 100*(`Occupation (current): 7. Sales and customer service occupations`/`Occupation (current): Total: All usual residents aged 16 years and over in employment the week before the census`))
df1 <- df1 %>% mutate(pc_process_plant_machine = 100*(`Occupation (current): 8. Process, plant and machine operatives`/`Occupation (current): Total: All usual residents aged 16 years and over in employment the week before the census`))
df1 <- df1 %>% mutate(pc_elementary = 100*(`Occupation (current): 9. Elementary occupations`/`Occupation (current): Total: All usual residents aged 16 years and over in employment the week before the census`))

df1 <- df1 %>% mutate(pc_higher_occupations1_2 = pc_managers_directors_seniorofficials + pc_professional)
df1 <- df1 %>% mutate(pc_owtherwhitecollar_occupations3_4 = pc_associate_professional_technical + pc_admin_secretarial)
df1 <- df1 %>% mutate(pc_manual_occupations6_7_8_9 = pc_caring_leisure_service + pc_sales_cust_service +pc_process_plant_machine+ pc_elementary)


df1 <- df1 %>% select(-`geography code`,-geography , -date)
lsoa = left_join(lsoa,df1, by = c("lsoa21cd" = "lsoa21cd"))






##### ---------------- make a master csv file -----------------
names(lsoa)

lsoa <- as.data.frame(lsoa)
write_csv(lsoa,"data/census21/LSOA_master/lsoa21_master.csv")


## ----- make a master spatial data file ------




require(sf)
require(tmap)

## read spatial LSOA boundary files
lsoa_sf  <- st_read("data/census21/boundaries/LSOA_21BGC/LSOA_(Dec_2021)_Boundaries_Generalised_Clipped_EW_(BGC).shp")
#check the co-ordinate reference system. If you want to change it us function  st_transform()
st_crs(lsoa_sf)

lsoa_sf <- left_join(lsoa_sf,lsoa, by = c("LSOA21CD" ="lsoa21cd"))

#save the file in geopackage format.
st_write(lsoa_sf,"data/census21/LSOA_master/lsoa21_master.gpkg")







