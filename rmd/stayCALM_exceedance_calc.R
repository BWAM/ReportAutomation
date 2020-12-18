#Exceedance calculation with StayCALM
#12_18_2020
#Reynolds,Conine and Smith

# Load a collection of tidyverse packages into the environment.
library(tidyverse)
# Load the stayCALM package into the environment.
library(stayCALM)

# Extract the package root with base R functions.
# This directory will provide relative paths between machines.
root.dir <- gsub("(stayCALM)(.*$)", "\\1", getwd())

wqs.df <- stayCALM::nysdec_wqs
data("wipwl.df")

wqs_wipwl.df <- wipwl.df %>%
  # Subset columns
  select(seg_id, class, spatial_extent, water_type) %>% 
  # Ensure all rows are unique representations of the data.
  distinct() %>%
  # Join the DF with the WQS by the specified columns.
  left_join(wqs.df,
            by = c("class",
                   "spatial_extent",
                   "water_type"))

# These data sets have been added to the stayCALM package as .Rda files.
# This data will eventually be housed in an authoritative database.
data(smas.df) 
data(lmas.df)

# Append the SMAS and LMAS data by row.
org.df <- rbind(smas.df, lmas.df)

# # Keep only the dates within the 10-year assessment period.
# # StayCALM will flag these dates as outside of the assessment period, but
# # this can inflate the number of un-assessed waters reported. It is easier,
# # to filter out the older data here if it is not of interest.
# day_zero <- stayCALM::date_subtraction(.date = Sys.Date(),
#                                        .subtract = "10 years")
# org.df <- org.df[org.df$date >= day_zero, ]

#limit to just the data set we want for wallkill
cols.keep<-names(org.df)

#need to have the insitu data in there too

raw.wallkill<-read.csv(file.path(here::here(),
                         "data",
                         "chemistry_sp_incl.csv"),
               stringsAsFactors = FALSE)


raw.wallkill<-raw.wallkill %>% 
  rename(value=result_value,
         units=result_unit,
         site_id=SITE_ID,
         sample_id=sample_delivery_group,
         info_type=INFO_TYPE,
         date=sample_date,
         seg_id=SH_PWL_ID,
         parameter=chemical_name,
         data_provider=Project_name)

x1<-raw.wallkill[,names(raw.wallkill) %in% cols.keep] 
x1$depth<-"NA"


insitu<-read.csv(file.path(here::here(),
                                 "data",
                                 "in.situ_sp_included.csv"),
                       stringsAsFactors = FALSE)

insitu$PARAMETER_NAME<-tolower(insitu$PARAMETER_NAME)
insitu$PARAMETER_NAME<-gsub(" ","_",insitu$PARAMETER_NAME)

insitu<-insitu %>% #nee and data provider and quant limit, need pwl id, info_type
  rename(site_id=SITE_ID,
         parameter=PARAMETER_NAME,
         date=Date,
         value=RESULT,
         units=unit,
         validator_qualifiers=VALIDATOR_QUAL,
         interpreted_qualifiers=VALIDATOR_QUAL_REASON,
         sample_id=SAMPLE_EVENT_INFO_HISTORY_ID
  ) %>% 
  mutate(fraction=case_when(
    parameter %in% "ph"~"total",
    parameter %in% "dissolved_oxygen"~"dissolved",
    TRUE~"total")) %>% 
  mutate(data_provider="NA",quantitation_limit="NA",info_type="NA")

x2<-insitu[,names(insitu) %in% cols.keep] 
#x2 needs PWL ID

pwl<-x1 %>% 
  select(seg_id,site_id) %>% 
  distinct()
x2<-merge(x2,pwl,by="site_id")

x3<-rbind(x1,x2)

#fix the chemicals to match wqs file
x3$parameter<-tolower(x3$parameter)

x3<-x3 %>% 
  mutate(parameter=case_when(
    parameter %in% "nitrogen, ammonia (as n)"~ "ammonia",
    parameter %in% "chloride (as cl)"~"chloride",
    parameter %in% "total dissolved solids (residue, filterable)" ~ "total_dissolved_solids",
    parameter %in% "nitrogen, nitrate (as n)"  ~ "nitrate",
    parameter %in% "nitrogen, nitrite" ~"nitrite",
    parameter %in%  "nitrate+nitrite as nitrogen" ~"nitrate_nitrite",
    parameter %in% "sulfate (as so4)"  ~ "sulfate",
    parameter %in% "hardness (as caco3)"     ~ "hardness",
    TRUE ~ paste(parameter)
    
  )) %>% 
  mutate(units=case_when(
    parameter %in% "ph"~"ph_units",
    TRUE~paste(units)
  ))

org.df<-x3
org.df$units<-tolower(org.df$units)
org.df$date<-as.Date(org.df$date,"%m/%d/%Y")

chem_extract.df <- org.df %>% 
  filter(parameter %in% c("hardness", "ph", "temperature")) %>% 
  tidyr::pivot_wider(
    id_cols = sample_id,
    names_from = "parameter",
    values_from = c("value", "units"),
    names_sep = "_",
    values_fn = list(value = mean,
                     units = unique,
                     na.rm = TRUE)
  ) %>% 
  right_join(org.df, by = "sample_id")

chem.df <- merge(x = chem_extract.df, 
                 y = wqs_wipwl.df,
                 by = c("seg_id", "parameter",
                        "fraction", "units"))

chem.df <- thresh_determination(chem.df)

chem.df$date<-as.Date(chem.df$date,"%m/%d/%Y")
chem.df$year<-format(chem.df$date,"%Y")
chem.df$month<-format(chem.df$date,"%m")

chem.df$assessment_id <- group_id(.data = chem.df,
                                  .keep = c("seg_id",
                                            "parameter",
                                            "fraction"),
                                  .numeric = TRUE)

chem.df$within_period <- assessment_period(.date_vec = chem.df$date,
                                           .n_years_ago = 10)

prepped.df <- prep_values(.data = chem.df,
                          .block_col = "block",
                          .value_col = "value",
                          .statistic_col = "statistic",
                          .new_value_col = "result",
                          .min_n_col = "min_n")

selected.df <-
  subset(
    prepped.df,
    select = c(
      "assessment_id",
      "seg_id",
      "site_id",
      "water_type",
      "type",
      "use",
      "standard_type",
      "group",
      "block",
      "statistic",
      "parameter",
      "fraction",
      "units",
      "result",
      "date",
      "year",
      "within_period",
      "direction",
      "threshold",
      "summarize_rows",
      "summarize_rows_operator",
      "wqs_75p_threshold",
      "data_provider"
    )
  )

selected.df$attaining_wqs <- attaining(selected.df$result,
                                       selected.df$direction,
                                       selected.df$threshold)

selected.df$attaining_75 <- attaining(selected.df$result,
                                      selected.df$direction,
                                      selected.df$wqs_75p_threshold)

temp4<-selected.df%>%
  group_by(site_id, parameter, fraction, date)%>%
  summarize(n=n(),
            combined=sum(attaining_wqs),
            WQS_attain_combined= ifelse(combined==n, yes=TRUE, no=FALSE),
            num_exceedances=(n-combined))

#From here you should be able to left join pwls or raw data back on or filter to do summaries to create tables etc. 

#clean up
rm(list=ls()[! ls() %in% c("temp4","selected.df","wqs_wipwl.df")])

