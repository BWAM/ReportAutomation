#get the new metrics script
metrics.all<-read.csv("data/metric.with.all.fields.csv",stringsAsFactors = FALSE)

sites<-read.csv("data/sites_wallkill.csv",stringsAsFactors = FALSE )
sites.l<-unique(sites$SH_SITE_ID)

metrics.wk<-metrics.all %>% 
  filter(MSSIH_EVENT_SMAS_HISTORY_ID %in% sites.l)

sites<-sites %>% 
  select(order, group,SH_SITE_ID,SH_PWL_ID,SH_WQ_STANDARD)

metrics.wk$MSSIH_EVENT_SMAS_SAMPLE_DATE<-as.Date.character(metrics.wk$MSSIH_EVENT_SMAS_SAMPLE_DATE,
                                                           "%m/%d/%Y")
metrics.wk<-metrics.wk %>% 
  mutate(year=format(MSSIH_EVENT_SMAS_SAMPLE_DATE,"%Y")) %>% 
  rename("SH_SITE_ID"=MSSIH_EVENT_SMAS_HISTORY_ID)

metrics.df<-merge(metrics.wk,sites,by="SH_SITE_ID")
  
write.csv(metrics.df,"data/metrics.wallkill.csv",row.names = FALSE)

#get sturgeon pool data in here
#12/7/2020
#the insitu
sp_insitu<-read.csv("data/sturgeonpool.csv",stringsAsFactors = FALSE)#sturgeon pool
insitu<-read.csv("data/insitu_chem.csv",stringsAsFactors = FALSE)
params.is<-unique(insitu$PARAMETER_NAME)
#see if they match

is.nonmatch<-sp_insitu %>% 
  filter(!Characteristic.Name %in% params.is)#the only nonmatching ones are the ones that don't exist inthe SBU data set

#rename to match fields
sp_insitu<-sp_insitu %>% 
  rename(PARAMETER_NAME=Characteristic.Name,
         Date=SAMPLE_DATE,
         RESULT=Result.Value,
         unit=Result.Unit,
         SITE_ID=SITE_HISTORY_ID)

#create depth column in the insitu.df
insitu$depth<-"NA"
#combine the files
in.situ.all<-merge(sp_insitu,insitu,all = TRUE)

write.csv(in.situ.all,"data/in.situ_sp_included.csv",row.names = FALSE)

#chemistry
sp_chem<-read.csv("data/sp_chem.csv",stringsAsFactors = FALSE) #sturgeon pool
chem<-read.csv("data/chemistry.csv",stringsAsFactors = FALSE)
params.chem<-unique(chem$chemical_name)

sp_chem<-sp_chem %>%  #rename fields to match the chemistry file for wallkill
  rename(chemical_name=Characteristic.Name,
         fraction=Result.Sample.Fraction,
         sample_date=SAMPLE_DATE,
         result_value=Result.Value,
         result_unit=Result.Unit,
         validator_qualifiers=INTERPRETED_QUALIFIERS,
         SITE_ID=SITE_HISTORY_ID,
         group=INFO_TYPE)

nonm<-sp_chem %>% 
  filter(!chemical_name %in% params.chem)

#merge the files

chem_all<-merge(chem,sp_chem,all=TRUE)

write.csv(chem_all,"data/chemistry_sp_incl.csv",row.names = FALSE)

