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
  
write.csv(metrics.wk,"data/metrics.wallkill.csv",row.names = FALSE)
