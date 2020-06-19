
data_path <-  "C:/Users/andre/Dropbox/WASHB-Bangladesh-Data/1-primary-outcome-datasets/Public/"

anthro <- read.csv(paste0(data_path, "washb-bangladesh-anthro-public.csv"))
tr <- read.csv(paste0(data_path, "washb-bangladesh-tr-public.csv"))
enrol <- read.csv(paste0(data_path, "washb-bangladesh-enrol-public.csv"))

df <- left_join(anthro, tr, by = c("block", "clusterid"))
df <- left_join(df, enrol, by = c("dataid"))

colnames(df)
df <- df %>% subset(., select = c("laz","tr","momage","hfias","sex","elec"))
saveRDS(df, here::here("regression/washb_data.RDS"))
