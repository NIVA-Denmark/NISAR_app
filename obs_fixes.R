if(F){

dfObs <- read.table("data/NISAR_obs.csv",stringsAsFactors=F,header=T,fileEncoding="UTF-8",sep=";")

dfObs <- dfObs %>%
  filter(!ShapeID %in% c(168,169))

dfObs <- dfObs %>%
  filter(AphiaID != 127188)

dfObs <- dfObs %>% 
  filter(Source != "KU Fish")

write.table(dfObs,file="data/NISAR_obs.csv",col.names=T,row.names=F,sep=";",na="",quote=T,fileEncoding="UTF-8")


  
df1 <- dfObs %>%
  filter(Source=="MONIS-5") %>%
  distinct(Lat,Lon) %>%
  mutate(MID=row_number())

write.table(df1,file="../NISAR/20200512/monis5stns.csv",
            col.names=T,row.names=F,sep=";",na="",quote=T,fileEncoding="UTF-8")


df2 <- read.table("../NISAR/20200512/monis5stnsRegions.csv",
                  stringsAsFactors=F,header=T,fileEncoding="UTF-8",sep=";") %>%
  select(MID,REGIONID)

df1 <- df1 %>%
  left_join(df2,by="MID") 

df1 <- df1 %>%
  select(-MID) %>%
  rename(REGIONIDfix=REGIONID)

dfObs <- read.table("data/NISAR_obs.csv",stringsAsFactors=F,header=T,fileEncoding="UTF-8",sep=";")

dfObs <- dfObs %>%
  left_join(df1,by=c("Lon","Lat"))

dfObs <- dfObs %>%
  mutate(REGIONID=ifelse(is.na(REGIONIDfix),REGIONID,REGIONIDfix))

dfObs <- dfObs %>% 
  select(-REGIONIDfix)

write.table(dfObs,file="data/NISAR_obs.csv",col.names=T,row.names=F,sep=";",na="",quote=T,fileEncoding="UTF-8")


}
