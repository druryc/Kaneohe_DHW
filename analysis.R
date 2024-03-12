library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(scales)
library(broom)
library(noaaoceans)
library(cowplot)
library(slider)
library(ggrepel)
options(scipen=999)

####### NDBC MOKH1 DATA ##################################################################### ####
#using noaaoceans loop to pull data from 1994-2023
#https://tidesandcurrents.noaa.gov/inventory.html?id=1612480
mokh1_raw<-data.frame()
for (i in seq(1994,2023,1)){
  df<-query_coops_data(station_id = 1612480,
                               start_date = paste0(i,"0101"),
                               end_date = paste0(i,"1231"),
                               data_product = 'water_temperature',
                               units="metric",
                               time_zone="lst",
                               interval = 'h')
  mokh1_raw<-mokh1_raw%>%bind_rows(.,df)
  }

mokh1<-mokh1_raw%>%
  mutate(date_time=ymd_hm(t))%>%
  rename(temp=v)%>%
  select(date_time,temp)%>%
  mutate(date=date(date_time))%>%
  group_by(date)%>%
  summarise(temp=mean(as.numeric(temp)))%>% #daily averages
  mutate(year=year(date))%>%
  filter(year<1998|year>2001)%>% #reported sensor issue
  mutate(site="MOKH1")%>%
  select(date,site,temp)

rm(df);rm(mokh1_raw)
####### NMFS KOKOHEAD DATA ################################################################## #####
#data from Jokiel & Brown 2004 - Table 2 - doi: 10.1111/j.1365-2486.2004.00836.x
jokiel<-as.data.frame(c(22.8,22.8,23.3,24.0,25.2,26.4,26.4,26.8,26.7,26.3,24.9,23.2))%>%rename(jokiel=1)

#from https://www.fisheries.noaa.gov/inport/item/5828
nmfs_raw<-read_csv("./data/re004aa1_v/re004aa1_v.csv")%>%
  clean_names()%>%
  select(year,month,day,sea_temperature_c)%>%
  unite(date,year,month,day,sep="-")%>%
  mutate(date=ymd(date))%>%
  rename(temp=sea_temperature_c)%>%
  drop_na()

correction<-nmfs_raw%>%filter(date<"1969-01-01")%>%
  mutate(month=month(date),
         year=year(date))%>%
  group_by(month,year)%>%
  summarise(temp=mean(temp))%>%
  group_by(month)%>%
  summarise(temp=mean(temp))%>%
  bind_cols(jokiel)%>%
  mutate(correction=jokiel-temp)%>%
  select(month,correction)

nmfs<-nmfs_raw%>%mutate(month=month(date))%>%
  left_join(.,correction,by="month")%>%
  mutate(temp_corr=temp+correction)%>%mutate(site="NMFS")%>%
  select(date,temp,temp_corr,site)%>%
  rename(temp_raw=temp,temp=temp_corr)%>%
  select(date,site,temp)

rm(jokiel);rm(correction);rm(nmfs_raw)
####### CORAL REEF ECOLOGY LAB DATA ######################################################### ####
#old CREL data (before 2005) shared by Kuulei 
#using pacIOOS retrieval to get recent (since 2005) HIMB weather station (Coral Reef Ecology Lab Data)
#https://pae-paha.pacioos.hawaii.edu/erddap/tabledap/aws_himb.csv?time%2Csea_water_temperature&time%3E=%20%20%202005-01-01T10%3A00%3A00Z%09

files<-list.files(path = "./data/CREL/", pattern = "*.xls", full.names = TRUE, recursive = TRUE)
crel_raw<-list()

for (i in files){
  temp<-read_xls(i,sheet="Daily Weather",skip=7)%>%
    clean_names()%>%
    select(date,c_6)%>%rename(temp=2)
  crel_raw[[i]]<-temp
}

old_crel<-bind_rows(crel_raw)%>%
  mutate(date=ymd(date))%>%filter(temp>15,temp<35)%>%mutate(site="CREL")%>%
  mutate(year=year(date))%>% #supplied by daily average or daily reading
  filter(year!=2002,year!=1994)

recent_crel<-read_csv("data/CREL/aws_himb_b16f_e8da_cdfd.csv")%>%
  filter(time!="UTC")%>%
  mutate(date_time=ymd_hms(time))%>%
  mutate(date_time=with_tz(date_time,tzone="US/Hawaii"))%>%
  select(-time)%>%
  rename(temp=1)%>%
  mutate(date=date(date_time))%>%
  group_by(date)%>%
  summarise(temp=mean(as.numeric(temp)))%>% #daily averages
  filter(temp>15,temp<35)%>%
  mutate(site="CREL")%>%
  mutate(year=year(date))%>%
  ungroup()%>%
  filter(year!=2005,year<2024)

table(old_crel$year)

crel<-bind_rows(old_crel,recent_crel)%>%
  select(date,site,temp)
rm(crel_raw);rm(old_crel);rm(recent_crel);rm(temp)
####### CRIMP DATA ########################################################################## ####
#CRIMP1 #wget -r -np -nH --cut-dirs=5 "*.csv" https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0100069/
#CRIMP2 #wget -r -np -nH --cut-dirs=5 "*.csv" https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0157415/
# system("rm ./data/*/*.pdf")
# system("rm ./data/*/*.xml")
# system("rm ./data/*/index*")
# system("rm ./data/*/*Log.csv")

files<-c(list.files(path = "data/0157415", pattern = "*.csv", full.names = TRUE, recursive = TRUE),
            list.files(path = "data/0100069", pattern = "*.csv", full.names = TRUE, recursive = TRUE))
crimp_raw<-list()

for (i in files){
  skip<-min(grep("Mooring",read_lines(i,n_max=10)))
  temp<-read_csv(i,skip = skip)%>%
    select(4,5,13)%>%rename(date=1,time=2,temp=3)%>%
    drop_na()%>%
    unite(date_time,date,time,sep=" ")%>%
    mutate(date_time=mdy_hms(date_time))
  crimp_raw[[i]]<-temp
}

crimp<-bind_rows(crimp_raw)%>%
  mutate(date=date(date_time))%>%
  group_by(date)%>%
  summarise(temp=mean(as.numeric(temp)))%>%
  filter(temp>15,temp<35)%>%
  mutate(site="CRIMP")%>%
  select(date,site,temp)

rm(crimp_raw);rm(temp)  
####### OVERALL DATA ######################################################################## ####
master_temp<-bind_rows(mokh1,crimp,nmfs)%>%
  drop_na()%>%distinct()%>%
  group_by(date)%>%
  mutate(kbay_mean=mean(temp))%>% #calculate daily average temp across instruments
  mutate(year=year(date))%>% 
  mutate(month=month(date))%>%
  mutate(n_day=yday(date)-1)%>%
  mutate(dummy_date=as.Date(n_day,origin="1994-01-01"))%>%
  select(date,dummy_date,year,month,n_day,site,temp,kbay_mean)

table(master_temp$year,master_temp$site)
saveRDS(master_temp,"~/Desktop/master_temp")

####### MMM CALCULATIONS #################################################################### ####
monthly_summary<-master_temp%>%
  ungroup()%>%
  filter(site=="MOKH1",year<2004,year!=1996,year>1992)%>% #filter to non-bleaching years, 1993-2003 inclusive to eliminate partial data
  group_by(year,month)%>%
  summarise(monthly_mean=mean(kbay_mean)) #calculate mean of every month from 1993-2003

table(monthly_summary$year)

mmm_12<-monthly_summary%>%ungroup()%>%select(year,month,monthly_mean)%>%
  distinct()%>%
  group_by(month)%>%
  summarise(mmm=mean(monthly_mean),mmm_sd=sd(monthly_mean)) #calculate average and sd of monthly mean for each month

mmm<-as.numeric((mmm_12%>%slice_max(n=1,mmm))[2]);mmm

####### FIGURE: OVERALL DATASET ############################################################# #####
f1a<-ggplot(master_temp)+
  geom_point(aes(date,temp,color=site,group=site),size=0.5,alpha=0.7)+
  scale_x_date(date_breaks="5 year",minor_breaks=waiver(),labels=date_format("%Y"),limits = c(as.Date("1955-01-01"), NA),expand=c(0,0))+
  theme_classic(base_size=8)+
  ylab("Daily Mean Temp (°C)")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        legend.position=c(0.07,0.85),
        legend.background=element_blank(),
        legend.spacing.x=unit(0.05,"cm"),
        legend.key.size=unit(0.3,"cm"))+
  scale_y_continuous(breaks=seq(10,35,1))+
  coord_cartesian(ylim=c(19,31.5))+
  scale_color_manual(values=c("#00A6A6","#4E4B5C","#EF476F"),name="Site")

anno_f1b<-paste0("MMM=",round(mmm,3),"°C")
f1b<-ggplot(master_temp)+
  geom_hline(yintercept=mmm,linetype="dotted",linewidth=0.5,color="black",alpha=0.5)+
  geom_hline(yintercept=mmm+1,linetype="dotted",linewidth=0.5,color="red",alpha=0.5)+
  annotate("rect", xmin = as.Date("1994-01-01"), xmax = as.Date("1996-01-01"), ymin = 18, ymax = 34,alpha = .1,fill = "blue")+
  annotate("rect", xmin = as.Date("1997-01-01"), xmax = as.Date("2003-12-31"), ymin = 18, ymax = 34,alpha = .1,fill = "blue")+
  annotate("text",x=as.Date("1994-04-01"),y=20.75,label="Range used for MMM Calculations",size=2,hjust=0,fontface="italic")+
  geom_point(aes(date,kbay_mean),size=0.5,alpha=0.15,color="#4F6D7A")+
  theme_classic(base_size=8)+
  scale_x_date(date_breaks="5 year",minor_breaks=waiver(),labels=date_format("%Y"),limits = c(as.Date("1955-01-01"), NA),expand=c(0,0))+
  ylab("Daily Mean Temp (°C)")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_text(angle = 0,vjust=0.5),
        legend.position=c(0.9,0.2),
        legend.key.size=unit(0.3,"cm"))+
  scale_y_continuous(breaks=seq(10,35,1))+
  coord_cartesian(ylim=c(20,30))+
  annotate("text",y=20,x=as.Date("1994-04-01"),label=anno_f1b,size=2,color="black",hjust=0,fontface="italic")+
  annotate("text",y=mmm+0.4,x=as.Date("1956-01-01"),label="MMM",size=2,color="darkgray",hjust=0)+
  annotate("text",y=mmm+1.4,x=as.Date("1956-01-01"),label="Bleaching Threshold (MMM +1°C)",size=2,color="gray",hjust=0)

quartz(w=7.2,h=3.7)
plot_grid(f1a,NULL,f1b,align="h",axis="lr",ncol=1,labels=c("A","","C"),label_size=8,rel_heights=c(1,-0.11,1))

####### FIGURE: SLOPES ###################################################################### #####
slope<-master_temp%>%
  arrange(date)%>%
  select(date,kbay_mean)%>%
  distinct()%>%
  ungroup()%>%
  mutate(start=min(date))%>%
  mutate(duration=as.numeric(date-start))%>%
  arrange(date)%>%
  do(tidy(lm(kbay_mean ~ duration, data = .))) %>%
  filter(term=="duration")%>%
  mutate(rate=estimate*365,
         se=std.error*365);slope

anno_f2a<-paste0("Slope: ",round(slope$rate,3)," ±",round(slope$se,4),"°C/year (mean ±1SE)")
f2a<-ggplot(master_temp)+
  geom_hline(yintercept=mmm,linetype="dotted",linewidth=0.5,color="black",alpha=0.5)+
  geom_hline(yintercept=mmm+1,linetype="dotted",linewidth=0.5,color="red",alpha=0.5)+
  geom_point(aes(date,kbay_mean),size=0.5,alpha=0.15,color="#4F6D7A")+
  theme_classic(base_size=8)+
  scale_x_date(date_breaks="5 year",minor_breaks=waiver(),labels=date_format("%Y"),limits = c(as.Date("1955-01-01"), NA),expand=c(0,0))+
  ylab("Daily Mean Temp (°C)")+
  xlab("Year")+
  theme(axis.text.x=element_text(angle = 0,vjust=0.5),
        legend.position=c(0.9,0.2),
        legend.key.size=unit(0.3,"cm"))+
  scale_y_continuous(breaks=seq(10,35,1))+
  coord_cartesian(ylim=c(20,30))+
  annotate("text",y=20,x=as.Date("1956-01-01"),label=anno_f2a,size=2,color="blue",hjust=0)+
  geom_smooth(aes(date,kbay_mean),method="lm")+
  annotate("text",y=mmm+0.4,x=as.Date("1956-01-01"),label="MMM",size=2,color="darkgray",hjust=0)+
  annotate("text",y=mmm+1.4,x=as.Date("1956-01-01"),label="Bleaching Threshold (MMM +1°C)",size=2,color="gray",hjust=0)

f2b<-master_temp%>%
  arrange(date)%>%
  select(date,kbay_mean)%>%
  distinct()%>%
  filter(date>"1959-12-31")%>%
  ungroup()%>%
  mutate(decade=floor(year(date)/10)*10)%>%
  mutate(start=ymd(paste0(decade,"-01-01")))%>%
  mutate(duration=as.numeric(date-start))%>%
  arrange(date)%>%
  group_by(decade) %>%   
  do(tidy(lm(kbay_mean ~ duration, data = .))) %>%   
  filter(term=="duration")%>%
  mutate(rate=estimate*365,
         se=std.error*365)%>%
  ggplot()+
  geom_errorbar(aes(as.factor(decade),ymin=rate-se,ymax=rate+se,color=rate),width=0)+
  geom_point(aes(as.factor(decade),rate,color=rate),size=3)+
  ylab("Warming Rate Per Year (°C)")+
  xlab("Decade")+
  scale_color_gradient(low="orange",high="red",guide="none")+
  theme_classic(base_size=8)+
  theme(axis.text.x=element_text(angle=45,hjust=1))

quartz(w=5.2,h=2.5)
plot_grid(f2a,f2b,rel_widths=c(2,1),align="h",labels="AUTO",label_size=8)

####### FIGURE: YEARLY TIMESERIES ########################################################### #####
f3a<-ggplot(master_temp)+
  geom_hline(yintercept=mmm,linetype="dotted",size=0.5,color="black",alpha=0.5)+
  geom_hline(yintercept=mmm+1,linetype="dotted",size=0.5,color="red",alpha=0.5)+
  geom_line(aes(dummy_date,kbay_mean,group=year,color=year))+
  theme_classic(base_size=8)+
  scale_x_date(date_breaks="1 month",minor_breaks=waiver(),labels=date_format("%b"))+
  scale_y_continuous(breaks=seq(10,35,1),limits=c(19,31))+
  ylab("Daily Mean Temp (°C)")+
  xlab("Month")+
  theme(legend.key.size=unit(0.3,"cm"),
        legend.position=c(0.6,0.1),
        legend.background=element_blank())+
  scale_color_gradient2(low = "blue",mid="purple",high="orange",midpoint=1989,name="Year",breaks=seq(1955,2023,10))+
  annotate("text",y=mmm+0.4,x=as.Date("1994-01-01"),label="MMM",size=2,color="darkgray",hjust=0)+
  annotate("text",y=mmm+1.4,x=as.Date("1994-01-01"),label="Bleaching Threshold (MMM +1°C)",size=2,color="gray",hjust=0)+
  guides(colour = guide_colourbar(direction = "horizontal",
                                  barheight = unit(0.1, "cm"),
                                  barwidth = unit(5,"cm"),
                                  label.theme = element_text(angle = 90,hjust=1,vjust=1,size=6)));f3a

####### FIGURE: MONTHLY AVERAGES TIMESERIES ################################################# #####
#USING ENTIRE BAY AVERAGED TEMPERATURE ACROSS 3 INSTRUMENTS FOR ALL YEARS
#CALCULATE AVERAGE FOR EACH MONTH, USING YEARS AS REPLICATES
plotdata<-monthly_summary%>%select(year,month,monthly_mean)%>%distinct()

f3b<-ggplot()+
  geom_jitter(aes(as.factor(month),monthly_mean),alpha=0.5,size=2,height=0,width=0.2,data=plotdata)+
  geom_errorbar(aes(as.factor(month),ymin=mmm-mmm_sd,ymax=mmm+mmm_sd),color="black",width=0,data=mmm_12)+
  geom_point(aes(as.factor(month),mmm),color="blue",size=4,pch=18,data=mmm_12)+
  geom_hline(yintercept=mmm,linetype="dotted",size=0.5,color="black",alpha=0.5)+
  geom_hline(yintercept=mmm+1,linetype="dotted",size=0.5,color="red",alpha=0.5)+
  theme_classic(base_size=8)+
  scale_y_continuous(breaks=seq(10,35,1),limits=c(19,31))+
  ylab("Monthly Mean Temp (°C)")+
  xlab("Month")+
  annotate("text",x=1,y=20,label="MMM Calculation Years Only\n(1994,1995,1997,2002,2003)",size=2,hjust=0)+
  annotate("text",y=mmm+0.4,x=1,label="MMM",size=2,color="darkgray",hjust=0)+
  annotate("text",y=mmm+1.4,x=1,label="Bleaching Threshold (MMM +1°C)",size=2,color="gray",hjust=0)+
  theme(legend.position="none");f3b

quartz(w=7.2,h=3)
plot_grid(f3a,f3b,align="v",axis="tb",labels="AUTO",label_size=8,rel_widths=c(2,1))

  
####### FIGURE: DHW CALCULATIONS ############################################################ #####
dhw<-master_temp%>%select(date,kbay_mean)%>%distinct()%>%
  ungroup()%>%
  arrange(date)%>%
  complete(date=seq.Date(min(date),max(date),by="day"))%>%
  distinct()%>%
  fill(kbay_mean,.direction="up")%>%
  mutate(diff=kbay_mean-mmm)%>%
  yepmutate(diff=case_when(diff<1~0,
                         TRUE ~as.numeric(diff)))%>%
  #mutate(diff=replace_na(diff,0))%>%
  mutate(dhw=slide_dbl(diff,sum,.before=83))%>%
  mutate(year=year(date))%>%
  group_by(year)%>%
  mutate(max=max(dhw))%>%
  ungroup()%>%
  mutate(max=case_when(max<1~NA,
                       TRUE~as.numeric(max)))

labs<-dhw%>%filter(!is.na(max))%>%group_by(year)%>%
  slice_max(dhw,n=1,with_ties = FALSE)%>%select(date,year,dhw,max)%>%distinct()

quartz()
ggplot(dhw)+
  geom_line(aes(date,dhw,color=..y..))+
  geom_text_repel(aes(date,dhw,label=round(max,1)),data=labs,min.segment.length = 0,nudge_y = 1,size=2)+
  theme_classic(base_size=8)+
  scale_x_date(date_breaks="5 year",minor_breaks=waiver(),labels=date_format("%Y"),limits = c(as.Date("1955-01-01"), NA),expand=c(0,0))+
  scale_color_gradientn(colors=c(low="black",mid="red",high="orange"))

labs<-dhw%>%filter(!is.na(max))%>%group_by(year)%>%
  slice_max(dhw,n=1,with_ties = FALSE)%>%select(date,year,dhw,max)%>%distinct()%>%
  filter(date>"2000-01-01")

ggplot(dhw%>%filter(date>"2000-01-01"))+
  geom_line(aes(date,dhw,color=..y..))+
  geom_text_repel(aes(date,dhw,label=round(max,1)),data=labs,min.segment.length = 0,nudge_y = 1,size=2)+
  theme_classic(base_size=8)+
  scale_x_date(date_breaks="1 year",minor_breaks=waiver(),labels=date_format("%Y"),limits = c(as.Date("2000-01-01"), NA),expand=c(0,0))+
  scale_color_gradientn(colors=c(low="black",mid="red",high="orange"))

