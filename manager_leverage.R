library(dplyr)
library(Lahman)
library(baseballr)
library(rvest)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(stringr)
library(broom)
library(purrr)
library(tibble)
library(reshape2)
#import fangraphs leverage data
file_list <- list.files(path="C:/Users/peter/OneDrive/Documents/baseball-databases/fangraphs/data")
pbplev<-data.frame()
for(i in c(1:length(file_list))){
  onepbplev<-read.csv(paste0("C:/Users/peter/OneDrive/Documents/baseball-databases/fangraphs/data/",file_list[i]), na.strings=c("","NA"))
  pbplev<-rbind(pbplev,onepbplev)
}
names(pbplev)<-c('date','home','pa','inning','hitter','pitcher','outs','bases','score','pbp','li','re','we','wpa','re24')
pbplev$inning<-gsub(pbplev$inning,pattern = 'â–¼',replacement='bottom')
pbplev$inning<-gsub(pbplev$inning,pattern = 'â–²',replacement='top')
pbplev$inning[is.na(pbplev$inning)]<-0
pbplev<-pbplev%>%group_by(inning)%>%mutate(r=row_number())%>%ungroup()%>%mutate(gameID=ifelse(inning==0,r,NA))%>%select(-r)%>%fill(gameID)

#create a theseaurus for location, nickname, acronym
file <- read_html("https://en.wikipedia.org/wiki/Major_League_Baseball") 
mlbteams <- html_table(html_nodes(file, "table")[[3]],fill=T) 
mlbac<-read.csv('C:/Users/peter/OneDrive/Documents/baseball-databases/fangraphs/thes/acronym.csv')
mlbteams<-mlbteams%>%left_join(mlbac%>%filter(Last.Year=='Present')%>%select(Full.Team.Name,Team.ID,Lahman),by=c('Team'='Full.Team.Name'))%>%na.omit()%>%mutate(nickname=ifelse(word(Team, -1)%in%c('Sox',"Jays"),word(Team, -2,-1),word(Team, -1)))
#join
pbplev<-pbplev%>%left_join(mlbteams%>%select(Team.ID,Lahman,nickname),by=c('home'='nickname'))

#grab schedule to get away teams
hsched<-data.frame()
asched<-data.frame()
for(j in 2015:2019){  
  for(i in unique(pbplev$Team.ID)){
    inter<-team_results_bref(i,j)
    hsched<-rbind(hsched,inter%>%filter(H_A=='H'))
    asched<-rbind(asched,inter%>%filter(H_A=='A'))
  }
}
sched1<-hsched%>%unite('inter_date',c(Date,Year),sep=' ')%>%mutate(idate=trimws(gsub(inter_date,pattern = ".*,|\\s*\\([^\\)]+\\)",replacement = '')))
sched1$date<-as.character(as.Date(sched1$idate,'%B %d %Y'))

sched2<-asched%>%unite('inter_date',c(Date,Year),sep=' ')%>%mutate(idate=trimws(gsub(inter_date,pattern = ".*,|\\s*\\([^\\)]+\\)",replacement = '')))
sched2$date<-as.character(as.Date(sched2$idate,'%B %d %Y'))

#attach managers to teams, no playoffs
fpbplev<-pbplev%>%left_join(sched1%>%select(date,Tm,Opp,Gm),by=c('date','Team.ID'='Tm'))%>%left_join(sched2%>%select(date,Tm,Gm),by=c('date','Opp'='Tm'))%>%left_join(mlbteams%>%select(Team.ID,Lahman),by=c('Opp'='Team.ID'))%>%na.omit()
#looking for non one to ones, looks like double headers
fpbplev<-fpbplev%>%group_by(date,Lahman.x)%>%filter((n_distinct(gameID)==2&min(gameID)==gameID&min(Gm.x)==Gm.x&min(Gm.y)==Gm.y)|(n_distinct(gameID)==2&max(gameID)==gameID&max(Gm.x)==Gm.x&max(Gm.y)==Gm.y)|n_distinct(gameID)==1)

manager_lev<-fpbplev%>%mutate(yearID=as.numeric(gsub(date,pattern = '-.*',replacement = "")))%>%left_join(Managers%>%group_by(yearID,teamID)%>%mutate(n=n(),first=G[1])%>%select(yearID,teamID,playerID,inseason,n,first),by=c('yearID','Lahman.x'='teamID'))%>%filter(n==1|(n==2&inseason==1&as.numeric(Gm.x)<=as.numeric(first))|(n==2&inseason==2&as.numeric(Gm.x)>as.numeric(first)))%>%left_join(Managers%>%group_by(yearID,teamID)%>%mutate(an=n(),afirst=G[1])%>%select(yearID,teamID,playerID,inseason,an,afirst),by=c('yearID','Lahman.y'='teamID'))%>%filter(an==1|(an==2&inseason.y==1&as.numeric(Gm.y)<=as.numeric(afirst))|(an==2&inseason.y==2&as.numeric(Gm.y)>as.numeric(afirst)))
#decide which manager has the field
fmanager_lev<-manager_lev%>%mutate(pitching=ifelse(grepl(inning,pattern='top'),Lahman.x,Lahman.y),manager=ifelse(grepl(inning,pattern='top'),playerID.x,playerID.y))
fmanager_lev$pitcher<-gsub(fmanager_lev$pitcher,pattern=" Jr.",replacement='')
#looking for double matches of pitchers Tanner and Tayler Scott on the Orioles 2019
k<-fmanager_lev%>%left_join(Pitching%>%left_join(People)%>%mutate(pitcher=paste(substring(nameFirst,1,1),nameLast))%>%select(pitcher,yearID,bbrefID,teamID),by=c('pitcher','yearID','pitching'='teamID'))
names(k)[length(k)]<-'playerID'
rep_names<-unique(k%>%group_by(pbp,date,outs,score,inning)%>%filter(n_distinct(playerID)>1)%>%ungroup()%>%select(playerID,yearID))
necc_box<-data.frame()
for(i in 1:nrow(rep_names)){
  id<-playername_lookup(rep_names$playerID[i])
  inter_box<-scrape_statcast_savant_pitcher(start_date = paste0(rep_names$yearID[i],"-03-20"),end_date = paste0(rep_names$yearID[i],"-10-30"), pitcherid = id$key_mlbam)%>%mutate(playerID=rep_names$playerID[i])
  necc_box<-rbind(necc_box,inter_box)
}

f<-k%>%group_by(pbp,date,outs,score,inning)%>%filter(n_distinct(playerID)>1)%>%ungroup()
f$date<-as.Date(f$date)
anti<-f%>%left_join(necc_box%>%mutate(inning=ifelse(inning_topbot=='Bot',paste('bottom',inning),paste(tolower(inning_topbot),inning)))%>%group_by(game_date,inning,outs_when_up,playerID)%>%summarise(velo=mean(release_speed)),by=c('date'='game_date','playerID','inning','outs'='outs_when_up'))%>%filter(is.na(velo))%>%select(-velo)
anti$date<-as.character(anti$date)
final_manager_lev<-k%>%anti_join(anti)

#interesting visualization
gg_manager_lev<-final_manager_lev%>%mutate(jinning=trimws(gsub(inning,pattern = "[[:alpha:]]",replacement = "")),inning_=ifelse(as.numeric(jinning)>=9,9,as.numeric(jinning)),third=ifelse(inning_<4,'First',ifelse(inning_>6,'Third','Second')))
gg_manager_lev$inning_[gg_manager_lev$inning_=='9']<-'9+'
gg_manager_lev$inning_<-as.factor(gg_manager_lev$inning_)
ggplot(gg_manager_lev%>%filter(li<6.1), aes(x=li, fill=inning_, color=inning_))+geom_density(alpha=.4)+ facet_grid(third ~ .)+theme_fivethirtyeight()+labs(title='Leverage Index Distribution by Inning',color = "Inning",fill = "Inning")

#fangraphs id
l<-lapply(unique(final_manager_lev$playerID),function(x)as.data.frame(playername_lookup(x)))
joiner<-do.call(rbind,l)
final_manager_lev<-final_manager_lev%>%left_join(joiner%>%select(key_bbref,key_fangraphs),by=c('playerID'='key_bbref'))
final_manager_lev$key_fangraphs<-as.character(final_manager_lev$key_fangraphs)

#pitcher quality metric
firsttable<-fg_pitch_leaders(x=2013,y=2019,qual = 0,ind=1)%>%filter(playerid%in%unique(final_manager_lev$key_fangraphs))
fiptable<-firsttable%>%select(playerid,Name,Season,FIP)%>%spread(Season,FIP)
gsstable<-firsttable%>%mutate(gss=GS/G)%>%select(playerid,Name,Season,gss)%>%spread(Season,gss)
tbftable<-firsttable%>%select(playerid,Name,Season,TBF)%>%spread(Season,TBF)
table<-fiptable%>%left_join(gsstable,by=c('playerid','Name'))%>%left_join(tbftable,by=c('playerid','Name'))
#use coeffiecents
weights<-summary(lm(`2019.x`~`2018.x`+`2017.x`,table))
comb_table<-table%>%mutate(first_fip=(ifelse(is.na(`2013.x`),0,`2013.x`)*weights$coefficients[3,1]+ifelse(is.na(`2014.x`),0,`2014.x`)*weights$coefficients[2,1]+`2015.x`)/(1+ifelse(is.na(`2013.x`),0,weights$coefficients[3,1])+ifelse(is.na(`2014.x`),0,weights$coefficients[2,1])),second_fip=(ifelse(is.na(`2014.x`),0,`2014.x`)*weights$coefficients[3,1]+ifelse(is.na(`2015.x`),0,`2015.x`)*weights$coefficients[2,1]+`2016.x`)/(1+ifelse(is.na(`2014.x`),0,weights$coefficients[3,1])+ifelse(is.na(`2015.x`),0,weights$coefficients[2,1])),third_fip=(ifelse(is.na(`2015.x`),0,`2015.x`)*weights$coefficients[3,1]+ifelse(is.na(`2016.x`),0,`2016.x`)*weights$coefficients[2,1]+`2017.x`)/(1+ifelse(is.na(`2015.x`),0,weights$coefficients[3,1])+ifelse(is.na(`2016.x`),0,weights$coefficients[2,1])),fourth_fip=(ifelse(is.na(`2016.x`),0,`2016.x`)*weights$coefficients[3,1]+ifelse(is.na(`2017.x`),0,`2017.x`)*weights$coefficients[2,1]+`2018.x`)/(1+ifelse(is.na(`2016.x`),0,weights$coefficients[3,1])+ifelse(is.na(`2017.x`),0,weights$coefficients[2,1])),fifth_fip=(ifelse(is.na(`2017.x`),0,`2017.x`)*weights$coefficients[3,1]+ifelse(is.na(`2018.x`),0,`2018.x`)*weights$coefficients[2,1]+`2019.x`)/(1+ifelse(is.na(`2017.x`),0,weights$coefficients[3,1])+ifelse(is.na(`2018.x`),0,weights$coefficients[2,1])))

manager_lev_fip<-final_manager_lev%>%left_join(comb_table,by=c('key_fangraphs'='playerid'))%>%mutate(gss=ifelse(yearID==2015,`2015.y`,ifelse(yearID==2016,`2016.y`,ifelse(yearID==2017,`2017.y`,ifelse(yearID==2018,`2018.y`,`2019.y`)))),TBF=ifelse(yearID==2015,`2015`,ifelse(yearID==2016,`2016`,ifelse(yearID==2017,`2017`,ifelse(yearID==2018,`2018`,`2019`)))),fip=ifelse(yearID==2015,first_fip,ifelse(yearID==2016,second_fip,ifelse(yearID==2017,third_fip,ifelse(yearID==2018,fourth_fip,fifth_fip)))))
manager_lev_fip<-manager_lev_fip%>%ungroup()%>%mutate(jinning=trimws(gsub(inning,pattern = "[[:alpha:]]",replacement = "")),inning_=ifelse(as.numeric(jinning)>=9,9,as.numeric(jinning)),third=ifelse(inning_<4,'First',ifelse(inning_>6,'Third','Second')))
fip_plus_joiner<-manager_lev_fip%>%filter(TBF>30,gss<.85)%>%group_by(yearID,pitching,playerID)%>%summarise(fip=mean(fip))%>%group_by(yearID,pitching)%>%mutate(fip_tplus=fip*100/median(fip))
manager_lev_fip_plus<-manager_lev_fip%>%filter(TBF>30,gss<.85)%>%left_join(fip_plus_joiner%>%select(playerID,yearID,fip_tplus))%>%group_by(pitching,yearID)%>%mutate(var=var(fip),min=min(fip))

#note grouping leverage creates a hidden correlation
#more of an negative exponential dist of li, the less effect can be found
manager_lev_fip_group<-manager_lev_fip_plus%>%ungroup()%>%mutate(lig=ifelse(li<.4,0,ifelse(li<.79,.7,ifelse(li<1.25,1,ifelse(li<2,2,3)))))%>%group_by(lig)%>%mutate(li_group=mean(li))
manager_lev_fip_group$lig<-as.factor(manager_lev_fip_group$lig)

ggcor<-manager_lev_fip_group%>%group_by(jinning,li_group)%>%summarise(mfip=mean(fip_tplus))%>%group_by(jinning)%>%summarise(cor=cor(li_group,mfip))
ggplot(ggcor%>%filter(as.numeric(jinning)<10),aes(x=jinning,y=cor,fill=cor))+geom_bar(stat='identity')+theme_fivethirtyeight()+labs(title='Correlation between Leverage Groups and Pitcher Quality by Inning')+scale_fill_gradient(low = "forest green", high = "cadet blue")
#########
ggcor<-manager_lev_fip_group%>%group_by(jinning)%>%summarise(cor=cor(li,fip))
ggplot(ggcor%>%filter(as.numeric(jinning)<10),aes(x=jinning,y=cor,fill=cor))+geom_bar(stat='identity')+theme_fivethirtyeight()+labs(title='Correlation between Leverage and Pitcher Quality by Inning')+scale_fill_gradient(low = "forest green", high = "cadet blue")

###
m<-manager_lev_fip_group%>%group_by(jinning,manager,yearID,li_group)%>%summarise(mfip=mean(fip_tplus))%>%na.omit()%>%group_by(jinning,manager,yearID)%>%summarise(cor=cor(li_group,mfip))%>%left_join(manager_lev_fip_group%>%group_by(yearID,manager)%>% arrange(fip_tplus)%>%summarise(min=min(fip_tplus),range=max(fip_tplus)-min(fip_tplus),var=var(fip_tplus)))
m$manager<-as.factor(m$manager)
m$manager<-relevel(m$manager,'sciosmi01')
mod1<-summary(lm(cor~min+range+var+yearID,m%>%filter(jinning%in%c('7','8','9'))))

manager_table<-m%>%filter(jinning%in%c('7','8','9'))%>%mutate(eff=cor-(mod1$coefficients['(Intercept)',1]+mod1$coefficients['min',1]*min+mod1$coefficients['var',1]*var+mod1$coefficients['yearID',1]*yearID))%>%group_by(manager,yearID)%>%summarise(eff=mean(eff))%>%spread(yearID,eff)%>%left_join(People%>%select(nameFirst,nameLast,playerID),by=c('manager'='playerID'))
manager_table$na_count <- apply(manager_table, 1, function(x) sum(is.na(x[-which(names(x)=='2019')])))
manager_table$na_count1 <- apply(manager_table, 1, function(x) sum(is.na(x[-which(names(x)%in%c('2019','2018'))])))
manager_table$na_count2 <- apply(manager_table, 1, function(x) sum(is.na(x[-which(names(x)%in%c('2019','2018','2017'))])))

k<-as.data.frame(manager_table%>%mutate(score_comb=(ifelse(!is.na(`2015`),`2015`,0)+ifelse(!is.na(`2016`),`2016`,0)+ifelse(!is.na(`2017`),`2017`,0)+ifelse(!is.na(`2018`),`2018`,0))/(4-na_count),score_comb1=(ifelse(!is.na(`2015`),`2015`,0)+ifelse(!is.na(`2016`),`2016`,0)+ifelse(!is.na(`2017`),`2017`,0))/(3-na_count2),score_comb2=(ifelse(!is.na(`2015`),`2015`,0)+ifelse(!is.na(`2016`),`2016`,0))/(2-na_count1)))
#4 years
cor(k%>%filter(!is.na(`2019`)&!is.na(score_comb))%>%select(`2019`),k%>%filter(!is.na(`2019`)&!is.na(score_comb))%>%select(score_comb))
#1 year
cor(k%>%filter(!is.na(`2015`)&!is.na(`2016`))%>%select(`2016`),k%>%filter(!is.na(`2015`)&!is.na(`2016`))%>%select(`2015`))
cor(k%>%filter(!is.na(`2017`)&!is.na(`2016`))%>%select(`2016`),k%>%filter(!is.na(`2017`)&!is.na(`2016`))%>%select(`2017`))
cor(k%>%filter(!is.na(`2017`)&!is.na(`2018`))%>%select(`2018`),k%>%filter(!is.na(`2017`)&!is.na(`2018`))%>%select(`2017`))
cor(k%>%filter(!is.na(`2018`)&!is.na(`2019`))%>%select(`2018`),k%>%filter(!is.na(`2018`)&!is.na(`2019`))%>%select(`2019`))

#2 years
cor(k%>%filter(!is.na(score_comb2)&!is.na(`2017`))%>%select(`2017`),k%>%filter(!is.na(score_comb2)&!is.na(`2017`))%>%select(score_comb2))
#3 years
cor(k%>%filter(!is.na(score_comb1)&!is.na(`2018`))%>%select(`2018`),k%>%filter(!is.na(score_comb1)&!is.na(`2018`))%>%select(score_comb1))

summary(lm(`2019`~score_comb,k))

totable<-manager_table%>%ungroup()%>%unite('name',c(nameFirst,nameLast),sep=" ")%>%select(-c(na_count,na_count1,na_count2))%>%melt(c('name','manager'))%>%na.omit()
totable$yearID<-as.numeric(as.character(totable$variable))
ftotable<-totable%>%left_join(Managers%>%select(yearID,teamID,playerID,G),by=c('yearID','manager'='playerID'))%>%left_join(unique(Teams%>%select(teamID,yearID,name)),by=c('teamID','yearID'))%>%filter(G>50)%>%select(name.y,name.x,yearID,value)%>%arrange(value)
names(ftotable)<-c('Team','Manager','Year','Score')
write.csv(ftotable,'C:/Users/peter/OneDrive/Documents/baseball-databases/fangraphs/thes/data.csv',row.names = F)
write.csv(manager_table%>%ungroup()%>%unite('name',c(nameFirst,nameLast),sep=" ")%>%select(name,`2015`,`2016`,`2017`,`2018`,`2019`),'C:/Users/peter/OneDrive/Documents/baseball-databases/fangraphs/thes/data2.csv',row.names = F)
#################
saves<-Pitching%>%filter(SV>0)%>%group_by(teamID,yearID)%>%summarise(n=n(),var=var(SV))%>%group_by(yearID)%>%summarise(n=sum(n)/n(),var=mean(var,na.rm = T))
cor((saves%>%filter(yearID>1968))$n,(saves%>%filter(yearID>1968))$var)
ggplot(saves%>%filter(yearID>1968),aes(yearID,var,col=n))+geom_point()+geom_smooth(se=F)+theme_fivethirtyeight()+labs(title="Variance in Teams Save Totals Since Save Rule",col="Number of Players with Saves per Team")
