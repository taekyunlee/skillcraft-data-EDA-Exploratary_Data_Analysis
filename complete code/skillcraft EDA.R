# skillcraft data  탐색적 데이터 분석 
# 등급별로 Skillcraft 데이터의 탐색적 자료분석 
#  산접도 찍고 등급별로 색깔입혀
skillcraft <-read.csv('C:/Users/renz/Desktop/RJ/SkillCraft1_Dataset.csv')
        
str(skillcraft)
class(skillcraft)
summary(skillcraft)

sum(is.na(skillcraft)) # 결측값
par(mfrow =c(1,1))
names = c('Bronze', 'Silver', 'Gold', 'Platinum', 'Diamond', 'Master', 'GrandMaster', 'Professional' )
barplot(table(skillcraft$LeagueIndex), names.arg = names,  main ='The number of user by LeagueIndex' ,xlab = 'LeagueIndex', ylab ='The number of user')

skillcraft[skillcraft$LeagueIndex == 1 , "LeagueIndex"] = 'Bronze'  # 값 바꾸기   
skillcraft[skillcraft$LeagueIndex == 2 , "LeagueIndex"] = 'Silver'
skillcraft[skillcraft$LeagueIndex == 3 , "LeagueIndex"] = 'Gold'
skillcraft[skillcraft$LeagueIndex == 4 , "LeagueIndex"] = 'Platinum'
skillcraft[skillcraft$LeagueIndex == 5 , "LeagueIndex"] = 'Diamond'
skillcraft[skillcraft$LeagueIndex == 6 , "LeagueIndex"] = 'Master'
skillcraft[skillcraft$LeagueIndex == 7 , "LeagueIndex"] = 'GrandMaster'
skillcraft[skillcraft$LeagueIndex == 8 , "LeagueIndex"] = 'Professional'


skillcraft$LeagueIndex <-factor(skillcraft$LeagueIndex, levels=c('Bronze', 'Silver', 'Gold', 'Platinum', 'Diamond', 'Master', 'GrandMaster', 'Professional' ), ordered=TRUE)
# factor변수로 순서설정해서 넣고 변동된것을 다시 대입해주어야 한다.
# boxplot
boxplot(as.numeric(skillcraft$HoursPerWeek)~skillcraft$LeagueIndex, main = 'HoursPerWeek by LeagueIndex',
        xlab = 'LeagueIndex' , ylab = 'HoursPerweek')  
boxplot(skillcraft$APM~skillcraft$LeagueIndex, main = 'APM by LeagueIndex',
        xlab = 'LeagueIndex' , ylab = 'APM (Action Per Minute)')
boxplot(skillcraft$MinimapAttacks~skillcraft$LeagueIndex, main = 'MinimapAttacks by LeagueIndex',
        xlab = 'LeagueIndex' , ylab = 'MinimapAttacks')
boxplot(skillcraft$UniqueUnitsMade~skillcraft$LeagueIndex, main = 'UniqueUnitsMade by LeagueIndex',
        xlab = 'LeagueIndex' , ylab = 'UniqueUnitsMade')
boxplot(skillcraft$WorkersMade~skillcraft$LeagueIndex, main = 'WorkersMade by LeagueIndex',
        xlab = 'LeagueIndex' , ylab = 'WorkersMade')
#boxplot(skillcraft$TotalMapExplored~skillcraft$LeagueIndex, main = 'TotalMapExplored by LeagueIndex',
#        xlab = 'LeagueIndex' , ylab = 'TotalMapExplored')
boxplot(skillcraft$ActionLatency~skillcraft$LeagueIndex, main = 'ActionLatency by LeagueIndex',
        xlab = 'LeagueIndex' , ylab = 'ActionLatency')
boxplot(skillcraft$ActionsInPAC~skillcraft$LeagueIndex, main = 'ActionsInPAC by LeagueIndex',
        xlab = 'LeagueIndex' , ylab = 'ActionsInPAC')
     

# histogram

par(mfrow =c(4,1))

hb <- hist(skillcraft[skillcraft$LeagueIndex=='Bronze', 'MinimapRightClicks'])
hb$counts=hb$counts/sum(hb$counts)
plot(hb,xlab ='Bronze',ylab = 'relative Frequency', main='MinimapRightClicks by LeagueIndex',col=colors()[1],nclass = 15,xlim =c(0, 0.005) ) 

hs <- hist(skillcraft[skillcraft$LeagueIndex=='Silver', 'MinimapRightClicks'])
hs$counts=hs$counts/sum(hs$counts)
plot(hs,xlab ='Silver',ylab = 'relative Frequency', main='MinimapRightClicks by LeagueIndex',col=colors()[10],nclass = 15,xlim =c(0, 0.005) )  

hg <- hist(skillcraft[skillcraft$LeagueIndex=='Gold', 'MinimapRightClicks'])
hg$counts=hg$counts/sum(hg$counts)
plot(hg,xlab ='Gold',ylab = 'relative Frequency', main='MinimapRightClicks by LeagueIndex',col=colors()[200],nclass = 15,xlim =c(0, 0.005) )  

hp <- hist(skillcraft[skillcraft$LeagueIndex=='Platinum', 'MinimapRightClicks'])
hp$counts=hp$counts/sum(hp$counts)
plot(hp,xlab ='Platinum',ylab = 'relative Frequency', main='MinimapRightClicks by LeagueIndex',col=colors()[30],nclass = 15,xlim =c(0, 0.005) )  

hd <- hist(skillcraft[skillcraft$LeagueIndex=='Diamond', 'MinimapRightClicks'])
hd$counts=hd$counts/sum(hd$counts)
plot(hd,xlab ='Diamond',ylab = 'relative Frequency', main='MinimapRightClicks by LeagueIndex',col=colors()[65],nclass = 15,xlim =c(0, 0.005) )  

hm <- hist(skillcraft[skillcraft$LeagueIndex=='Master', 'MinimapRightClicks'])
hm$counts=hm$counts/sum(hm$counts)
plot(hm,xlab ='Master',ylab = 'relative Frequency', main='MinimapRightClicks by LeagueIndex',col=colors()[73],nclass = 15,xlim =c(0, 0.005) )  

hgm <- hist(skillcraft[skillcraft$LeagueIndex=='GrandMaster', 'MinimapRightClicks'])
hgm$counts=hgm$counts/sum(hgm$counts)
plot(hgm,xlab ='GrandMaster',ylab = 'relative Frequency', main='MinimapRightClicks by LeagueIndex',col=colors()[84],nclass = 15,xlim =c(0, 0.005) )  

hpr <- hist(skillcraft[skillcraft$LeagueIndex=='Professional', 'MinimapRightClicks'])
hpr$counts=hpr$counts/sum(hpr$counts)
plot(hpr,xlab ='Professional',ylab = 'relative Frequency', main='MinimapRightClicks by LeagueIndex',col=colors()[90],nclass = 15,xlim =c(0, 0.005) )  

##########################################
str(skillcraft$TotalHours)
par(mfrow =c(4,1))
skillcraft$TotalHours <- as.numeric(skillcraft$TotalHours)

hb <- hist(skillcraft[skillcraft$LeagueIndex=='Bronze', 'TotalHours'])
hb$counts=hb$counts/sum(hb$counts)
plot(hb,xlab ='Bronze',ylab = 'relative Frequency', main='TotalHours by LeagueIndex by LeagueIndex',col=colors()[1] ,xlim =c(0, 250) ) 

hs <- hist(skillcraft[skillcraft$LeagueIndex=='Silver', 'TotalHours'])
hs$counts=hs$counts/sum(hs$counts)
plot(hs,xlab ='Silver',ylab = 'relative Frequency', main='TotalHours by LeagueIndex by LeagueIndex',col=colors()[10],nclass = 15,xlim =c(0, 250) )  

hg <- hist(skillcraft[skillcraft$LeagueIndex=='Gold', 'TotalHours'])
hg$counts=hg$counts/sum(hg$counts)
plot(hg,xlab ='Gold',ylab = 'relative Frequency', main='TotalHours by LeagueIndex by LeagueIndex',col=colors()[200],nclass = 15,xlim =c(0,250) )  

hp <- hist(skillcraft[skillcraft$LeagueIndex=='Platinum', 'TotalHours'])
hp$counts=hp$counts/sum(hp$counts)
plot(hp,xlab ='Platinum',ylab = 'relative Frequency', main='TotalHours by LeagueIndex by LeagueIndex',col=colors()[30],nclass = 15,xlim =c(0, 250) )  

hd <- hist(skillcraft[skillcraft$LeagueIndex=='Diamond', 'TotalHours'])
hd$counts=hd$counts/sum(hd$counts)
plot(hd,xlab ='Diamond',ylab = 'relative Frequency', main='TotalHours by LeagueIndex by LeagueIndex',col=colors()[65],nclass = 15,xlim =c(0,250) )  

hm <- hist(skillcraft[skillcraft$LeagueIndex=='Master', 'TotalHours'])
hm$counts=hm$counts/sum(hm$counts)
plot(hm,xlab ='Master',ylab = 'relative Frequency', main='TotalHours by LeagueIndex by LeagueIndex',col=colors()[73],nclass = 15,xlim =c(0, 250) )  

hgm <- hist(skillcraft[skillcraft$LeagueIndex=='GrandMaster', 'TotalHours'])
hgm$counts=hgm$counts/sum(hgm$counts)
plot(hgm,xlab ='GrandMaster',ylab = 'relative Frequency', main='TotalHours by LeagueIndex by LeagueIndex',col=colors()[84],nclass = 15,xlim =c(0, 250) )  

hpr <- hist(skillcraft[skillcraft$LeagueIndex=='Professional', 'TotalHours'])
hpr$counts=hpr$counts/sum(hpr$counts)
plot(hpr,xlab ='Professional',ylab = 'relative Frequency', main='TotalHours by LeagueIndex by LeagueIndex',col=colors()[90],nclass = 15,xlim =c(0, 250) )  

###################################################3



par(mfrow =c(1,1))
# uniqueHotkeys 빈도분석 
hotkey.table <-xtabs( ~ skillcraft$LeagueIndex + skillcraft$UniqueHotkeys, data = skillcraft)
barplot(hotkey.table,xlab ='Frequency of uniqueHotkeys',ylab ='The number of user', col = c('green','red',colors()[10],colors()[20],colors()[30],colors()[300],colors()[2],colors()[60])
        , legend.text = TRUE , args.legend =list(cex= 0.85,text.width=0.35, bty ='n',yjust=0.8), main = 'UniqueHotkeys' )

# 등급별 주당게임시간 평균
skillcraft$HoursPerWeek <-as.numeric(skillcraft$HoursPerWeek)
hpw <-aggregate(formula = skillcraft$HoursPerWeek ~ skillcraft$LeagueIndex , 
          data = skillcraft , FUN = mean, na.rm = TRUE)

barplot(names.arg = names,hpw[,'skillcraft$HoursPerWeek' ], xlab ='LeagueIndex', ylab = 'Time',main= 'Average of HoursPerWeek' )

# 등급별 다루기 어려운 유닛의 평균수의 평균

comunit <-aggregate(formula = skillcraft$ComplexUnitsMade ~ skillcraft$LeagueIndex , 
                data = skillcraft , FUN = mean, na.rm = TRUE)

barplot(names.arg = names,comunit[,'skillcraft$ComplexUnitsMade' ], xlab ='LeagueIndex', ylab = 'units',main= 'Average of ComplexUnitsMade' )

# unique units made 평균

uniqunit <-aggregate(formula = skillcraft$UniqueUnitsMade ~ skillcraft$LeagueIndex , 
                    data = skillcraft , FUN = mean, na.rm = TRUE)

barplot(names.arg = names, uniqunit[,'skillcraft$UniqueUnitsMade' ], xlab ='LeagueIndex', ylab = 'units',main= 'Average of UniqueUnitsMade' )

# total map explored 평균 

uniqunit <-aggregate(formula =skillcraft$TotalMapExplored ~ skillcraft$LeagueIndex , 
                     data = skillcraft , FUN = mean, na.rm = TRUE)

barplot(names.arg = names, uniqunit[,'skillcraft$TotalMapExplored' ], xlab ='LeagueIndex', ylab = 'units',main= 'Average of TotalMapExplored' )

# ggplot 산점도 이용
if(!require(ggplot2)){install.packages("ggplot2"); library(ggplot2)}

scatterplot = ggplot(data = skillcraft ,
                     aes(x = skillcraft$APM, y = skillcraft$ActionLatency,  col = skillcraft$LeagueIndex)) + geom_point() +  xlab('APM') +  ylab("ActionLatency") + 
  ggtitle('scatteplot of APM & ActionLatency')

scatterplot

scatterplot = ggplot(data = skillcraft ,
                     aes(x = skillcraft$GapBetweenPACs, y = skillcraft$SelectByHotkeys,  col = skillcraft$LeagueIndex)) + geom_point() +  xlab('GapBetweenPACs') +  ylab("SelectByHotkeys") + 
  ggtitle('scatterplot of GapBetweenPACs & SelectByHotkeys')
scatterplot

names(skillcraft)






















































































