library("ggplot2")
library("dplyr")
library("dbConnect")
secrets <- function() {
  path <- "./secrets.json"
  if (!file.exists(path)) {
    stop("Can't find settings file")
  }
  jsonlite::read_json(path)
}

settings <- secrets()

setwd <- function(dir) {
  if (missing(dir) || is.null(dir) || dir == "") {
    dir <- settings[[5]]
  }
  base::setwd(dir)
}
setwd()
setwd("./data")
#Connect to the database.
con <- dbConnect(RPostgres::Postgres(), dbname = "tf2",
                 host = settings[[1]], port = settings[[2]],
                 user = settings[[3]], password = settings[[4]])
res <- dbSendQuery(con, "select * from public.player_map_names_2")
df0 <- dbFetch(res)

#Get class data
res <- dbSendQuery(con, "select * from public.season_27_stats")
s27 <- dbFetch(res) %>% filter (div == "prem")
res <- dbSendQuery(con, "select * from public.season_26_stats")
s26 <- dbFetch(res)
res <- dbSendQuery(con, "select * from public.season_25_stats")
s25 <- dbFetch(res)
res <- dbSendQuery(con, "select * from public.season_24_stats")
s24 <- dbFetch(res)
res <- dbSendQuery(con, "select * from public.season_23_stats")
s23 <- dbFetch(res)
res <- dbSendQuery(con, "select * from public.season_22_stats")
s22 <- dbFetch(res)
res <- dbSendQuery(con, "select * from public.season_21_stats")
s21 <- dbFetch(res)
res <- dbSendQuery(con, "select * from public.season_20_stats")
s20 <- dbFetch(res)
res <- dbSendQuery(con, "select * from public.season_19_stats")
s19 <- dbFetch(res)
res <- dbSendQuery(con, "select * from public.season_18_stats")
s18 <- dbFetch(res)
res <- dbSendQuery(con, "select * from public.season_17_stats")
s17 <- dbFetch(res)
res <- dbSendQuery(con, "select * from public.season_16_stats")
s16 <- dbFetch(res)
res <- dbSendQuery(con, "select * from public.season_15_stats")
s15 <- dbFetch(res)
res <- dbSendQuery(con, "select * from public.season_14_stats")
s14 <- dbFetch(res)

allSeasons <- rbind(s27,s26,s25,s24,s23,s22,s21,s20,s19,s18,s17,s15,s14)
allSeasons <- allSeasons %>% dplyr::filter(div == "prem")
allSeasons <- allSeasons[,2]
allSeasons <- allSeasons[!duplicated(allSeasons)]
write.csv(allSeasons,'allSeasons.csv')

res <- dbSendQuery(con, "select * from public.map_data")
gamedata <- dbFetch(res)
write.csv(gamedata, 'gamedata.csv')

generateStats <- function(tf2class,season, sno) {
  df0 <- season[season$class_primary == tf2class,]
  result <- df0 %>% group_by(name,class_primary) %>%
  summarise("total_mins_played" = sum(mins_total/60),
            "season" = sno,
            "kills_per_minute" = (sum(kills)/sum(mins_total/60)),
            "kills_per_death" = (sum(kills)/sum(deaths)),
            "ka_d" = (sum(kills) + sum(assists))/sum(deaths),
            "damage_per_minute" = sum(damage)/sum(mins_total/60),
            "heals_per_minute" = sum(heals_received)/sum(mins_total/60),
            "damage_per_heal" = sum(damage)/sum(heals_received),
            "caps_per_minute" = sum(caps)/sum(mins_total/60),
            "deaths_per_minute" = sum(deaths)/sum(mins_total/60),
            "dt_per_minute" = sum(damage_taken)/sum(mins_total/60),
            "dt_per_heal" = sum(damage_taken)/sum(heals_received),
            "damage_per_dt" = sum(damage)/sum(damage_taken))
  return(result)
}

scout.s27 <- generateStats("scout",s27, 27)
scout.s26 <- generateStats("scout",s26, 26)
scout.s25 <- generateStats("scout",s25, 25)
scout.s24 <- generateStats("scout",s24, 24)
scout.s23 <- generateStats("scout",s23, 23)
scout.s22 <- generateStats("scout",s22, 22)
scout.s21 <- generateStats("scout",s21, 21)
scout.s20 <- generateStats("scout",s20, 20)
scout.s19 <- generateStats("scout",s19, 19)
scout.s18 <- generateStats("scout",s18, 18)
scout.s17 <- generateStats("scout",s17, 17)
scout.s16 <- generateStats("scout",s16, 16)
scout.s15 <- generateStats("scout",s15, 15)
scout.s14 <- generateStats("scout",s14, 14)
allScout <- rbind(scout.s27, scout.s26, scout.s25, scout.s24, scout.s23,scout.s22,scout.s21,scout.s20,scout.s19, scout.s18, scout.s17, scout.s16, scout.s15, scout.s14)

allScoutFilter <- allScout %>% filter(total_mins_played > 119)

#temp translation in lieu of normalisation
temp<-allScoutFilter[(allScoutFilter$season == 20) | (allScoutFilter$season == 19) | (allScoutFilter$season == 18) | (allScoutFilter$season == 17) | (allScoutFilter$season == 13) |  (allScoutFilter$season == 12)  ,]
temp1<-allScoutFilter[(allScoutFilter$season == 21),]
temp2<-allScoutFilter[(allScoutFilter$season == 22) | (allScoutFilter$season == 23) | (allScoutFilter$season == 24) | (allScoutFilter$season == 25) | (allScoutFilter$season == 26) | (allScoutFilter$season == 27)  ,]
temp3<-allScoutFilter[(allScoutFilter$season == 16), ]
temp4<-allScoutFilter[(allScoutFilter$season == 15), ]
temp5<-allScoutFilter[(allScoutFilter$season == 14), ]

temp$heals_per_minute <- temp$heals_per_minute + (mean(temp2$heals_per_minute) - mean(temp$heals_per_minute))
temp1$heals_per_minute <- temp1$heals_per_minute + (mean(temp2$heals_per_minute) - mean(temp1$heals_per_minute))
temp3$heals_per_minute <- temp3$heals_per_minute + (mean(temp2$heals_per_minute) - mean(temp3$heals_per_minute))
temp4$heals_per_minute <- temp4$heals_per_minute + (mean(temp2$heals_per_minute) - mean(temp4$heals_per_minute))
temp5$heals_per_minute <- temp5$heals_per_minute + (mean(temp2$heals_per_minute) - mean(temp5$heals_per_minute))
allScoutFilter1<-rbind(temp,temp1,temp2,temp3,temp4,temp5)

write.csv(allScoutFilter1,'allScoutFilter1.csv')

#Soldiers
soldier.s27 <- generateStats("soldier",s27, 27)
soldier.s26 <- generateStats("soldier",s26, 26)
soldier.s25 <- generateStats("soldier",s25, 25)
soldier.s24 <- generateStats("soldier",s24, 24)
soldier.s23 <- generateStats("soldier",s23, 23)
soldier.s22 <- generateStats("soldier",s22, 22)
soldier.s21 <- generateStats("soldier",s21, 21)
soldier.s20 <- generateStats("soldier",s20, 20)
soldier.s19 <- generateStats("soldier",s19, 19)
soldier.s18 <- generateStats("soldier",s18, 18)
soldier.s17 <- generateStats("soldier",s17, 17)
soldier.s16 <- generateStats("soldier",s16, 16)
soldier.s15 <- generateStats("soldier",s15, 15)
soldier.s14 <- generateStats("soldier",s14, 14)
allSoldier <- rbind(soldier.s27, soldier.s26, soldier.s25, soldier.s24, soldier.s23, soldier.s22, soldier.s21, soldier.s20, soldier.s19, soldier.s18, soldier.s17, soldier.s16, soldier.s15, soldier.s14)

allSoldierFilter <- allSoldier %>% filter(total_mins_played > 119)

#temp translation in lieu of normalisation
temp<-allSoldierFilter[(allSoldierFilter$season == 20) | (allSoldierFilter$season == 19) | (allSoldierFilter$season == 18) | (allSoldierFilter$season == 17) | (allSoldierFilter$season == 13) |  (allSoldierFilter$season == 12)  ,]
temp1<-allSoldierFilter[(allSoldierFilter$season == 21),]
temp2<-allSoldierFilter[(allSoldierFilter$season == 22) | (allSoldierFilter$season == 23) | (allSoldierFilter$season == 24) | (allSoldierFilter$season == 25) | (allSoldierFilter$season == 26) | (allSoldierFilter$season == 27)  ,]
temp3<-allSoldierFilter[(allSoldierFilter$season == 16), ]
temp4<-allSoldierFilter[(allSoldierFilter$season == 15), ]
temp5<-allSoldierFilter[(allSoldierFilter$season == 14), ]

temp$heals_per_minute <- temp$heals_per_minute + (mean(temp2$heals_per_minute) - mean(temp$heals_per_minute))
temp1$heals_per_minute <- temp1$heals_per_minute + (mean(temp2$heals_per_minute) - mean(temp1$heals_per_minute))
temp3$heals_per_minute <- temp3$heals_per_minute + (mean(temp2$heals_per_minute) - mean(temp3$heals_per_minute))
temp4$heals_per_minute <- temp4$heals_per_minute + (mean(temp2$heals_per_minute) - mean(temp4$heals_per_minute))
temp5$heals_per_minute <- temp5$heals_per_minute + (mean(temp2$heals_per_minute) - mean(temp5$heals_per_minute))
allSoldierFilter1<-rbind(temp,temp1,temp2,temp3,temp4,temp5)

write.csv(allSoldierFilter1,'allSoldierFilter1.csv')

#Demoman
demo.s27 <- generateStats("demoman",s27, 27)
demo.s26 <- generateStats("demoman",s26, 26)
demo.s25 <- generateStats("demoman",s25, 25)
demo.s24 <- generateStats("demoman",s24, 24)
demo.s23 <- generateStats("demoman",s23, 23)
demo.s22 <- generateStats("demoman",s22, 22)
demo.s21 <- generateStats("demoman",s21, 21)
demo.s20 <- generateStats("demoman",s20, 20)
demo.s19 <- generateStats("demoman",s19, 19)
demo.s18 <- generateStats("demoman",s18, 18)
demo.s17 <- generateStats("demoman",s17, 17)
demo.s16 <- generateStats("demoman",s16, 16)
demo.s15 <- generateStats("demoman",s15, 15)
demo.s14 <- generateStats("demoman",s14, 14)
allDemo <- rbind(demo.s27, demo.s26, demo.s25, demo.s24, demo.s23, demo.s22, demo.s21, demo.s20, demo.s19, demo.s18, demo.s17, demo.s16, demo.s15,demo.s14)

allDemoFilter <- allDemo %>% filter(total_mins_played > 119)

#temp translation in lieu of normalisation
temp<-allDemoFilter[(allDemoFilter$season == 20) | (allDemoFilter$season == 19) | (allDemoFilter$season == 18) | (allDemoFilter$season == 17) | (allDemoFilter$season == 13) |  (allDemoFilter$season == 12)  ,]
temp1<-allDemoFilter[(allDemoFilter$season == 21),]
temp2<-allDemoFilter[(allDemoFilter$season == 22) | (allDemoFilter$season == 23) | (allDemoFilter$season == 24) | (allDemoFilter$season == 25) | (allDemoFilter$season == 26) | (allDemoFilter$season == 27)  ,]
temp3<-allDemoFilter[(allDemoFilter$season == 16), ]
temp4<-allDemoFilter[(allDemoFilter$season == 15), ]
temp5<-allDemoFilter[(allDemoFilter$season == 14), ]

temp$heals_per_minute <- temp$heals_per_minute + (mean(temp2$heals_per_minute) - mean(temp$heals_per_minute))
temp1$heals_per_minute <- temp1$heals_per_minute + (mean(temp2$heals_per_minute) - mean(temp1$heals_per_minute))
temp3$heals_per_minute <- temp3$heals_per_minute + (mean(temp2$heals_per_minute) - mean(temp3$heals_per_minute))
temp4$heals_per_minute <- temp4$heals_per_minute + (mean(temp2$heals_per_minute) - mean(temp4$heals_per_minute))
temp5$heals_per_minute <- temp5$heals_per_minute + (mean(temp2$heals_per_minute) - mean(temp5$heals_per_minute))
allDemoFilter1<-rbind(temp,temp1,temp2,temp3,temp4,temp5)

write.csv(allDemoFilter1,'allDemoFilter1.csv')

demoplot3 <- ggplot(allDemoFilter1, aes(x=heals_per_minute, y=damage_per_minute, label=allDemoFilter1$name)) + geom_point(aes(colour=factor(allDemoFilter1$season))) + geom_text(nudge_y=-2.0)
demoplot3 <- demoplot3 + labs(title="Seasons 14-27 Scout Heals vs. Damage (Minimum 60 Minutes Played)", x="Heals per Minute", y="Damage per Minute")
demoplot3 <- demoplot3 + stat_smooth(method="glm")

###MEDIC

generateMedStats <- function(tf2class,season, sno) {
  df0 <- season[season$class_primary == tf2class,]
  result <- df0 %>% group_by(name,class_primary) %>%
    summarise("total_mins_played" = sum(mins_total/60),
              "season" = sno,
              "kills_per_minute" = (sum(kills)/sum(mins_total/60)),
              "kills_per_death" = (sum(kills)/sum(deaths)),
              "ka_d" = (sum(kills) + sum(assists))/sum(deaths),
              "damage_per_minute" = sum(damage)/sum(mins_total/60),
              "heals_per_minute" = sum(heals_received)/sum(mins_total/60),
              "damage_per_heal" = sum(damage)/sum(heals_received),
              "caps_per_minute" = sum(caps)/sum(mins_total/60),
              "deaths_per_minute" = sum(deaths)/sum(mins_total/60),
              "dt_per_minute" = sum(damage_taken)/sum(mins_total/60),
              "dt_per_heal" = sum(damage_taken)/sum(heals_received),
              "damage_per_dt" = sum(damage)/sum(damage_taken),
              "ubers_per_minute" = sum(ubers)/sum(mins_total/60),
              "healing_per_minute" = sum(healing_done)/sum(mins_total/60),
              "drops_per_minute" = sum(drops)/sum(mins_total/60),
              "drops_per_uber" = sum(drops)/sum(ubers)
              )
  return(result)
}

medic.s27 <- generateMedStats("medic",s27, 27)
medic.s26 <- generateMedStats("medic",s26, 26)
medic.s25 <- generateMedStats("medic",s25, 25)
medic.s24 <- generateMedStats("medic",s24, 24)
medic.s23 <- generateMedStats("medic",s23, 23)
medic.s22 <- generateMedStats("medic",s22, 22)
medic.s21 <- generateMedStats("medic",s21, 21)
medic.s20 <- generateMedStats("medic",s20, 20)
medic.s19 <- generateMedStats("medic",s19, 19)
medic.s18 <- generateMedStats("medic",s18, 18)
medic.s17 <- generateMedStats("medic",s17, 17)
medic.s16 <- generateMedStats("medic",s16, 16)
medic.s15 <- generateMedStats("medic",s15, 15)
medic.s14 <- generateMedStats("medic",s14, 14)
allMedic <- rbind(medic.s27, medic.s26, medic.s25, medic.s24, medic.s23, medic.s22, medic.s21, medic.s20, medic.s19, medic.s18, medic.s17, medic.s16, medic.s15, medic.s14)

allmedicFilter <- allMedic %>% filter(total_mins_played > 119)

#temp translation in lieu of normalisation
temp<-allmedicFilter[(allmedicFilter$season == 20) | (allmedicFilter$season == 19) | (allmedicFilter$season == 18) | (allmedicFilter$season == 17) | (allmedicFilter$season == 13) |  (allmedicFilter$season == 12)  ,]
temp1<-allmedicFilter[(allmedicFilter$season == 21),]
temp2<-allmedicFilter[(allmedicFilter$season == 22) | (allmedicFilter$season == 23) | (allmedicFilter$season == 24) | (allmedicFilter$season == 25) | (allmedicFilter$season == 26) | (allmedicFilter$season == 27)  ,]
temp3<-allmedicFilter[(allmedicFilter$season == 16), ]
temp4<-allmedicFilter[(allmedicFilter$season == 15), ]
temp5<-allmedicFilter[(allmedicFilter$season == 14), ]

temp$heals_per_minute <- temp$heals_per_minute + (mean(temp2$heals_per_minute) - mean(temp$heals_per_minute))
temp1$heals_per_minute <- temp1$heals_per_minute + (mean(temp2$heals_per_minute) - mean(temp1$heals_per_minute))
temp3$heals_per_minute <- temp3$heals_per_minute + (mean(temp2$heals_per_minute) - mean(temp3$heals_per_minute))
temp4$heals_per_minute <- temp4$heals_per_minute + (mean(temp2$heals_per_minute) - mean(temp4$heals_per_minute))
temp5$heals_per_minute <- temp5$heals_per_minute + (mean(temp2$heals_per_minute) - mean(temp5$heals_per_minute))
temp$healing_per_minute <- temp$healing_per_minute + (mean(temp2$healing_per_minute) - mean(temp$healing_per_minute))
temp1$healing_per_minute <- temp1$healing_per_minute + (mean(temp2$healing_per_minute) - mean(temp1$healing_per_minute))
temp3$healing_per_minute <- temp3$healing_per_minute + (mean(temp2$healing_per_minute) - mean(temp3$healing_per_minute))
temp4$healing_per_minute <- temp4$healing_per_minute + (mean(temp2$healing_per_minute) - mean(temp4$healing_per_minute))
temp5$healing_per_minute <- temp5$healing_per_minute + (mean(temp2$healing_per_minute) - mean(temp5$healing_per_minute))

allmedicFilter1<-rbind(temp,temp1,temp2,temp3,temp4,temp5)

write.csv(allmedicFilter1,'allmedicFilter1.csv')
