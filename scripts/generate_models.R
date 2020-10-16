##Dependencies
library("ggplot2")
library("dplyr")
library("dbConnect")
library("fitdistrplus")

secrets <- function(prepend=NULL) {
  path <- "./secrets.json"
  if(!is.null(prepend)) {
    path <- paste(prepend,"/secrets.json",sep="")
  }
  if (!file.exists(path)) {
    stop("Can't find settings file")
  }
  jsonlite::read_json(path)
}

##Need to redirect for the log analyser.
if(!grepl('scripts', getwd(), fixed=TRUE)) {
  settings <- secrets('../../scripts')
} else {
  settings <- secrets()
}

setwd <- function(dir) {
  if (missing(dir) || is.null(dir) || dir == "") {
    dir <- settings[[5]]
  }
  base::setwd(dir)
}

##Check p-values
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

##LINEAR MODELS
damage_class_lm <- function(dataframe, y, map) {
  temp <- dataframe[dataframe$map == map, ]
  output <- lm(get(y) ~ hpm + mins_total, data = temp)
  return(output)
}

damage_class_lm_multimap <- function(dataframe, y) {
  temp <- dataframe
  output <- lm(get(y) ~ hpm + mins_total, data = temp)
  return(output)
}

lm_variables <- c("dpm", "kad", "dtm", "deathspm", "cpm", "damageperdt", "kpm", "kd")
damage_classes <- c("scout", "demoman", "soldier")
all_maps <- c("granary", "product", "gullywash", "snakewater", "process",
              "prolands", "logjam", "kalinka", "reckoner", "sunshine","metalworks","bagel", "villa")

##EXPONENTIAL MODELS - need to make sure models are fitting each variable correctly!
damage_class_em <- function(dataframe, y, map) {
  temp <- dataframe[dataframe$map == map, ]
  temp <- temp %>% filter(season > 21)
  temp <- temp[!is.infinite(temp[[y]]), ]
  ##Select an approximate theta that is lower than min(y) and greater than zero
  theta <- min(temp[[y]]) * 0.5
  output <- lm(log(get(y) - theta) ~ hpm + mins_total, data = temp)
  return(output)
}

damage_class_em_multimap <- function(dataframe, y) {
  temp <-dataframe
  temp <- temp %>% filter(season > 21)
  temp <- temp[!is.infinite(temp[[y]]), ]
  theta <- min(temp[[y]]) * 0.5
  output <- lm(log(get(y) - theta) ~ hpm + mins_total, data = temp)
  return(output)
}

em_variables <- c("damageperheal")

generate_models <- function(player.data) {
  ##class data frames for models
  scout <- player.data[player.data$class_primary == "scout", ]
  scout <- scout %>% distinct()
  ##scout <- scout[scout$season > 21 ,]
  soldier <- player.data[player.data$class_primary == "soldier", ]
  soldier <- soldier %>% distinct()
  ##soldier <- soldier[soldier$season > 21, ]
  demoman <- player.data[player.data$class_primary == "demoman", ]
  demoman <- demoman %>% distinct()
  ##demoman <- demoman[demoman$season > 21, ]
  medic <- player.data[player.data$class_primary == "medic", ]
  medic <- medic[medic$season > 21, ]
  medic <- medic %>% distinct()
  medic$ubersperheal <- medic$chargespm / medic$healing
  medic$deathsperdt <- medic$deathspm / medic$dtm

  #med specific stats
  medic[is.infinite(medic$dropsperuber), ]$dropsperuber <- medic[is.infinite(medic$dropsperuber), ]$drops
  tryCatch({medic[is.nan(medic$dropsperuber), ]$dropsperuber <- 0}, error=function(err){print(err)},finally={})

  ##Generate Linear Models
  for (class in damage_classes) {
    setwd()
    setwd(paste("./data/models/",class,sep=""))
    for (var in lm_variables) {
      for (map in all_maps) {
        lm <- damage_class_lm(get(class), var, map)
        p <- lmp(lm)
        if(p>0.05) {
          print(paste('WARNING: Model has p>0.05: p=', p, " ", class,"-",map,"-",var,".rds",sep=""))
        }
        saveRDS(lm, paste(class,"-",map,"-",var,".rds",sep=""))
      }
    }
  }

  koth <- c("product","bagel")
  cp <- c("granary", "gullywash", "prolands", "logjam", "reckoner", "sunshine", "process", "metalworks", "kalinka", "snakewater", "villa")
  map_types <-c("koth", "cp")

  for (class in damage_classes) {
    setwd()
    setwd(paste("./data/models/",class,sep=""))
    for (var in lm_variables) {
      for (type in map_types) {
        temp <- get(class)[get(class)$map %in% get(type), ]
        lm <- damage_class_lm_multimap(temp, var)
        p <- lmp(lm)
        if(p>0.05) {
          print(paste('WARNING: Model has p>0.05: p=',p," ",class,"-",type,"-",var,".rds",sep=""))
        }
        saveRDS(lm, paste(class,"-",type,"-",var,".rds",sep=""))
      }
    }
  }
  for (class in damage_classes) {
    setwd()
    setwd(paste("./data/models/",class,sep=""))
    for (var in em_variables) {
      for (map in all_maps) {
        lm <- damage_class_em(get(class), var, map)
        p <- lmp(lm)
        if(p>0.05) {
          print(paste('WARNING: Model has p>0.05: p=', p, " ", class,"-",map,"-",var,".rds",sep=""))
        }
        saveRDS(lm, paste(class,"-",map,"-",var,".rds",sep=""))
      }
    }
  }

  for (class in damage_classes) {
    setwd()
    setwd(paste("./data/models/",class,sep=""))
    for (var in em_variables) {
      for (type in map_types) {
        temp <- get(class)[get(class)$map %in% get(type), ]
        lm <- damage_class_em_multimap(temp, var)
        p <- lmp(lm)
        if(p>0.05) {
          print(paste('WARNING: Model has p>0.05: p=',p," ",class,"-",type,"-",var,".rds",sep=""))
        }
        saveRDS(lm, paste(class,"-",type,"-",var,".rds",sep=""))
      }
    }
  }
}

##set blank columns
set_blank_exp <- function(class) {
  temp <- class
  allvars <- c("dpm", "kad", "dtm", "deathspm", "cpm", "damageperdt", "kpm", "damageperheal","kd")
  for (var in allvars) {
    new_var <- paste("exp_",var,sep="")
    exp_var <- paste("perf_",var,sep="")
    perc_var <- paste("perc_",var,sep="")
    temp[[new_var]] <- 0
    temp[[exp_var]] <- 0
    temp[[perc_var]] <- 0
  }
  return(temp)
}

##prediction function
predict_stats <- function(class, class_name) {
  setwd()
  temp <- class
  tf2maps <- c("granary", "gullywash", "prolands", "logjam", "reckoner", "sunshine", "process", "metalworks", "kalinka", "snakewater", "product", "bagel", "villa")
  allvars <- c("dpm", "kad", "dtm", "deathspm", "cpm", "damageperdt", "kpm", "damageperheal", "kd")
  for (map_name in tf2maps) {
    if(map_name %in% c("kalinka", "reckoner","metalworks", "villa")) {
      for(var in allvars) {
        new_var <- paste("exp_",var,sep="")
        lm_path <- paste("./data/models/",class_name,"/",class_name,"-","cp","-",var,".rds",sep="")
        lm <- readRDS(lm_path)
        temp[temp$map == map_name, ][[new_var]] <- predict(lm, temp[temp$map == map_name, ])
      }
    } else if(map_name %in% c("bagel")) {
      for(var in allvars) {
        new_var <- paste("exp_",var,sep="")
        lm_path <- paste("./data/models/",class_name,"/",class_name,"-","koth","-",var,".rds",sep="")
        lm <- readRDS(lm_path)
        temp[temp$map == map_name, ][[new_var]] <- predict(lm, temp[temp$map == map_name, ])
      }
    } else {
      for(var in allvars) {
        new_var <- paste("exp_",var,sep="")
        lm_path <- paste("./data/models/",class_name,"/",class_name,"-",map_name,"-",var,".rds",sep="")
        lm <- readRDS(lm_path)
        temp[temp$map == map_name, ][[new_var]] <- predict(lm, temp[temp$map == map_name, ])
      }
    }
  }
  return(temp)
}

perf_stats <- function(class) {
  temp <- class
  allvars <- c("dpm", "kad", "cpm", "damageperdt", "kpm","kd")
  invertvars <-c("deathspm","dtm")
  for (var in allvars) {
    exp_var <- paste("exp_",var,sep="")
    new_var <- paste("perf_",var,sep="")
    temp[[new_var]] <- temp[[var]] / temp[[exp_var]]
  }
  for (var in invertvars) {
    exp_var <- paste("exp_",var,sep="")
    new_var <- paste("perf_",var,sep="")
    temp[[new_var]] <- temp[[exp_var]] / temp[[var]]
  }
  return(temp)
}
