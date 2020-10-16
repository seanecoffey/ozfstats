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

##Distribution Fitting
damage_class_gamma <- function(dataframe, y, map_name) {
  out <- tryCatch(
    {
      temp <- dataframe[dataframe$map == map_name, ]
      ##Replace non-zero vals
      output <- fitdist(replace(temp[[y]], which(temp[[y]]==0), 0.01), "gamma", method="mle", lower=c(0,0))
      return(output)
    }, error = function(err) {
      print(dataframe[1,]$class_primary)
      print(paste("Couldn't fit gamma dist for ", y, map_name, sep=""))
    }, finally = {
    }

  )
  return(out)
}

damage_class_gamma_multimap <- function(dataframe, y) {
  out <- tryCatch(
    {
      temp <- dataframe
      output <- fitdist(replace(temp[[y]], which(temp[[y]]==0), 0.01), "gamma", method="mle", lower=c(0,0))
      return(output)
    }, error = function(err) {
      print(paste("Couldn't fit gamma dist for ", y, sep=""))
    }, finally = {
    }

  )
  return(out)
}

gamma_mean <- function(gammadist) {
  a <- coef(gammadist)[[1]]
  b <- coef(gammadist)[[2]]
  mean <- a/b
  return(mean)
}

gamma_sd <- function(gammadist) {
  a <- coef(gammadist)[[1]]
  b <- coef(gammadist)[[2]]
  sd <- sqrt(a/(b^2))
  return(sd)
}

gamma_percentile <- function(x, gammadist) {
  shape <- coef(gammadist)[[1]]
  rate <- coef(gammadist)[[2]]
  if(x==0) {
    x <- 0.001
  }
  p <- pgamma(x, shape, rate)
  return(p)
}

gamma_percentile_vec <- Vectorize(gamma_percentile, vectorize.args ="x")

##DISTRIBUTIONS
gamma_variables <- c("dpm", "kad", "dtm", "deathspm", "cpm", "damageperdt", "kpm", "kd")
gamma_variables_heals <- c("damageperheal")
damage_classes <- c("scout", "demoman", "soldier")
all_maps <- c("granary", "product", "gullywash", "snakewater", "process",
              "prolands", "logjam", "kalinka", "reckoner", "sunshine","metalworks","bagel", "villa")


##Generate distributions
generate_dists <- function(player.data) {
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

  ##Generate Distributions
  for (class in damage_classes) {
    setwd()
    setwd(paste("./data/distributions/",class,sep=""))
    for (var in gamma_variables) {
      for (map in all_maps) {
        gdist <- damage_class_gamma(get(class), var, map)
        saveRDS(gdist, paste(class,"-",map,"-",var,"-gamma.rds",sep=""))
      }
    }
  }

  for (class in damage_classes) {
    setwd()
    setwd(paste("./data/distributions/",class,sep=""))
    for (var in gamma_variables_heals) {
      for (map in all_maps) {
        temp <- get(class)
        temp <- temp %>% filter(season > 21)
        gdist <- damage_class_gamma(temp, var, map)
        saveRDS(gdist, paste(class,"-",map,"-",var,"-gamma.rds",sep=""))
      }
    }
  }

  koth <- c("product","bagel")
  cp <- c("granary", "gullywash", "prolands", "logjam", "reckoner", "sunshine", "process", "metalworks", "kalinka", "snakewater", "villa")
  map_types <-c("koth", "cp")

  for (class in damage_classes) {
    setwd()
    setwd(paste("./data/distributions/",class,sep=""))
    for (var in gamma_variables) {
      for (type in map_types) {
        temp <- get(class)[get(class)$map %in% get(type), ]
        gdist <- damage_class_gamma_multimap(temp, var)
        saveRDS(gdist, paste(class,"-",type,"-",var,"-gamma.rds",sep=""))
      }
    }
  }

  for (class in damage_classes) {
    setwd()
    setwd(paste("./data/distributions/",class,sep=""))
    for (var in gamma_variables_heals) {
      for (type in map_types) {
        temp <- get(class)[get(class)$map %in% get(type), ]
        temp <- temp %>% filter(season > 21)
        gdist <- damage_class_gamma_multimap(temp, var)
        saveRDS(gdist, paste(class,"-",type,"-",var,"-gamma.rds",sep=""))
      }
    }
  }
  ##MEDICS
  med_vars <- c("kad", "chargespm","healing","cpm","dropspm","deathspm","dtm","ubersperheal","deathsperdt","dropsperuber")
  setwd()
  setwd(paste("./data/distributions/medic",sep=""))
  for (var in med_vars) {
    for (map in all_maps) {
      gdist <- damage_class_gamma(medic, var, map)
      saveRDS(gdist, paste("medic-",map,"-",var,"-gamma.rds",sep=""))
    }
  }

  koth <- c("product", "bagel")
  cp <- c("granary", "gullywash", "prolands", "logjam", "reckoner", "sunshine", "process", "metalworks", "kalinka", "snakewater", "villa")
  map_types <-c("koth", "cp")
  setwd()
  setwd(paste("./data/distributions/medic",sep=""))
  for (var in med_vars) {
    for (type in map_types) {
      temp <- medic[medic$map %in% get(type), ]
      gdist <- damage_class_gamma_multimap(temp, var)
      saveRDS(gdist, paste("medic-",type,"-",var,"-gamma.rds",sep=""))
    }
  }
}

##percentile calcs
percentile_stats <- function(class, class_name) {
  temp <- class
  tf2maps <- c("granary", "gullywash", "prolands", "logjam", "reckoner", "sunshine", "process", "metalworks", "kalinka", "snakewater", "product", "bagel", "villa")
  allvars <- c("dpm", "kad", "dtm", "deathspm", "cpm", "damageperdt", "kpm", "damageperheal", "kd")
  for (map_name in tf2maps) {
    if(map_name %in% c("kalinka", "reckoner","metalworks", "villa")) {
      for(var in allvars) {
        new_var <- paste("perc_",var,sep="")
        setwd()
        home_path <- getwd()
        dist_path <- paste(home_path,"/data/distributions/",class_name,"/",class_name,"-","cp","-",var,"-gamma.rds",sep="")
        dist <- readRDS(dist_path)
        test <- temp[temp$map == map_name, ][[var]]
        perc <- gamma_percentile_vec(test, dist)
        temp[temp$map == map_name, ][[new_var]] <- perc
      }
    } else if(map_name %in% c("bagel")) {
      for(var in allvars) {
        new_var <- paste("perc_",var,sep="")
        setwd()
        home_path <- getwd()
        dist_path <- paste(home_path,"/data/distributions/",class_name,"/",class_name,"-","koth","-",var,"-gamma.rds",sep="")
        dist <- readRDS(dist_path)
        test <- temp[temp$map == map_name, ][[var]]
        perc <- gamma_percentile_vec(test, dist)
        temp[temp$map == map_name, ][[new_var]] <- perc
      }
    } else {
      for(var in allvars) {
        new_var <- paste("perc_",var,sep="")
        setwd()
        home_path <- getwd()
        dist_path <- paste(home_path,"/data/distributions/",class_name,"/",class_name,"-",map_name,"-",var,"-gamma.rds",sep="")
        dist <- readRDS(dist_path)
        test <- temp[temp$map == map_name, ][[var]]
        perc <- gamma_percentile_vec(test, dist)
        temp[temp$map == map_name, ][[new_var]] <- perc
      }
    }
  }
  return(temp)
}

#efficiency calc for dm
calc_effic <- function(season, perc_damageperdt, perc_damageperheal) {
  if(season > 21) {
    eff <- (perc_damageperdt + perc_damageperheal)/2
  } else {
    eff <- (perc_damageperdt)
  }
  return(eff)
}

calc_effic_vec = Vectorize(calc_effic, vectorize.args=c("season", "perc_damageperdt", "perc_damageperheal"))

set_blank_med_exp <- function(class) {
  temp <- class
  med_vars <- c("kad", "chargespm","healing","cpm","dropspm","deathspm","dtm","ubersperheal","deathsperdt","dropsperuber")
  for (var in med_vars) {
    new_var <- paste("exp_",var,sep="")
    exp_var <- paste("perf_",var,sep="")
    perc_var <- paste("perc_",var,sep="")
    temp[[new_var]] <- 0
    temp[[exp_var]] <- 0
    temp[[perc_var]] <- 0
  }
  return(temp)
}

percentile_med_stats <- function(class, class_name) {
  temp <- class
  tf2maps <- c("granary", "gullywash", "prolands", "logjam", "reckoner", "sunshine", "process", "metalworks", "kalinka", "snakewater", "product","bagel", "villa")
  med_vars <- c("kad", "chargespm","healing","cpm","dropspm","deathspm","dtm","ubersperheal","deathsperdt","dropsperuber")
  for (map_name in tf2maps) {
    if(map_name %in% c("kalinka", "reckoner","metalworks", "villa")) {
      for(var in med_vars) {
        new_var <- paste("perc_",var,sep="")
        setwd()
        home_path <- getwd()
        dist_path <- paste(home_path,"/data/distributions/",class_name,"/",class_name,"-","cp","-",var,"-gamma.rds",sep="")
        dist <- readRDS(dist_path)
        perc <- gamma_percentile_vec(temp[temp$map == map_name, ][[var]], dist)
        temp[temp$map == map_name, ][[new_var]] <- perc
      }
    } else if(map_name %in% c("bagel")) {
      for(var in med_vars) {
        new_var <- paste("perc_",var,sep="")
        setwd()
        home_path <- getwd()
        dist_path <- paste(home_path,"/data/distributions/",class_name,"/",class_name,"-","koth","-",var,"-gamma.rds",sep="")
        dist <- readRDS(dist_path)
        perc <- gamma_percentile_vec(temp[temp$map == map_name, ][[var]], dist)
        temp[temp$map == map_name, ][[new_var]] <- perc
      }
    } else {
      for(var in med_vars) {
        new_var <- paste("perc_",var,sep="")
        setwd()
        home_path <- getwd()
        dist_path <- paste(home_path,"/data/distributions/",class_name,"/",class_name,"-",map_name,"-",var,"-gamma.rds",sep="")
        dist <- readRDS(dist_path)
        perc <- gamma_percentile_vec(temp[temp$map == map_name, ][[var]], dist)
        temp[temp$map == map_name, ][[new_var]] <- perc
      }
    }
  }
  return(temp)
}
