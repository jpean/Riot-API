library(purrr)
library(jsonlite)
library(dplyr)

# Constants
main_url <- ".api.riotgames.com/lol"

summoner_url<-"summoner/v4/summoners" 

champion_url<-"platform/v3/champion-rotations"

status_url <- "status/v3/shard-data"

match_url<-"match/v4"

mastery_url <- "champion-mastery/v4/champion-masteries/by-summoner"

score_url <- "champion-mastery/v4/scores/by-summoner"

league_url<- "league/v4"

spectator_url <- "spectator/v4"

regions <-c("br1", "eun1", "euw1", "kr", "la1", "la2", "na1", "oce1", "ru", "tr1")

request_type<-c("summoner"=summoner_url,"champion"=champion_url,"match"=match_url,
                "matchlist"=matchlist_url,"mastery"=mastery_url, "status"=status_url,
                "score"=score_url,"league"=league_url,"spectator"=spectator_url)

# Base functions

create_url <- function(region, api_type, api_value){
  base_type <- strsplit(api_type, "/")[[1]][1]
  if (is.null(api_value)){
    paste("https:/", paste0(region, main_url), api_type, sep = "/")
  } else {
    paste("https:/", paste0(region, main_url), api_type, api_value, sep = "/")
  }
}

add_api_key <- function(str, api_key){
  paste0(str, "?api_key=", api_key)
}

api_fetch <- function(api_type, api_value = NULL,...,
                      region = Sys.getenv("LOLAPI_REGION"), 
                      api_key = Sys.getenv("LOLAPI_KEY"), 
                      api_function = create_url) {
  res <- api_function(region, api_type, api_value) %>% 
    add_api_key(api_key)
  safeJSON <- purrr::safely(function(x) jsonlite::fromJSON(x, simplifyVector =  FALSE))
  while(is.null(out <- safeJSON(res)$result)) out <- safeJSON(res)$result
  out
}

