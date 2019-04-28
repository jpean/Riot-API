api_setup <- function(api_key, region){
  if (!region %in% regions) stop("invalid region")
  Sys.setenv(LOLAPI_REGION = region)
  Sys.setenv(LOLAPI_KEY = api_key)
}

get_summoner_id<- function(player_name){
  api_type <-request_type[["summoner"]]
  api_value<-player_name
  api_fetch(api_type,api_value)
}

get_champion_mastery <- function(player_id, champion_id = NULL){
  api_type <-request_type[["mastery"]]
  api_value <-if(!is.null(champion_id)){
    paste0(player_id,"/by-champion/", champion_id)
  } else player_id
  api_fetch(api_type, api_value)
}

get_matchlist <- function(accountId){
  api_type <-request_type[["matchlist"]]
  api_value <-accountId
  api_fetch(api_type, api_value)
}

get_match <- function(matchId){
  api_type <-request_type[["match"]]
  api_value <-matchId
  api_fetch(api_type, api_value)
}
