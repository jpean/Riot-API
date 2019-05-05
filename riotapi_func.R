api_setup <- function(api_key, region){
  if (!region %in% regions) stop("invalid region")
  Sys.setenv(LOLAPI_REGION = region)
  Sys.setenv(LOLAPI_KEY = api_key)
}

get_summoner<- function(summoner,by_name=FALSE,by_account=FALSE,by_PUUID=FALSE){
  api_type <-request_type[["summoner"]]
  api_value<-if(by_name){
    paste0("by-name/", summoner)
  } else if(by_account){
    paste0("by-account/", summoner)
  } else if(by_PUUID){
    paste0("by-puuid/", summoner)
  } else {summoner} #this would be by summoner ID
  return(api_fetch(api_type,api_value))
}

get_champion<- function(){
  api_type <-request_type[["champion"]]
  return(api_fetch(api_type))
}

get_champion_mastery <- function(player_id, champion_id = NULL,score=FALSE){
  api_type <-request_type[["mastery"]]
  if(score){
    api_type<-request_type[["score"]]
    return(api_fetch(api_type))
  }
  api_value <-if(!is.null(champion_id)){
    paste0(player_id,"/by-champion/", champion_id)
  } else player_id
  return(api_fetch(api_type, api_value))
}

get_matchlist <- function(accountId){
  api_type <-request_type[["matchlist"]]
  api_value <-accountId
  return(api_fetch(api_type, api_value))
}

get_match <- function(matchId=NA,accountId=NA,tournament=NA,matchlist=FALSE,timeline=FALSE){
  api_type <-request_type[["match"]]
  if(matchlist){
    api_value <-paste0("matchlists/by-account/", accountId)
    return(api_fetch(api_type, api_value))
  } else if (timeline){
    api_value <-paste0("timelines/by-match/", matchId)
    return(api_fetch(api_type, api_value))
  } else if (!is.na(tournament)){
    api_value <- paste("by-tournament-code",tournament,"ids",sep="/")
    if(is.na(matchId)){
      api_value <- paste(matchId,"by-tournament-code",tournament,"ids",sep="/")
    }
    return(api_fetch(api_type, api_value))
  } else {
    api_value <-matchId
    return(api_fetch(api_type, api_value))
  }
}

get_league <- function(tier=NA,queue=NA,division=NA,summoner_id=NA,league_id=NA){
  api_type <-request_type[["league"]]
  api_value<-if(!is.na(division)){
    paste("entries",queue,tier,division,sep="/")
  }else if(!is.na(tier)){
    paste0(tier,"leagues/by-queue/",queue)
  }else if(!is.na(summoner_id)){
    paste0("entries/by-summoner/",summoner_id)
  }else {
    paste0("leagues/",league_id)
  }
  return(api_fetch(api_type,api_value))
}

get_status <-function(){
  api_type <- request_type[["status"]]
  return(api_fetch(api_type))
}

get_spectator <- function(summoner_id){
  api_type<-request_type[["spectator"]]
  api_value<-if(is.na(summoner_id)){
    paste("featured-games")
  } else {
    paste0("active-game/by-summoner/",summoner_id)
  }
  return(api_fetch(api_type,api_value))
}








