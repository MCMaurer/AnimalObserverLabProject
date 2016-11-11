small_tables <- function(behaviors.json, layout_info.json)
{
####path to behaviors.json
behav <- fromJSON(paste(behaviors.json, collapse=""))
####path to layout_info.json
layout <- fromJSON(paste(layout_info.json, collapse=""))
####path to sample output data file

getListHeaders <- function(jsonList){
if(length(jsonList)==0) return(NULL)
temp <- names(unlist(jsonList))
temp2 <- unlist(strsplit(temp, split="[.]name"))
return(unique(unlist(strsplit(temp2, split="[.]"))))
}

getLevels <- function(clade){
if(length(clade)==0) return(NULL)
ans <- list()
headers <- getListHeaders(clade)
temp <- unlist(clade)
for(i in 1:length(headers)){
	ans[[i]] <- unique(as.character(temp[grep(gsub("X_","_",make.names(paste0(headers[i], ".name"))), gsub("X_","_",make.names(names(temp))))]))
}
names(ans) <- headers
return(ans)
}

ans <- list()
##
ans$dyadic <- getLevels(behav$dyadic)
ans$solo <- getLevels(behav$solo)
ans$scan <- getLevels(behav$scan)
##
dayVars <- list()
dayVars$dayVars <- layout$day_variables
ans$sessionVars <- getLevels(dayVars)
##
focalVars <- list()
focalVars$focalVars <- layout$focal_variables
ans$focalVars <- getLevels(focalVars)
##
scanVars <- list()
scanVars$scanVars <- layout$scan_variables
ans$scanVars <- getLevels(scanVars)
##
continuousVars <- list()
continuousVars$continuousVars <- layout$continuous_focal_variables
ans$continuousVars <- getLevels(continuousVars)
return(ans)
}





