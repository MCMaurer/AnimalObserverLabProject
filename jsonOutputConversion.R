jsonOutputConversion <- function(json.output.file, behaviors.json, layout_info.json, colmerge=F)
{
####path to behaviors.json
behav <- fromJSON(paste(behaviors.json, collapse=""))
####path to layout_info.json
layout <- fromJSON(paste(layout_info.json, collapse=""))
####path to sample output data file
if(is.null(json.output.file)) {dat <- NULL} else {dat <- fromJSON(paste(json.output.file, collapse=""))}

getListHeaders <- function(jsonList){
if(length(jsonList)==0) return(NULL)
temp <- names(unlist(jsonList))
temp2 <- unlist(strsplit(temp, split="[.]name"))
return(unique(unlist(strsplit(temp2, split="[.]"))))
}

#################get the list of behaviors and modifiers here:
behaviorHeaders <- getListHeaders(behav$dyadic)
#################same for scan:
scanHeaders <- getListHeaders(behav$scan)
#################same for self:
selfHeaders <- getListHeaders(behav$solo)
#################same for self:
continuousVars <- list()
continuousVars$continuousVars <- layout$continuous_focal_variables
continuousVarsHeaders <- getListHeaders(continuousVars)
#################same for scanvars:
scanVars <- list()
scanVars$scanVars <- layout$scan_variables
scanVarsHeaders <- getListHeaders(scanVars)
#################same for focalvars:
focalVars <- list()
focalVars$focalVars <- layout$focal_variables
focalVarsHeaders <- getListHeaders(focalVars)
#################same for dayvars:
dayVars <- list()
dayVars$dayVars <- layout$day_variables
dayVarsHeaders <- getListHeaders(dayVars)

NAcheck <- function(x){
	ifelse (is.null(x), NA,x)
}
#################list_sessions
sessionsTable <- matrix(nrow=0, ncol=11)
colnames(sessionsTable) <- c(
	"device_ID",
	"session_start_timeStamp", 
	"session_end_timeStamp",
	"group_ID",
	"pin_code_name",
	"layout_info_json_version",
	"behaviors_json_version",
	"gps_on",
	"compass_on",
	"map_mode_on",
	"physical_contact_threshold"
)

if(length(dat$data$sessions)>0){
for (i in 1:length(dat$data$sessions)){
	session <- dat$data$sessions[[i]]
	sessionsTable <- rbind(sessionsTable, as.character(c(
	NAcheck(session$device_ID),
	NAcheck(session$arrival_time),
	NAcheck(session$departure_time),
	NAcheck(session$group_id),
	NAcheck(session$pin_name),
	NAcheck(session$layout_info_JSON_file_ID),
	NAcheck(session$behaviors_JSON_file_ID),
	NAcheck(session$gps_on),
	NAcheck(session$compass_on),	
	NAcheck(session$map_mode_on),
	NAcheck(layout$physical_contact_threshold))))
	}
}

##########generic function helping parsing hierarchical data
varMatrix <- function(observations, headers){
	newTable <- matrix(nrow=0, ncol=length(headers))
	if(length(observations)>0){
	for(j in 1:length(observations)){
		var <- observations[j]
		varNameTemp <- names(var)
		modifiers <- unlist(var)
		newnames <- unlist(lapply(strsplit(names(modifiers), split="[.]"), function(v) v[-1]))
		headers <- unlist(lapply(strsplit(headers, split="[*]"), function(v) v[1]))
		newRow <- rep(NA,length(headers))
		newRow[match(newnames, headers)] <- modifiers
		newRow[1] <- varNameTemp
		newTable <- rbind(newTable, newRow)
	}
	}
	return(newTable)
}



#################list_dayVars
dayVarsTable <- matrix(nrow=0, ncol=2+length(dayVarsHeaders))
colnames(dayVarsTable) <- c(
	"device_ID",
	"session_start_timeStamp", 
	dayVarsHeaders
)
if(length(dat$data$sessions)>0){
for (i in 1:length(dat$data$sessions)){
	session <- dat$data$sessions[[i]]
	dayVarsDetailsTemp <- session$details#[grep("Observer name", names(session$details), invert=T)]
	newDayVarsTable <- varMatrix(dayVarsDetailsTemp, dayVarsHeaders)
	NAcheck(session$device_ID)
	NAcheck(session$arrival_time)
	dayVarsTable <- rbind(dayVarsTable, cbind(NAcheck(session$device_ID), NAcheck(session$arrival_time), newDayVarsTable))
	}
}
##################list_focalVars
focalVarsTable <- matrix(nrow=0, ncol=3+length(focalVarsHeaders))
colnames(focalVarsTable) <- c(
	"device_ID",
	"session_start_timeStamp",
	"focal_start_timeStamp",
	focalVarsHeaders
)

if(length(dat$data$sessions)>0){
for (i in 1:length(dat$data$sessions)){
	session <- dat$data$sessions[[i]]
	if(length(session$focals)>0){
	for (j in 1:length(session$focals)){
		focal <- session$focals[[j]]
		focalVarsDetailsTemp <- focal$details
		newFocalVarsTable <- varMatrix(focalVarsDetailsTemp, focalVarsHeaders)
	focalVarsTable <- rbind(focalVarsTable, cbind(NAcheck(session$device_ID), NAcheck(session$arrival_time), NAcheck(focal$start_time), newFocalVarsTable))
	}
	}
	}
}

##################list_continuousVars
continuousVarsTable <- matrix(nrow=0, ncol=3+length(continuousVarsHeaders))
colnames(continuousVarsTable) <- c(
	"device_ID",
	"session_start_timeStamp",
	"focal_start_timeStamp",
	continuousVarsHeaders
)

if(length(dat$data$sessions)>0){
for (i in 1:length(dat$data$sessions)){
	session <- dat$data$sessions[[i]]
	if(length(session$focals)>0){
	for (j in 1:length(session$focals)){
		focal <- session$focals[[j]]
		continuousVarsDetailsTemp <- focal$continuous_focal_vars
		if(is.null(focal$continuous_focal_vars)) {
			next
			}
		newContinuousVarsTable <- varMatrix(continuousVarsDetailsTemp, continuousVarsHeaders)
	continuousVarsTable <- rbind(continuousVarsTable, cbind(NAcheck(session$device_ID), NAcheck(session$arrival_time), NAcheck(focal$start_time), newContinuousVarsTable))
	}
	}
	}
}

##################list_scanVars
scanVarsTable <- matrix(nrow=0, ncol=4+length(scanVarsHeaders))
colnames(scanVarsTable) <- c(
	"device_ID",
	"session_start_timeStamp",
	"focal_start_timeStamp",
	"scan_timeStamp",
	scanVarsHeaders
)

if(length(dat$data$sessions)>0){
for (i in 1:length(dat$data$sessions)){
	session <- dat$data$sessions[[i]]
	if(length(session$focals)>0){
	for (j in 1:length(session$focals)){
		focal <- session$focals[[j]]
		if(length(focal$scans)>0){
		for (k in 1:length(focal$scans)){
		scan <- focal$scans[[k]]
		scanVarsDetailsTemp <- scan$details
		newscanVarsTable <- varMatrix(scanVarsDetailsTemp, scanVarsHeaders)
		scanVarsTable <- rbind(scanVarsTable, cbind(NAcheck(session$device_ID), NAcheck(session$arrival_time), NAcheck(focal$start_time), NAcheck(scan$timestamp), newscanVarsTable))
		}
		}
		}
	}
	}
}

#################list_focals
focalsTable <- matrix(nrow=0, ncol=7)
colnames(focalsTable) <- c(
	"device_ID",
	"session_start_timeStamp", 
	"focal_start_timeStamp",
	"focal_end_timeStamp", 
	"focal_set_duration", 
	"focal_set_scan_interval",
	"focal_individual_ID"
)

if(length(dat$data$sessions)>0){
for (i in 1:length(dat$data$sessions)){
	session <- dat$data$sessions[[i]]
	if(length(session$focal)>0){
	for (j in 1:length(session$focal)){
		focal <- session$focal[[j]]
		focalsTable <- rbind(focalsTable, as.character(c(
		NAcheck(session$device_ID),
		NAcheck(session$arrival_time),
		NAcheck(focal$start_time),
		NAcheck(focal$end_time),
		NAcheck(focal$duration),
		NAcheck(focal$scan_interval),
		NAcheck(focal$animal_id))))
		}
	}
}
}


#################
behaviorsTable <- matrix(nrow=0, ncol=11+length(behaviorHeaders)+length(selfHeaders))
colnames(behaviorsTable) <- c(
	"device_ID",
	"session_start_timeStamp", 
	"focal_start_timeStamp",
	"behavior_timeStamp", 
	"actor", 
	"subject",
	 behaviorHeaders, selfHeaders,
	"comment",
	"latitude", 
	"longitude",
	"gps_horizontal_precision",
	"altitude"
	)

if(length(dat$data$sessions)>0){
behaviorHeaders2 <- unlist(lapply(strsplit(behaviorHeaders, split="[*]"), function(v) v[1]))
selfHeaders2 <- unlist(lapply(strsplit(selfHeaders, split="[*]"), function(v) v[1]))
for (i in 1:length(dat$data$sessions)){
	session <- dat$data$sessions[[i]]
	if(length(session$focal)>0){
	for (j in 1:length(session$focal)){
		focal <- session$focal[[j]]
		if(length(focal$behaviors)>0){
			for (k in 1:length(focal$behaviors)){
				behavior <- focal$behaviors[[k]]
				behaviorDetailsTemp <- behavior$details
				behaviorDetailsTemp2 <- character(length(c(selfHeaders, behaviorHeaders)))
				if(behavior$actor==behavior$subject){
				behaviorDetailsTemp2[length(behaviorHeaders)+ match(names(unlist(behaviorDetailsTemp)), selfHeaders2)] <- unlist(behaviorDetailsTemp)
				} else {
				behaviorDetailsTemp2[match(names(unlist(behaviorDetailsTemp)), behaviorHeaders2)] <- unlist(behaviorDetailsTemp)
				}
				behaviorsTable <- rbind(behaviorsTable, as.character(c(
				NAcheck(session$device_ID),
				NAcheck(session$arrival_time),
				NAcheck(focal$start_time),	
				NAcheck(behavior$timestamp),
				NAcheck(behavior$actor),
				NAcheck(behavior$subject),
				behaviorDetailsTemp2,
				NAcheck(behavior$comment),
				NAcheck(behavior$lat),
				NAcheck(behavior$lon),
				NAcheck(behavior$gpsPrecision),
				NAcheck(behavior$alt)							
				)))
				}
			}
		}
	}
}
}

if(colmerge){
temp <- behaviorsTable[,7:(6+length(c(selfHeaders, behaviorHeaders)))]
identicals <- colnames(temp)[duplicated(colnames(temp))]
if(length(identicals)>0){
for(i in identicals)
{
	mergure <- apply(temp[,colnames(temp)==i], 1, function(v) paste(v[v!=""], sep=";"))
	if(class(mergure)=="list") mergure <- unlist(lapply(mergure, function(v) ifelse(length(v)==0, "",v)))
	temp[,colnames(temp)==i][,1] <- mergure
	temp <- temp[,-(which(colnames(temp)==i)[-1])]
}
behaviorsTable <- cbind(behaviorsTable[,1:6], temp, behaviorsTable[,(ncol(behaviorsTable)-4):ncol(behaviorsTable)])
}
}
##############################
scansTable <- matrix(nrow=0, ncol=13+length(scanHeaders))
colnames(scansTable) <- c(
	"device_ID",
	"session_start_timeStamp", 
	"focal_start_timeStamp",
	"scan_timeStamp",
	"scanned_individual_ID",
	scanHeaders,
	"x_position",
	"y_position",
	"physical_contact_threshold",
	"latitude", 
	"longitude",
	"gps_horizontal_precision",
	"altitude",
	"compass_bearing"
)

if(length(dat$data$sessions)>0){
scanHeaders2 <- unlist(lapply(strsplit(scanHeaders, split="[*]"), function(v) v[1]))
for (i in 1:length(dat$data$sessions)){
	session <- dat$data$sessions[[i]]
	if(length(session$focal)>0){
	for (j in 1:length(session$focal)){
		focal <- session$focal[[j]]
		if(length(focal$scans)>0){
			for (k in 1:length(focal$scans)){
				scan <- focal$scans[[k]]
				if(length(scan$observations)>0){
				for(m in 1:length(scan$observations))
					{
					observation <- scan$observations[[m]]
					observationDetailsTemp <- observation$details	
					observationDetailsTemp2 <- character(length(scanHeaders))
					observationDetailsTemp2[match(names(unlist(observationDetailsTemp)), scanHeaders2)] <- unlist(observationDetailsTemp)
					scansTable <- rbind(scansTable, as.character(c(
					NAcheck(session$device_ID),
					NAcheck(session$arrival_time),
					NAcheck(focal$start_time),
					NAcheck(scan$timestamp),
					NAcheck(observation$actor),
					observationDetailsTemp2,
					NAcheck(observation$x_delta),
					NAcheck(observation$y_delta),
					layout$physical_contact_threshold,
					NAcheck(scan$lat),
					NAcheck(scan$lon),
					NAcheck(scan$gpsPrecision),
					NAcheck(scan$alt),
					NAcheck(scan$compassBearing))))
					}
				}
				}
			}
		}
	}
}
}

##out_of_viewData #background tap time and date, background tap action, background tap latitude, background tap longitude,
backgroundTapsTable <- matrix(nrow=0, ncol=9)
colnames(backgroundTapsTable) <- c(
	"device_ID",
	"session_start_timeStamp", 
	"focal_start_timeStamp",
	"backgroundTap_timeStamp",
	"description",
	"latitude", 
	"longitude",
	"gps_horizontal_precision",
	"altitude"
	)

if(length(dat$data$sessions)>0){
for (i in 1:length(dat$data$sessions)){
	session <- dat$data$sessions[[i]]
	if(length(session$focal)>0){
		for (j in 1:length(session$focal)){
			focal <- session$focal[[j]]
			if (length(focal$background_taps)>0){
				for (k in 1:length(focal$background_taps)){
					backgroundTap <- focal$background_taps[[k]]
					backgroundTapsTable <- rbind(backgroundTapsTable, as.character(c(
					NAcheck(session$device_ID),
					NAcheck(session$arrival_time),
					NAcheck(focal$start_time),
					NAcheck(backgroundTap$timestamp),
					NAcheck(backgroundTap$text),
					NAcheck(backgroundTap$lat),
					NAcheck(backgroundTap$lon),
					NAcheck(backgroundTap$gpsPrecision),
					NAcheck(backgroundTap$alt)
					)))
				}
			}
		}
	}
}
}

#######commentsData
commentsTable <- matrix(nrow=0, ncol=9)
colnames(commentsTable) <- c(
	"device_ID",
	"session_start_timeStamp", 
	"focal_start_timeStamp",
	"comment_timeStamp",
	"comment_text",
	"latitude", 
	"longitude",
	"gps_horizontal_precision",
	"altitude"
	)
	
if(length(dat$data$sessions)>0){
for (i in 1:length(dat$data$sessions)){
	session <- dat$data$sessions[[i]]
	if(length(session$focal)>0){
		for (j in 1:length(session$focal)){
			focal <- session$focal[[j]]
			if (length(focal$text)>0){
				for (k in 1:length(focal$text)){
					text <- focal$text[[k]]
					commentsTable <- rbind(commentsTable, as.character(c(
					NAcheck(session$device_ID),
					NAcheck(session$arrival_time),
					NAcheck(focal$start_time),
					NAcheck(text$timestamp),
					NAcheck(text$text),
					NAcheck(text$lat),
					NAcheck(text$lon),
					NAcheck(text$gpsPrecision),
					NAcheck(text$alt)
					)))
				}
			}
		}
	}
}
}

sessionsTable <- data.frame(sessionsTable, row.names=NULL, check.names=F)
focalsTable <- data.frame(focalsTable, row.names=NULL, check.names=F)
behaviorsTable <- data.frame(behaviorsTable, row.names=NULL, check.names=F)
scansTable <- data.frame(scansTable, row.names=NULL, check.names=F)
backgroundTapsTable <- data.frame(backgroundTapsTable, row.names=NULL, check.names=F)
commentsTable <- data.frame(commentsTable, row.names=NULL, check.names=F)
dayVarsTable <- data.frame(dayVarsTable, row.names=NULL, check.names=F)
focalVarsTable <- data.frame(focalVarsTable, row.names=NULL, check.names=F)
continuousVarsTable <- data.frame(continuousVarsTable, row.names=NULL, check.names=F)
scanVarsTable <- data.frame(scanVarsTable, row.names=NULL, check.names=F)

return(list(sessionsTable= sessionsTable,
focalsTable= focalsTable,
behaviorsTable= behaviorsTable,
scansTable= scansTable,
backgroundTapsTable = backgroundTapsTable,
commentsTable = commentsTable,
dayVarsTable = dayVarsTable,
focalVarsTable = focalVarsTable,
continuousVarsTable = continuousVarsTable,
scanVarsTable = scanVarsTable))
}






