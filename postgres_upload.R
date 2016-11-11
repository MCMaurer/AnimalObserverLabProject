timeFormat <- function(stamp){
	paste(unlist(strsplit(stamp, ",")), collapse=" ")
}

naFormat <- function(v){
	ifelse(is.na(v),'NULL',v)
}

uploadSessionsTable <- function(sessionsTable, con){
	for (i in 1:nrow(sessionsTable)){
	command <- paste("INSERT INTO main_tables.list_sessions(device_ID, session_start_time, session_end_time, group_ID, pin_code_name, layout_info_json_version, behaviors_json_version, gps_on, compass_on, map_mode_on, physical_contact_threshold)
    SELECT 
    '",as.character(sessionsTable[i,]$device_ID),"',
    '",timeFormat(as.character(sessionsTable[i,]$session_start_timeStamp)),"',
    '",timeFormat(as.character(sessionsTable[i,]$session_end_timeStamp)),"',
    '", as.character(sessionsTable[i,]$group_ID),"',
    '", as.character(sessionsTable[i,]$pin_code_name),"',
    '", as.character(sessionsTable[i,]$layout_info_json_version),"',
    '", as.character(sessionsTable[i,]$behaviors_json_version),"',
    '", as.character(sessionsTable[i,]$gps_on),"',
    '", as.character(sessionsTable[i,]$compass_on),"',
    '", as.character(sessionsTable[i,]$map_mode_on),"',
    '", as.character(sessionsTable[i,]$physical_contact_threshold),"'
	WHERE NOT EXISTS (SELECT 1 from main_tables.list_sessions WHERE device_ID='",as.character(sessionsTable[i,]$device_ID),"' AND session_start_time='",timeFormat(as.character(sessionsTable[i,]$session_start_timeStamp)),"');", sep="")
	command <- gsub("''", "NULL", command)
	#command <- gsub("NA", "NULL", command)
	command <- gsub("'NULL'", "NULL", command)
	command <- gsub("'NA'", "NULL", command)
	dbGetQuery(con, command)
	}
}


uploadFocalsTable <- function(focalsTable, con){
	for (i in 1:nrow(focalsTable)){
	command <- paste("INSERT INTO main_tables.list_focals(device_ID, session_start_time, focal_start_time, focal_end_time, set_duration, set_scan_interval, focal_individual_ID)
    SELECT 
    '",as.character(focalsTable[i,]$device_ID),"',
    '",timeFormat(as.character(focalsTable[i,]$session_start_timeStamp)),"',
    '",timeFormat(as.character(focalsTable[i,]$focal_start_timeStamp)),"',
    '", as.character(focalsTable[i,]$focal_end_timeStamp),"',
    '", as.character(focalsTable[i,]$focal_set_duration),"',
    '", as.character(focalsTable[i,]$focal_set_scan_interval),"',
    '", as.character(focalsTable[i,]$focal_individual_ID),"'
	WHERE NOT EXISTS (SELECT 1 from main_tables.list_focals WHERE device_ID='",as.character(focalsTable[i,]$device_ID),"' AND focal_start_time='",timeFormat(as.character(focalsTable[i,]$focal_start_timeStamp)),"');", sep="")
	command <- gsub("''", "NULL", command)
	#command <- gsub("NA", "NULL", command)
	command <- gsub("'NULL'", "NULL", command)
	command <- gsub("'NA'", "NULL", command)	
	dbGetQuery(con, command)
	}
}


uploadBehaviorsTable <- function(behaviorsTable, con){
	tableHeaders <- fixHeader(names(behaviorsTable))
	for (i in 1:nrow(behaviorsTable)){
		command <- paste("INSERT INTO main_tables.list_behaviors(device_ID, focal_start_time, behavior_time, actor, subject, ",
	paste(gsub("[.]", "_", tableHeaders[7:(length(tableHeaders)-5)]), collapse=", "),
	", comment, latitude, longitude, gps_horizontal_precision, altitude)
    SELECT 
    '",as.character(behaviorsTable[i,]$device_ID),"',
    '",timeFormat(as.character(behaviorsTable[i,]$focal_start_timeStamp)),"',
    '", timeFormat(as.character(behaviorsTable[i,]$behavior_timeStamp)),"',
    '", as.character(behaviorsTable[i,]$actor),"',
    '", as.character(behaviorsTable[i,]$subject),"',
    ", paste0("'",paste(as.character(unlist(as.list(behaviorsTable[i,7:(length(tableHeaders)-5)]))), collapse="', '"),"'"),",
    '", as.character(behaviorsTable[i,]$comment),"',
    ", naFormat(as.character(behaviorsTable[i,]$latitude)),",
    ", naFormat(as.character(behaviorsTable[i,]$longitude)),",
    ", naFormat(as.character(behaviorsTable[i,]$gps_horizontal_precision)),",
    ", naFormat(as.character(behaviorsTable[i,]$altitude)),"
	WHERE NOT EXISTS (SELECT 1 from main_tables.list_behaviors WHERE device_ID='",as.character(behaviorsTable[i,]$device_ID),"' AND behavior_time='",timeFormat(as.character(behaviorsTable[i,]$behavior_timeStamp)),"' AND actor='",as.character(behaviorsTable[i,]$actor),"' AND	subject='",as.character(behaviorsTable[i,]$subject),"');", sep="")
		command <- gsub("''", "NULL", command)
		#command <- gsub("NA", "NULL", command)
		command <- gsub("'NULL'", "NULL", command)
		command <- gsub("'NA'", "NULL", command)
		dbGetQuery(con, command)
	}
}

uploadScansTable <- function(scansTable, con){
	tableHeaders <- fixHeader(names(scansTable))
	for (i in 1:nrow(scansTable)){
		command <- paste("INSERT INTO main_tables.list_scans(device_ID, focal_start_time, scan_time, latitude, longitude, gps_horizontal_precision, altitude)
    SELECT 
    '",as.character(scansTable[i,]$device_ID),"',
    '",timeFormat(as.character(scansTable[i,]$focal_start_timeStamp)),"',
    '", timeFormat(as.character(scansTable[i,]$scan_timeStamp)),"',
    ", naFormat(as.character(scansTable[i,]$latitude)),",
    ", naFormat(as.character(scansTable[i,]$longitude)),",
    ", naFormat(as.character(scansTable[i,]$gps_horizontal_precision)),",
    ", naFormat(as.character(scansTable[i,]$altitude)),"
	WHERE NOT EXISTS (SELECT 1 from main_tables.list_scans WHERE device_ID='",as.character(scansTable[i,]$device_ID),"' AND scan_time ='",timeFormat(as.character(scansTable[i,]$scan_timeStamp)),"');", sep="")
		command <- gsub("''", "NULL", command)
		#command <- gsub("NA", "NULL", command)
		command <- gsub("'NULL'", "NULL", command)
		command <- gsub("'NA'", "NULL", command)
		dbGetQuery(con, command)
	}
}

uploadScanData <- function(scansTable, con){
	tableHeaders <- fixHeader(names(scansTable))
	for (i in 1:nrow(scansTable)){
		command <- paste("INSERT INTO main_tables.scan_data(device_ID, scan_time, scanned_individual_ID, ",
	paste(gsub("[.]", "_", tableHeaders[6:(length(tableHeaders)-8)]), collapse=", "),
	", x_position, y_position)
    SELECT 
    '",as.character(scansTable[i,]$device_ID),"',
    '", timeFormat(as.character(scansTable[i,]$scan_timeStamp)),"',
    '", as.character(scansTable[i,]$scanned_individual_ID),"',
    '", paste(as.character(unlist(as.list(scansTable[i,6:(length(tableHeaders)-8)]))), collapse="', '"),"',
    ", as.character(scansTable[i,]$x_position),",
    ", as.character(scansTable[i,]$y_position),"
	WHERE NOT EXISTS (SELECT 1 from main_tables.scan_data WHERE device_ID='",as.character(scansTable[i,]$device_ID),"' AND scan_time='",timeFormat(as.character(scansTable[i,]$scan_timeStamp)),"' AND scanned_individual_ID ='",as.character(scansTable[i,]$scanned_individual_ID),"');", sep="")
		command <- gsub("''", "NULL", command)
		#command <- gsub("NA", "NULL", command)
		command <- gsub("'NULL'", "NULL", command)
		command <- gsub("'NA'", "NULL", command)
		dbGetQuery(con, command)
	}
}


uploadScanVariables <- function(scanVarsTable, con){
	tableHeaders <- fixHeader(names(scanVarsTable))
	for (i in 1:nrow(scanVarsTable)){
		command <- paste("INSERT INTO main_tables.scan_variables(device_ID, scan_time, ",
	paste(gsub("[.]", "_", tableHeaders[5:(length(tableHeaders))]), collapse=", "),
	")
    SELECT 
    '",as.character(scanVarsTable[i,]$device_ID),"',
    '", timeFormat(as.character(scanVarsTable[i,]$scan_timeStamp)),"',
    '",paste(as.character(unlist(as.list(scanVarsTable[i,5:(length(tableHeaders))]))), collapse="', '"),"'
	WHERE NOT EXISTS (SELECT 1 from main_tables.scan_variables WHERE device_ID='",as.character(scanVarsTable[i,]$device_ID),"' AND scan_time='",timeFormat(as.character(scanVarsTable[i,]$scan_timeStamp)),"' AND scanVars ='", as.character(scanVarsTable[i,5]),"');", sep="")
		command <- gsub("''", "NULL", command)
		#command <- gsub("NA", "NULL", command)
		command <- gsub("'NULL'", "NULL", command)
		command <- gsub("'NA'", "NULL", command)
		dbGetQuery(con, command)
	}
}

uploadContinuousVariables <- function(continuousVarsTable, con){
	tableHeaders <- fixHeader(names(continuousVarsTable))
	for (i in 1:nrow(continuousVarsTable)){
		command <- paste("INSERT INTO main_tables.continuous_focal_variables(device_ID, focal_start_time, ",
	paste(gsub("[.]", "_", tableHeaders[4:(length(tableHeaders))]), collapse=", "),
	")
    SELECT 
    '",as.character(continuousVarsTable[i,]$device_ID),"',
    '", timeFormat(as.character(continuousVarsTable[i,]$focal_start_timeStamp)),"',
    '",paste(as.character(unlist(as.list(continuousVarsTable[i,4:(length(tableHeaders))]))), collapse="', '"),"'
	WHERE NOT EXISTS (SELECT 1 from main_tables.continuous_focal_variables WHERE device_ID='",as.character(continuousVarsTable[i,]$device_ID),"' AND focal_start_time ='",timeFormat(as.character(continuousVarsTable[i,]$focal_start_timeStamp)),"' AND continuousVars ='", as.character(continuousVarsTable[i,4]),"');", sep="")
		command <- gsub("''", "NULL", command)
		#command <- gsub("NA", "NULL", command)
		command <- gsub("'NULL'", "NULL", command)
		command <- gsub("'NA'", "NULL", command)
		dbGetQuery(con, command)
	}
}

uploadFocalVariables <- function(focalVarsTable, con){
	tableHeaders <- fixHeader(names(focalVarsTable))
	for (i in 1:nrow(focalVarsTable)){
		command <- paste("INSERT INTO main_tables.focal_variables(device_ID, focal_start_time, ",
	paste(gsub("[.]", "_", tableHeaders[4:(length(tableHeaders))]), collapse=", "),
	")
    SELECT 
    '",as.character(focalVarsTable[i,]$device_ID),"',
    '", timeFormat(as.character(focalVarsTable[i,]$focal_start_timeStamp)),"',
    '", paste(as.character(unlist(as.list(focalVarsTable[i,4:(length(tableHeaders))]))), collapse="', '"),"'
	WHERE NOT EXISTS (SELECT 1 from main_tables.focal_variables WHERE device_ID='",as.character(focalVarsTable[i,]$device_ID),"' AND focal_start_time ='",timeFormat(as.character(focalVarsTable[i,]$focal_start_timeStamp)),"' AND focalVars ='", as.character(focalVarsTable[i,4]),"');", sep="")
		command <- gsub("''", "NULL", command)
		#command <- gsub("NA", "NULL", command)
		command <- gsub("'NULL'", "NULL", command)
		command <- gsub("'NA'", "NULL", command)
		dbGetQuery(con, command)
	}
}

uploadSessionVariables <- function(sessionVarsTable, con){
	tableHeaders <- fixHeader(names(sessionVarsTable))
	for (i in 1:nrow(sessionVarsTable)){
		command <- paste("INSERT INTO main_tables.session_variables(device_ID, session_start_time, ",
	paste(gsub("[.]", "_", tableHeaders[3:(length(tableHeaders))]), collapse=", "),
	")
    SELECT 
    '",as.character(sessionVarsTable[i,]$device_ID),"',
    '", timeFormat(as.character(sessionVarsTable[i,]$session_start_timeStamp)),"',
    '", paste(as.character(unlist(as.list(sessionVarsTable[i,3:(length(tableHeaders))]))), collapse="', '"),"'
	WHERE NOT EXISTS (SELECT 1 from main_tables.session_variables WHERE device_ID='",as.character(sessionVarsTable[i,]$device_ID),"' AND session_start_time ='",timeFormat(as.character(sessionVarsTable[i,]$session_start_timeStamp)),"' AND dayVars ='", as.character(sessionVarsTable[i,3]),"');", sep="")
		command <- gsub("''", "NULL", command)
		#command <- gsub("NA", "NULL", command)
		command <- gsub("'NULL'", "NULL", command)
		command <- gsub("'NA'", "NULL", command)
		dbGetQuery(con, command)
	}
}

uploadBackgroundTapsTable <- function(backgroundTapsTable, con){
	tableHeaders <- fixHeader(names(backgroundTapsTable))
	for (i in 1:nrow(backgroundTapsTable)){
		command <- paste("INSERT INTO main_tables.list_background_taps(device_ID, focal_start_time, tap_time, description, latitude, longitude, gps_horizontal_precision, altitude)
    SELECT 
    '",as.character(backgroundTapsTable[i,]$device_ID),"',
    '", timeFormat(as.character(backgroundTapsTable[i,]$focal_start_timeStamp)),"',
    '", timeFormat(as.character(backgroundTapsTable[i,]$backgroundTap_timeStamp)),"',
    '", as.character(backgroundTapsTable[i,]$description),"',
	", naFormat(as.character(backgroundTapsTable[i,]$latitude)),",
    ", naFormat(as.character(backgroundTapsTable[i,]$longitude)),",
    ", naFormat(as.character(backgroundTapsTable[i,]$gps_horizontal_precision)),",
    ", naFormat(as.character(backgroundTapsTable[i,]$altitude)),"

	WHERE NOT EXISTS (SELECT 1 from main_tables.list_background_taps WHERE device_ID='",as.character(backgroundTapsTable[i,]$device_ID),"' AND focal_start_time ='",timeFormat(as.character(backgroundTapsTable[i,]$focal_start_timeStamp)),"' AND tap_time ='",timeFormat(as.character(backgroundTapsTable[i,]$backgroundTap_timeStamp)),"');", sep="")
		command <- gsub("''", "NULL", command)
		#command <- gsub("NA", "NULL", command)
		command <- gsub("'NULL'", "NULL", command)
		command <- gsub("'NA'", "NULL", command)
		dbGetQuery(con, command)
	}
}


uploadCommentTable <- function(commentsTable, con){
	tableHeaders <- fixHeader(names(commentsTable))
	for (i in 1:nrow(commentsTable)){
		temp <- gsub("'", "`", as.character(commentsTable[i,]$comment_text))
		command <- paste("INSERT INTO main_tables.list_comments(device_ID, focal_start_time, comment_time, comment)
    SELECT 
    '",as.character(commentsTable[i,]$device_ID),"',
    '", timeFormat(as.character(commentsTable[i,]$focal_start_timeStamp)),"',
    '", timeFormat(as.character(commentsTable[i,]$comment_timeStamp)),"',
    '", temp,"'
	WHERE NOT EXISTS (SELECT 1 from main_tables.list_comments WHERE device_ID='",as.character(commentsTable[i,]$device_ID),"' AND focal_start_time ='",timeFormat(as.character(commentsTable[i,]$focal_start_timeStamp)),"' AND comment_time ='",timeFormat(as.character(commentsTable[i,]$comment_timeStamp)),"');", sep="")
		command <- gsub("''", "NULL", command)
		#command <- gsub("NA", "NULL", command)
		command <- gsub("'NULL'", "NULL", command)
		command <- gsub("'NA'", "NULL", command)
		dbGetQuery(con, command)
	}
}



