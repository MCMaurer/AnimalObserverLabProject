readLayoutJson <- function(dat)
{
	
	pincodes <- data.frame(users=unlist(dat$pin_codes), pin_codes=names(dat$pin_codes))
	
	if(is.null(dat$sftp)){
	sftp_hostname="enter host IP (optional)"
	sftp_username="enter user name (optional)"
	sftp_password="enter password (optional)"
	} else {
	sftp_hostname=dat$sftp$hostname
	sftp_username=dat$sftp$username
	sftp_password=dat$sftp$password
	}
	
	return(list( pincodes, list(
	version=dat$version,
	default_focal_duration=dat$time_variables$default_focal_duration,
	default_scan_interval=dat$time_variables$default_scan_interval,
	focal_starts_with_scan=dat$time_variables$focal_starts_with_scan,
	scan_20sec_notification=dat$time_variables$scan_20sec_notification,
	multiple_group_selection=dat$multiple_group_selection,
	continuous_focal_variable_collection=dat$continuous_focal_variable_collection,
	previous_scan_settings_saved=dat$save_previous_scan_settings,
	scene_width_meters=dat$scene_width_meters,
	scene_initial_zoom_scale=dat$scene_initial_zoom_scale,
	map_scene_width_meters=dat$map_scene_width_meters,
	map_scene_initial_zoom_scale=dat$map_scene_initial_zoom_scale,
	physical_contact_threshold_meters=dat$physical_contact_threshold,
	scan_alert=dat$scan_alert,
	default_scan_compass_setting=dat$scan_compass,
	default_map_mode_setting=dat$map_mode,
	background_tapped=dat$background_tap$tapped,
	background_untapped=dat$background_tap$untapped,
	sftp_hostname= sftp_hostname,
	sftp_username= sftp_username,
	sftp_password= sftp_password
	)))
}

createLayoutJSON <- function(temp)
{

short <- function(x){
as.character(unlist(x))
}
res <- list()
res$version <- short(temp[[6]]$version)
res$day_variables <- temp[[2]][[1]][[1]]
res$focal_variables <- temp[[3]][[1]][[1]]
res$scan_variables <- temp[[4]][[1]][[1]]
if(!is.null(temp[[5]])){
res$continuous_focal_variables <- temp[[5]][[1]][[1]]
}
	
res$pin_codes <- list()
for (i in 1:nrow(temp[[1]])) res$pin_codes[[i]] <- as.character(temp[[1]][i,1])
names(res$pin_codes) <- temp[[1]][,2]

res$background_tap <- list()
res$background_tap$tapped <- short(temp[[6]]$background_tapped)
res$background_tap$untapped <- short(temp[[6]]$background_untapped)
res$time_variables <- list()
res$time_variables$default_focal_duration <- as.numeric(short(temp[[6]]$default_focal_duration))
res$time_variables$default_scan_interval <- as.numeric(short(temp[[6]]$default_scan_interval))
res$time_variables$focal_starts_with_scan <- as.logical(short(temp[[6]]$focal_starts_with_scan))
res$time_variables$scan_20sec_notification <- as.logical(short(temp[[6]]$scan_20sec_notification))
res$multiple_group_selection <- as.logical(short(temp[[6]]$multiple_group_selection))
res$continuous_focal_variable_collection <- as.logical(short(temp[[6]]$continuous_focal_variable_collection))

if(is.null(temp[[5]])) {
	res$continuous_focal_variable_collection <- FALSE
}

res$save_previous_scan_settings <- as.logical(short(temp[[6]]$previous_scan_settings_saved))
res$scene_width_meters <- as.numeric(short(temp[[6]]$scene_width_meters))
res$scene_initial_zoom_scale <- as.numeric(short(temp[[6]]$scene_initial_zoom_scale))
res$map_scene_width_meters <- as.numeric(short(temp[[6]]$map_scene_width_meters))
res$map_scene_initial_zoom_scale <- as.numeric(short(temp[[6]]$map_scene_initial_zoom_scale))
res$physical_contact_threshold <- as.numeric(short(temp[[6]]$physical_contact_threshold_meters))
res$scan_alert <- as.logical(short(temp[[6]]$scan_alert))
res$scan_compass <- as.logical(short(temp[[6]]$default_scan_compass_setting))
res$map_mode <- as.logical(short(temp[[6]]$default_map_mode_setting))
if(as.character(short(temp[[6]]$sftp_hostname))!="enter host IP (optional)"){
res$sftp <- list()
res$sftp$hostname <- as.character(short(temp[[6]]$sftp_hostname))
res$sftp$username <- as.character(short(temp[[6]]$sftp_username))
res$sftp$password <- as.character(short(temp[[6]]$sftp_password))
}
#cat(file=stderr(), paste(res, "\n\n\n\n"))
#cat(file=stderr(), toJSON(res))
return(toJSON(res))
}


