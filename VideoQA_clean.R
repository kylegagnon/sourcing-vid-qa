require(RMySQL)

# RMySQL connection to production db
productiondb <- dbConnect(MySQL(), user='root', password='root', dbname='production', host='127.0.0.1')


#########################################################
### R - E - S - U - L - T - S - - - T - A - B - L - E ###
#########################################################
# Read first 8000 rows from results table
###results <- dbGetQuery(productiondb, "select * from results limit 10000")
results <- dbGetQuery(productiondb, "select * from results")

# Recode 'os' variable.
mac <- 0*nrow(results$os)
mac[results$os == "mac"] <- 1
win <- 0*nrow(results$os)
win[results$os == "win"] <- 1
ios <- 0*nrow(results$os)
ios[results$os == "ios"] <- 1
android <- 0*nrow(results$os)
android[results$os == "android"] <- 1
os_data <- as.data.frame(cbind(mac, win, ios, android))
os_data[is.na(os_data)] <- 0


# Duration (length of video) like most naturally occurring events is power-law distributed. 
#Taking square root of duration should create more spread between shorter length videos which 
# will be crucial to identifying poor videos.
duration <- results$duration
dur_sqrt <- sqrt(duration)
dur_sqrt_std <- as.data.frame((dur_sqrt - mean(dur_sqrt, na.rm=TRUE)) / sd(dur_sqrt, na.rm=TRUE))
dur_sqrt_std[is.na(dur_sqrt_std)] <- min(dur_sqrt_std, na.rm=TRUE)

# Convert submission time into date object in R
submission_time <- strptime(results$created_at, "%Y-%m-%d %H:%M:%S")

# Find day of week for submission and convert to numeric with Monday = 0 through Sunday = 6, then z-score
submission_DOW <- weekdays(submission_time)
submission_DOW[submission_DOW == "Monday"] <- 0
submission_DOW[submission_DOW == "Tuesday"] <- 1
submission_DOW[submission_DOW == "Wednesday"] <- 2
submission_DOW[submission_DOW == "Thursday"] <- 3
submission_DOW[submission_DOW == "Friday"] <- 4
submission_DOW[submission_DOW == "Saturday"] <- 5
submission_DOW[submission_DOW == "Sunday"] <- 6
submission_DOW <- as.numeric(submission_DOW)
sub_DOW_std <- (submission_DOW - mean(submission_DOW)) / sd(submission_DOW)

# Find time of day for submission, convert into seconds, then z-score
require(lubridate)
submission_TOD <- strftime(submission_time, format="%H:%M:%S")
submission_TOD <- lubridate::period_to_seconds(hms(submission_TOD))
sub_TOD_std <- as.data.frame((submission_TOD - mean(submission_TOD)) / sd(submission_TOD))

new_results <- cbind(as.data.frame(as.factor(results$with_pay)), results$has_profile_image, os_data, dur_sqrt_std, sub_DOW_std, sub_TOD_std)
new_results_ids <- as.data.frame(cbind(results$id, results$participant_id, results$study_id, results$state))
colnames(new_results) <- c("pay","profile_ima", "mac", "win", "ios", "android", "duration", "week_time", "day_time")
colnames(new_results_ids) <- c("id", "part_id", "study_id", "state")
new_results <- cbind(new_results_ids, new_results)

# Remove all objects in workspace except for connection to DB, new_results, and new_results_ids
#rm(list = setdiff(ls(), c("new_results", "new_results_ids", "productiondb")))

result_pIDs <- as.character(unique(new_results_ids$part_id))


#############################################################################
### P - A - R - T - I - C - I - P - A - N - T - S - - - T - A - B - L - E ###
#############################################################################
# Will need to cross-reference ids in results with ids in participants table.
# Grabs all rows from participants table that correspond to all participant ids in new_results (results table currently in R).
participants <- dbGetQuery(productiondb, paste("SELECT * FROM participants WHERE participants.id in(",paste(result_pIDs,collapse=","),")",sep=""))

birth_dates <- strptime(participants$birth_date, "%Y-%m-%d %H:%M:%S")
age <- round(as.numeric(Sys.Date() - as.Date(birth_dates))/365)
device_own <- as.data.frame(cbind(participants$has_android_phone,participants$has_android_tablet,participants$has_ios_phone,participants$has_ios_tablet,participants$has_osx,participants$has_windows))
colnames(device_own) <- c("own_and_phone", "own_and_tab", "own_ios_phone", "own_ios_tab", "own_osx", "own_win")
sign_in_count <- participants$sign_in_count

new_participants <- as.data.frame(cbind(participants$id, age, device_own, sign_in_count))
colnames(new_participants) <- c("pID", "Age", "own_and_phone", "own_and_tab", "own_ios_phone", "own_ios_tab", "own_osx", "own_win", "Sign_Ins")

temp_part_long <- as.data.frame(matrix(data=NA,nrow=nrow(new_results), ncol=ncol(new_participants)))

for(id in 1:nrow(new_participants)){
	temp_part_long[which(new_results$part_id == new_participants$pID[id]),] <- new_participants[new_participants$pID[id],]
}
colnames(temp_part_long) <- c("pID", "Age", "own_and_phone", "own_and_tab", "own_ios_phone", "own_ios_tab", "own_osx", "own_win", "Sign_Ins")

qa_data <- cbind(new_results, temp_part_long)

# number of no pays
num_no_pays <- length(which(as.numeric(qa_data$pay)==1))
no_pays <- qa_data[which(as.numeric(qa_data$pay)==1),]

# number of pays
num_pays <- length(which(as.numeric(qa_data$pay)==2))
pays <- qa_data[which(as.numeric(qa_data$pay)==2),]

# Create training data set that gives equal numbers of no pays to pays
train_proportion <- 0.5

if(num_no_pays < num_pays){
	train_no_pays <- no_pays[sample(1:nrow(no_pays), round(num_no_pays * train_proportion)), ]
	train_pays <- pays[sample(1:nrow(pays), round(num_no_pays * train_proportion)), ]
	} else {
	train_no_pays <- no_pays[sample(1:nrow(no_pays), round(num_pays * train_proportion)), ]
	train_pays <- pays[sample(1:nrow(pays), round(num_pays * train_proportion)), ]
	}

train_data <- rbind(train_no_pays, train_pays)
train_data <- train_data[,5:ncol(train_data)]

hour_of_week <- submissions_TOD
hour_of_week[which(submissions$submission_DOW == "Monday")] <- submissions$TimeOfDay[which(submissions$submission_DOW == "Monday")]
hour_of_week[which(submissions$submission_DOW == "Tuesday")] <- submissions$TimeOfDay[which(submissions$submission_DOW == "Tuesday")] + 86400
hour_of_week[which(submissions$submission_DOW == "Wednesday")] <- submissions$TimeOfDay[which(submissions$submission_DOW == "Wednesday")] + (86400 * 2)
hour_of_week[which(submissions$submission_DOW == "Thursday")] <- submissions$TimeOfDay[which(submissions$submission_DOW == "Thursday")] + (86400 * 3)
hour_of_week[which(submissions$submission_DOW == "Friday")] <- submissions$TimeOfDay[which(submissions$submission_DOW == "Friday")] + (86400 * 4)
hour_of_week[which(submissions$submission_DOW == "Saturday")] <- submissions$TimeOfDay[which(submissions$submission_DOW == "Saturday")] + (86400 * 5)
hour_of_week[which(submissions$submission_DOW == "Sunday")] <- submissions$TimeOfDay[which(submissions$submission_DOW == "Sunday")] + (86400 * 6)
hour_of_week <- (hour_of_week/120)
for(time in 1:nrow(submissions)){
	if(submissions$submission_DOW == "Monday"){

		} else if(submissions$submission_DOW == "Tuesday"){

		} else if(submissions$submission_DOW == "Wednesday"){

		} else if(submissions$submission_DOW == "Thursday"){

		} else if(submissions$submission_DOW == "Friday"){

		} else if(submissions$submission_DOW == "Saturday"){

		} else if(submissions$submission_DOW == "Sunday"){
}



#####################################################################
### H - I - G - H - L - I - G - H - T - S - - - T - A - B - L - E ###
#####################################################################
#highlights <- dbGetQuery(productiondb, paste("SELECT * FROM highlight_videos WHERE highlight_videos.id in(",paste(pIDs,collapse=","),")",sep=""))
