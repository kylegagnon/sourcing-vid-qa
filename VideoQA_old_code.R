



#########################################################
### R - E - S - U - L - T - S - - - T - A - B - L - E ###
#########################################################
# Read first 8000 rows from results table
###results = dbGetQuery(productiondb, "select * from results limit 10000")
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
#dur_sqrt <- sqrt(duration)
#dur_sqrt_std <- as.data.frame((dur_sqrt - mean(dur_sqrt, na.rm=TRUE)) / sd(dur_sqrt, na.rm=TRUE))
#dur_sqrt_std[is.na(dur_sqrt_std)] <- min(dur_sqrt_std, na.rm=TRUE)

# Convert submission time into date object in R
submission_time <- strptime(results$created_at, "%Y-%m-%d %H:%M:%S")

# Find day of week for submission and convert to numeric with Monday = 0 through Sunday = 6, then z-score
submission_DOW <- weekdays(submission_time)
submission_DOW_char <- submission_DOW
submission_DOW[submission_DOW == "Monday"] <- 0
submission_DOW[submission_DOW == "Tuesday"] <- 1
submission_DOW[submission_DOW == "Wednesday"] <- 2
submission_DOW[submission_DOW == "Thursday"] <- 3
submission_DOW[submission_DOW == "Friday"] <- 4
submission_DOW[submission_DOW == "Saturday"] <- 5
submission_DOW[submission_DOW == "Sunday"] <- 6
submission_DOW <- as.numeric(submission_DOW)
#sub_DOW_std <- (submission_DOW - mean(submission_DOW)) / sd(submission_DOW)

# Find time of day for submission, convert into seconds, then z-score
require(lubridate)
submission_TOD <- strftime(submission_time, format="%H:%M:%S")
submission_TOD <- lubridate::period_to_seconds(hms(submission_TOD))
#sub_TOD_std <- as.data.frame((submission_TOD - mean(submission_TOD)) / sd(submission_TOD))
### This block of code needs de-bugging, but should convert time stamp into hour of the week for graphing purposes.
# hour_of_week <- submission_TOD
# hour_of_week[which(submissions$submission_DOW == "Monday")] <- submissions$TimeOfDay[which(submissions$submission_DOW == "Monday")]
# hour_of_week[which(submissions$submission_DOW == "Tuesday")] <- submissions$TimeOfDay[which(submissions$submission_DOW == "Tuesday")] + 86400
# hour_of_week[which(submissions$submission_DOW == "Wednesday")] <- submissions$TimeOfDay[which(submissions$submission_DOW == "Wednesday")] + (86400 * 2)
# hour_of_week[which(submissions$submission_DOW == "Thursday")] <- submissions$TimeOfDay[which(submissions$submission_DOW == "Thursday")] + (86400 * 3)
# hour_of_week[which(submissions$submission_DOW == "Friday")] <- submissions$TimeOfDay[which(submissions$submission_DOW == "Friday")] + (86400 * 4)
# hour_of_week[which(submissions$submission_DOW == "Saturday")] <- submissions$TimeOfDay[which(submissions$submission_DOW == "Saturday")] + (86400 * 5)
# hour_of_week[which(submissions$submission_DOW == "Sunday")] <- submissions$TimeOfDay[which(submissions$submission_DOW == "Sunday")] + (86400 * 6)
# hour_of_week <- (hour_of_week/120)
# for(time in 1:nrow(submissions)){
# 	if(submissions$submission_DOW == "Monday"){
# 		} else if(submissions$submission_DOW == "Tuesday"){
# 		} else if(submissions$submission_DOW == "Wednesday"){
# 		} else if(submissions$submission_DOW == "Thursday"){
# 		} else if(submissions$submission_DOW == "Friday"){
# 		} else if(submissions$submission_DOW == "Saturday"){
# 		} else if(submissions$submission_DOW == "Sunday"){
# 	}
# }

# Convert submission time into date object in R
paid_date <- strptime(results$paid_at, "%Y-%m-%d %H:%M:%S")
new_results <- cbind(as.data.frame(as.factor(results$with_pay)), results$has_profile_image, os_data, duration, submission_DOW, submission_TOD, submission_time, paid_date)
new_results_ids <- as.data.frame(cbind(results$id, results$participant_id, results$study_id, results$state))
colnames(new_results) <- c("pay","profile_ima", "mac", "win", "ios", "android", "duration_secs", "week_time", "day_time", "submit_date", "paid_on")
colnames(new_results_ids) <- c("id", "part_id", "study_id", "state")
new_results <- cbind(new_results_ids, new_results)
new_results$part_id <- as.numeric(as.character(new_results$part_id))
new_results$study_id <- as.numeric(as.character(new_results$study_id))


#############################################################################
### P - A - R - T - I - C - I - P - A - N - T - S - - - T - A - B - L - E ###
#############################################################################
# Will need to cross-reference ids in results with ids in participants table.
# Grabs all rows from participants table that correspond to all participant ids in new_results (results table currently in R).
result_pIDs <- as.character(unique(new_results_ids$part_id))
participants <- dbGetQuery(productiondb, paste("SELECT * FROM participants WHERE participants.id in(",paste(result_pIDs,collapse=","),")",sep=""))

pID <- participants$id
birth_dates <- strptime(participants$birth_date, "%Y-%m-%d %H:%M:%S")
age <- data.frame("age" = round(as.numeric(Sys.Date() - as.Date(birth_dates))/365))
device_own <- data.frame(cbind(participants$has_android_phone,participants$has_android_tablet,participants$has_ios_phone,participants$has_ios_tablet,participants$has_osx,participants$has_windows))
colnames(device_own) <- c("own_and_phone", "own_and_tab", "own_ios_phone", "own_ios_tab", "own_osx", "own_win")
sign_in_count <- data.frame("Sign_Ins" = participants$sign_in_count)
cur_sign_in <- data.frame("Cur_Sign_In" = participants$current_sign_in_at)
last_sign_in <- data.frame("Last_Sign_In" = participants$last_sign_in_at)

# Parse legacy demographics data -- the demographics entered when participant signs up.
gender <- data.frame("gender" = tolower(str_extract(str_extract(participants$demographics_legacy, "gender: [:alpha:]*"), "[:alpha:]*$")))
parent <- data.frame("parent" = tolower(str_extract(str_extract(participants$demographics_legacy, "parent: [:alpha:]*"), "[:alpha:]*$")))
married <- data.frame("married" = tolower(str_extract(str_extract(participants$demographics_legacy, "married: [:alpha:]*"), "[:alpha:]*$")))
income <- data.frame("income" = as.numeric(gsub("'", "", str_extract(str_extract(participants$demographics_legacy, "income: '[0-9]'"), "'[0-9]'$"))))
comp_skill <- data.frame("comp_skill" = as.numeric(gsub("'", "", str_extract(str_extract(participants$demographics_legacy, "computer_skill: '[0-9]'"), "'[0-9]'$"))))
education <- data.frame("education" = as.numeric(gsub("'", "", str_extract(str_extract(participants$demographics_legacy, "education: '[0-9]'"), "'[0-9]'$"))))

new_participants <- cbind(pID, age, gender, parent, married, income, comp_skill, education, device_own, sign_in_count, cur_sign_in, last_sign_in)
new_participants[new_participants==""] <- NA

### This one takes forever. Should consider a faster approach.
new_results <- new_results[-which(new_results$part_id %in% new_participants$pID == FALSE), ]
LongData <- NULL
for(id in 42714:nrow(new_results)){
	temp_data <- cbind(new_results[id,], new_participants[new_participants$pID==new_results$part_id[id],])
	LongData <- rbind(LongData, temp_data)
}
### Need Speed / Need Speed / Need Speed
# Probably should just do as much of this as possible in mySQL and then read in table.


##################################################################################
### Separate sample studies (S.D. or S.M. in result title) from all other studies.
##################################################################################
studies <- dbGetQuery(productiondb, "select * from studies")
sample_study_id <- studies$id[grep("^S.(D|M).", studies$title)]
study_type <- rep('client', nrow(LongData))
study_type[LongData$study_id %in% sample_study_id] <- 'sample'
study_type <- as.factor(study_type)

# Add study type as factor to LongData
LongData <- cbind(LongData, study_type)

# Bring in number of participants needed for study from studies table.
sample_req <- matrix(nrow=nrow(LongData), ncol=1)
for(row in 1:nrow(studies)){
	cur_id <- studies$id[row]
	sample_req[LongData$study_id %in% cur_id] <- studies$participants_needed[row]
}
as.data.frame(sample_req)
colnames(sample_req) <- "sample_req"
LongData <- cbind(LongData, sample_req)

# Import study type from studies table (Desktop or Mobile) and append on LongData.
study_device_used <- matrix(nrow=nrow(LongData), ncol=1)
for(row in 1:nrow(studies)){
	cur_id <- studies$id[row]
	study_device_used[LongData$study_id %in% cur_id] <- studies$type[row]
}
as.data.frame(study_device_used)
colnames(study_device_used) <- "StudyDevice"
LongData <- cbind(LongData, study_device_used)


###################################################################################
### Import study invitation information from study_invitations table into LongData.
###################################################################################
study_invite_data <- dbGetQuery(productiondb, "select * from study_invitations")

LongData <- left_join(LongData, study_invite_data, by=c("part_id", "study_id"))



# Keep invite table and then add stuff to it. The LongData is oriented around results, nested within people and studies.
# This invite table will be oriented around invites. We'll add to it whether or not the person took the study, the state, when
# they took the study, if they got paid, and demographics for the person.









########*!*!*!*%$@$%^(&^(&^#*@$%^%$&#%^*^%&@%$&@#^*&^(&^*#^%*&*$%^*)))
########*!*!*!*%$@$%^(&^(&^#*@$%^%$&#%^*^%&@%$&@#^*&^(&^*#^%*&*$%^*)))
########*!*!*!*%$@$%^(&^(&^#*@$%^%$&#%^*^%&@%$&@#^*&^(&^*#^%*&*$%^*)))
########*!*!*!*%$@$%^(&^(&^#*@$%^%$&#%^*^%&@%$&@#^*&^(&^*#^%*&*$%^*)))
########*!*!*!*%$@$%^(&^(&^#*@$%^%$&#%^*^%&@%$&@#^*&^(&^*#^%*&*$%^*)))
########*!*!*!*%$@$%^(&^(&^#*@$%^%$&#%^*^%&@%$&@#^*&^(&^*#^%*&*$%^*)))
########*!*!*!*%$@$%^(&^(&^#*@$%^%$&#%^*^%&@%$&@#^*&^(&^*#^%*&*$%^*)))
########*!*!*!*%$@$%^(&^(&^#*@$%^%$&#%^*^%&@%$&@#^*&^(&^*#^%*&*$%^*)))
########*!*!*!*%$@$%^(&^(&^#*@$%^%$&#%^*^%&@%$&@#^*&^(&^*#^%*&*$%^*)))
########*!*!*!*%$@$%^(&^(&^#*@$%^%$&#%^*^%&@%$&@#^*&^(&^*#^%*&*$%^*)))

#### BETTER WAY BELOW #######

# Ok, short way to combine all the tables necessary.
colnames(participants)[1] <- "participant_id"
Data = left_join(study_invite_data, participants, by = "participant_id")
Data = left_join(Data, results, by = c("participant_id", "study_id"))
colnames(studies)[1] <- "study_id"
Data = left_join(Data, studies, by = "study_id")

iColNames <- colnames(study_invite_data)
pColNames <- colnames(participants)[-1]
pColNames <- paste0(pColNames, "_parts")
rColNames <- colnames(results)[-grep("(study|participant)_id", colnames(results))]
rColNames <- paste0(rColNames, "_results")
sColNames <- colnames(studies)[-grep("study_id", colnames(studies))]
sColNames <- paste0(sColNames, "_studies")
newColNames <- c(iColNames, pColNames, rColNames, sColNames)
colnames(Data) <- newColNames

sample_study_id <- Data$id[grep("^S.(D|M).", Data$title_studies)]
study_type <- rep('client', nrow(Data))
study_type[sample_study_id] <- 'sample'
study_type <- as.factor(study_type)
Data <- cbind(Data, study_type)

birth_dates <- strptime(Data$birth_date_parts, "%Y-%m-%d %H:%M:%S")
age <- data.frame("age" = round(as.numeric(Sys.Date() - as.Date(birth_dates))/365))

# Parse legacy demographics data -- the demographics entered when participant signs up.
gender <- data.frame("gender" = tolower(str_extract(str_extract(Data$demographics_legacy_parts, "gender: [:alpha:]*"), "[:alpha:]*$")))
parent <- data.frame("parent" = tolower(str_extract(str_extract(Data$demographics_legacy_parts, "parent: [:alpha:]*"), "[:alpha:]*$")))
married <- data.frame("married" = tolower(str_extract(str_extract(Data$demographics_legacy_parts, "married: [:alpha:]*"), "[:alpha:]*$")))
income <- data.frame("income" = as.numeric(gsub("'", "", str_extract(str_extract(Data$demographics_legacy_parts, "income: '[0-9]'"), "'[0-9]'$"))))
comp_skill <- data.frame("comp_skill" = as.numeric(gsub("'", "", str_extract(str_extract(Data$demographics_legacy_parts, "computer_skill: '[0-9]'"), "'[0-9]'$"))))
education <- data.frame("education" = as.numeric(gsub("'", "", str_extract(str_extract(Data$demographics_legacy_parts, "education: '[0-9]'"), "'[0-9]'$"))))

Data <- cbind(Data, age, gender, parent, married, income, comp_skill, education)




























# How many over-sourced participants?


# Create subsets of long_file based on study_id.
data <- long_file[long_file$state != "not_started", ]
study_subsets <- split(data, data$study_id)
comp_pay <- lapply(study_subsets, function(x, study_subsets$) length(which()))


study_stats <- NULL
study_ids <- unique(data$study_id)
for(id in study_ids[1:length(study_ids)]){
	sub_data <- data[data$study_id == id, ]
	comp_pay <- nrow(sub_data[sub_data$state == "complete" & sub_data$pay == "1",])
	rej_pay <- nrow(sub_data[sub_data$state == "rejected" & sub_data$pay == "1",])
	err_pay <- nrow(sub_data[sub_data$state == "errored" & sub_data$pay == "1",])
	total_results <- nrow(sub_data)
	temp <- c(id, comp_pay, rej_pay, err_pay, total_results)
	study_stats <- rbind(study_stats, temp)
}

study_stats <- as.data.frame(study_stats)
colnames(study_stats) <- c("study_id", "com_pay", "rej_pay", "err_pay", "total")
client_paid_data <- data[data$study_type == "client" & data$pay == "1",]
