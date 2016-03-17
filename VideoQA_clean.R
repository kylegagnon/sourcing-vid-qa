require(RMySQL)
require(dplyr)
require(plyr)
require(lubridate)
<<<<<<< HEAD
require(stringr)
require(data.table)
=======
>>>>>>> 977e647f0b65b3fddc2d87152069d655e31213ea

# RMySQL connection to production db
productiondb = dbConnect(MySQL(), user='root', password='root', dbname='production', host='127.0.0.1', port=3307)
results = dbGetQuery(productiondb, "select * from results")
participants = dbGetQuery(productiondb, "select * from participants")
studies = dbGetQuery(productiondb, "select * from studies")
study_invite_data = dbGetQuery(productiondb, "select * from study_invitations")

# Join all tables into one data frame.
colnames(participants)[1] = "participant_id"
Data = left_join(study_invite_data, participants, by = "participant_id")
Data = left_join(Data, results, by = c("participant_id", "study_id"))
colnames(studies)[1] = "study_id"
Data = left_join(Data, studies, by = "study_id")
<<<<<<< HEAD
=======
rm(results, participants, studies, study_invite_data)
>>>>>>> 977e647f0b65b3fddc2d87152069d655e31213ea

# Remove key variables (overlapping) and rename Data columns.
iColNames = colnames(study_invite_data)
pColNames = colnames(participants)[-1]
pColNames = paste0(pColNames, "_parts")
rColNames = colnames(results)[-grep("(study|participant)_id", colnames(results))]
rColNames = paste0(rColNames, "_results")
sColNames = colnames(studies)[-grep("study_id", colnames(studies))]
sColNames = paste0(sColNames, "_studies")
newColNames = c(iColNames, pColNames, rColNames, sColNames)
colnames(Data) = newColNames
<<<<<<< HEAD
rm(iColNames, pColNames, rColNames, sColNames, newColNames, results, participants, studies, study_invite_data)

# Identify sample studies and create new factor column indicating client or sample study.
sample_study_id = grep("^\\bS.(D|M).\\b", Data$title_studies, perl=TRUE)
study_type = rep('client', nrow(Data))
study_type[sample_study_id] = 'sample sourcing'
trash_study_id = grep("\\btrash\\b", Data$title_studies, perl=TRUE)
study_type[trash_study_id] = 'trashed'
demo_study_id = grep("\\b(sample|sample study)\\b", Data$title_studies, ignore.case=TRUE, perl=TRUE)
study_type[demo_study_id] = 'demos'
study_type = as.factor(study_type)
Data = cbind(Data, study_type)
rm(sample_study_id, trash_study_id, demo_study_id, study_type)
=======
rm(iColNames, pColNames, rColNames, sColNames, newColNames)

# Identify sample studies and create new factor column indicating client or sample study.
sample_study_id = grep("^\\bS.(D|M).\\b", Data$title_studies)
study_type = rep('client', nrow(Data))
study_type[sample_study_id] = 'sample sourcing'
trash_study_id = grep("\\btrash\\b", Data$title_studies)
study_type[trash_study_id] = 'trashed'
demo_study_id = grep("\\b(sample|sample study)\\b", Data$title_studies, ignore.case=TRUE)
study_type[demo_study_id] = 'demos'
study_type = as.factor(study_type)
Data = cbind(Data, study_type)
rm(sample_Study_id, study_type)
>>>>>>> 977e647f0b65b3fddc2d87152069d655e31213ea

# Parse legacy demographics data -- the demographics entered when participant signs up.
birth_dates = strptime(Data$birth_date_parts, "%Y-%m-%d %H:%M:%S")
age = data.frame("age" = round(as.numeric(Sys.Date() - as.Date(birth_dates))/365))
gender = data.frame("gender" = tolower(str_extract(str_extract(Data$demographics_legacy_parts, "gender: [:alpha:]*"), "[:alpha:]*$")))
parent = data.frame("parent" = tolower(str_extract(str_extract(Data$demographics_legacy_parts, "parent: [:alpha:]*"), "[:alpha:]*$")))
married = data.frame("married" = tolower(str_extract(str_extract(Data$demographics_legacy_parts, "married: [:alpha:]*"), "[:alpha:]*$")))
income = data.frame("income" = as.numeric(gsub("'", "", str_extract(str_extract(Data$demographics_legacy_parts, "income: '[0-9]'"), "'[0-9]'$"))))
comp_skill = data.frame("comp_skill" = as.numeric(gsub("'", "", str_extract(str_extract(Data$demographics_legacy_parts, "computer_skill: '[0-9]'"), "'[0-9]'$"))))
education = data.frame("education" = as.numeric(gsub("'", "", str_extract(str_extract(Data$demographics_legacy_parts, "education: '[0-9]'"), "'[0-9]'$"))))
<<<<<<< HEAD

Data = cbind(Data, age, gender, parent, married, income, comp_skill, education)
rm(birth_dates, age, gender, parent, married, income, comp_skill, education)

gender = as.character(Data$gender)
gender[grep("^f$", gender)] = "female"
gender[grep("^(meat|queer|wat)$", gender)] = NA
gender[nchar(gender)==0] = NA
gender = as.factor(gender)
Data$gender = gender

parent = as.character(Data$parent)
parent[nchar(parent)==0] = NA
parent = as.factor(parent)
Data$parent = parent

married = as.character(Data$married)
married[nchar(married)==0] = NA
married = as.factor(married)
Data$married = married
=======
Data = cbind(Data, age, gender, parent, married, income, comp_skill, education)
rm(birth_dates, age, gender, parent, married, income, comp_skill, education)

### If already partially cleaned, load from binary file.
load("~/Desktop/SourcingData.RData")
keepCols = c(1:5, 7:9, 13:21, 25:36, 40:43, 45, 47, 49:54, 56, 60, 66, 71:72, 74:75, 78, 80:84, 86:87, 89, 98, 104, 106, 113:120)
Data = Data[,keepCols]
>>>>>>> 977e647f0b65b3fddc2d87152069d655e31213ea

######################################################################
# Parse email addresses to distinguish between byop tpsp and ingested.
# Use these binary columns to filter by sourcing sample type.
######################################################################
byop_email = grep("byop", Data$email_parts, ignore.case=TRUE, perl=TRUE)
byop_study = rep(0, nrow(Data))
byop_study[byop_email] = 1

tpsp_email = grep("tpsp", Data$email_parts, ignore.case=TRUE, perl=TRUE)
tpsp_study = rep(0, nrow(Data))
tpsp_study[tpsp_email] = 1

ingest_email = grep("ingested", Data$email_parts, ignore.case=TRUE, perl=TRUE)
ingest_study = rep(0, nrow(Data))
ingest_study[ingest_email] = 1

ye_email = setdiff(setdiff(setdiff(grep("[[:alnum:]]+\\@[youeye]+\\.com", Data$email_parts, ignore.case=TRUE, perl=TRUE), byop_email), tpsp_email), ingest_email)
internal_study = rep(0, nrow(Data))
internal_study[ye_email] = 1

no_pay_pp_email = grep("((no|do not) pay(?!(pal| pal))|youeye|^testing$)"
	,Data$paypal_email_parts, ignore.case=TRUE, perl=TRUE)
no_pay_email = rep(0, nrow(Data))
no_pay_email[no_pay_pp_email] = 1

Data = cbind(Data[,1:3], byop_study, tpsp_study, ingest_study, internal_study, no_pay_email, Data[,4:ncol(Data)])
<<<<<<< HEAD
rm(byop_email, byop_study, ingest_email, ingest_study, internal_study, no_pay_email, no_pay_pp_email, tpsp_email, tpsp_study, ye_email)

#################################
### Grab lat-long for participant
### and convert to timezone
#################################
=======

>>>>>>> 977e647f0b65b3fddc2d87152069d655e31213ea

#########################
### Result / Invite Level
#########################
<<<<<<< HEAD
# ! ! ! ! ! Use block of commented code only after fixing times by identifying time zone or IP address.
# Grab IP addresses for results
# and convert to lat-long and timezone
# #IP address in base64 UTF8, so we decode into raw and convert to character
# ip_add = paste(gsub("(\n|ip: )", "", str_extract(Data$request_results[1:115], "[:alnum:].+\n$")), "CR")
# decode_ips = ip_add[grep("[a-z]+", ip_add, ignore.case=TRUE)]
# require(base64enc)
# decoded_ips = rawToChar(base64decode(decode_ips))
# ip_add[grep("[a-z]+", ip_add, ignore.case=TRUE)] == decoded_ips

# # Calculate time of day invite sent.
# invite_time = strptime(Data$created_at, "%Y-%m-%d %H:%M:%S")
# Invite_TOD = ((lubridate::period_to_seconds(hms(strftime(invite_time, format="%H:%M:%S"))))/60)/60

# # Calculate day of week invite sent.
# Invite_DOW = weekdays(strptime(Data$created_at, "%Y-%m-%d %H:%M:%OS"))
# Invite_DOW[as.character(Invite_DOW) == "Monday"] = 0
# Invite_DOW[as.character(Invite_DOW) == "Tuesday"] = 1
# Invite_DOW[as.character(Invite_DOW) == "Wednesday"] = 2
# Invite_DOW[as.character(Invite_DOW) == "Thursday"] = 3
# Invite_DOW[as.character(Invite_DOW) == "Friday"] = 4
# Invite_DOW[as.character(Invite_DOW) == "Saturday"] = 5
# Invite_DOW[as.character(Invite_DOW) == "Sunday"] = 6

# # Calculate time of day result submitted.
# submission_time = strptime(Data$created_at_results, "%Y-%m-%d %H:%M:%S")
# Submit_TOD = ((lubridate::period_to_seconds(hms(strftime(submission_time, format="%H:%M:%S"))))/60)/60

# # Calculate day of week result submitted.
# Submit_DOW = weekdays(strptime(Data$created_at_results, "%Y-%m-%d %H:%M:%OS"))
# Submit_DOW[as.character(Submit_DOW) == "Monday"] = 0
# Submit_DOW[as.character(Submit_DOW) == "Tuesday"] = 1
# Submit_DOW[as.character(Submit_DOW) == "Wednesday"] = 2
# Submit_DOW[as.character(Submit_DOW) == "Thursday"] = 3
# Submit_DOW[as.character(Submit_DOW) == "Friday"] = 4
# Submit_DOW[as.character(Submit_DOW) == "Saturday"] = 5
# Submit_DOW[as.character(Submit_DOW) == "Sunday"] = 6

# Data = cbind(Data, Submit_DOW, Invite_DOW, Invite_TOD)
# rm(invite_time, Invite_TOD, Invite_DOW, submission_time, Submit_TOD, Submit_DOW)

# Calculate number of days since person joined the panel up to when the ith invite was sent. Initially in seconds, then convert to days.
InvTimes_days = as.numeric(difftime(Data$created_at, Data$created_at_parts, units="days"))
Data = cbind(Data, InvTimes_days)

# Calculate number of days since person joined the panel up to when the ith result was submitted.
SubmitTimes_days = as.numeric(difftime(Data$created_at_results, Data$created_at_parts, units="days"))
Data  = cbind(Data, SubmitTimes_days)

# Calculate number of days since person joined the panel up to when the ith payment was made to them.
PayTimes_days = as.numeric(difftime(Data$paid_at_results, Data$created_at_parts, units="days"))
=======
# Calculate time of day invite sent.
invite_time = strptime(Data$created_at, "%Y-%m-%d %H:%M:%S")
Invite_TOD = ((lubridate::period_to_seconds(hms(strftime(invite_time, format="%H:%M:%S"))))/60)/60

# Calculate day of week invite sent.
Invite_DOW = weekdays(strptime(Data$created_at, "%Y-%m-%d %H:%M:%OS"))
Invite_DOW[as.character(Invite_DOW) == "Monday"] = 0
Invite_DOW[as.character(Invite_DOW) == "Tuesday"] = 1
Invite_DOW[as.character(Invite_DOW) == "Wednesday"] = 2
Invite_DOW[as.character(Invite_DOW) == "Thursday"] = 3
Invite_DOW[as.character(Invite_DOW) == "Friday"] = 4
Invite_DOW[as.character(Invite_DOW) == "Saturday"] = 5
Invite_DOW[as.character(Invite_DOW) == "Sunday"] = 6

# Calculate time of day result submitted.
submission_time = strptime(Data$created_at_results, "%Y-%m-%d %H:%M:%S")
Submit_TOD = ((lubridate::period_to_seconds(hms(strftime(submission_time, format="%H:%M:%S"))))/60)/60

# Calculate day of week result submitted.
Submit_DOW = weekdays(strptime(Data$created_at_results, "%Y-%m-%d %H:%M:%OS"))
Submit_DOW[as.character(Submit_DOW) == "Monday"] = 0
Submit_DOW[as.character(Submit_DOW) == "Tuesday"] = 1
Submit_DOW[as.character(Submit_DOW) == "Wednesday"] = 2
Submit_DOW[as.character(Submit_DOW) == "Thursday"] = 3
Submit_DOW[as.character(Submit_DOW) == "Friday"] = 4
Submit_DOW[as.character(Submit_DOW) == "Saturday"] = 5
Submit_DOW[as.character(Submit_DOW) == "Sunday"] = 6

Data = cbind(Data, Submit_DOW, Invite_DOW, Invite_TOD)
rm(invite_time, Invite_TOD, Invite_DOW, submission_time, Submit_TOD, Submit_DOW)

# Calculate number of days since person joined the panel up to when the ith invite was sent. Initially in seconds, then convert to days.
InvTimes_days = as.numeric(strptime(Data$created_at, "%Y-%m-%d %H:%M:%OS") - strptime(Data$created_at_parts, "%Y-%m-%d %H:%M:%OS"))/60/60/24
Data = cbind(Data, InvTimes_days)

# Calculate number of days since person joined the panel up to when the ith result was submitted.
SubmitTimes_days = as.numeric(strptime(Data$created_at_results, "%Y-%m-%d %H:%M:%OS") - strptime(Data$created_at_parts, "%Y-%m-%d %H:%M:%OS"))/60/60/24
Data  = cbind(Data, SubmitTimes_days)

# Calculate number of days since person joined the panel up to when the ith payment was made to them.
PayTimes_days = as.numeric(strptime(Data$paid_at_results, "%Y-%m-%d %H:%M:%OS") - strptime(Data$created_at_parts, "%Y-%m-%d %H:%M:%OS"))/60/60/24
>>>>>>> 977e647f0b65b3fddc2d87152069d655e31213ea
Data  = cbind(Data, PayTimes_days)

# Calculate number of days between ith invite and ith result submission.
InvToSubmitTimes_days = SubmitTimes_days - InvTimes_days

# Calculate number of days between ith result submission and ith payment.
SubmitToPayTimes_days = PayTimes_days - SubmitTimes_days
Data = cbind(Data, InvToSubmitTimes_days, SubmitToPayTimes_days)
rm(InvTimes_days, SubmitTimes_days, PayTimes_days, InvToSubmitTimes_days, SubmitToPayTimes_days)

<<<<<<< HEAD

### !!! Change this so you just convert existing data frame "Data" into data.table to save memory.
# Turn Data into data table
allData = data.table(Data)

# Calculate a bunch of frequencies within person and study re: paid/unpaid submissions crossed by result state. Also add time to invite average and other things like this.
PayState_p_s =  allData[,.(PayCompNum = length(which(with_pay_results==1 & state_results == "complete"))
	, NoPayCompNum = length(which(with_pay_results==0 & state_results == "complete"))
	, PayRejNum = length(which(with_pay_results==1 & state_results == "rejected"))
	, NoPayRejNum = length(which(with_pay_results==0 & state_results == "rejected"))
	, PayErrNum = length(which(with_pay_results==1 & state_results == "errored"))
	, NoPayErrNum = length(which(with_pay_results==0 & state_results == "errored"))
	, PayTotal = length(which(with_pay_results==1))
	, NoPayTotal = length(which(with_pay_results==0))
	, InvRespRate = length(which(!is.na(created_at_results))) / (length(id))
	, SubmitTime_avg = mean(InvToSubmitTimes_days, na.rm=TRUE)
	, CompPaySubmitTime_min = min(InvToSubmitTimes_days[which(with_pay_results==1 & state_results == "complete")], na.rm=TRUE)
	, RejPaySubmitTime_min = min(InvToSubmitTimes_days[which(with_pay_results==1 & state_results == "rejected")], na.rm=TRUE)
	, PayTime_avg = mean(SubmitToPayTimes_days, na.rm=TRUE)), by=.(participant_id,study_id)]
rm(allData)

PayState_p_s$CompPaySubmitTime_min[which(is.infinite(PayState_p_s$CompPaySubmitTime_min))] = NA
PayState_p_s$RejPaySubmitTime_min[which(is.infinite(PayState_p_s$RejPaySubmitTime_min))] = NA
# Remove all duplicate submissions for each person for a given study.
rData = Data[!duplicated(Data[c("study_id", "participant_id")]),]
rData = data.table(rData)
Demogs_p_s = rData[, .(gender, age, income, education, comp_skill, parent, married, type_studies, study_type, byop_study, tpsp_study, ingest_study, internal_study, no_pay_email
	, sign_in_count_parts, has_android_phone_parts, has_android_tablet_parts, has_ios_phone_parts, has_ios_tablet_parts
	, has_osx_parts, has_windows_parts, has_profile_image_results, participants_needed_studies, created_at_parts, created_at_studies), by=.(participant_id,study_id)]
rm(rData)

# This data table basically collapses over time within a study, but maintains time across studies.
Data_p_s = left_join(Demogs_p_s, PayState_p_s, by=c("participant_id", "study_id"))
rm(Demogs_p_s, PayState_p_s)

Study_data =  Data_p_s[,.(study_create_date = created_at_studies[1]
	, type_studies = type_studies[1]
	, study_type = study_type[1]
	, SampleReq = participants_needed_studies[1]
	, PayCompNum = sum(PayCompNum)
	, NoPayCompNum = sum(NoPayCompNum)
	, PayRejNum = sum(PayRejNum)
	, NoPayRejNum = sum(NoPayRejNum)
	, PayErrNum = sum(PayErrNum)
	, NoPayErrNum = sum(NoPayErrNum)
	, PayTotal = sum(PayTotal)
	, NoPayTotal = sum(NoPayTotal)
	, InvRespRate = mean(InvRespRate, na.rm=TRUE)
	, SubmitTime_avg = mean(SubmitTime_avg, na.rm=TRUE)
	, CompPaySubmitTime_avg = mean(CompPaySubmitTime_min, na.rm=TRUE)
	, RejPaySubmitTime_avg = mean(RejPaySubmitTime_min, na.rm=TRUE)
	, CompPaySubmitTime_med = median(CompPaySubmitTime_min, na.rm=TRUE)
	, RejPaySubmitTime_med = median(RejPaySubmitTime_min, na.rm=TRUE)
	, PayTime_avg= mean(PayTime_avg, na.rm=TRUE)
	, InvRespRate_med = as.double(median(InvRespRate, na.rm=TRUE))
	, SubmitTime_med = as.double(median(SubmitTime_avg, na.rm=TRUE))
	, PayTime_med = as.double(median(PayTime_avg, na.rm=TRUE))
	, AverageAge = mean(age, na.rm=TRUE)
	, AverageIncome = mean(income, na.rm=TRUE)
	, AverageEducation = mean(education, na.rm=TRUE)
	, Parent_prop = length(which(parent=="true")) / length(parent)
	, Married_prop = length(which(married=="true")) / length(married)
	, ProfileIma_prop = length(which(has_profile_image_results==1)) / length(has_profile_image_results)
	, NumFemale = length(which(gender == "female"))
	, NumMale = length(which(gender == "male"))
	, SignInAvg = mean(sign_in_count_parts, na.rm=TRUE)
	, SignIn_med = as.double(median(sign_in_count_parts, na.rm=TRUE))
	, BYOP_prop = length(which(byop_study==1)) / length(byop_study)
	, TPS_prop = length(which(tpsp_study==1)) / length(tpsp_study)
	, PANEL_prop = length(which(byop_study==0 & tpsp_study==0 & ingest_study==0)) / length(byop_study)
	), by=.(study_id)]

Study_data = Study_data[order(Study_data$study_create_date),]
timediff = c(0, diff.Date(as.Date(Study_data$study_create_date)))
time_days = cumsum(timediff)
Study_data[,timdiff:=timediff]
Study_data[,time_days:=time_days]

#clean up Study_data
Study_data1415 = filter(Study_data, study_create_date > as.Date("2013-12-31"))
c_sd = filter(Study_data, SampleReq<100, type_studies=="DesktopStudy"
	, study_type=="client", SubmitTime < 30
	, AverageAge < 85, (PayTotal+NoPayTotal)<100
	, (PayTotal+NoPayTotal)>4)


Part_data =  Data_p_s[,.(gender = gender[1], age=age[1], income=income[1], education=education[1], comp_skill=comp_skill[1]
	, parent = parent[1], married=married[1], SignIns = sign_in_sount_parts[1], own_and_phone = has_android_phone_parts[1]
	, own_and_tablet = has_android_tablet_parts[1], own_ios_phone = has_ios_phone_parts[1], own_ios_tablet = has_ios_tablet_parts[1]
	, own_osx = has_osx_parts[1], own_windows = has_windows_parts[1], profileIma = has_profile_image_results[1], account_create = created_at_parts[1]
	, PayCompNum = sum(PayCompNum)
	, NoPayCompNum = sum(NoPayCompNum)
	, PayRejNum = sum(PayRejNum)
	, NoPayRejNum = sum(NoPayRejNum)
	, PayErrNum = sum(PayErrNum)
	, NoPayErrNum = sum(NoPayErrNum)
	, PayTotal = sum(PayTotal)
	, NoPayTotal = sum(NoPayTotal)
	, InvRespRate = sum()

	study_create_date = created_at_studies[1]
	, type_studies = type_studies[1]
	, study_type = study_type[1]
	, SampleReq = participants_needed_studies[1]
	, PayCompNum = sum(PayCompNum)
	, NoPayCompNum = sum(NoPayCompNum)
	, PayRejNum = sum(PayRejNum)
	, NoPayRejNum = sum(NoPayRejNum)
	, PayErrNum = sum(PayErrNum)
	, NoPayErrNum = sum(NoPayErrNum)
	, PayTotal = sum(PayTotal)
	, NoPayTotal = sum(NoPayTotal)
	, InvRespRate = mean(InvRespRate, na.rm=TRUE)
	, SubmitTime = mean(SubmitTime, na.rm=TRUE)
	, PayTime= mean(PayTime, na.rm=TRUE)
	, InvRespRate_med = as.double(median(InvRespRate, na.rm=TRUE))
	, SubmitTime_med = as.double(median(SubmitTime, na.rm=TRUE))
	, PayTime_med = as.double(median(PayTime, na.rm=TRUE))
	, AverageAge = mean(age, na.rm=TRUE)
	, AverageIncome = mean(income, na.rm=TRUE)
	, AverageEducation = mean(education, na.rm=TRUE)
	, Parent_prop = length(which(parent=="true")) / length(parent)
	, Married_prop = length(which(married=="true")) / length(married)
	, ProfileIma_prop = length(which(has_profile_image_results==1)) / length(has_profile_image_results)
	, NumFemale = length(which(gender == "female"))
	, NumMale = length(which(gender == "male"))
	, SignInAvg = mean(sign_in_count_parts, na.rm=TRUE)
	, SignIn_med = as.double(median(sign_in_count_parts, na.rm=TRUE))
	, BYOP_prop = length(which(byop_study==1)) / length(byop_study)
	, TPS_prop = length(which(tpsp_study==1)) / length(tpsp_study)
	, PANEL_prop = length(which(byop_study==0 & tpsp_study==0 & ingest_study==0)) / length(byop_study)
	), by=.(participant_id)]

























#######################################
### Participant per Study Calculations
#######################################
# Calculate response to invite rate.
(NumInvites = ddply(Data,
	, .(participant_id, study_id), summarize
	, NumInvites = length(id) ))
Data = left_join(Data, NumInvites, by = c("participant_id", "study_id"))

(NumSubmits = ddply(Data,
	, .(participant_id, study_id), summarize
	, NumSubmits = length(which(!is.na(id_results))) ))
Data = left_join(Data, NumSubmits, by = c("participant_id", "study_id"))

# Calculate Quality Submission Rate (# paid results submitted / # total results submitted)
(NumPayResults = ddply(Data
	, .(participant_id, study_id), summarize
	, NumPayResults = length(which(with_pay_results == "1")) ))
Data = left_join(Data, NumPayResults, by = c("participant_id", "study_id"))

(NumNoPayResult = ddply(Data,
	, .(participant_id, study_id), summarize
	, NumNoPayResult = length(which(with_pay_results == "0")) ))
Data = left_join(Data, NumNoPayResult, by = c("participant_id", "study_id"))

# Calculate number of completes, rejects, errored, not started.
(NumComp = ddply(Data,
	, .(participant_id, study_id), summarize
	, NumComp = length(which(state_results=="complete")) ))
Data = left_join(Data, NumComp, by = c("participant_id", "study_id"))

(NumError = ddply(Data,
	, .(participant_id, study_id), summarize
	, NumError = length(which(state_results=="errored")) ))
Data = left_join(Data, NumError, by = c("participant_id", "study_id"))

(NumReject = ddply(Data,
	, .(participant_id, study_id), summarize
	, NumReject = length(which(state_results=="rejected")) ))
Data = left_join(Data, NumReject, by = c("participant_id", "study_id"))

(NumNoStart = ddply(Data,
	, .(participant_id, study_id), summarize
	, NumNoStart = length(which(state_results=="not_started")) ))
Data = left_join(Data, NumNoStart, by = c("participant_id", "study_id"))

# Calculate average time to respond to invite.
(AvgSubmitTimes_days = ddply(Data,
	, .(participant_id, study_id), summarize
	, AvgSubmitTimes_days = mean(SubmitTimes_days, na.rm=TRUE) ))
Data = left_join(Data, AvgSubmitTimes_days, by = c("participant_id", "study_id"))

# Calculate average time to get paid after invite.
(AvgPayTimes_days = ddply(Data,
	, .(participant_id, study_id), summarize
	, AvgPayTimes_days = mean(SubmitToPayTimes_days, na.rm=TRUE) ))
Data = left_join(Data, AvgPayTimes_days, by = c("participant_id", "study_id"))

QualSubmitRate = Data$NumPayResults / (Data$NumPayResults + Data$NumNoPayResults)

RespInvRate = Data$NumSubmits / Data$NumInvites

Data = cbind(Data, QualSubmitRate, RespInvRate)





























=======
#####################
### Participant Level
#####################
# Calculate Quality Submission Rate (# paid results submitted / # total results submitted)
(PayResult_part_count = ddply(filter(Data, study_type == "client")
	, ~ participant_id, summarize
	, PayResult_part_count = length(which(with_pay_results == "1")) ))
Data = left_join(Data, PayResult_part_count, by = "participant_id")

(NoPayResult_part_count = ddply(filter(Data, study_type == "client")
	, ~ participant_id, summarize
	, NoPayResult_part_count = length(which(with_pay_results == "0")) ))
Data = left_join(Data, NoPayResult_part_count, by = "participant_id")

(NumInvite_part_count = ddply(filter(Data, study_type == "client")
	, ~ participant_id, summarize
	, NumInvite_part_count = length(with_pay_results) ))
Data = left_join(Data, NumInvite_part_count, by = "participant_id")

QualSubmitRate = Data$PayResult_part_count / (Data$PayResult_part_count + Data$NoPayResult_part_count)
RespInvRate = (Data$PayResult_part_count + Data$NoPayResult_part_count) / Data$NumInvite_part_count
Data = cbind(Data, QualSubmitRate, RespInvRate)
>>>>>>> 977e647f0b65b3fddc2d87152069d655e31213ea

# Calculate Number of Sign Ins per Day.
SignInRate_day = (Data$sign_in_count_parts / as.numeric(Sys.time() - strptime(Data$created_at_parts, "%Y-%m-%d %H:%M:%OS")))
Data = cbind(Data, SignInRate_day)

# Calculate Average duration of result submitted per participant.
(Dur_sec_part_avg = ddply(filter(Data, study_type == "client")
	, ~ participant_id, summarize
	, Dur_sec_part_avg = mean(duration_results, na.rm=TRUE) ))
Data = left_join(Data, Dur_sec_part_avg, by = "participant_id")

# Calculate Average time between invite and response per participant.
(InvToRespTimes_hrs_part_avg = ddply(filter(Data, study_type == "client")
	, ~ participant_id, summarize
	, InvToRespTimes_hrs_part_avg = mean(InvToRespTimes_hrs, na.rm=TRUE) ))
Data = left_join(Data, InvToRespTimes_hrs_part_avg, by = "participant_id")
rm(InvToRespTimes_hrs_part_avg, Dur_sec_part_avg, SignInRate_day
	, RespInvRate, QualSubmitRate, NumInvite_part_count
	, NoPayResult_part_count, PayResult_part_count)

###############
### Study Level
###############
# Calculate Study Centered Duration of Submission
(Duration_study_avg = ddply(filter(Data, study_type == "client")
	, ~ study_id, summarize
	, duration_grp_avg = mean(duration_results, na.rm=TRUE)))
Data = left_join(Data, Duration_study_avg, by = "study_id")
Duration_grp_c = Data$duration_results - Data$duration_grp_avg
Data = cbind(Data, Duration_grp_c)
rm(Duration_grp_c, Duration_study_avg)
# Compute number of study_states by pay/no pay for each study.
(NumCompPay = ddply(Data
	, ~ study_id, summarize
	, NumCompPay = length(which(state_results == "complete" & with_pay_results == "1"))))
(NumCompNoPay = ddply(Data
	, ~ study_id, summarize
	, NumCompNoPay = length(which(state_results == "complete" & with_pay_results == "0"))))
(NumRejPay = ddply(Data
	, ~ study_id, summarize
	, NumRejPay = length(which(state_results == "rejected" & with_pay_results == "1"))))
(NumRejNoPay = ddply(Data
	, ~ study_id, summarize
	, NumRejNoPay = length(which(state_results == "rejected" & with_pay_results == "0"))))
(NumErrPay = ddply(Data
	, ~ study_id, summarize
	, NumErrPay = length(which(state_results == "errored" & with_pay_results == "1"))))
(NumErrNoPay = ddply(Data
	, ~ study_id, summarize
	, NumErrNoPay = length(which(state_results == "errored" & with_pay_results == "0"))))
(NumNoStPay = ddply(Data
	, ~ study_id, summarize
	, NumNoStPay = length(which(state_results == "not_started" & with_pay_results == "1"))))
(NumNoStNoPay = ddply(Data
	, ~ study_id, summarize
	, NumNoStNoPay = length(which(state_results == "not_started" & with_pay_results == "0"))))
(NumPay = ddply(Data
	, ~ study_id, summarize
	, NumPay = length(which(with_pay_results == "1"))))
(NumSubmits = ddply(Data
	, ~ study_id, summarize
	, NumSubmits = length(study_id)))
Data = left_join(Data, NumPay, by = "study_id")
Data = left_join(Data, NumSubmits, by = "study_id")
Data = left_join(Data, NumNoStNoPay, by = "study_id")
Data = left_join(Data, NumNoStPay, by = "study_id")
Data = left_join(Data, NumErrPay, by = "study_id")
Data = left_join(Data, NumErrNoPay, by = "study_id")
Data = left_join(Data, NumRejPay, by = "study_id")
Data = left_join(Data, NumRejNoPay, by = "study_id")
Data = left_join(Data, NumCompPay, by = "study_id")
Data = left_join(Data, NumCompNoPay, by = "study_id")
rm(NumPay, NumSubmits, NumNoStNoPay, NumNoStPay, NumErrPay, NumErrNoPay, NumRejPay, NumRejNoPay, NumCompPay, NumCompNoPay)


#################################
### Filter out all Sample Studies
################################# 
# Grab only client studies, that are not ingestion studies,
# and where the participants were NOT YE employees.
clData = filter(Data, study_type=="client", ingest_study==0, internal_study==0)
# Rearrange data by participant_id and study_id
clData = arrange(clData, desc(participant_id), study_id)




# Grab only rows in which participant responded to invite
clData = filter(clData, !is.na(state_results))

part_cols <- grep("(_par(t|ts)|participant_id)", colnames(clData))
study_cols <- grep("(study_id|_studies|NumSubmits|NumPay|NumNoStNoPay|NumNoStPay|NumErrNoPay|NumErrPay|NumRejNoPay|NumRejPay|NumCompNoPay|NumCompPay)", colnames(clData))


m1Data = filter(Data, byop_study == 0, tpsp_study == 0, ingest_study == 0, internal_study == 0, no_pay_email == 0, !is.na(id_results))

# Number of studies invited to.
(NumInv_p = ddply(m1Data
                    , ~ participant_id, summarize
                    , NumInv_p = length(unique(study_id))))

# Number of studies submitted a paid result to.
(NumSubPay_p = ddply(filter(m1Data, with_pay_results == 1)
                    , ~ participant_id, summarize
                    , NumSubPay_p = length(unique(study_id))))











