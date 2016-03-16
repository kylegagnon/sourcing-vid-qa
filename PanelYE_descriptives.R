# Descriptive stats on YE panel data from prouction DB

smData = cbind(Data[,1:3], id_results=Data[,39], Data[,4:10], Data[,15:17]
	, created_at_parts=Data[,20], Data[,27:35], has_profile_image_results=Data[,40], Data[,42:43], state_results=Data[,45]
	, completion_state_results=Data[,46], Data[,47:48], os_results=Data[,50], Data[,51:52], Data[,54], Data[,56:58], fulfilled_at_studies=Data[,60], Data[,61:62], Data[,64:81])


# Summary demographics for participants
pData = smData[!duplicated(smData$participant_id), ]

#####################
### Participant Level
#####################
# Calculate Quality Submission Rate (# paid results submitted / # total results submitted)
(PayResult_part_count = ddply(filter(smData, study_type == "client")
	, ~ participant_id, summarize
	, PayResult_part_count = length(which(with_pay_results == "1")) ))
smData = left_join(smData, PayResult_part_count, by = "participant_id")

(NoPayResult_part_count = ddply(filter(smData, study_type == "client")
	, ~ participant_id, summarize
	, NoPayResult_part_count = length(which(with_pay_results == "0")) ))
smData = left_join(smData, NoPayResult_part_count, by = "participant_id")

(NumInvite_part_count = ddply(filter(smData, study_type == "client")
	, ~ participant_id, summarize
	, NumInvite_part_count = length(with_pay_results) ))
smData = left_join(smData, NumInvite_part_count, by = "participant_id")

QualSubRate = smData$PayResult_part_count / (smData$PayResult_part_count + smData$NoPayResult_part_count)
RespInvRate = (smData$PayResult_part_count + smData$NoPayResult_part_count) / smData$NumInvite_part_count
smData = cbind(smData, QualSubRate, RespInvRate)

# Calculate Number of Sign Ins per Day.
SignInRate_day = (smData$sign_in_count_parts / as.numeric(Sys.time() - strptime(smData$created_at_parts, "%Y-%m-%d %H:%M:%OS")))
smData = cbind(smData, SignInRate_day)

# Calculate Average duration of result submitted per participant.
(Dur_sec_part_avg = ddply(filter(smData, study_type == "client")
	, ~ participant_id, summarize
	, Dur_sec_part_avg = mean(duration_results, na.rm=TRUE) ))
smData = left_join(smData, Dur_sec_part_avg, by = "participant_id")

pData =

rData =

iData =
