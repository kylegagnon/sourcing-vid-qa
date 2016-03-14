detrend = function(var, type="linear") {
	if(is.character(var)){
		print("Error: Variable entered is of type character.")
		stop
	}
	if(type=="linear" & length(var)>2) {
		time = 1:length(var)
		data = as.data.frame(cbind(var, time))
		output = lm(var ~ time, data=data)
	} if else(type=="linear" & length(var)<2) {
		print("Error: Variable entered has length less than 2. Linear fit can only be estimated with a minimum of 2 data poitns.")
		stop
	}
	detrend_var = coefficients(output)[1] + residuals.lm(output)
	return(detrend_var)
}