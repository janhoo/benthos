j2dm <-
function(x,stage="adult",depth=3,catalog=TRUE,relative=TRUE,verbose=FALSE){
	method<-26
	return(aqaconvert(x,stage=stage,depth=depth,method=method,catalog=catalog,relative=relative,verbose=verbose))
	
}
