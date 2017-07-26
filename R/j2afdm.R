j2afdm <-
function(x,stage="adult",depth=3,catalog=TRUE,relative=TRUE,verbose=FALSE){
	method<-28
	return(aqaconvert(x,stage=stage,depth=depth,method=method,catalog=catalog,relative=relative,verbose=verbose))
	
}
