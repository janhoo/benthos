c2p <-
function(x,stage="adult",depth=3,catalog=TRUE,relative=TRUE,verbose=FALSE){
	method<-32
	return(aqaconvert(x,stage=stage,depth=depth,method=method,catalog=catalog,relative=relative,verbose=verbose))
	
}
