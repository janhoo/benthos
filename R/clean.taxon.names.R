clean.taxon.names <-
function(x){
	y<-tolower(x)
	y<-sub(" sp\\.$",'',y)
	y<-sub(" sp$",'',y)
	y<-sub(" spp\\.$",'',y)
	y<-sub(" spp$",'',y)
	y<-sub(" +$",'',y)
	y<-sub("^ +",'',y)
	y<-gsub(" {2,}",' ',y)
	#y<-sub("  ",' ',y, fixed=T)
	y<-factor(y)
	return(y)
}
