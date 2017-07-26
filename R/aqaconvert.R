aqaconvert <-
function(x,stage="adult",depth=3,method,catalog=TRUE,relative=TRUE,verbose=FALSE){
#################################################
#                  subroutines                  #
#################################################
get.itis.taxonomy <- function(taxon,relative=relative,depth=depth,verbose=verbose){
    coltaxlevel<-1
    colstart<-2
    colsearchend<-4
    colend<-19
    
    	for (i in c(colsearchend:colstart)){
		if (taxon %in% idb[,i]) { 
			row<-match(taxon,idb[,i])
			colstartreading<-match(idb[row,coltaxlevel],names(idb))
			if(relative){
				coltaxend<-colstartreading+2*depth
				if(coltaxend > colend){coltaxend<-colend}
			} else {
				coltaxend<-colsearchend+2*depth
				if ( colstartreading > coltaxend ) {return(NA)}
			}
			a<-sapply(idb[match(taxon,idb[,i]),c(colstartreading:coltaxend)],as.character)
			a<-a[!is.na(a)]
			if(verbose){cat("Hit in catalog. ")}
			return(a)
		}
	}
	return(NA)
}

#################################################
#is.taxon.conv <- function(taxon,db=db){
#	return(taxon %in% db[,1])
#}
#################################################
#is.in.conv <- function(taxon,db=db){
#	return(length(which(db[,1:7]==taxon))!=0)
#}
#################################################
conversion.coeff <- function(taxon,method,start=1,depth=8,verbose=verbose){	
	for (a in 1:length(taxon)){
		m<-mean(db[which(db[,start:depth]==taxon[a],arr.ind=T)[,1],method],na.rm=TRUE)
		if(is.na(m)){
			m<-NA
		} else {
			if(verbose){cat(paste("Vals of",taxon[a],":",format(m,digits=4), sep = " ", collapse = NULL))}
			break
		}
	}
	if(is.na(m) & verbose){cat("No vals. You may increase depth. ")}
	return(m)
}
#################################################
#################################################
rank.conversion.coeff <- function(taxon,start=1,depth=7,verbose=verbose){
	# find taxon and return rank; recursive
	k<-NA
	for (i in seq(start,depth,1)){
		if (taxon %in% db[,i]) { 
			k<-i
			if(verbose){cat("Hit in conv. ")}
			break
		} 
	}
	if (is.na(k) & verbose){cat("Not in conv. ") }
	return(k)
}
#################################################
find.conversion.coeff <- function(taxon,method,start=1,depth=3,verbose=verbose){
	# find average conversion factor for taxon; recursive
	# tested and verified!. This function does, what it should do
	stopifnot(start>0,depth>0,depth<8)
		for (j in seq(start,depth,1)){
			type<-db[match(taxon,db[,start]),j]
			m<-mean(db[db[,j]==type,method],na.rm=T)
			if(is.na(m)){
				m<-NA
			} else {
				if(verbose){cat(paste("Vals of ",taxon,":",format(m,digits=4)))}
				break
			}
		}
		if(is.na(m) & verbose){cat("No vals. You may increase depth. ")}
		return(m)
}
find.me.average<-function(taxon=taxon,method=method,rank=rank,depth=depth,relative=TRUE,verbose=verbose){
	# tested and verified!. This function does, what it should do
	if(relative){
		from<-rank
		to<-if(depth+rank-1 > 7) {7} else {depth+rank-1}
	} else {
		from<-rank
		to<-depth
	}
	av <- if (is.na(rank) | to<from) {NA} else {find.conversion.coeff(taxon=taxon,start=from,depth=to,method=method,verbose=verbose)}
	return(av)
}
#################################################
#                    main                       #
#################################################	
	stopifnot(depth>0,depth<8) 
		#data(conv)
		#data(itis)	
		#x <- readRDS(system.file("help", "aliases.rds", package="MASS"))
	if(depth==1){catalog<-FALSE}


	data("conv", envir = environment())
	data("itis", envir = environment())
	idb<-itis
	db<-stage.subset(db=conv,stage=stage)
	x<-clean.taxon.names(x)
	y<-data.frame(taxon=levels(x))
	y$coeff<-NA
	for ( i in seq(1,nrow(y),1) ) {
		taxon<-as.character(y[i,1])
		if(verbose){cat(paste("\n"," ...lookup ",taxon,": "))}
		rank<-rank.conversion.coeff(taxon=taxon,verbose=verbose)
		if ( !is.na(rank) ) {
			y$coeff[i]<-find.me.average(taxon=taxon,method=method,rank=rank,depth=depth,relative=relative,verbose=verbose)
		} else if (catalog) {
			taxtree <- get.itis.taxonomy( taxon=taxon,relative=relative,depth=depth,verbose=verbose)
			if(is.na(taxtree[1])){
				if(verbose){cat("Not in catalog")}
				next
			}
			y$coeff[i] <- conversion.coeff(taxon=taxtree,method=method,verbose=verbose)	
		} else if(verbose){cat(paste(taxon, " ... not found!"))}
	}
	if(verbose){cat("\n")}
	z<-data.frame(taxon=x,n=seq(1,length(x),1))
	out<-merge(y,z,all.y=TRUE)
	return(out[order(out$n),"coeff"])
}
