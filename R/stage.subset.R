stage.subset <-
function(db,stage="adult"){
	stopifnot(stage %in% c("adult", "juvenil", "larvae", "all", "nonadult", "nolarvae")) 
	#stages: 
	switch(stage,
		adult=db[db$stage=="adult",],
		juvenil=db[db$stage=="juvenil",],
		larvae=db[db$stage=="larvae",],
		all=db,
		nonadult=db[db$stage!="adult",],
		nolarvae=db[db$stage!="larvae",]
	)
}
