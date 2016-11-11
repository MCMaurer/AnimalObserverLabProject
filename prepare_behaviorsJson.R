###################recursive algorithm:
listFromCsv <- function(dat)
{
if(is.null(dat)) {
	return(NULL)
} else {
names(dat) <- gsub("[.]", "_", names(dat))##necessary due to use of unlist in jsonoutputconversion.R
allbehav <- dat[!duplicated(dat),]	


for(i in 1:ncol(allbehav)) allbehav[,i] <- factor(allbehav[,i], levels=as.character(unique(allbehav[,i])))

for (i in ncol(allbehav):1){
	allbehav[allbehav[,i]=="" & !is.na(allbehav[,i]),i] <- NA
	allbehav <- allbehav[order(allbehav[,i]),]
}
row.names(allbehav) <- 1:nrow(allbehav)

assignIndex <- function(dat){
	dat <- as.data.frame(dat)
	if (ncol(dat)==1) return(rep(1, nrow(dat))) else {
	dupvec <- duplicated(dat[,-ncol(dat)])
	ans <- rep(NA, length(dupvec))
	index <- 0
	for(i in 1:length(dupvec)){
		if(!dupvec[i]) index <- index+1
		ans[i] <- index
	}
	return(ans)
	}
}

dupvec <- assignIndex(allbehav[,1:ncol(allbehav)])
twigs <- list()
index <- 0
add <- FALSE
for(i in 1:nrow(allbehav))
{
	#print(i)
	if(!is.na(allbehav[i,ncol(allbehav)]))
	{
		if(!add){
			index <- index+1
			addIndex <- 2
			twigs[[index]] <- list()
			twigs[[index]][[1]] <- list() 
			twigs[[index]][[1]][[1]] <- list(name=as.character(allbehav[i,ncol(allbehav)]))
			names(twigs[[index]]) <- names(allbehav)[ncol(allbehav)]
		} else 
		{
			twigs[[index]][[1]][[addIndex]] <- list(name=as.character(allbehav[i,ncol(allbehav)]))
			names(twigs[[index]]) <- names(allbehav)[ncol(allbehav)]
			addIndex <- addIndex+1
		}
	} else {
		index <- index+1
		twigs[[index]] <- NA
	}
	if(i<nrow(allbehav)) {
		add <- (dupvec[i+1]==dupvec[i])
		}
}
#twigs
dupvec2 <- dupvec
for(i in (ncol(allbehav)-1):1)
{
	#print(paste("i=", i))
	index <- 0
	twigs2 <- list()
	dupvec1 <- dupvec2
	dupvec2 <- assignIndex(allbehav[,1:i])
	
	add <- FALSE
	insert <- TRUE
	for (j in 1:nrow(allbehav)){
		#print(paste("j=", j))
		#print(paste("add = ", add))
		#print(paste("insert = ", insert))		
		if (insert){
		if (!add){
			index <- index+1
			addIndex <- 2
			 if (is.na(allbehav[j,i])) {
					twigs2[[index]] <- twigs[[dupvec1[j]]]
				} else {
					if (length(twigs[[dupvec1[j]]][[1]])==1) { 
					if (is.na(twigs[[dupvec1[j]]][[1]])) {
					twigs2[[index]] <- list()
					twigs2[[index]][[1]] <- list() 
					twigs2[[index]][[1]][[1]] <- list(name=as.character(allbehav[j,i]))
					names(twigs2[[index]]) <- names(allbehav)[i]	
				} else {
				twigs2[[index]] <- list()
				twigs2[[index]][[1]] <- list()
				twigs2[[index]][[1]][[1]] <- list(name=as.character(allbehav[j,i])[1], twigs[[dupvec1[j]]][[1]])
				names(twigs2[[index]]) <- names(allbehav)[i]
				names(twigs2[[index]][[1]][[1]]) <- c("name",names(twigs[[dupvec1[j]]]))
				}
				} else {
				twigs2[[index]] <- list()
				twigs2[[index]][[1]] <- list()
				twigs2[[index]][[1]][[1]] <- list(name=as.character(allbehav[j,i])[1], twigs[[dupvec1[j]]][[1]])
				names(twigs2[[index]]) <- names(allbehav)[i]
				names(twigs2[[index]][[1]][[1]]) <- c("name",names(twigs[[dupvec1[j]]]))
				}
				}
		} else
		{ ##add=T
			if (is.na(allbehav[j,i])) {
						twigs2[[index]][[1]][[addIndex]] <- twigs[[dupvec1[j]]][[1]][[1]]
			} else {
			if (length(twigs[[dupvec1[j]]][[1]])==1) { 
					if (is.na(twigs[[dupvec1[j]]][[1]])) {
					twigs2[[index]][[1]][[addIndex]] <- list(name=as.character(allbehav[j,i]))
					names(twigs2[[index]][[1]][[addIndex]]) <- "name"	
				} else {
					twigs2[[index]][[1]][[addIndex]] <- list(name=as.character(allbehav[j,i]), twigs[[dupvec1[j]]][[1]])
					names(twigs2[[index]][[1]][[addIndex]]) <- c("name",names(twigs[[dupvec1[j]]]))
				}
				}	
			else {
			twigs2[[index]][[1]][[addIndex]] <- list(name=as.character(allbehav[j,i]), twigs[[dupvec1[j]]][[1]])
			names(twigs2[[index]][[1]][[addIndex]]) <- c("name",names(twigs[[dupvec1[j]]])) 
			}
			}
			addIndex <- addIndex+1
		}
		} #end insert
		if (j<nrow(allbehav)){
		add <- (dupvec2[j+1]==dupvec2[j])
		insert <-  (dupvec1[j+1]!=dupvec1[j])
		}
	}
	twigs <- twigs2
}
return(twigs2)
}
}


##########################################################################
##########################################################################
##########################################################################

prepareBehaviorsJson <- function(dyadic.all, scan.all, solo.all, version){
	dyadic <- listFromCsv(dat=dyadic.all)
	scan <- listFromCsv(dat= scan.all)
	solo <- listFromCsv(dat= solo.all)
	all <- c(version, dyadic, scan, solo)
	names(all) <- c("version", "dyadic", "scan", "solo")
	return(toJSON(all))
}
