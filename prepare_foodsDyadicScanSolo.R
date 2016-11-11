
###############
######prepare dyadic.all, scan.all and solo.all for prepare_behaviorsJson.R::prepareBehaviorsJson()
###############

insert.foods <- function(dat, foods)
{	
### divide data into those with context FEE and those of other contexts
if(is.null(dat$'_FOOD')) return(dat)
if(ncol(dat)==1) return(foods)
dat <- data.frame(indices=1:nrow(dat), dat, check.names=F)
dyadic_fee <- dat[dat$'_FOOD'=="_FOOD" & !is.na(dat$'_FOOD'),] # 
dyadic_nonfee <- dat[!(dat$'_FOOD'=="_FOOD" & !is.na(dat$'_FOOD')),]

### duplicate food_ rows with all possible foods rows (all plant species and items)
dyadic_fee2 <- dyadic_fee[rep(seq(1,nrow(dyadic_fee)), each=nrow(foods)),]
dim(dyadic_fee2)
indexx <- which(names(dyadic_fee2)=="_FOOD")#returns # of column which contains FOOD 
if(ncol(dyadic_fee2)==indexx){##last column is food
  dyadic_fee3 <- data.frame(dyadic_fee2[,1:(indexx-1)], foods[rep(1:nrow(foods), nrow(dyadic_fee)),], check.names=F)
  names(dyadic_fee3)[1:(indexx-1)]<-names(dyadic_fee2)[1:(indexx-1)]
} else {
dyadic_fee3 <- data.frame(dyadic_fee2[,1:(indexx-1)], foods[rep(1:nrow(foods), nrow(dyadic_fee)),],dyadic_fee2[,(indexx+1):ncol(dyadic_fee2)], check.names=F)
names(dyadic_fee3)[1:(indexx-1)]<-names(dyadic_fee2)[1:(indexx-1)]
names(dyadic_fee3)[(indexx+ncol(foods)):ncol(dyadic_fee3)]<-names(dyadic_fee2)[(indexx+1):ncol(dyadic_fee2)]
}
empty <- foods[1,]; empty[1,] <- NA
if(ncol(dyadic_nonfee)==indexx){
  dyadic_nonfee3 <- data.frame(dyadic_nonfee[,1:(indexx-1)], empty[rep(1, nrow(dyadic_nonfee)),], check.names=F)
} else {
dyadic_nonfee3 <- data.frame(dyadic_nonfee[,1:(indexx-1)], empty[rep(1, nrow(dyadic_nonfee)),], dyadic_nonfee[,(indexx+1):ncol(dyadic_nonfee)], check.names=F)
}
names(dyadic_nonfee3)<-names(dyadic_fee3)

dyadic2 <- rbind(dyadic_nonfee3, dyadic_fee3)
dyadic3 <- dyadic2[order(dyadic2[,1]),]
return(dyadic3[,-1])
}

dyadicScanSolo <- function(dyadic, scan, solo, foods){
dyadic2 <- insert.foods(dat=dyadic, foods=foods)
scans2 <- insert.foods(dat=scan, foods=foods)
self2 <- insert.foods(dat=solo, foods=foods)
return(list(dyadic.all=dyadic2, scan.all=scans2, solo.all=self2))
}
