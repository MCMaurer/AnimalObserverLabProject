prepareGroupCompo <- function(grcompo)
{
grcompo$ID <- factor(grcompo$ID, levels=as.character(unique(grcompo$ID)))
for(i in 1:ncol(grcompo))
{
	grcompo[,i] <- as.character(grcompo[,i])
	grcompo[is.na(grcompo[,i]),i] <- ""
}
grcompo$GR <- as.factor(grcompo$GR)

group <- list()
for (i in 1:length(levels(grcompo$GR)))
{
group[[i]] <- list()
group[[i]]$group <- levels(grcompo$GR)[i]
group[[i]]$animals <- list()
subgrcompo <- grcompo[grcompo$GR==levels(grcompo$GR)[i],]
for (j in 1:nrow(subgrcompo))
{
group[[i]]$animals[[j]] <- list()

temp <- which(!names(subgrcompo)%in%c("GR"))
for(k in 1:length(temp)){
	group[[i]]$animals[[j]][[k]] <- subgrcompo[j,temp[k]]
}
names(group[[i]]$animals[[j]]) <- names(subgrcompo)[temp]
}
}
return(toJSON(group))
}
