# Note: "species" in this script is the same as a "category" mentioned in the paper
# Note: "pe" stands for "professional ecologist"

######################################
# create classification and pe datasets for analysis

#strings should be read as characters by default, not factors
options(stringsAsFactors=F)

class.data<-read.table("your path here/all_raw_classification_data_CandS_Species_ID_MS.txt", sep="\t", header=T)
nrow(class.data)
#369671
length(unique(class.data$Video.ID))
#13531

pe.data<-read.table("your path here/all_pe_data_CandS_Species_ID_MS.txt", sep="\t", header=T)
nrow(pe.data)
#13814
length(unique(pe.data$Video.ID))
#13531


############################################################
# step 1 - remove pe videos 

######## unidentifiable
#remove videos that contain at least one unidentifiable species
table(pe.data$Species)
uid.agg<-aggregate(pe.data$Species, list(pe.data$Video.ID), function(x){
    sum(x=="unidentifiable")
})

pe.data<-pe.data[!pe.data$Video.ID %in% uid.agg$Group.1[uid.agg$x>0],]
nrow(pe.data)
#13756
length(unique(pe.data$Video.ID))
#13476
#removed 55 videos that contained an unidentifiable species

####### multispecies
#remove videos that contain 2+ C&S categories
pe.ms.table<-table(pe.data$Video.ID)
table(pe.ms.table)
#     1     2     3 
# 13212   248    16

#there are 264 pe multispecies videos at this stage
pe.data<-pe.data[pe.data$Video.ID %in% names(pe.ms.table[pe.ms.table==1]),]
nrow(pe.data)
#13212
length(unique(pe.data$Video.ID))
#13212
#removed 264 multispecies videos

####### remove videos the pe classified only as duiker
table(pe.data$Species)
pe.data<-pe.data[pe.data$Species!="duiker.sp",]
nrow(pe.data)
#13095
#removed 117 videos with duiker classifications

##### make sure classification data only has the same videos present
class.data<-class.data[class.data$Video.ID %in% pe.data$Video.ID,]
nrow(class.data)
#355199
length(unique(class.data$Video.ID))
#13095


############################################################
# step 2 - cleaning
# remove videos that had a bugged species retirement clip (more than 15 total classifications with 1+ nonblank classification)
# and clean the data: remove duplicated user.clip bouts, aggregate multilines for same user/species 

######### #remove buggy videos
#get retirement status of each clip
#a bout of classifying is signified using Classification.ID
clip.info<-data.frame(Clip.ID=sort(unique(class.data$Clip.ID)), Video.ID=NA, num.blank.bouts=NA, num.nonblank.bouts=NA)
nrow(clip.info)
#52380
clip.info$Video.ID<-class.data$Video.ID[match(clip.info$Clip.ID, class.data$Clip.ID)]

start.time<-Sys.time()
for(i in 1:nrow(clip.info)){
    cat(i, round(i/52380*100, 2), "% \n")

    sub.class<-class.data[class.data$Clip.ID==clip.info$Clip.ID[i],]
    clip.info$num.blank.bouts[i]<-length(unique(sub.class$Classification.ID[sub.class$Species=="blank"]))
    clip.info$num.nonblank.bouts[i]<-length(unique(sub.class$Classification.ID[sub.class$Species!="blank"]))

    # flush.console()
}
end.time<-Sys.time()
end.time - start.time
#11 minutes

buggy.clips<-clip.info[(clip.info$num.nonblank.bouts+clip.info$num.blank.bouts>15) & clip.info$num.nonblank.bouts>0,]
nrow(buggy.clips)
#156
length(unique(buggy.clips$Video.ID))
#156

class.data<-class.data[!class.data$Video.ID %in% buggy.clips$Video.ID,]
nrow(class.data)
#348162
length(unique(class.data$Video.ID))
#12939

pe.data<-pe.data[!pe.data$Video.ID %in% buggy.clips$Video.ID,]
nrow(pe.data)
#12939

#removed 156 videos with buggy retirement clips

######### remove duplicate user.clip bouts, i.e. when a user sees the same clip multiple times
#Classification.ID is used as a bout ID
class.data<-class.data[order(class.data$User.Name, class.data$Clip.ID, class.data$Classification.ID),]
class.data$User.Clip<-paste(class.data$User.Name, class.data$Clip.ID, sep="_")

num.bouts.per.user.clip<-aggregate(class.data$Classification.ID, list(class.data$User.Clip), function(x){length(unique(x))})
table(num.bouts.per.user.clip$x)
#      1      2      3      4      5      6      7      8      9     10     11     13     16     19     21     33     64    199 
# 327908   5242    467     72     19     14     12      5      2      6      2      1      4      1      1      1      1      1

user.clips.with.multi.bouts<-num.bouts.per.user.clip[num.bouts.per.user.clip$x>1,]

multi.bout.data<-class.data[class.data$User.Clip %in% user.clips.with.multi.bouts$Group.1,]
nrow(multi.bout.data)
#13223

#find the bouts to delete. for each user.clip, remove all bouts except the first
dup.bouts<-unlist(lapply(unique(multi.bout.data$User.Clip), function(clip.arg){

	sub.bouts<-unique(multi.bout.data$Classification.ID[multi.bout.data$User.Clip==clip.arg])
	return(sub.bouts[-1])

}))
length(dup.bouts)
#7138

nrow(class.data)
#348162
length(unique(class.data$Clip.ID))
#51756
length(unique(class.data$Video.ID))
#12939
class.data<-class.data[!class.data$Classification.ID %in% dup.bouts,]
nrow(class.data)
#340911
length(unique(class.data$Clip.ID))
#51756
length(unique(class.data$Video.ID))
#12939

#check to make sure all user.clips only have 1 bout now
table(aggregate(class.data$Classification.ID, list(class.data$User.Clip), function(x){length(unique(x))})$x)
#      1 
# 333759


####### remove any species=blank rows where there are blanks and species present in same bout
bout.table<-table(class.data$Classification.ID)
table(bout.table)
#      1      2      3      4      5      6      7      8      9     10     11 
# 329450   2814    854    277    191     80     49     21     15      6      2

start.time<-Sys.time()
blank.row.numbers.to.remove<-unlist(lapply(names(bout.table[bout.table>1]), function(bout.arg){
    sub.data<-class.data[class.data$Classification.ID==bout.arg,]

    #if a bout has both blanks and species, return blank row numbers
    if(sum(sub.data$Species!="blank")>0 & sum(sub.data$Species=="blank")>0){
        return(sub.data$Unique.Row.Number[sub.data$Species=="blank"])
    } else {
        return(NA)
    }
}))
end.time<-Sys.time()
end.time - start.time
#1.8 minutes
length(blank.row.numbers.to.remove)
#4309
blank.row.numbers.to.remove<-blank.row.numbers.to.remove[!is.na(blank.row.numbers.to.remove)]
length(blank.row.numbers.to.remove)
#0, no instances of blanks and species together in same bout


######## aggregate repeated categories into one line that contains the total number of individuals in that category summed across all lines for that category
# we don't need the behaviors/ages/sexes for anything in this analysis so don't bother merging them
# to get final number.individuals, add up the numbers in the number.individuals column, with 5+ being changed to 5 as a conservative measure
class.data$Classification.ID.Species<-paste(class.data$Classification.ID, class.data$Species, sep="_")
bout.species.table<-table(class.data$Classification.ID.Species)
table(bout.species.table)
#      1      2      3      4      5      6      7      8      9     10     11 
# 331111   2173    800    275    174     65     47     21     15      4      2

#turn date.time into character because otherwise it gets messy
class.data$Date.Time<-as.character(class.data$Date.Time)

#separate out the data that don't need to be aggregated
nonrepeated.data<-class.data[class.data$Classification.ID.Species %in% names(bout.species.table[bout.species.table==1]),]
nrow(nonrepeated.data)
#331111

#separate out the data to be aggregated, then recombine with data that doesn't need to be aggregated later
repeated.data<-class.data[class.data$Classification.ID.Species %in% names(bout.species.table[bout.species.table>1]),]
nrow(repeated.data)
#9800

bouts.to.agg<-names(bout.species.table[bout.species.table>1])
length(bouts.to.agg)
#3576

agg.repeat.data<-data.frame(matrix(NA, ncol=ncol(class.data), nrow=length(bouts.to.agg)))
colnames(agg.repeat.data)<-colnames(class.data)

start.time<-Sys.time()
for(i in 1:length(bouts.to.agg)){
    # cat(i, "\n")
    sub.data<-repeated.data[repeated.data$Classification.ID.Species==bouts.to.agg[i],]
    agg.repeat.data[i,]<-as.character(sub.data[1,])
    agg.counts<-gsub(sub.data$Number.Individuals, pattern="5+", replacement="5", fixed=T)
    agg.repeat.data$Number.Individuals[i]<-sum(as.numeric(agg.counts))
    # flush.console()
}
end.time<-Sys.time()
end.time - start.time
#5 seconds

#join the newly aggregated data with the original data that didn't need aggregation
class.data<-rbind(nonrepeated.data, agg.repeat.data)
nrow(class.data)
#334687
length(unique(class.data$Clip.ID))
#51576
length(unique(class.data$Video.ID))
#12939

bout.table<-table(class.data$Classification.ID)
table(bout.table)
#      1      2      3      4      5 
# 332852    892     10      4      1 

#so there are 907 multispecies/multicategory classifications


###############################################
# remove videos that have clips that only have 1 or 2 blanks and no nonblanks
# these are insufficiently retired as blank 

clip.info<-clip.info[clip.info$Video.ID %in% class.data$Video.ID,]
nrow(clip.info)
#51756

clip.info$num.blank.bouts2<-NA
clip.info$num.nonblank.bouts2<-NA

start.time<-Sys.time()
for(i in 1:nrow(clip.info)){
    cat(i, round(i/51756*100, 2), "% \n")

    sub.class<-class.data[class.data$Clip.ID==clip.info$Clip.ID[i],]
    clip.info$num.blank.bouts2[i]<-length(unique(sub.class$Classification.ID[sub.class$Species=="blank"]))
    clip.info$num.nonblank.bouts2[i]<-length(unique(sub.class$Classification.ID[sub.class$Species!="blank"]))

    # flush.console()
}
end.time<-Sys.time()
end.time - start.time
#19 minutes

short.blank.clips<-clip.info[clip.info$num.blank.bouts2<=2 & clip.info$num.nonblank.bouts2==0,]
nrow(short.blank.clips)
#1036 clips
length(unique(short.blank.clips$Video.ID))
#982 videos have at least one problematic clip and will be removed

class.data<-class.data[!class.data$Video.ID %in% short.blank.clips$Video.ID,]
nrow(class.data)
#315893
length(unique(class.data$Video.ID))
#11957

pe.data<-pe.data[!pe.data$Video.ID %in% short.blank.clips$Video.ID,]
nrow(pe.data)
#11957

#################################################
# step 3: keep only single species videos
# get median number of species per clip, then keep only videos with 4 median single species clips

# clips with all blanks will have a species number of 0
# some blanks and some species will have blanks removed then median species calculated
num.species.df<-data.frame(Clip.ID=unique(class.data$Clip.ID), median.num.species=NA)
total.rows<-nrow(num.species.df)

start.time<-Sys.time()
for(i in 1:nrow(num.species.df)){
	cat(i, round(i/total.rows*100, 2), "% \n")
	sub.data<-class.data[class.data$Clip.ID==num.species.df$Clip.ID[i],]
	sub.num.users.nonblank<-length(unique(sub.data$User.Name[sub.data$Species!="blank"]))

	if(sub.num.users.nonblank==0){
		num.species.df$median.num.species[i]<-0
	} else {
		sub.data<-sub.data[sub.data$Species!="blank",]
		num.species.df$median.num.species[i]<-median(table(sub.data$User.Name))
	}

	# flush.console()
}
end.time<-Sys.time()
end.time - start.time
#16 minutes

table(num.species.df$median.num.species)
#     0     1   1.5     2 
# 29430 18325    19    54

single.sp.clip.info<-num.species.df[num.species.df$median.num.species %in% c(0,1),]
nrow(single.sp.clip.info)
#47755
single.sp.data<-class.data[class.data$Clip.ID %in% single.sp.clip.info$Clip.ID,]
nrow(single.sp.data)
#314822
length(unique(single.sp.data$Clip.ID))
#47755
length(unique(single.sp.data$Video.ID))
#11957

#keep only videos that have 4 single species clips
single.sp.clip.info$Video.ID<-class.data$Video.ID[match(single.sp.clip.info$Clip.ID, class.data$Clip.ID)]
clip.table<-table(single.sp.clip.info$Video.ID)
table(clip.table)
    # 1     2     3     4 
    # 2     8    51 11896
#so we lose 61 videos because we don't believe they are single species

class.data<-class.data[class.data$Video.ID %in% names(clip.table[clip.table==4]),]
nrow(class.data)
#313505
length(unique(class.data$Clip.ID))
#47584
length(unique(class.data$Video.ID))
#11896

pe.data<-pe.data[pe.data$Video.ID %in% names(clip.table[clip.table==4]),]
nrow(pe.data)
#11896

#remove unnecessary columns
class.data<-class.data[,-which(colnames(class.data) %in% c("User.Clip","Classification.ID.Species"))]

write.table(class.data, "your path here/class_data_for_analysis_CandS_Species_ID_MS.txt", sep="\t", row.names=F, col.names=T)
write.table(pe.data, "your path here/pe_data_for_analysis_CandS_Species_ID_MS.txt", sep="\t", row.names=F, col.names=T)


