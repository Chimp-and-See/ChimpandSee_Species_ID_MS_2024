# This script finds the consensus category per clip and video for original, umbrella duiker, and umbrella designations.
# Umbrella duiker: all duikers together as duiker, all hogs together as hog, large ungulate and small antelope together as hoofed animal.
# Umbrella: duikers, large ungulate, and small antelope together as ungulate, all hogs together as hog.

# How to find the consensus category per clip:
# The consensus category is the single category with a proportion of the responses >=0.5. 
# If there are 2 categories that are each 50% of the responses, or if there is one category that is most frequenet but at a proportion less than 0.5, then the clip doesn't reach consensus. 

# The proportion agreement for each clip is the proportion of classifications that match the pe category, regardless of what the consensus category is.
# The consensus proportion is the proportion of responses that belong to the consensus category (range 0.5-1).
# If a clip has 1, 2, or 3 nonblank classifications and the rest are blank, then that clip is instead considered "presence undetermined".

# To find the video consensus: 
# First remove any presence undetermined clips. 
# A video with all blank clips is consensus blank. If not all clips are consensus blank, then consider only clips that are not consensus blank. 
# If all remaining nonblank clips are nonconsensus, then the video is nonconsensus. 
# Otherwise, consider only clip(s) that have the highest consensus proportion. If those remaining clips reach a consensus on different categories, the video does not reach consensus. If the remaining clips all reach consensus on the same category, then the video reaches consensus on that category. 
# The proportion agreement for the video is the average proportion agreement of the clips that were considered for the consensus category determination.

# Note: "species" in this script is the same as a "category" mentioned in the paper

#############################################
#       Original category designation       #
#############################################
#strings should be read as characters by default, not factors
options(stringsAsFactors=F)

# Bring in pe data
pe.data<-read.table("your path here/pe_data_for_analysis_CandS_Species_ID_MS.txt", sep="\t", header=T)
nrow(pe.data)
#11896

######################## Find consensus species per clip
class.data<-read.table("your path here/class_data_for_analysis_CandS_Species_ID_MS.txt", sep="\t", header=T)
nrow(class.data)
#313505
class.data<-class.data[order(class.data$Clip.ID),]

#create summ.data to house summaries for each clip
summ.data<-data.frame(Clip.ID=sort(unique(class.data$Clip.ID)), Video.ID=NA, PE.Species=NA, Consensus.Species=NA, Consensus.Prop=NA, Prop.Agreement=NA, Number.Blanks=NA, Number.Nonblanks=NA)
summ.data$Video.ID<-class.data$Video.ID[match(summ.data$Clip.ID, class.data$Clip.ID)]
summ.data$PE.Species<-pe.data$Species[match(summ.data$Video.ID, pe.data$Video.ID)]
nrow(summ.data)
#47584

consensus.threshold<-0.5

# Summarize the consensus per clip and proportion agreement to pe.id
start.time<-Sys.time()
for(i in 1:nrow(summ.data)){
    cat(i, round(i/47584*100, 2), "% \n")
	flush.console()
	sub.class<-class.data[class.data$Clip.ID==summ.data$Clip.ID[i],]
	sub.PE.Species<-pe.data$Species[pe.data$Video.ID==summ.data$Video.ID[i]]

	Number.Blanks<-sum(sub.class$Species=="blank")
	Number.Nonblanks<-sum(sub.class$Species!="blank")

	summ.data$Number.Blanks[i]<-Number.Blanks
	summ.data$Number.Nonblanks[i]<-Number.Nonblanks

	# if the clip is only blanks, then species is blank
	if(Number.Nonblanks==0){
		summ.data$Consensus.Species[i]<-"blank"
		summ.data$Consensus.Prop[i]<-1
		summ.data$Prop.Agreement[i]<-mean(sub.class$Species==sub.PE.Species)
		
	# if there are enough nonblanks to find a consensus, find a consensus using nonblank classifications only
	} else if(Number.Nonblanks>=4){
		sub.class<-sub.class[sub.class$Species!="blank",]	
		
		sp.table<-table(sub.class$Species)
		max.prop<-max(sp.table)/sum(sp.table)
		max.sp<-names(sp.table[sp.table==max(sp.table)])
		# if only one species has the maximum proportion of responses, and the prop is at least 0.5, then clip reaches consensus on that species
		if(length(max.sp)==1 & max.prop>=consensus.threshold){
			summ.data$Consensus.Species[i]<-max.sp
			summ.data$Consensus.Prop[i]<-max.prop
			summ.data$Prop.Agreement[i]<-mean(sub.class$Species==sub.PE.Species)
		# otherwise doesn't reach consensus
		} else {
			summ.data$Consensus.Species[i]<-"no consensus"
		}
	
	# if there are too few nonblank clips, then it's presence undetermined
	} else if(Number.Nonblanks<=3){
			summ.data$Consensus.Species[i]<-"presence undetermined"
	}
}
end.time<-Sys.time()
end.time - start.time
#19 mins

sum(is.na(summ.data$Consensus.Species))
#0

table(summ.data$Consensus.Species)

#                  bird                 blank            chimpanzee 
#                   288                 29360                   672 
#           dark duiker              elephant      giant forest hog 
#                    48                     6                    11 
#          hippopotamus                 human      Jentink's duiker 
#                    76                   881                   123 
#        large ungulate               leopard          no consensus 
#                     4                    89                  2028 
#   other (non-primate)       other (primate)              pangolin 
#                   142                  1994                    13 
#             porcupine presence undetermined            red duiker 
#                    26                  1763                  2632 
#         red river hog                rodent        small antelope 
#                   453                   165                    32 
#             small cat     small grey duiker               warthog 
#                    10                  6583                    10 
#          zebra duiker 
#                   175 


write.table(summ.data, "your path here/consensus_per_clip_original_CandS_Species_ID_MS.txt", sep="\t", row.names=F, col.names=T)


######################## Find consensus species per video
clip.cons.data<-read.table("your path here/consensus_per_clip_original_CandS_Species_ID_MS.txt", sep="\t", header=T, stringsAsFactor=F)
nrow(clip.cons.data)
#47584
length(unique(clip.cons.data$Video.ID))
#11896

clip.cons.data<-clip.cons.data[order(clip.cons.data$Video.ID),]

video.cons<-data.frame(Video.ID=unique(clip.cons.data$Video.ID), PE.Species=NA, Consensus.Species=NA, Consensus.Prop=NA, Prop.Agreement=NA, Consensus.Type=NA)
video.cons$PE.Species<-clip.cons.data$PE.Species[match(video.cons$Video.ID, clip.cons.data$Video.ID)]
nrow(video.cons)
#11896

start.time<-Sys.time()
for(i in 1:nrow(video.cons)){
    # cat(i, round(i/11896*100, 2), "% \n")
	sub.cons<-clip.cons.data[clip.cons.data$Video.ID==video.cons$Video.ID[i],]
	
	# First remove any clips that are presence undetermined
	sub.cons<-sub.cons[sub.cons$Consensus.Species!="presence undetermined",]

	# If there are any remaining clips, get consensus
	if(nrow(sub.cons)>0){
		# If there are only blanks, then video is blank
		if(sum(sub.cons$Consensus.Species=="blank")==nrow(sub.cons)){
			video.cons$Consensus.Type[i]<-"Consensus blank"
			video.cons$Consensus.Species[i]<-"blank"

		# If there are only nonblank nonconsensus clips, don't continue, video is non consensus
		} else if(sum(sub.cons$Consensus.Species=="no consensus")==sum(sub.cons$Consensus.Species!="blank")){
			video.cons$Consensus.Type[i]<-"All nonblank clips are nonconsensus"
			video.cons$Consensus.Species[i]<-"no consensus"

		} else {
			# Remove any nonconsensus species and blanks
			sub.cons<-sub.cons[!sub.cons$Consensus.Species %in% c("no consensus","blank"),]
			
			# Keep only rows that have the highest consensus prop
			sub.cons<-sub.cons[sub.cons$Consensus.Prop==max(sub.cons$Consensus.Prop),]
			
			# If there are multiple species present, then no consensus
			if(length(unique(sub.cons$Consensus.Species))>1){
				video.cons$Consensus.Type[i]<-"Conflicting consensus species"
				video.cons$Consensus.Species[i]<-"no consensus"
			
			# If there's just one species present, then video reaches consensus on that species, at that max proportion, with the average Prop.Agreement
			} else {
				video.cons$Consensus.Type[i]<-"Consensus reached"
				video.cons$Consensus.Species[i]<-sub.cons$Consensus.Species[1]
				video.cons$Consensus.Prop[i]<-sub.cons$Consensus.Prop[1]
				video.cons$Prop.Agreement[i]<-mean(sub.cons$Prop.Agreement)
			}
		}
	# Otherwise whole video is presence undetermined	
	} else {
		video.cons$Consensus.Species[i]<-"presence undetermined"
		video.cons$Consensus.Type[i]<-"presence undetermined"		
	}
}
end.time<-Sys.time()
end.time - start.time
#28 seconds

table(video.cons$Consensus.Type, useNA="always")
# All nonblank clips are nonconsensus       Conflicting consensus species 
#                                 615                                  19 
#                     Consensus blank                   Consensus reached 
#                                3665                                7595 
#               presence undetermined                                <NA> 
#                                   2                                   0 

table(video.cons$Consensus.Type, video.cons$PE.Species=="blank")
#                                       FALSE TRUE
#   All nonblank clips are nonconsensus   608    7
#   Conflicting consensus species          19    0
#   Consensus blank                      1106 2559
#   Consensus reached                    7544   51
#   presence undetermined                   2    0


######## Remove presence undetermined videos
video.cons<-video.cons[video.cons$Consensus.Species!="presence undetermined",]
nrow(video.cons)
#11894

write.table(video.cons, "your path here/consensus_per_video_original_CandS_Species_ID_MS.txt", sep="\t", row.names=F, col.names=T)



#############################################
#        Umbrella duiker designation        #
#############################################

# Bring in pe data
pe.data<-read.table("your path here/pe_data_for_analysis_CandS_Species_ID_MS.txt", sep="\t", header=T)
nrow(pe.data)
#11896

######################## Find consensus species per clip
class.data<-read.table("your path here/class_data_for_analysis_CandS_Species_ID_MS.txt", sep="\t", header=T)
nrow(class.data)
#313505
class.data<-class.data[order(class.data$Clip.ID),]

# Create summ.data to house summaries for each clip
summ.data.umbrella.duiker<-data.frame(Clip.ID=sort(unique(class.data$Clip.ID)), Video.ID=NA, PE.Species=NA, Consensus.Species=NA, Consensus.Prop=NA, Prop.Agreement=NA, Number.Blanks=NA, Number.Nonblanks=NA)
summ.data.umbrella.duiker$Video.ID<-class.data$Video.ID[match(summ.data.umbrella.duiker$Clip.ID, class.data$Clip.ID)]
summ.data.umbrella.duiker$PE.Species<-pe.data$Umbrella.Duiker.Species[match(summ.data.umbrella.duiker$Video.ID, pe.data$Video.ID)]
nrow(summ.data.umbrella.duiker)
#47584

consensus.threshold<-0.5

# Summarize the consensus per clip and proportion agreement to pe.id
start.time<-Sys.time()
for(i in 1:nrow(summ.data.umbrella.duiker)){
    # cat(i, round(i/47584*100, 2), "% \n")
	sub.class<-class.data[class.data$Clip.ID==summ.data.umbrella.duiker$Clip.ID[i],]
	sub.PE.Species<-pe.data$Umbrella.Duiker.Species[pe.data$Video.ID==summ.data.umbrella.duiker$Video.ID[i]]

	Number.Blanks<-sum(sub.class$Umbrella.Duiker.Species=="blank")
	Number.Nonblanks<-sum(sub.class$Umbrella.Duiker.Species!="blank")

	summ.data.umbrella.duiker$Number.Blanks[i]<-Number.Blanks
	summ.data.umbrella.duiker$Number.Nonblanks[i]<-Number.Nonblanks
	
	# If the clip is only blanks, then species is blank
	if(Number.Nonblanks==0){
		summ.data.umbrella.duiker$Consensus.Species[i]<-"blank"
		summ.data.umbrella.duiker$Consensus.Prop[i]<-1
		summ.data.umbrella.duiker$Prop.Agreement[i]<-mean(sub.class$Umbrella.Duiker.Species==sub.PE.Species)
		
	# If there are enough nonblanks to find a consensus, find a consensus using nonblank classifications only
	} else if(Number.Nonblanks>=4){
		sub.class<-sub.class[sub.class$Umbrella.Duiker.Species!="blank",]	
		
		sp.table<-table(sub.class$Umbrella.Duiker.Species)
		max.prop<-max(sp.table)/sum(sp.table)
		max.sp<-names(sp.table[sp.table==max(sp.table)])
		# If only one species has the maximum proportion of responses, and the prop is at least 0.5, then clip reaches consensus on that species
		if(length(max.sp)==1 & max.prop>=consensus.threshold){
			summ.data.umbrella.duiker$Consensus.Species[i]<-max.sp
			summ.data.umbrella.duiker$Consensus.Prop[i]<-max.prop
			summ.data.umbrella.duiker$Prop.Agreement[i]<-mean(sub.class$Umbrella.Duiker.Species==sub.PE.Species)
		# Otherwise doesn't reach consensus
		} else {
			summ.data.umbrella.duiker$Consensus.Species[i]<-"no consensus"
		}
	
	# If there are too few nonblank clips, then it's presence undetermined
	} else if(Number.Nonblanks<=3){
			summ.data.umbrella.duiker$Consensus.Species[i]<-"presence undetermined"
	}
}
end.time<-Sys.time()
end.time - start.time
#21 mins

sum(is.na(summ.data.umbrella.duiker$Consensus.Species))
#0

table(summ.data.umbrella.duiker$Consensus.Species)
    #              bird                 blank            chimpanzee                duiker 
    #               288                 29360                   672                 11221 
    #          elephant          hippopotamus                   hog         hoofed.animal 
    #                 6                    75                   558                    28 
    #             human               leopard          no consensus   other (non-primate) 
    #               881                    89                   295                   141 
    #   other (primate)              pangolin             porcupine presence undetermined 
    #              1994                    13                    26                  1763 
    #            rodent             small cat 
    #               164                    10 



write.table(summ.data.umbrella.duiker, "your path here/consensus_per_clip_umbrella_duiker_CandS_Species_ID_MS.txt", sep="\t", row.names=F, col.names=T)


######################## Find consensus species per video
clip.cons.data.umbrella.duiker<-read.table("your path here/consensus_per_clip_umbrella_duiker_CandS_Species_ID_MS.txt", sep="\t", header=T, stringsAsFactor=F)
nrow(clip.cons.data.umbrella.duiker)
#47584
length(unique(clip.cons.data.umbrella.duiker$Video.ID))
#11896

clip.cons.data.umbrella.duiker<-clip.cons.data.umbrella.duiker[order(clip.cons.data.umbrella.duiker$Video.ID),]

video.cons.umbrella.duiker<-data.frame(Video.ID=unique(clip.cons.data.umbrella.duiker$Video.ID), PE.Species=NA, Consensus.Species=NA, Consensus.Prop=NA, Prop.Agreement=NA, Consensus.Type=NA)
video.cons.umbrella.duiker$PE.Species<-clip.cons.data.umbrella.duiker$PE.Species[match(video.cons.umbrella.duiker$Video.ID, clip.cons.data.umbrella.duiker$Video.ID)]
nrow(video.cons.umbrella.duiker)
#11896

start.time<-Sys.time()
for(i in 1:nrow(video.cons.umbrella.duiker)){
    # cat(i, round(i/11896*100, 2), "% \n")
	sub.cons<-clip.cons.data.umbrella.duiker[clip.cons.data.umbrella.duiker$Video.ID==video.cons.umbrella.duiker$Video.ID[i],]
	
	# First remove any clips that are presence undetermined
	sub.cons<-sub.cons[sub.cons$Consensus.Species!="presence undetermined",]

	# If there are any remaining clips, get consensus
	if(nrow(sub.cons)>0){
		# If there are only blanks, then video is blank
		if(sum(sub.cons$Consensus.Species=="blank")==nrow(sub.cons)){
			video.cons.umbrella.duiker$Consensus.Type[i]<-"Consensus blank"
			video.cons.umbrella.duiker$Consensus.Species[i]<-"blank"

		# If there are only nonblank nonconsensus clips, don't continue, video is non consensus
		} else if(sum(sub.cons$Consensus.Species=="no consensus")==sum(sub.cons$Consensus.Species!="blank")){
			video.cons.umbrella.duiker$Consensus.Type[i]<-"All nonblank clips are nonconsensus"
			video.cons.umbrella.duiker$Consensus.Species[i]<-"no consensus"

		} else {
			# Remove any nonconsensus species and blanks
			sub.cons<-sub.cons[!sub.cons$Consensus.Species %in% c("no consensus","blank"),]
			
			# Keep only rows that have the highest consensus prop
			sub.cons<-sub.cons[sub.cons$Consensus.Prop==max(sub.cons$Consensus.Prop),]
			
			# If there are multiple species present, then no consensus
			if(length(unique(sub.cons$Consensus.Species))>1){
				video.cons.umbrella.duiker$Consensus.Type[i]<-"Conflicting consensus species"
				video.cons.umbrella.duiker$Consensus.Species[i]<-"no consensus"
			
			# If there's just one species present, then video reaches consensus on that species, at that max proportion, with the average Prop.Agreement
			} else {
				video.cons.umbrella.duiker$Consensus.Type[i]<-"Consensus reached"
				video.cons.umbrella.duiker$Consensus.Species[i]<-sub.cons$Consensus.Species[1]
				video.cons.umbrella.duiker$Consensus.Prop[i]<-sub.cons$Consensus.Prop[1]
				video.cons.umbrella.duiker$Prop.Agreement[i]<-mean(sub.cons$Prop.Agreement)

			}
		}
	# Otherwise whole video is presence undetermined	
	} else {
		video.cons.umbrella.duiker$Consensus.Species[i]<-"presence undetermined"
		video.cons.umbrella.duiker$Consensus.Type[i]<-"presence undetermined"
		
	}
}
end.time<-Sys.time()
end.time - start.time
#19 seconds

table(video.cons.umbrella.duiker$Consensus.Type, useNA="always")
# All nonblank clips are nonconsensus       Conflicting consensus species 
#                                 107                                  15 
#                     Consensus blank                   Consensus reached 
#                                3665                                8107 
#               presence undetermined                                <NA> 
#                                   2                                   0 

table(video.cons.umbrella.duiker$Consensus.Type, video.cons.umbrella.duiker$PE.Species=="blank")
#                                       FALSE TRUE
#   All nonblank clips are nonconsensus   101    6
#   Conflicting consensus species          15    0
#   Consensus blank                      1106 2559
#   Consensus reached                    8055   52
#   presence undetermined                   2    0


######## Remove presence undetermined videos for now
video.cons.umbrella.duiker<-video.cons.umbrella.duiker[video.cons.umbrella.duiker$Consensus.Species!="presence undetermined",]
nrow(video.cons.umbrella.duiker)
#11894


write.table(video.cons.umbrella.duiker, "your path here/consensus_per_video_umbrella_duiker_CandS_Species_ID_MS.txt", sep="\t", row.names=F, col.names=T)




#############################################
#                  umbrella                 #
#############################################

# Bring in pe data
pe.data<-read.table("your path here/pe_data_for_analysis_CandS_Species_ID_MS.txt", sep="\t", header=T)
nrow(pe.data)
#11896

######################## Find consensus species per clip
class.data<-read.table("your path here/class_data_for_analysis_CandS_Species_ID_MS.txt", sep="\t", header=T)
nrow(class.data)
#313505
class.data<-class.data[order(class.data$Clip.ID),]

# Create summ.data to house summaries for each clip
summ.data.umbrella<-data.frame(Clip.ID=sort(unique(class.data$Clip.ID)), Video.ID=NA, PE.Species=NA, Consensus.Species=NA, Consensus.Prop=NA, Prop.Agreement=NA, Number.Blanks=NA, Number.Nonblanks=NA)
summ.data.umbrella$Video.ID<-class.data$Video.ID[match(summ.data.umbrella$Clip.ID, class.data$Clip.ID)]
summ.data.umbrella$PE.Species<-pe.data$Umbrella.Species[match(summ.data.umbrella$Video.ID, pe.data$Video.ID)]
nrow(summ.data.umbrella)
#47584

consensus.threshold<-0.5

# Summarize the consensus per clip and proportion agreement to pe.id
start.time<-Sys.time()
for(i in 1:nrow(summ.data.umbrella)){
    # cat(i, round(i/47584*100, 2), "% \n")
	sub.class<-class.data[class.data$Clip.ID==summ.data.umbrella$Clip.ID[i],]
	sub.PE.Species<-pe.data$Umbrella.Species[pe.data$Video.ID==summ.data.umbrella$Video.ID[i]]

	Number.Blanks<-sum(sub.class$Umbrella.Species=="blank")
	Number.Nonblanks<-sum(sub.class$Umbrella.Species!="blank")

	summ.data.umbrella$Number.Blanks[i]<-Number.Blanks
	summ.data.umbrella$Number.Nonblanks[i]<-Number.Nonblanks
	
	# If the clip is only blanks, then species is blank
	if(Number.Nonblanks==0){
		summ.data.umbrella$Consensus.Species[i]<-"blank"
		summ.data.umbrella$Consensus.Prop[i]<-1
		summ.data.umbrella$Prop.Agreement[i]<-mean(sub.class$Umbrella.Species==sub.PE.Species)
		
	# If there are enough nonblanks to find a consensus, find a consensus using nonblank classifications only
	} else if(Number.Nonblanks>=4){
		sub.class<-sub.class[sub.class$Umbrella.Species!="blank",]	
		
		sp.table<-table(sub.class$Umbrella.Species)
		max.prop<-max(sp.table)/sum(sp.table)
		max.sp<-names(sp.table[sp.table==max(sp.table)])
		# If only one species has the maximum proportion of responses, and the prop is at least 0.5, then clip reaches consensus on that species
		if(length(max.sp)==1 & max.prop>=consensus.threshold){
			summ.data.umbrella$Consensus.Species[i]<-max.sp
			summ.data.umbrella$Consensus.Prop[i]<-max.prop
			summ.data.umbrella$Prop.Agreement[i]<-mean(sub.class$Umbrella.Species==sub.PE.Species)
		# Otherwise doesn't reach consensus
		} else {
			summ.data.umbrella$Consensus.Species[i]<-"no consensus"
		}
	
	# If there are too few nonblank clips, then it's presence undetermined
	} else if(Number.Nonblanks<=3){
			summ.data.umbrella$Consensus.Species[i]<-"presence undetermined"
	}
}
end.time<-Sys.time()
end.time - start.time
#20 mins

sum(is.na(summ.data.umbrella$Consensus.Species))
#0

table(summ.data.umbrella$Consensus.Species)
        #          bird                 blank            chimpanzee              elephant 
        #           288                 29360                   672                     6 
        #  hippopotamus                   hog         hoofed.animal                 human 
        #            75                   555                 11318                   881 
        #       leopard          no consensus   other (non-primate)       other (primate) 
        #            89                   230                   141                  1994 
        #      pangolin             porcupine presence undetermined                rodent 
        #            13                    26                  1763                   164 
        #     small cat 
        #             9 

write.table(summ.data.umbrella, "your path here/consensus_per_clip_umbrella_CandS_Species_ID_MS.txt", sep="\t", row.names=F, col.names=T)


######################## Find consensus species per video
clip.cons.data.umbrella<-read.table("your path here/consensus_per_clip_umbrella_CandS_Species_ID_MS.txt", sep="\t", header=T, stringsAsFactor=F)
nrow(clip.cons.data.umbrella)
#47584
length(unique(clip.cons.data.umbrella$Video.ID))
#11896

clip.cons.data.umbrella<-clip.cons.data.umbrella[order(clip.cons.data.umbrella$Video.ID),]

video.cons.umbrella<-data.frame(Video.ID=unique(clip.cons.data.umbrella$Video.ID), PE.Species=NA, Consensus.Species=NA, Consensus.Prop=NA, Prop.Agreement=NA, Consensus.Type=NA)
video.cons.umbrella$PE.Species<-clip.cons.data.umbrella$PE.Species[match(video.cons.umbrella$Video.ID, clip.cons.data.umbrella$Video.ID)]
nrow(video.cons.umbrella)
#12876

start.time<-Sys.time()
for(i in 1:nrow(video.cons.umbrella)){
    # cat(i, round(i/11896*100, 2), "% \n")
	sub.cons<-clip.cons.data.umbrella[clip.cons.data.umbrella$Video.ID==video.cons.umbrella$Video.ID[i],]
	
	# First remove any clips that are presence undetermined
	sub.cons<-sub.cons[sub.cons$Consensus.Species!="presence undetermined",]

	# If there are any remaining clips, get consensus
	if(nrow(sub.cons)>0){
		# If there are only blanks, then video is blank
		if(sum(sub.cons$Consensus.Species=="blank")==nrow(sub.cons)){
			video.cons.umbrella$Consensus.Type[i]<-"Consensus blank"
			video.cons.umbrella$Consensus.Species[i]<-"blank"

		# If there are only nonblank nonconsensus clips, don't continue, video is non consensus
		} else if(sum(sub.cons$Consensus.Species=="no consensus")==sum(sub.cons$Consensus.Species!="blank")){
			video.cons.umbrella$Consensus.Type[i]<-"All nonblank clips are nonconsensus"
			video.cons.umbrella$Consensus.Species[i]<-"no consensus"

		} else {
			# Remove any nonconsensus species and blanks
			sub.cons<-sub.cons[!sub.cons$Consensus.Species %in% c("no consensus","blank"),]
			
			# Keep only rows that have the highest consensus prop
			sub.cons<-sub.cons[sub.cons$Consensus.Prop==max(sub.cons$Consensus.Prop),]
			
			# If there are multiple species present, then no consensus
			if(length(unique(sub.cons$Consensus.Species))>1){
				video.cons.umbrella$Consensus.Type[i]<-"Conflicting consensus species"
				video.cons.umbrella$Consensus.Species[i]<-"no consensus"
			
			# If there's just one species present, then video reaches consensus on that species, at that max proportion, with the average Prop.Agreement
			} else {
				video.cons.umbrella$Consensus.Type[i]<-"Consensus reached"
				video.cons.umbrella$Consensus.Species[i]<-sub.cons$Consensus.Species[1]
				video.cons.umbrella$Consensus.Prop[i]<-sub.cons$Consensus.Prop[1]
				video.cons.umbrella$Prop.Agreement[i]<-mean(sub.cons$Prop.Agreement)

			}
		}
	# Otherwise whole video is presence undetermined	
	} else {
		video.cons.umbrella$Consensus.Species[i]<-"presence undetermined"
		video.cons.umbrella$Consensus.Type[i]<-"presence undetermined"
		
	}
}
end.time<-Sys.time()
end.time - start.time
#20 seconds

table(video.cons.umbrella$Consensus.Type, useNA="always")
# All nonblank clips are nonconsensus       Conflicting consensus species 
#                                  93                                  15 
#                     Consensus blank                   Consensus reached 
#                                3665                                8121 
#               presence undetermined                                <NA> 
#                                   2                                   0 

table(video.cons.umbrella$Consensus.Type, video.cons.umbrella$PE.Species=="blank")

#                                       FALSE TRUE
#   All nonblank clips are nonconsensus    89    4
#   Conflicting consensus species          15    0
#   Consensus blank                      1106 2559
#   Consensus reached                    8067   54
#   presence undetermined                   2    0



######## Remove presence undetermined videos for now
video.cons.umbrella<-video.cons.umbrella[video.cons.umbrella$Consensus.Species!="presence undetermined",]
nrow(video.cons.umbrella)
#11894


write.table(video.cons.umbrella, "your path here/consensus_per_video_umbrella_CandS_Species_ID_MS.txt", sep="\t", row.names=F, col.names=T)


