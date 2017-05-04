require(ggplot2)
require(png)

########################
#HBUG GO TERM FULL GRAPH
#JHR 
#Created:05/02/2017
#Last Update:05/03/2017

#####User Input Variables##############################################################################################################
#Plot
plotcolors=c('#d8b365','#5ab4ac')
ylabelsize='20'
xlabelsize='20'
yticksize='12'
xticksize='12'

#Legend
legendname="Transcript Sets"
legendlabels=c("Gold-tier","All PUTs")
legendtitlesize='20'
legendlabelsize='14'
legendposition=c(.95,.90) #grid system, (1,1) is top right

#Export as .png parameters
multiplotwidth=8000
multiplotheight=5000
#######################################################################################################################################


####Load Data####
fullbp <- read.table('fullbp.txt', sep='\t', header=TRUE)
fullcc <- read.table('fullcc.txt', sep='\t', header=TRUE)
fullmf <- read.table('fullmf.txt', sep='\t', header=TRUE)



###Name wrangling###
#Some names are very long, so lets split them up... 
#Don't mess up any of the '; GO' parts, as the next bit uses it as a search pattern and inserts a newline in between

#bp
levels(fullbp$Go_category)<-gsub('regulation of transcription, DNA-templated; GO:0006355', 'regulation of transcription,\nDNA-templated; GO:0006355', levels(fullbp$Go_category))
levels(fullbp$Go_category)<-gsub('endoplasmic reticulum membrane; GO:0005789', 'endoplasmic\nreticulum membrane; GO:0005789', levels(fullbp$Go_category))
levels(fullbp$Go_category)<-gsub('intracellular protein transport; GO:0006886', 'intracellular\nprotein transport; GO:0006886', levels(fullbp$Go_category))
levels(fullbp$Go_category)<-gsub('transcription, DNA-templated; GO:0006351', 'transcription,\nDNA-templated; GO:0006351', levels(fullbp$Go_category))
levels(fullbp$Go_category)<-gsub('carbohydrate metabolic process; GO:0005975', 'carbohydrate\nmetabolic process; GO:0005975', levels(fullbp$Go_category))

#cc
levels(fullcc$Go_category)<-gsub('integral component of membrane; GO:0016021', 'integral component\nof membrane; GO:0016021', levels(fullcc$Go_category))
levels(fullcc$Go_category)<-gsub('integral component of membrane; GO:0016021', 'integral component\nof membrane; GO:0016021', levels(fullcc$Go_category))
levels(fullcc$Go_category)<-gsub('endoplasmic reticulum membrane; GO:0005789', 'endoplasmic\nreticulum membrane; GO:0005789', levels(fullcc$Go_category))
levels(fullcc$Go_category)<-gsub('intracellular protein transport', 'intracellular\nprotein transport', levels(fullcc$Go_category))

#mf
levels(fullmf$Go_category)<-gsub('', '', levels(fullmf$Go_category))
levels(fullmf$Go_category)<-gsub('structural constituent of ribosome; GO:0003735', 'structural constituent\nof ribosome; GO:0003735', levels(fullmf$Go_category))
levels(fullmf$Go_category)<-gsub('transcription factor activity, sequence-specific DNA binding; GO:0003700', 'transcription factor activity,\nsequence-specific DNA binding; GO:0003700', levels(fullmf$Go_category))

###Newlines for names and GO IDs, do this after the other name wrangling
levels(fullbp$Go_category)<-gsub('; GO', ';\nGO', levels(fullbp$Go_category))
levels(fullcc$Go_category)<-gsub('; GO', ';\nGO', levels(fullcc$Go_category))
levels(fullmf$Go_category)<-gsub('; GO', ';\nGO', levels(fullmf$Go_category))



##########Create Bar Plots##########

####FULLBP####
#Top, has a legend, no x axis label
#Base plot
fullbpplot<-ggplot(fullbp,aes(x=reorder(Go_category, -Percent), y=Percent, fill=Group)) + 
  geom_bar(stat="identity", position=position_dodge())

#Add some Jazz to it...
#First remove grey background, doing this later messes things up...
fullbpplot<-fullbpplot+ theme_bw()

#Label axes
fullbpplot<-fullbpplot+xlab(' ') + ylab('Biological Process')+ theme(axis.title.y=element_text(size=ylabelsize))

#Change axis tick sizes...
fullbpplot<-fullbpplot+theme(axis.text.x=element_text(size=xticksize))+theme(axis.text.y=element_text(size=yticksize))

#Enter fill and legend modifications
fullbpplot<-fullbpplot +theme(legend.position=legendposition)+scale_fill_manual(values=plotcolors, name=legendname,labels=legendlabels)+
  theme(legend.title=element_text(size=legendtitlesize))+ theme(legend.text=element_text(size=legendlabelsize))

#Change background, outlines
fullbpplot<-fullbpplot+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank())+
  theme(axis.line = element_line(color = 'black'))


#####SLIMCC####
#Middle, has no legend or x axis label
#Base plot
fullccplot<-ggplot(fullcc,aes(x=reorder(Go_category, -Percent), y=Percent, fill=Group)) + 
  geom_bar(stat="identity", position=position_dodge())

#Add some Jazz to it...
#First remove grey background, doing this later messes things up...
fullccplot<-fullccplot+theme_bw()

#Label axes
fullccplot<-fullccplot+xlab(' ')+ylab('Cellular Component')+theme(axis.title.y=element_text(size=ylabelsize))

#Change tick sizes...
fullccplot<-fullccplot+theme(axis.text.x=element_text(size=xticksize))+theme(axis.text.y=element_text(size=xticksize))

#Enter fill and legend modifications-No legend here for multiplot
fullccplot<-fullccplot+scale_fill_manual(values=plotcolors)

#Change background, outlines
fullccplot<-fullccplot+guides(fill=FALSE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank())+
  theme(axis.line = element_line(color = 'black')) 


#####SLIMMF####
#Base plot
#Bottom, has no legend, does have x axis label
fullmfplot<-ggplot(fullmf,aes(x=reorder(Go_category, -Percent), y=Percent, fill=Group)) + 
  geom_bar(stat="identity", position=position_dodge())

#Add some Jazz to it...  
#First remove grey background, doing this later messes things up...
fullmfplot<-fullmfplot+theme_bw()

#Label axes
fullmfplot<-fullmfplot+xlab('GO Category')+ theme(axis.title.x=element_text(size=xlabelsize)) + ylab('Molecular Function')+ theme(axis.title.y=element_text(size=ylabelsize))

#Change tick sizes...
fullmfplot<-fullmfplot+theme(axis.text.x=element_text(size=xticksize))+theme(axis.text.y=element_text(size=yticksize))

#Enter fill and legend modifications-No legend here for multiplot
fullmfplot<-fullmfplot+scale_fill_manual(values=plotcolors)

#Change background, outlines
fullmfplot<-fullmfplot+guides(fill=FALSE)+
  theme(panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.border= element_blank())+
  theme(axis.line = element_line(color = 'black'))



####Open png file####
png(filename = "fullall.png", width = multiplotwidth, height = multiplotheight,
    units = "px",pointsize=12, bg="white", res=300)

####MULTIPLOT####
#Enter the plots into the multiplot function
multiplot(fullbpplot, fullccplot, fullmfplot, cols=1)

####Close png file####
dev.off()
