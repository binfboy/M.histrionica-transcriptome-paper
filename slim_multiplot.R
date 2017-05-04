require(ggplot2)
require(png)

########################
#HBUG GO TERM SLIM GRAPH
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
slimbp <- read.table('slimbp.txt', sep='\t', header=TRUE)
slimcc <- read.table('slimcc.txt', sep='\t', header=TRUE)
slimmf <- read.table('slimmf.txt', sep='\t', header=TRUE)



####Name wrangling####
#Some names are very long, so lets split them up... 
#Don't mess up any of the '; GO' parts, as the next bit uses it as a search pattern and inserts a newline in between

levels(slimbp$Go_category)<-gsub('component organization', 'component\norganization', levels(slimbp$Go_category))

###Newlines for names and GO IDs, do this after the other name wrangling
levels(slimbp$Go_category)<-gsub('; GO', ';\nGO', levels(slimbp$Go_category))
levels(slimcc$Go_category)<-gsub('; GO', ';\nGO', levels(slimcc$Go_category))
levels(slimmf$Go_category)<-gsub('; GO', ';\nGO', levels(slimmf$Go_category))



##########Create Bar Plots##########

####SLIMBP####
#Top, has a legend, no x axis label
#Base plot
slimbpplot<-ggplot(slimbp,aes(x=reorder(Go_category, -Percent), y=Percent, fill=Group)) + 
  geom_bar(stat="identity", position=position_dodge()) + theme_bw()

#Add some Jazz to it...
#First remove grey background, doing this later messes things up...
slimbpplot<-slimbpplot + theme_bw()

#Label axes
slimbpplot<-slimbpplot+xlab(' ')+ylab('Biological Process')+theme(axis.title.y=element_text(size=ylabelsize))

#Change axis tick sizes...
slimbpplot<- slimbpplot+ theme(axis.text.x=element_text(size=xticksize))+theme(axis.text.y=element_text(size=yticksize))

#Enter fill and legend modifications
slimbpplot<-slimbpplot+theme(legend.position=legendposition)+scale_fill_manual(values=plotcolors, name="Transcript Sets",labels=c("Gold-tier","All PUTs"))+
  theme(legend.title=element_text(size=legendtitlesize))+theme(legend.text=element_text(size =legendlabelsize))

#Change background, outlines
slimbpplot<-slimbpplot+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank())+
  theme(axis.line = element_line(color = 'black'))


#####SLIMCC####
#Middle, has no legend or x axis label
#Base plot
slimccplot<-ggplot(slimcc,aes(x=reorder(Go_category,-Percent),y=Percent,fill=Group)) + 
  geom_bar(stat="identity",position=position_dodge())

#Add some Jazz to it...
#First remove grey background, doing this later messes things up...
slimccplot<-slimccplot+theme_bw()

#Label axes
slimccplot<-slimccplot+xlab(' ')+ylab('Cellular Component')+theme(axis.title.y=element_text(size=20))

#Change tick sizes...
slimccplot<-slimccplot+theme(axis.text.x=element_text(size=xticksize))+theme(axis.text.y=element_text(size=yticksize))

#Enter fill and legend modifications-No legend here for multiplot
slimccplot<-slimccplot+scale_fill_manual(values=plotcolors)
                                             
#Change background, outlines
slimccplot<-slimccplot+guides(fill=FALSE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank())+
  theme(axis.line = element_line(color = 'black')) 
  

#####SLIMMF####
#Base plot
slimmfplot<-ggplot(slimmf,aes(x=reorder(Go_category,-Percent),y=Percent,fill=Group)) + 
  geom_bar(stat="identity", position=position_dodge())

#Add some Jazz to it...  
#First remove grey background, doing this later messes things up...
slimmfplot<-slimmfplot+theme_bw()

#Label axes
slimmfplot<-slimmfplot+xlab('GO Category')+ theme(axis.title.x=element_text(size=xlabelsize))+ylab('Molecular Function')+theme(axis.title.y=element_text(size=ylabelsize))

#Change tick sizes...
slimmfplot<-slimmfplot+theme(axis.text.x=element_text(size=xticksize))+theme(axis.text.y=element_text(size=yticksize))

#Enter fill and legend modifications-No legend here for multiplot
slimmfplot<-slimmfplot+scale_fill_manual(values=plotcolors)

#Change background, outlines
slimmfplot<-slimmfplot +  guides(fill=FALSE)+
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),panel.border=element_blank())+
  theme(axis.line = element_line(color = 'black'))


####Open png file####
png(filename = "slimall.png", width=multiplotwidth, height=multiplotheight,
    units = "px",pointsize=12, bg="white", res=300)

####MULTIPLOT####
#Enter the plots into the multiplot function
multiplot(slimbpplot, slimccplot, slimmfplot, cols=1)

####Close png file####
dev.off()



