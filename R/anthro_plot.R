

anthro.plot<-
  ggplot(data = sum2, aes(x = clim, y = lenper*100)) +
  geom_bar(aes(fill = dicat), stat = "identity", position = "stack")+
  scale_fill_manual(values = c("#91cf60","#91bfdb","#fc8d59"), name = "Likely change\nin discharge") + 
  facet_grid(mo~SMC_Name, switch="x") + xlab("") + ylab("% stream-length") + 
  # ggtitle("% stream-length") +
  scale_y_continuous(breaks=c(50,100))+
  theme_minimal( base_family = 'serif')+
  theme(legend.position = "bottom",
        panel.background = element_blank(),
        # panel.border = element_rect(colour = 'gray80', fill = NA, size = 0.5),
        # panel.grid = element_blank(), #panel.background = element_rect(color="gray80",fill=NULL),
        strip.background   = element_blank(), 
        strip.text.y= element_text(angle=0, hjust=0, vjust=0.5),
        strip.placement = "outside",
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8))


ggsave(anthro.plot, filename = "anthro.plot.tiff", dpi=300, height=6, width=5)


head(sum2)

library(reshape2)
write.table(dcast(data=sum1, SMC_Name+mo~clim+flocat, value.var="len"), file="clipboard", sep="\t", row.names=F)
write.table(dcast(data=sum2, SMC_Name+mo~clim+dicat, value.var="len"), file="clipboard", sep="\t", row.names=F)

ggplot(data = sum2[which(sum2$mo %in% c("Jan","Apr","Jul","Oct") ),], aes(x = clim, y = len)) +
  geom_bar(aes(fill = dicat), stat = "identity", position = "stack")+
  scale_fill_manual(values = c("#91cf60","#91bfdb","#fc8d59"), name = "Likely change\nin discharge") + 
  facet_grid(mo~SMC_Name) + xlab("") + ylab("Total stream-length (km)") + 
  ggtitle("Anthropogenic flow") + 
  theme(legend.position = "bottom")

