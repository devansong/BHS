###########################################################
###########################################################
###########################################################
################Anne Devan-Song############################
################Code for Laliberte et al. BHS##############
###########2022, Bend, OR##################################
###########################################################
###########################################################
#######Code to Plot age distribution#######################


graphics.off()
rm(list = ls()) # clear working directory
library(ggplot2)
df <- read.csv("fulldataset.csv")


#Code lamb data for plotting
nolamb <- df[is.na(df$LambDelta),]
nrow(nolamb) #73
nolamb$lamb_PA <- "Lamb.none"
nolamb$lamb_bin <- '1'

deadlamb <- subset(df, LambDelta == "TRUE") 
nrow(deadlamb) #34
deadlamb$lamb_PA <- "Lamb.dead"
deadlamb$lamb_bin <- '2'

livelamb <- subset(df, LambDelta == "FALSE" & LambTime == 8) 
nrow(livelamb) #9
livelamb$lamb_PA <- "Lamb.live"
livelamb$lamb_bin <- '2'

unklamb <- subset(df, LambDelta == "FALSE" & LambTime <8)
nrow(unklamb) #18
unklamb$lamb_PA <- "Lamb.unk"
unklamb$lamb_bin <- '2'

df <- rbind(nolamb, deadlamb, livelamb, unklamb)


colnames(df)

newdf <- df[, c("Capture.ID", 
            "Animal.ID", 
            "pop", 
            "age", 
            "sex",
            "lamb_bin")]
mal <- subset(newdf, sex=="male")
mal$Group <- "Male"
fwlamb <- subset(newdf, sex=="female" & lamb_bin==2)
fwlamb$Group <- "F w lamb"
fwolamb <- subset(newdf, sex=="female" & lamb_bin==1)
fwolamb$Group <- "F w/o lamb"

finaldf <- rbind(mal, fwlamb, fwolamb)


plot <- ggplot(finaldf, aes(x=Group, y = age, color=Group))+ 
  geom_jitter(width=0.1, height=0, alpha=0.5, size=3.5) + 
  scale_color_manual(values=c("#bc5090", "#003f5c", "#ffa600"))+
  facet_wrap(~pop) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        legend.position="none", 
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  xlab("")+ 
  scale_y_continuous(breaks = round(seq(min(0), max(10), by = 2),10), limits=c(0.5,11.5))+
    ylab("Age (years)")

png("Age_dist_pop.png", units="in", width=5, height=5, res=300)
plot
dev.off()
