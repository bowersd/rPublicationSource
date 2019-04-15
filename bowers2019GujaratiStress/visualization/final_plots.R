library(ggplot2)
library(tidyr)
library(dplyr)
library(gridExtra)
library(ellipse)
library(plyr)

##data is available upon request
guj = read.table("/home/sautedman/sideProjects/gujarati/stress/flashStickBackup/master/master.tsv", header=TRUE, sep="\t")

#{subsetting, variable management
#vowels need to be rendered in IPA in plots, but are in ascii in guj$Vowel
guj$v = factor(guj$Vowel, levels = c("a", "o", "e", "u", "i", "A"))
levels(guj$v) = list("\u0251"="a", "o"="o", "e"="e", "u"="u", "i"="i", "\u0259" = "A")

#all core initial stress items + others (used in F0 comparison in initial stress hypothesis)
gujinit = guj[guj$StressHyp=='initial' | guj$Word %in% 
                    list( 
                         "amboro",
                         "daabori",
                         "naarangi",
                         "naaraajgi",
                         "jaamburo",
                         "limburi",
                         "parikshaa",
                         "kavitaa",
                         "daagino",
                         "nagino",
                         "hoshiyaar"), ]

gujinit$SyllPos = factor(gujinit$SyllPos, levels = c("initial", "medial", "final"))
levels(gujinit$SyllPos) = c(1,2,3) 

gujinit = gujinit %>% group_by(SpeakerId) %>% mutate(min_max=min(F0_max), min_min=min(F0_min))

#subset of data for f1-f2 plot in initial stress condition
initmost = guj[(guj$StressHyp == "initial"|guj$Word %in% list("amboro", "daabori")) , ]
initmostF = guj[(guj$StressHyp == "initial"|guj$Word %in% list("amboro", "daabori")) & guj$Gender == "female", ]
initmostM = guj[(guj$StressHyp == "initial"|guj$Word %in% list("amboro", "daabori")) & guj$Gender == "male", ]
#} subsetting, variable management end

#{figure 1
extrap_f0_min <- ggplot(gujinit, aes(x=as.integer(SyllPos), y=F0_min-min_min, color=Vowel)) +
    geom_smooth(method="loess", se=F, span = 1, size=0.5)+
    scale_x_continuous(breaks=c(1, 2, 3))+
    ylab('F0 minimum (relative to lowest value for speaker)')+
    xlab('Syllable Position')+
    facet_wrap(Gender ~ SpeakerId)

pdf("/home/sautedman/publications/rPublicationSource/bowers2019GujaratiStress//visualization/images/extrapolatedf0.pdf")
grid.arrange(extrap_f0_min)
dev.off()
#} figure 1 end

#{figure 5

#library(ggplot2)
##library(cowplot)
#library(tidyr)
#library(dplyr)
#library(gridExtra)

#need to plot ellipses, means, and environment labels for vowel categories
pos_df_ell <- data.frame()
for(g in levels(initmost$v)){
pos_df_ell <- rbind(pos_df_ell, cbind(as.data.frame(with(initmost[initmost$v==g,], ellipse(cor(F2_Hz, F1_Hz), 
                                         scale=c(sd(F2_Hz),sd(F1_Hz)), 
                                         centre=c(mean(F2_Hz),mean(F1_Hz))))),group=g))
}

#v_ellipses = data.frame()
#for(x in levels(initmost$v)){
#    for(y in levels(initmost$SyllPos)){
#        for(z in levels(initmost$Gender)){
#            print(v_ellipses)
#            v_ellipses = rbind(v_ellipses, cbind(as.data.frame(with(initmost[initmost$v==x & 
#                                                                    initmost$SyllPos==y & 
#                                                                    initmost$Gender==z,], ellipse(cor(F2_Hz, F1_Hz),
#                                                                                                  scale=c(sd(F2_Hz, sd(F1_Hz)),
#                                                                                                          centre=c(mean(F2_Hz),mean(F1_Hz)))))), group=x))
#        }
#    }
#}


pos_df_lab = data.frame(
	F2 = c(650, 750, 1080,  2900, 
               #female labels
               1770, 1730, 1700,
               2880, 2800, 2300,
               #male labels
               1450, 1440, 1420,
               2500, 2340, 2300),
	F1 = c(350, 500, 750,  275, 
               #female labels
               990, 890, 820,
               340, 380, 410,
               #male labels
               750, 710, 680,
               320, 365, 410),
	label = c("u", "o",  "\u0251",  "i", 
                  #female labels
                  "#_", "\u0259_", "u_",
                  "#_", "o/\u0251_", "",
                  #male labels
                  "#_", "\u0259_", "u_",
                  "#_", "o_", "\u0251_"),
	v = c("u", "o",  "\u0251",  "i", 
              "", "", "",
              "", "", "", 
              "", "", "",
              "", "", "" )
	)

pos_means_f = ddply(initmostF, .(v, SyllPos), summarize,
	F1_sd = sqrt(var(F1_Hz)),
	F2_sd = sqrt(var(F2_Hz)),
	F1_se = sqrt(var(F1_Hz))/length(F1_Hz), 
	F2_se = sqrt(var(F2_Hz))/length(F2_Hz),
	F1_Hz = mean(F1_Hz),
	F2_Hz = mean(F2_Hz))

pos_means_m = ddply(initmostM, .(v, SyllPos), summarize,
	F1_sd = sqrt(var(F1_Hz)),
	F2_sd = sqrt(var(F2_Hz)),
	F1_se = sqrt(var(F1_Hz))/length(F1_Hz), 
	F2_se = sqrt(var(F2_Hz))/length(F2_Hz),
	F1_Hz = mean(F1_Hz),
	F2_Hz = mean(F2_Hz))

f1f2init = ggplot(initmost, aes(x=F2_Hz, y=F1_Hz, group=v)) +
	geom_point(shape=1, aes(color=v)) +
	ggtitle("F1 and F2")+
        theme(plot.title = element_text(hjust=0.5)) +
	ylab("F1 (Hz)") +
	xlab("F2 (Hz)") +
        coord_cartesian(ylim=c(240,1300), xlim=c(500,3100))+
	scale_y_reverse() + scale_x_reverse() +
	#ellipses
	geom_path(data=pos_df_ell, aes(x=x, y=y, group=group), size=0.5, linetype = 2) +
	geom_text(data = pos_df_lab, aes(x=F2, y=F1, label=label)) +
	#means and error bars
	geom_point(data = pos_means_f, aes(shape = SyllPos),  size=3, color = "dark gray") +
	scale_shape_manual(values=c(1, 2,0), name="Syllable Position ") +
	geom_errorbarh(data = pos_means_f, aes(xmin=F2_Hz - F2_sd, xmax=F2_Hz + F2_sd, y = F1_Hz, height = 0.01), color = "dark gray") +
	geom_errorbar(data = pos_means_f, aes(ymin=F1_Hz - F1_sd, ymax=F1_Hz + F1_sd, x = F2_Hz), color = "dark gray") +
	geom_point(data = pos_means_m, aes(shape = SyllPos),  size=3, fill = NA) +
	geom_errorbarh(data = pos_means_m, aes(xmin=F2_Hz - F2_sd, xmax=F2_Hz + F2_sd, y = F1_Hz, height = 0.01), color = "black") +
	geom_errorbar(data = pos_means_m, aes(ymin=F1_Hz - F1_sd, ymax=F1_Hz + F1_sd, x = F2_Hz), color = "black") 

#cairo_pdf("/home/sautedman/publications/rPublicationSource/bowers2019GujaratiStress/visualization/images/all-formants-position.pdf")
f1f2init
#dev.off()

#}figure 5 end

#{figure 6
#}figure 6 end

#{figure 7
#}figure 7 end

#{figure 8
#}figure 8 end
