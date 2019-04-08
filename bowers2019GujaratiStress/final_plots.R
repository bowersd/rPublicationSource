library(ggplot2)
library(plyr)
library(ellipse)
library(cowplot)
library(tidyr)
library(dplyr)
library(gridExtra)

#data is available upon request
guj = read.table("/home/sautedman/sideProjects/gujarati/stress/flashStickBackup/master/master.tsv", header = TRUE, sep = "\t")

#{figure 1
gujallinit = guj[guj$StressHyp=='initial' | guj$Word %in% 
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
gujallinit$SyllPos = factor(gujallinit$SyllPos, levels = c("initial", "medial", "final"))
levels(gujallinit$SyllPos) = c(1,2,3) 

pFemalemin <- ggplot(gujallinit[gujallinit$Gender=="female", ] , aes(x=as.integer(SyllPos), y=F0_min-min_min, color=Vowel)) +
    geom_point(shape=1) +
    geom_smooth(method="loess", se=F, span = 1)+
    scale_x_continuous(breaks=c(1, 2, 3))+
    ylab('F0 minimum (relative to lowest value for speaker)')+
    xlab('Syllable Position')+
    ggtitle("Female Participants")+
    theme(plot.title = element_text(hjust=0.5))+
    facet_grid(. ~ SpeakerId)

pMalemin <- ggplot(gujallinit[gujallinit$Gender=="male", ] , aes(x=as.integer(SyllPos), y=F0_min-min_min, color=Vowel)) +
    geom_smooth(method="loess", se=F, span = 1, size=0.5)+
    scale_x_continuous(breaks=c(1, 2, 3))+
    ylab('F0 minimum (relative to lowest value for speaker)')+
    xlab('Syllable Position')+
    ggtitle("Male Participants")+
    theme(plot.title = element_text(hjust=0.5))+
    facet_wrap( ~ SpeakerId, nrow=3)

pdf("/home/sautedman/sideProjects/gujarati/stress/flashStickBackup/visualization/color/extrapolatedf0.pdf")
grid.arrange(pMalemin, pFemalemin, nrow=2)
dev.off()

#}end figure 1

#{figure 5

#vowels need to be rendered in IPA in plots, but are in ascii in guj$Vowel
guj$v = factor(guj$Vowel, levels = c("a", "o", "e", "u", "i", "A"))
levels(guj$v) = list("\u0251"="a", "o"="o", "e"="e", "u"="u", "i"="i", "\u0259" = "A")

#subset of data for f1-f2 plot in initial stress condition
initmost = guj[(guj$StressHyp == "initial"|guj$Word %in% list("amboro", "daabori")) , ]
initmostF = guj[(guj$StressHyp == "initial"|guj$Word %in% list("amboro", "daabori")) & guj$Gender == "female", ]
initmostM = guj[(guj$StressHyp == "initial"|guj$Word %in% list("amboro", "daabori")) & guj$Gender == "male", ]

#need to plot ellipses, means, and environment labels for vowel categories
pos_df_ell <- data.frame()
for(g in levels(initmost$v)){
pos_df_ell <- rbind(pos_df_ell, cbind(as.data.frame(with(initmost[initmost$v==g,], ellipse(cor(F2_Hz, F1_Hz), 
                                         scale=c(sd(F2_Hz),sd(F1_Hz)), 
                                         centre=c(mean(F2_Hz),mean(F1_Hz))))),group=g))
}

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
	theme_bw() +
	geom_point(shape=1, aes(color=Gender)) +
        scale_color_manual(values = c("light gray", "dark gray"))+
	#axes, etc
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

cairo_pdf("/home/sautedman/sideProjects/gujarati/stress/flashStickBackup/visualization/grayscale/all-formants-position.pdf")
f1f2init
dev.off()

#}figure 5 end
