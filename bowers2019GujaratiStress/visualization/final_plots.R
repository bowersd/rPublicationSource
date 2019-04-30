library(ggplot2)
library(tidyr)
library(plyr)
library(dplyr)
library(gridExtra)
library(ellipse)

##data is available upon request
guj = read.table("/home/sautedman/sideProjects/gujarati/stress/flashStickBackup/master/master.tsv", header=TRUE, sep="\t")

#{subsetting, variable management
#vowels need to be rendered in IPA in plots, but are in ascii in guj$Vowel
guj$v = factor(guj$Vowel, levels = c("a", "o", "e", "u", "i", "A"))
levels(guj$v) = list("\u0251"="a", "o"="o", "e"="e", "u"="u", "i"="i", "\u0259" = "A")

guj$SyllPos = factor(guj$SyllPos, levels = c("initial", "medial", "final"))
levels(guj$SyllPos) = c(1,2,3) 

#all core initial stress items + others (used in F0 comparison in initial stress hypothesis)
initall = guj[guj$StressHyp=='initial' | guj$Word %in% 
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

initall = initall %>% group_by(SpeakerId) %>% mutate(min_max=min(F0_max), min_min=min(F0_min))

#subset of data for f1-f2 plot in initial stress condition
initmost = guj[(guj$StressHyp == "initial"|guj$Word %in% list("amboro", "daabori")) , ]
initmost_f = guj[(guj$StressHyp == "initial"|guj$Word %in% list("amboro", "daabori")) & guj$Gender == "female", ]
initmost_m = guj[(guj$StressHyp == "initial"|guj$Word %in% list("amboro", "daabori")) & guj$Gender == "male", ]

initcore = guj[guj$StressHyp == "initial", ]

son = guj[guj$StressHyp == "sonority" & guj$StressVal != "controversial" , ]
son_m = guj[guj$StressHyp == "sonority" & guj$StressVal != "controversial" & guj$Gender == "male", ]
son_f = guj[guj$StressHyp == "sonority" & guj$StressVal != "controversial" & guj$Gender == "female", ]
#} subsetting, variable management end

#{figure 1
extrap_f0_min <- ggplot(initall, aes(x=as.integer(SyllPos), y=F0_min-min_min, color=Vowel)) +
    theme_bw()+
    geom_smooth(method="loess", se=F, span = 1, size=0.5)+
    scale_x_continuous(breaks=c(1, 2, 3))+
    ylab('F0 minimum (relative to lowest value for speaker)')+
    xlab('Syllable Position')+
    facet_wrap(Gender ~ SpeakerId)

pdf("/home/sautedman/publications/rPublicationSource/bowers2019GujaratiStress/visualization/images/extrapolatedf0.pdf")
grid.arrange(extrap_f0_min)
dev.off()
#} figure 1 end

#{figure 5 and accompanying female plot (appendix)
##{auxiliary dataframes
#need to plot ellipses, means, and environment labels for vowel categories
pos_df_ell_m <- data.frame()
for(g in levels(initmost_m$v)){
  pos_df_ell_m <- rbind(pos_df_ell_m, cbind(as.data.frame(with(initmost_m[initmost_m$v==g,], ellipse(cor(F2_Hz, F1_Hz), 
                                                                                             scale=c(sd(F2_Hz),sd(F1_Hz)), 
                                                                                             centre=c(mean(F2_Hz),mean(F1_Hz))))),group=g))
}

pos_df_lab_m = data.frame(
  F2 = c(650, 750, 1020,  2650, 
         #male syllable position labels
         1450, 1440, 1420,
         2500, 2340, 2300),
  F1 = c(350, 500, 750,  275, 
         #male syllable position labels
         760, 720, 690,
         340, 365, 410),
  label = c("u", "o",  "\u0251",  "i", 
            #male environment labels
            "#_", "\u0259_", "u_",
            "#_", "o_", "\u0251_"),
  v = c("u", "o",  "\u0251",  "i", 
        "", "", "",
        "", "", "")
)

pos_means_m = ddply(initmost_m, .(v, SyllPos), summarize,
	F1_sd = sqrt(var(F1_Hz)),
	F2_sd = sqrt(var(F2_Hz)),
	F1_se = sqrt(var(F1_Hz))/length(F1_Hz), 
	F2_se = sqrt(var(F2_Hz))/length(F2_Hz),
	F1_Hz = mean(F1_Hz),
	F2_Hz = mean(F2_Hz))

pos_df_ell_f <- data.frame()
for(g in levels(initmost_f$v)){
  pos_df_ell_f <- rbind(pos_df_ell_f, cbind(as.data.frame(with(initmost_f[initmost_f$v==g,], ellipse(cor(F2_Hz, F1_Hz), 
                                                                                                     scale=c(sd(F2_Hz),sd(F1_Hz)), 
                                                                                                     centre=c(mean(F2_Hz),mean(F1_Hz))))),group=g))
}

pos_df_lab_f = data.frame(
  F2 = c(620, 750, 1150,  3000, 
         #female syll position labels
         1770, 1730, 1700,
         2880, 2800, 2300),
  F1 = c(350, 500, 750,  400, 
         #female syll position labels
         1000, 900, 840,
         310, 400, 410),
  label = c("u", "o",  "\u0251",  "i", 
            #female environment labels
            "#_", "\u0259_", "u_",
            "#_", "o/\u0251_", ""),
  v = c("u", "o",  "\u0251",  "i", 
        "", "", "",
        "", "", "")
)

pos_means_f = ddply(initmost_f, .(v, SyllPos), summarize,
                    F1_sd = sqrt(var(F1_Hz)),
                    F2_sd = sqrt(var(F2_Hz)),
                    F1_se = sqrt(var(F1_Hz))/length(F1_Hz), 
                    F2_se = sqrt(var(F2_Hz))/length(F2_Hz),
                    F1_Hz = mean(F1_Hz),
                    F2_Hz = mean(F2_Hz))
##}auxiliary dataframes end

##{plots
f1f2init_m = ggplot(initmost_m, aes(x=F2_Hz, y=F1_Hz, group=v)) +
  theme_bw() +
  geom_point(shape=1, aes(color=v)) +
  ylab("F1 (Hz)") +
  xlab("F2 (Hz)") +
  scale_color_discrete(name = "Vowel") + 
  #coord_cartesian(ylim=c(240,1300), xlim=c(500,3100))+
  scale_y_reverse() +
  scale_x_reverse() +
  #ellipses
  geom_path(data=pos_df_ell_m, aes(x=x, y=y, group=group), size=0.5, linetype = 2) +
  geom_text(data = pos_df_lab_m, aes(x=F2, y=F1, label=label)) +
  #means and error bars
  scale_shape_manual(values=c(1, 2,0), name="Position ") +
  geom_point(data = pos_means_m, aes(shape = SyllPos),  size=3, fill = NA) +
  geom_errorbarh(data = pos_means_m, aes(xmin=F2_Hz - F2_sd, xmax=F2_Hz + F2_sd, y = F1_Hz, height = 0.01), color = "black") +
  geom_errorbar(data = pos_means_m, aes(ymin=F1_Hz - F1_sd, ymax=F1_Hz + F1_sd, x = F2_Hz), color = "black") 


f1f2init_f = ggplot(initmost_f, aes(x=F2_Hz, y=F1_Hz, group=v)) +
  theme_bw() +
  geom_point(shape=1, aes(color=v)) +
  ylab("F1 (Hz)") +
  xlab("F2 (Hz)") +
  scale_color_discrete(name = "Vowel") + 
  #coord_cartesian(ylim=c(240,1300), xlim=c(500,3100))+
  scale_y_reverse() +
  scale_x_reverse() +
  #ellipses
  geom_path(data=pos_df_ell_f, aes(x=x, y=y, group=group), size=0.5, linetype = 2) +
  geom_text(data = pos_df_lab_f, aes(x=F2, y=F1, label=label)) +
  #means and error bars
  geom_point(data = pos_means_f, aes(shape = SyllPos),  size=3, color = "black") +
  scale_shape_manual(values=c(1, 2,0), name="Position ") +
  geom_errorbarh(data = pos_means_f, aes(xmin=F2_Hz - F2_sd, xmax=F2_Hz + F2_sd, y = F1_Hz, height = 0.01), color = "black") +
  geom_errorbar(data = pos_means_f, aes(ymin=F1_Hz - F1_sd, ymax=F1_Hz + F1_sd, x = F2_Hz), color = "black")
##} plots end

##{write out
cairo_pdf("/home/sautedman/publications/rPublicationSource/bowers2019GujaratiStress/visualization/images/formants-position-male.pdf", width=5, height=5)
f1f2init_m
dev.off()

cairo_pdf("/home/sautedman/publications/rPublicationSource/bowers2019GujaratiStress/visualization/images/formants-position-female.pdf", width=5, height=5)
f1f2init_f
dev.off()
##}write out end
#}figure 5 and accompanying female plot (appendix) end

#{figure 6
dur_pos = ggplot(data=initcore, aes(x=v, y=Duration_ms, fill = SyllPos)) +
		geom_boxplot() +
		scale_y_continuous(name ="Duration (ms)") +
    scale_x_discrete(name = "Vowel") + 
    scale_fill_discrete(name="Position") +
		theme_classic()

cairo_pdf("/home/sautedman/publications/rPublicationSource/bowers2019GujaratiStress/visualization/images/duration-position.pdf", width=5, height=4)
dur_pos
dev.off()
#}figure 6 end

#{figure 7
##{auxiliary dataframes (labels, ellipses, etc)
str_means_m = ddply(son_m, .(v, StressVal), summarize,
	F1_sd = sqrt(var(F1_Hz)),
	F2_sd = sqrt(var(F2_Hz)),
	F1_se = sqrt(var(F1_Hz))/length(F1_Hz), 
	F2_se = sqrt(var(F2_Hz))/length(F2_Hz),
	F1_Hz = mean(F1_Hz),
	F2_Hz = mean(F2_Hz))

df_ell_m <- data.frame()
for(g in levels(son_m$v)){
df_ell_m <- rbind(df_ell_m, cbind(as.data.frame(with(son_m[son_m$v==g,], ellipse(cor(F2_Hz, F1_Hz), 
                                         scale=c(sd(F2_Hz),sd(F1_Hz)), 
                                         centre=c(mean(F2_Hz),mean(F1_Hz))))),group=g))
}

df_lab_m = data.frame(
	F2 = c(650, 690, 1060, 1240, 2250, 2700,
               720, 820, 1180, 1360, 2000, 2500,#u
               730, 790, 1200, 1540, 2030, 2500 #s
               ),
	F1 = c(350, 440, 560, 750, 470, 355,
               390, 430, 520, 760, 470, 355, #u
               365, 450, 585, 750, 450, 335  #s
               ),
	label = c("u", "o", "\u0259", "\u0251", "e", "i",
                  "\u0251_", "_i", "u_", "#_", "\u0251_", "\u0251_",
                  "i_", "_o", "\u0259_", "", "\u0259_", "\u0259_"  ),
	v = c("u", "o", "\u0259", "\u0251", "e", "i")
	)

str_means_f = ddply(son_f, .(v, StressVal), summarize,
                    F1_sd = sqrt(var(F1_Hz)),
                    F2_sd = sqrt(var(F2_Hz)),
                    F1_se = sqrt(var(F1_Hz))/length(F1_Hz), 
                    F2_se = sqrt(var(F2_Hz))/length(F2_Hz),
                    F1_Hz = mean(F1_Hz),
                    F2_Hz = mean(F2_Hz))

df_ell_f <- data.frame()
for(g in levels(son_f$v)){
  df_ell_f <- rbind(df_ell_f, cbind(as.data.frame(with(son_f[son_f$v==g,], ellipse(cor(F2_Hz, F1_Hz), 
                                                                                 scale=c(sd(F2_Hz),sd(F1_Hz)), 
                                                                                 centre=c(mean(F2_Hz),mean(F1_Hz))))),group=g))
}

df_lab_f = data.frame(
  F2 = c(650, 690, 1200, 1480, 2750, 3000,
         720, 820, 1400, 1620, 1970, 2480,#u
         730, 790, 1500, 1540, 1970, 2480 #s
  ),
  F1 = c(350, 440, 650, 1000, 500, 280,
         390, 430, 610, 925, 430, 340, #u
         365, 450, 670, 750, 395, 315  #s
  ),
  label = c("u", "o", "\u0259", "\u0251", "e", "i",
            "", "", "u_", "#_", "\u0251_", "\u0251_",
            "", "", "\u0259_", "", "\u0259_", "\u0259_"  ),
  v = c("u", "o", "\u0259", "\u0251", "e", "i")
)
##}auxiliary data frames end

##{plots
f1f2_m = ggplot(son_m, aes(x=F2_Hz, y=F1_Hz, group=v)) +
  theme_bw() +
	geom_point(shape=1, aes(color=v)) +
  scale_color_discrete(name="Vowel") +
	#axes, etc
	ylab("F1 (Hz)") +
	xlab("F2 (Hz)") +
	scale_y_reverse() +
  scale_x_reverse() +
	#ellipses
	geom_path(data=df_ell_m, aes(x=x, y=y, group=group), size=0.5, linetype = 2) +
	geom_text(data = df_lab_m, aes(x=F2, y=F1, label=label)) +
	#means and error bars
	geom_point(data = str_means_m, aes(shape = StressVal),  size=4, fill = "black") +
	scale_shape_manual(values=c(21, 24), name="Stress Value") +
	geom_errorbarh(data = str_means_m, aes(xmin=F2_Hz - F2_sd, xmax=F2_Hz + F2_sd, y = F1_Hz, height = 0.01), color = "black") +
	geom_errorbar(data = str_means_m, aes(ymin=F1_Hz - F1_sd, ymax=F1_Hz + F1_sd, x = F2_Hz), color = "black")

f1f2_f = ggplot(son_f, aes(x=F2_Hz, y=F1_Hz, group=v)) +
  theme_bw() +
  geom_point(shape=1, aes(color=v)) +
  scale_color_discrete(name="Vowel") +
  #axes, etc
  ylab("F1 (Hz)") +
  xlab("F2 (Hz)") +
  scale_y_reverse() +
  scale_x_reverse() +
  #ellipses
  geom_path(data=df_ell_f, aes(x=x, y=y, group=group), size=0.5, linetype = 2) +
  geom_text(data = df_lab_f, aes(x=F2, y=F1, label=label)) +
  #means and error bars
  geom_point(data = str_means_f, aes(shape = StressVal),  size=4, fill = "black") +
  scale_shape_manual(values=c(21, 24), name="Stress Value") +
  geom_errorbarh(data = str_means_f, aes(xmin=F2_Hz - F2_sd, xmax=F2_Hz + F2_sd, y = F1_Hz, height = 0.01), color = "black") +
  geom_errorbar(data = str_means_f, aes(ymin=F1_Hz - F1_sd, ymax=F1_Hz + F1_sd, x = F2_Hz), color = "black")
##} plots end

##{write out
cairo_pdf("/home/sautedman/publications/rPublicationSource/bowers2019GujaratiStress/visualization/images/formants-sonority-male.pdf", width=5, height=5)
f1f2_m
dev.off()

cairo_pdf("/home/sautedman/publications/rPublicationSource/bowers2019GujaratiStress/visualization/images/formants-sonority-female.pdf", width=5, height=5)
f1f2_f
dev.off()
##}write out end

#}figure 7 end

#{figure 8
dur_str = ggplot(data=son, aes(x=v, y=Duration_ms, fill = StressVal)) +
		geom_boxplot() +
		scale_y_continuous(name ="Duration (ms)") + 
                scale_x_discrete(name = "Vowel") + 
		scale_fill_discrete(name = "Stress Value") + 
		theme_classic()

cairo_pdf("/home/sautedman/publications/rPublicationSource/bowers2019GujaratiStress/visualization/images/duration-sonority.pdf", width=5, height=4)
dur_str
dev.off()
#}figure 8 end
