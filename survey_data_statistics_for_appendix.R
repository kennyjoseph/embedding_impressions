source("util.r")

# Our survey data
data_df <- fread("survey_data/our_impression_data_clean.csv")
data_df$dimension <- str_to_title(data_df$dimension)


##############################################
############ Compare our survey data to UGA EPA data######
###########################################
epa_test <- data_df[qtype=="affective"][, as.list(smean.cl.boot(value)),by=.(identity,dimension)]

uga_data <- data.table(read.dta13("survey_data/FullCleanUGAData.dta"))
uga_data <- uga_data[grepl("i_",termID)]
uga_data <- uga_data[, .(termID,E,P,A)]
uga_data$termID <- sub("^i_","",uga_data$termID)
uga_data$termID <- stri_replace_all_fixed( tolower(uga_data$termID),"_", " ")
setnames(uga_data, c("identity","Evaluation","Potency","Activity"))

uga_epa <- melt(uga_data[identity %in% epa_test$identity],
                id="identity" )[!is.na(value)][, as.list(smean.cl.boot(value)), .(identity,variable)]
uga_epa$data <- "Prior Study"
setnames(uga_epa, "variable","dimension")

epa_test <- epa_test[,.(identity,dimension,Mean,Lower,Upper)]
epa_test$data <- "Current Study"

to_plot <- rbind(uga_epa,epa_test,use.names=F)
theme_set(theme_light(16))
p <- ggplot(to_plot, aes(reorder_within(identity,Mean,dimension),
                         Mean, ymin=Lower,ymax=Upper,color=data)) + 
  geom_pointrange(position=position_dodge(.5)) + 
  facet_wrap(~dimension, scales="free") + 
  coord_flip() + 
  scale_x_reordered() + 
  ylab("Mean value over survey respondents") + 
  xlab("Identity")
ggsave("img/epa_survey.pdf",h=12,w=11)
################################


z <- data_df[qtype == "association"][, as.list(smean.cl.boot(value)),by=.(identity,qentity)]
z$color <- "black"
z[qentity == "business" & identity %in% c("executives", "consultants", "secretaries", "interns", "bankers", "bosses")]$color <- "red"
z[qentity == "medicine" & identity %in% c("doctors","physicians","surgeons","nurses","patients","dentists")]$color <- "red"
z[qentity == "justice" & identity %in% c("judges","criminals","lawyers","witnesses","cops","police officers")]$color <- "red"
z[qentity == "family" & identity %in% c("brothers","sisters","daughters","sons","fathers","mothers")]$color <- "red"
z[qentity == "politics" & identity %in% c("conservatives","democrats","liberals","republicans","politicians","senators")]$color <- "red"

pt <- ggplot(z, aes(reorder_within(identity,Mean,qentity), Mean,ymin=Lower,ymax=Upper,color=color)) + geom_pointrange() + facet_wrap(~qentity,scales="free", nrow=2) + scale_x_reordered() + coord_flip()
pt <- pt + ylab("Mean value over survey respondents") +  xlab("Identity")
pt <- pt + scale_color_manual(values=c("black"="black","red"="red")) + theme(legend.position = "none")
ggsave("img/association_survey.pdf", h=25,w=15)
#############


z <- data_df[dimension == "Age"][, as.list(smean.cl.boot(value)),
                               by=.(identity,dimension)]
p_age <- ggplot(z, aes(reorder_within(identity,Mean,dimension), 
                       Mean,ymin=Lower,ymax=Upper)) + 
                geom_pointrange() + 
                facet_wrap(~dimension,scales="free", nrow=2) + 
                scale_x_reordered() + 
                coord_flip() + 
                ylab("Expected Age\nAccording to Survey Data") +  
                xlab("Identity")

z <- data_df[dimension == "Gender"][, as.list(smean.cl.boot(value)),
                                 by=.(identity,dimension)]
p_gend <- ggplot(z, aes(reorder_within(identity,Mean,dimension), 
                        Mean,ymin=Lower,ymax=Upper)) + 
  geom_pointrange() + 
  facet_wrap(~dimension,scales="free", nrow=2) + 
  scale_x_reordered() + 
  coord_flip() + 
  xlab("Identity") + 
  scale_y_continuous("Expected Gender\nAccording to Survey Data",
                    limits=c(min(z$Lower),max(z$Upper)), 
                    breaks = c(min(z$Lower)+.25,0,max(z$Upper)-.25),
                    labels=c("All\nMale","Gender\nNeutral","All\nFemale"))
ggsave("img/age_gend.pdf", plot_grid(p_age,p_gend), h=12.5,w=10)

z <- data_df[qtype == "trait" 
             & !dimension %in% c("Gender","Age")][, 
                                                  as.list(smean.cl.boot(value)),
                                                  by=.(identity,dimension)]

pt <- ggplot(z, aes(reorder_within(identity,Mean,dimension), 
                    Mean/100,ymin=Lower/100,ymax=Upper/100)) + 
  geom_pointrange() + 
  facet_wrap(~dimension,scales="free_y", nrow=1) + 
  scale_x_reordered() + 
  coord_flip() + 
  geom_hline(yintercept=.2,color='red',size=1.2) + 
  ylab("Mean value over survey respondents") +  
  xlab("Identity") + 
  scale_y_continuous("Expected % of People Holding Identity that\nare this Race According to Survey Data",
                    labels=percent)
ggsave("img/trait_survey.pdf", h=12.5,w=18)



