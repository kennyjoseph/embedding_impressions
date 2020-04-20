source("util.r")

###### Load in results from python code that generated position values
emb_res_from_python <- rbindlist(lapply(Sys.glob("measurement_results/*"),fread))
# Only use debiased embeddings to make a point at the end
emb_res_from_python <- emb_res_from_python[embedding != "hdb_w2v"]

## The Caliksan measure is really oddly distributed, see Kawin Ethayarajh's work. 
## We're going to drop it for this study
emb_res_from_python <- emb_res_from_python[fun != "caliksan_fun"]

# Only going to keep terms in all embeddings
n_emb <- length(unique(emb_res_from_python$embedding))
identities <- emb_res_from_python[, length(unique(embedding)), by = identity][V1 == n_emb]$identity
emb_res_from_python <- emb_res_from_python[identity %in% identities]

# Merge in the information about each measure
measure_info <- fread("measure_info.csv")
measure_info[, mkind := ifelse(paper == "this_short", 
                               "Survey-matched",
                               ifelse(paper == "this_long", 
                                      "Survey-augmented", "Related Work"))]
setnames(measure_info, "name", "dim_class")
emb_res_from_python  <- merge(emb_res_from_python, measure_info, by="ind")
setnames(emb_res_from_python, "name","dimension_end")
setnames(emb_res_from_python, "ind", "mid")

###### Double check opposite ends of dimension spectrum are indeed correlated perfectly at -1
ggplot(spread(emb_res_from_python[dimension_end %in% c("bad","good") 
                                  & embedding == "crawl-300d-2M-subword.vec" &
                                    mid == 0], dimension_end,value), aes(bad,good)) + geom_point() 

########### We're just going to look at the one end of the spectrum
#### (except for multiclass)
## Also note, we're excluding the Kozlowski et al. dimensions here, because 
ends_of_interest <- c("good"="evaluation",
                      "strong"="potency",
                      "active"="activity",
                      "old"="age",
                      "woman"="gender",
                      "politics"="politics",
                      "justice"="justice",
                      "medicine"="medicine",
                      "business"="business",
                      "education"="education",
                      "religion"="religion",
                      "family"="family",
                      "latino"="latino",
                      "asian"="asian",
                      "middle eastern"="middle eastern",
                      "black"="black",
                      "white"="white",
                      "open"="openness",
                      "conscientious"="conscientiousness",
                      "extroverted"="extroversion",
                      "agreeable"="agreeableness",
                      "neurotic"="neuroticism"
)
emb_res_from_python <- emb_res_from_python[dimension_end %in% names(ends_of_interest)]
emb_res_from_python[,dimension := mapvalues(dimension_end,names(ends_of_interest),
                                            unlist(as.vector(ends_of_interest)))]

emb_res_from_python$dimension <- factor(emb_res_from_python$dimension, 
                                        levels=unique(emb_res_from_python$dimension), 
                                        labels=str_to_title(unique(emb_res_from_python$dimension)))


#### OK, now have to Sync the metrics because the PCA can cause problems....

emb_res_spr <- spread(emb_res_from_python[,.(embedding,
                                             dimension,
                                             mkind,fun,identity,value,mid)], fun, value)

my_bin <- function(data, mapping, ..., low = "#132B43", high = "#56B1F7") {
  p <- ggplot(data = data, mapping = mapping)  + geom_point(alpha=.4)
  p <- p + stat_smooth()
  return(p)
}

# plt <- ggpairs(sample_n(emb_res_spr[, 6:12,with=F],20000),
#         lower = list(
#           continuous = my_bin
#         ),
#         upper = list(
#           continuous = wrap("cor", size = 6.75, alignPercent = 1)
#         ))
#ggsave("img/emb_cor_preclean.pdf",plt, h=10,w=10)

emb_res_spr <- spread(emb_res_from_python[,.(embedding,
                                             dimension,
                                             mkind,fun,identity,value,mid)], 
                      fun, value)

emb_res_spr[, bolukbasi_fun := align_measures(bolukbasi_fun, garg), by=.(mid,embedding,dimension)]
emb_res_spr[, ripa_bolukbasi_fun := align_measures(ripa_bolukbasi_fun, garg), by=.(mid,embedding,dimension)]
emb_res_spr[, kozlowski_fun := align_measures(kozlowski_fun, garg), by=.(mid,embedding,dimension)]
emb_res_spr[, ripa := align_measures(ripa, garg), by=.(mid,embedding,dimension)]
emb_res_spr[, ripa_kozlowski_fun := align_measures(ripa_kozlowski_fun, garg), by=.(mid,embedding,dimension)]
emb_res_spr[, swinger := align_measures(swinger, garg), by=.(mid,embedding,dimension)]

# plt <- ggpairs(sample_n(emb_res_spr[!embedding %in% c("gn_glove","hdb_w2v"), 6:12,with=F],20000),
#                lower = list(
#                  continuous = my_bin
#                ),
#                upper = list(
#                  continuous = wrap("cor", size = 6.75, alignPercent = 1)
#                ))
# ggsave("img/emb_cor_postclean.pdf",plt, h=10,w=10)

#### In general, a fairly strong correlation between the different dimensions
smean.cl.boot(data.table(melt(cor(emb_res_spr[, 6:12,with=F])))[Var1 != Var2]$value)

####Finalize Embedding data
rq1_dat <- melt(emb_res_spr, id = c("embedding","dimension","mid",
                                    "identity", "mkind"))
setnames(rq1_dat, "value","emb_measure")

#####################################################
######## SET UP SURVEY DATA #########################
#####################################################
# Garg data
garg_survey <- fread("survey_data/garg_mturk_stereotypes.csv")
setnames(garg_survey, c("identity","consensus"))
garg_survey[, consensus := rescale(consensus)]
garg_survey$source <- "Bolukbasi et al."
garg_survey$dimension <- "Gender"
garg_survey[, se := sd(consensus)/sqrt(nrow(garg_survey))]

# UGA EPA data
uga_data <- data.table(read.dta13("survey_data/FullCleanUGAData.dta"))
uga_data <- uga_data[grepl("i_",termID)]
uga_data <- uga_data[, .(termID,E,P,A)]
uga_data$termID <- sub("^i_","",uga_data$termID)
uga_data$termID <- stri_replace_all_fixed( tolower(uga_data$termID),"_", " ")
setnames(uga_data, c("identity","Evaluation","Potency","Activity"))
uga_data <- melt(uga_data, id= "identity")
setnames(uga_data, "variable","dimension")
uga_data[ , rescale_value := rescale(value, from=c(-4.3,4.3))]
uga_sd <- uga_data[!is.na(value), list(consensus=mean(rescale_value),
                                       disagree=sd(rescale_value),
                                       se = sd(rescale_value)/sqrt(.N)), by = .(identity,dimension)]
uga_sd$source <- "UGA ACT Data"

# Personality data
personality_survey <- rbindlist(lapply(c(Sys.glob("survey_data/personality-bias_survey/profession/*"),
                                         Sys.glob("survey_data/personality-bias_survey/nationality/*"),
                                         Sys.glob("survey_data/personality-bias_survey/test_data/*")), read_personality), use.names=F)
personality_survey <- personality_survey[Total > 10 & Profession != ""]
personality_survey[, disagree_scale := 1-consensus(c(`-3`,`-2`,`-1`,`0`,`1`,`2`,`3`)), by=1:nrow(personality_survey)]
personality_survey[, se :=sd(c(rep(0,`-3`),rep(1/6,`-2`),rep(2/6,`-1`),rep(.5,`0`),
                             rep(4/6,`1`),rep(5/6,`2`),rep(1,`3`)))/sqrt(Total), 
                   by=1:nrow(personality_survey)] 
personality_survey <- personality_survey[, .(Profession, Avg, dim,disagree_scale,se)]
personality_survey[ , Avg := rescale(Avg, from=c(-3,3))]
setnames(personality_survey, c("Profession","dim","Avg"), c("identity", "dimension","consensus"))
personality_survey$dimension <- str_to_title(personality_survey$dimension)
personality_survey$source <- "Agarwal Personality"

# Our survey data
data_df <- fread("survey_data/our_impression_data_clean.csv")
data_df$dimension <- str_to_title(data_df$dimension)
sd_scores <- data_df[, list(consensus=mean(rescaled_value),
                            disagree=sd(rescaled_value),
                            se=sd(rescaled_value)/sqrt(.N)), by=.(identity,dimension)]
sd_scores <-sd_scores[!is.na(consensus)]
sd_scores$source <- "This Paper"

survey_data <- rbind(sd_scores[,.(identity, dimension,consensus,disagree,source,se)],
                   uga_sd[,.(identity, dimension,consensus,disagree,source,se)],
                   personality_survey[,.(identity, dimension,consensus,disagree_scale,source,se)],
                   garg_survey[, .(identity, dimension,consensus,source,se)],
                   fill=T)
survey_data[, rescaled_disagree := ifelse(is.na(disagree),disagree_scale,disagree)]
survey_data[, rescaled_disagree := rescale(rescaled_disagree), by=.(dimension,source)]


#########################################
######### PLOT FIGURE 1 #################
########################################
df_plt <- copy(data_df)
df_plt$dimension <- factor(df_plt$dimension,levels=c("Activity", "Black", "Latino", "Middle Eastern", 
                                                      "Family", "Gender", "Evaluation", "Asian", "Education", 
                                                      "Medicine", "Business", "White", "Age", "Potency", 
                                                      "Politics", "Religion", "Justice"))
df_plt <- df_plt[identity%in%c("child","secretary","thug","judge")]
df_plt$identity <- factor(df_plt$identity, levels=c("thug","child","judge","secretary"))

df_plt <- df_plt[dimension %in% c("Gender","Evaluation","Potency", "Black")]
df_plt[, dim_name := mapvalues(dimension,c("Black","Gender","Evaluation","Potency"),
                           c("Black\n(not black--black)", 
                             "Gender\n(man--woman)",
                             "Evaluation\n(bad--good)",
                             "Potency\n(weak--strong)"))]
df_plt$dim_name <- factor(df_plt$dim_name, levels=rev(c("Black\n(not black--black)", 
                                                    "Gender\n(man--woman)",
                                                    "Evaluation\n(bad--good)",
                                                    "Potency\n(weak--strong)")))
pl <- ggplot(df_plt, 
             aes(x=rescaled_value,y=dim_name))  + facet_wrap(~identity,nrow=1) +
  geom_density_ridges(
    jittered_points = TRUE,
    position = position_points_jitter(width = 0.05, height = 0),
    point_shape = '|', point_size = 3, point_alpha = 1, alpha = 0.7,
  ) + ylab("Dimension")+ scale_x_continuous("Impression",limits=c(0,1), breaks=c(0,.5,1), 
                                            labels=c("","",""))
ggsave("img/survey_res.pdf", pl, h=4., w=8)


##################################################
######## MERGE EMBEDDING AND SURVEY DATA##########
##################################################

surv_rq1_mg <- merge(survey_data, rq1_dat,by = c("identity","dimension"))
surv_rq1_mg <- surv_rq1_mg[order(emb_measure)]
surv_rq1_mg[, emb_rank := 1:.N, by = .(dimension,embedding, variable,mid,source)]

surv_rq1_mg$mid <- factor(surv_rq1_mg$mid)

##################################################
######## LOOK AT DIMENSION-LEVEL QUESTION##########
##################################################


grpby <- c("dimension","source","embedding","mid","mkind","variable")
rq1 <- surv_rq1_mg[, list(corr= cor(consensus,emb_measure)), by= grpby]
rq1$mid <- relevel(rq1$mid, ref = "10")
rq1$dimension <- relevel(rq1$dimension, ref = "Politics")
rq1$variable <- relevel(rq1$variable, ref = "garg")
mod1 <- lm(corr~dimension+embedding+variable+mid,data=rq1[source == "This Paper"])
ag_dat <- copy(rq1[source == "Agarwal Personality"])
ag_dat$dimension <- relevel(ag_dat$dimension, ref = "Conscientiousness")
mod2 <- lm(corr~dimension+embedding+variable,data=ag_dat)
mod3 <- lm(corr~embedding+variable+mid,data=rq1[source == "Bolukbasi et al."])
mod4 <- lm(corr~dimension+embedding+variable+mid,data=rq1[source == "UGA ACT Data"])

res <- rbind(create_ci_dat(mod1,do_exp=F)[, source := "This Paper"],
             create_ci_dat(mod2,do_exp=F)[, source := "Agarwal Personality"],
             create_ci_dat(mod3,do_exp=F)[, source := "Bolukbasi et al."],
             create_ci_dat(mod4,do_exp=F)[, source := "UGA ACT Data"]
)

res$name <- tolower(res$name)
#write.table(res$name, row.names=F)
res <- merge(res,data.frame(name=names(rq1_name_map), 
                            title= unlist(as.vector(rq1_name_map))), 
             by="name",all.x=T)
res <- cbind(res, str_split_fixed(res$title,": ",2))
res <- res[res$name != "(Intercept)",]
setnames(res, c("V1","V2"), c("grp","base_title"))


res$grp <- factor(res$grp, levels= c("Embedding","Dimension-inducing\nWordset" ,"Word Position\nMeasure","Dimension"))
res$source <- factor(res$source, levels=unique(res$source),
                     labels= c("This Paper", "Robinson et al.", "Agarwal et al.", "Bolukbasi et al."))
p <-ggplot(res[!is.na(res$m),], aes(reorder(base_title,m), m,ymin=l,ymax=u, color =source ))
p <- p + geom_pointrange() + coord_flip() + facet_grid(grp~.,  scales="free_y",space="free_y")
p <- p + scale_color_discrete("Survey Data Source") +guides( color=guide_legend(nrow=2,byrow=TRUE)) 
p <- p + theme(legend.position = "bottom") + xlab("")
p <- p + ylab("Coefficient")
ggsave("img/rq1_reg.pdf",p, h=9.5, w=7)



############ SHOW CORRELATES WITH DIMENSION  #################################

# Collect the data
best_possible_conds = rq1[, .SD[order(-corr)][1,], by=.(dimension,source)]
best_possible_conds[, .(embedding,mid,corr,dimension,source)][order(-corr)]

ck_why_corr_data <- merge(surv_rq1_mg, best_possible_conds, by = grpby)

our_labeling_data <- fread("survey_data/our_labeling_data.csv")

mlt_labeling_data <- melt(our_labeling_data, id=c("QuestionType","Query","Answer"), 
            measure=paste0("Answer",1:5))
mlt_labeling_data <- mlt_labeling_data[Answer != "none" & value !="none"]
mlt_labeling_data[, is_y := Answer == value]

rescaled_survey_data <- rescale_survey_data(data_df)
rescaled_survey_data[identity == "police officer"]$identity <- "police_officer"

mlt_labeling_data <- merge(mlt_labeling_data, 
                           rescaled_survey_data, 
                           by.x="Query", by.y="identity")
mlt_labeling_data <- merge(mlt_labeling_data, 
                           rescaled_survey_data, 
                           by.x="value", by.y="identity")
for(n in grep("[.]x",names(mlt_labeling_data),value=T)){ 
  subs <- substr(n, 1, nchar(n)-2)
  mlt_labeling_data$V1 <- (mlt_labeling_data[,get(n)] - 
                             mlt_labeling_data[,get(paste0(subs,".y"))])**2
  setnames(mlt_labeling_data, "V1", subs)
}
setnames(mlt_labeling_data, "Middle Eastern", "Middle_Eastern")

# Regression models for the separate question types
mod_isa <- glm(as.formula(paste0("is_y~",
                                 paste(names(mlt_labeling_data)[41:57], 
                                       collapse="+"), "")),
               mlt_labeling_data[QuestionType == "IsA"], 
               family=binomial)
mod_sw <- glm(as.formula(paste0("is_y~",
                                paste(names(mlt_labeling_data)[41:57], 
                                      collapse="+"), 
                                "")), 
              mlt_labeling_data[QuestionType != "IsA"], 
              family=binomial)

# Get results and (robust) confidence intervals
ci_isa <- create_ci_dat(mod_isa,do_exp = F)
ci_sw <- create_ci_dat(mod_sw, do_exp = F)
ci_isa$ty <- "IsA"
ci_sw$ty <- "SeenWith"
ci_mods <- rbind(ci_isa,ci_sw)
ci_mods$name <- factor(ci_mods$name, 
                       levels=ci_mods[ty=="IsA", mean(m), by = name][order(V1)]$name)
ci_mods[, inv_m := m]


## Plot for Appendix looking at Salience across SeenWith/IsA questions
spa1 <- spread(ci_mods[,.(m,ty,name)],ty,m)
setnames(spa1,c("name","mI","mS"))
spa2 <- spread(ci_mods[,.(l,ty,name)],ty,l)
setnames(spa2,c("name","lI","lS"))
spa3 <- spread(ci_mods[,.(u,ty,name)],ty,u)
setnames(spa3,c("name","uI","uS"))
spa <- merge(merge(spa1,spa2), spa3)
pz <- ggplot(spa, aes(mI,mS,label=name))+geom_point() + geom_text_repel() + geom_hline(yintercept = 0,color='red',linetype='dashed') + geom_vline(xintercept = 0,color='red',linetype='dashed') + geom_errorbarh(aes(xmin=lI,xmax=uI),color='blue',alpha=.5) + geom_errorbar(aes(ymin=lS,ymax=uS),color='blue',alpha=.5)
pz <- pz + xlab("Coefficient for IsA Questions") + ylab("Coefficient for SeenWith Questions")
ggsave("img/supplementary_salience.pdf", pz, h=4, w=5)


# Plot in paper comparing Figure 3 coefficients with Regression coefficients
sp <- spread(ci_mods[,.(inv_m,ty,name)],ty,inv_m)
sp[, Max := max(c(abs(IsA),abs(SeenWith))),by=name]
merged <- merge(sp, 
                ck_why_corr_data[source == "This Paper", 
                                  cor(emb_measure,consensus),by=dimension], 
                by.x="name",by.y="dimension")
merged <- merge(merged,ck_why_corr_data[source == "This Paper", 
                                        var(consensus), by = dimension],
                by.x="name", by.y="dimension")
df <- data.table(dimension=names(coef(mod1)),v=as.vector(coef(mod1)))
df <- df[grepl("dimension",dimension)]
df[, dimension := sub("dimension","",dimension)]
df <- rbind(df, data.table(dimension="Politics",v=0))
merged <- merge(merged, df, by.x="name", by.y="dimension")
setnames(merged, c("name","Similarity\nSalience",
                   "Associative\nSalience",
                   "Maximum Importance\nin IsA or SeenWith\nLabeling Questions",
                   "Correlation",
                   "Variance",
                   "Coefficient in\nFigure 3"))
mt <- melt(merged[,c(1,4,6,7),with=F], 
           id= c("name","Variance"))
mt$variable <- factor(mt$variable, levels=rev(levels(mt$variable)))
plt <- ggplot(mt, aes(`Variance`, value,label=name)) +
  geom_point() + stat_cor(color='red',aes(label = paste(..r.label..))) + 
  geom_text_repel() + facet_wrap(~variable, scales="free_y") + ylab("") + 
  stat_smooth(method='lm')
plt
ggsave("img/why_dim.pdf", plt, h=4, w=8)


################ IMPRESSION LEVEL  ###################


gen_rank <- function(dat){
  dat <- data.table(dat)
  d <- data.frame()
  for(i in 1:nrow(dat)){
    id_df <- dat[i,]
    ident <- id_df$identity[1]
    id_rank <- id_df$emb_rank
    nonid <- dat[-i,]
    all_below <- nonid[ (consensus + se) < (id_df$consensus - id_df$se)]
    all_above <- nonid[ (consensus - se) > (id_df$consensus + id_df$se)]
    d <- rbind(d, data.frame(N_low=nrow(all_below),
                             cor_low=nrow(all_below[emb_rank < id_rank]),
                             N_above=nrow(all_above),
                             cor_above= nrow(all_above[emb_rank > id_rank]),
                             identity=ident))
  }
  return(d)
}

#### Note: this line takes a while!
r <- surv_rq1_mg %>%
      group_by(dimension,embedding,mid,mkind,variable,source)  %>%
      group_modify(~gen_rank(.x))


cpr <- copy(r)
r$mid <- factor(r$mid)
r2 <- merge(r,surv_rq1_mg, by = c(grpby,"identity"))
r2 <- data.table(r2)
r2[, dist_from_median := consensus - median(consensus), by=grpby]
r2[, cor := cor_low + cor_above]
r2[, N := N_low + N_above]
r2[, perc := cor/N]
word_info <- fread("~/git/buffalo/acl_bias/word_dat.csv")
r2 <- merge(r2, word_info, by = "identity")
r2[, log_freq := log(freq)]
r2[is.infinite(log_freq)]$log_freq <- min(r2[!is.infinite(log_freq)]$log_freq)

mr <- merge(r2, best_possible_conds, by = grpby)
mr[, disagree_plot := ifelse(is.na(disagree), disagree_scale, disagree)]

pl <- ggplot(mr[source=='This Paper'], aes(dist_from_median,perc)) + geom_point(size=3,alpha=.4) + stat_smooth(size=1.3,method='gam', formula=y~s(x))+ scale_y_continuous("% Correctly Ranked (Above or Below)", labels=percent, limits=c(0,1)) + xlab("Distance from Median Impression") 
assoc1 <- ggplot(mr[dimension %in% c("Black")], aes(dist_from_median,perc,color = disagree,label=ifelse(abs(perc-.6) > .22,identity,NA))) + geom_point(size=3,alpha=1) + scale_color_gradientn("Std. Dev.", colors=c("blue","light blue","grey","orange","red")) + stat_smooth(method='gam', formula=y~s(x,k=5),se=F)  + geom_text_repel(color='black')+ scale_y_continuous("", labels=percent) + xlab("Distance from Median Impression")
assoc1 <- assoc1 + theme(legend.position = "none")

ggsave("img/impression_level_paper.pdf",ggarrange(pl, assoc1), h=4,w=8)

mr$synset_count <- log(mr$synset_count+1)
mod1a <- gam(perc~dimension+s(dist_from_median,k=5)+ s(log_freq,k=5) + s(synset_count,k=5) + s(disagree,k=5),
             family='binomial',weights=N,
             data=mr[source == "This Paper"])
pdf("img/this_best.pdf")
par(mfrow=c(2,2))
plot(mod1a)
dev.off()

mod2a <- gam(perc~s(dist_from_median,k=5)+ s(log_freq,k=5) + s(synset_count,k=5) ,
             family='binomial',weights=N,
             data=mr[source == "Bolukbasi et al."])
pdf("img/bol_best.pdf")
par(mfrow=c(2,2))
plot(mod2a)
dev.off()


mod3a <- gam(perc~dimension+s(dist_from_median,k=5)+ s(log_freq,k=5) + s(synset_count,k=5) + s(disagree_scale,k=5),
             family='binomial',weights=N,
             data=mr[source == "Agarwal Personality"])
pdf("img/ag_best.pdf")
par(mfrow=c(2,2))
plot(mod3a)
dev.off()

mod4a <- gam(perc~dimension+s(dist_from_median,k=5)+ s(log_freq,k=5) + s(synset_count,k=5) + s(disagree,k=5),
             family='binomial',weights=N,
             data=mr[source == "UGA ACT Data"])
pdf("img/act_best.pdf")
par(mfrow=c(2,2))
plot(mod4a)
dev.off()


############# Debiasing might not work for other classes of identities
emb_res <- fread("measurement_results/hdb_w2v.csv")[name == "man" & 
                                  fun == "garg" & ind ==30 & 
                                  ! identity %in% c("father","brother","son",
                                                    "daughter","mother","sister")]
plt_dat <- merge(data_df[dimension=="Gender",
                         .(rescaled_value,identity)][, mean(rescaled_value),by=identity],
                 emb_res, by="identity")
pz <- ggplot(plt_dat, 
             aes(value, V1,label= identity,color = identity %in% garg_survey$identity)) + geom_point() + stat_smooth(method='lm',se=F, size=1.2, alpha=.3)  + stat_cor() +geom_text_repel()  + theme(legend.position = 'none')
ggsave("~/Dropbox/Kenny/papers/current/lcss/acl2019-latex/deb.pdf", pz, h=5, w=6)


