#ANALYSis DATASET REPLICATION CUSKLEY ET AL. 2015

#=============================================
#---------------------------------------------
# 1. SETTING WORKING DIRECTORY & LOADING FILES
# --------------------------------------------
#=============================================

#---laptop
setwd("C:/Users/u0112465/OneDrive - KU Leuven/Onderzoek/Sterkewerkwoorden/Experiment_Cuskley")


#---loading files
d <- read.csv("Dataset_replication_Cuskleyetal_cleaned.csv")

#---loading libraries
library(dplyr)
library(lme4)
library(ggplot2)
library(effects)
library(vcd)
library(lsr)
#=============================================
#---------------------------------------------
# 2. PREPARATION ANALYSIS
# --------------------------------------------
#=============================================

#---drop data
d <- droplevels(filter(d, Inflection!="double")) #only keep strong and weak attestations
d <- droplevels(filter(d, Age>16)) #only keep participants >16
d <- droplevels(filter(d, Native_language!="other")) #drop participants with different native language than Dutch or French
d <- droplevels(filter(d, Finished==TRUE)) #only participants who finished
d <- droplevels(filter(d, !is.na(Nativeness))) #only participants for who we have information on Nativeness

#---exploring data
#t.test Age
b <- d %>%
  select(ResponseId, Gender, Age,Nativeness) %>%
  distinct(ResponseId, .keep_all=TRUE)

Age_native <- b$Age[b$Nativeness=="native"]
Age_non_native <- b$Age[b$Nativeness=="non_native"]

t.test(Age_native, Age_non_native, paired=FALSE,
       var.equal=TRUE)
cohensD(Age ~ Nativeness,
        data = b)

#Chi-square gender
b2 <- droplevels(filter(b, Gender!="Other"))
(t <- table(b2$Nativeness, b2$Gender))   # absolute frequencies

(fit <- chisq.test(t, correct=FALSE))  # chi-squared test

fit$observed                           # observed frequencies (again) 
fit$expected                           # expected frequencies
fit$residuals                          # cell-wise residuals 

#---center & standardize variables
d$cType_frequency_vowel <- scale(d$Type_frequency_vowel, scale=TRUE, center=TRUE)
d$cTrial_number <- scale(d$Trial_number, scale=TRUE, center=TRUE)
d$cAge <- scale(d$Age, scale=TRUE, center=TRUE)

#=============================================
#---------------------------------------------
# 3. ANALYSIS
# --------------------------------------------
#=============================================

#---Strong vs. weak (native & non-native speakers)

#Full model
fit <- glmer(Inflection ~ Nativeness +  lag_response + cType_frequency_vowel*Nativeness + cTrial_number  +
               (1+Nativeness|Verb) + (1|ResponseId), family=binomial, data=d)

#Leave out correlated random slope
d$Nativeness_b <- model.matrix(fit)[,2]

fit2 <- glmer(Inflection ~ Nativeness +  lag_response + cType_frequency_vowel*Nativeness + cTrial_number + 
                (Nativeness_b||Verb) + (1|ResponseId), family=binomial, data=d)

anova(fit, fit2) #fit2 better

#Leave out random slope Nativeness
fit3 <- glmer(Inflection ~ Nativeness +  lag_response + cType_frequency_vowel*Nativeness + cTrial_number + 
                (1|Verb) + (1|ResponseId), family=binomial, data=d)

anova(fit2, fit3) #fit2 better

#Leave out random intercept response id
#response id weglaten
fit4 <- glmer(Inflection ~ Nativeness +  lag_response + cType_frequency_vowel*Nativeness + cTrial_number +
                (Nativeness_b|Verb) , family=binomial, data=d)

anova(fit2, fit4) #fit2 better

#AFTER REVIEWS: Add age to model
fit2b <- glmer(Inflection ~ Nativeness +  Age + lag_response + cType_frequency_vowel*Nativeness + cTrial_number + 
                (Nativeness_b||Verb) + (1|ResponseId), family=binomial, data=d)
#AFTER REVIEWS: Add gender to model
d1 <- droplevels(filter(d, Gender!="Other"))
fit2c <- glmer(Inflection ~ Nativeness +  Gender + lag_response + cType_frequency_vowel*Nativeness + cTrial_number + 
                (Nativeness_b||Verb) + (1|ResponseId), family=binomial, data=d)

#Interaction vowel infinitive
fit_A <- glmer(Inflection ~ Nativeness +  lag_response + Present_stem_vowel*Nativeness + cTrial_number  +
               (1+Nativeness|Verb) + (1|ResponseId), family=binomial, data=d)
#leave out correlated random slope
fit_B <- glmer(Inflection ~ Nativeness +  lag_response + Present_stem_vowel*Nativeness + cTrial_number  +
                 (Nativeness_b||Verb) + (1|ResponseId), family=binomial, data=d)
anova(fit_A, fit_B) #fit_B is better
#leave out  random slope
fit_C <- glmer(Inflection ~ Nativeness +  lag_response + Present_stem_vowel*Nativeness + cTrial_number  +
                 (1|Verb) + (1|ResponseId), family=binomial, data=d)
anova(fit_B, fit_C) #fit_B is better, but doesn't converge
#add optimizer
fit_D <- glmer(Inflection ~ Nativeness +  lag_response + Present_stem_vowel*Nativeness + cTrial_number  +
                 (Nativeness_b|Verb) + (1|ResponseId), family=binomial, data=d, control=glmerControl(optimizer = "bobyqa"))

#---Strong vs. weak (non-native speakers only)
#Filter dataset
d_L2 <- droplevels(filter(d, Nativeness=="non_native"))

#Center & standardize variables
d_L2$cDutch_AoA <- scale(d_L2$Dutch_AoA, scale=TRUE, center=TRUE)
d_L2$cDutch_proficiency <- scale(d_L2$Dutch_proficiency, scale=TRUE, center=TRUE)
d_L2$cType_frequency_vowel <- scale(d_L2$Type_frequency_vowel, scale=TRUE, center=TRUE)
d_L2$cTrial_number <- scale(d_L2$Trial_number, scale=TRUE, center=TRUE)

#Full model
fit_a <- glmer(Inflection ~ lag_response + cDutch_proficiency + cType_frequency_vowel + cTrial_number + cDutch_AoA + English_L2_high_proficiency + German_L2_high_proficiency +
               (1 + cDutch_proficiency|Verb) + (1 + cDutch_AoA|Verb)+ (1 + English_L2_high_proficiency|Verb)+ (1 + German_L2_high_proficiency|Verb)+ (1|ResponseId), family=binomial, data=d_L2)
#Leave out random slope English_L2_high_proficiency
fit_b <- glmer(Inflection ~ lag_response + cDutch_proficiency + cType_frequency_vowel + cTrial_number + cDutch_AoA + English_L2_high_proficiency + German_L2_high_proficiency +
                 (1 + cDutch_proficiency|Verb) + (1 + cDutch_AoA|Verb)+ (1 + German_L2_high_proficiency|Verb)+ (1|ResponseId), family=binomial, data=d_L2)

anova(fit_a, fit_b) #fit_b better

#Leave out random slope German_L2_high_proficiency
fit_c <- glmer(Inflection ~ lag_response + cDutch_proficiency + cType_frequency_vowel + cTrial_number + cDutch_AoA + English_L2_high_proficiency + German_L2_high_proficiency +
                 (1 + cDutch_proficiency|Verb) + (1 + cDutch_AoA|Verb)+ (1|ResponseId), family=binomial, data=d_L2)

anova(fit_b, fit_c) #fit_c better

#Leave out random slope cDutch_AoA
fit_d <- glmer(Inflection ~ lag_response + cDutch_proficiency + cType_frequency_vowel + cTrial_number + cDutch_AoA + English_L2_high_proficiency + German_L2_high_proficiency +
(1 + cDutch_proficiency|Verb) +  (1|ResponseId), family=binomial, data=d_L2)

anova(fit_c, fit_d) #fit_d better

#Leave out random slope cDutch_proficiency
fit_e <- glmer(Inflection ~ lag_response + cDutch_proficiency + cType_frequency_vowel + cTrial_number + cDutch_AoA + English_L2_high_proficiency + German_L2_high_proficiency +
                  (1|ResponseId), family=binomial, data=d_L2)

anova(fit_d, fit_e) #fit_d better

#Leave out correlated random slope cDutch_profiency

fit_f <- glmer(Inflection ~ lag_response + cDutch_proficiency + cType_frequency_vowel + cTrial_number + cDutch_AoA + English_L2_high_proficiency + German_L2_high_proficiency +
                 (0 + cDutch_proficiency|Verb) +  (1|ResponseId), family=binomial, data=d_L2)

anova(fit_d, fit_f) #fit_d better
#Leave out German_L2_high_proficiency
fit_g <- glmer(Inflection ~ lag_response + cDutch_proficiency + cType_frequency_vowel + cTrial_number + cDutch_AoA + English_L2_high_proficiency +
                 (1 + cDutch_proficiency|Verb) +  (1|ResponseId), family=binomial, data=d_L2)

anova(fit_d, fit_g) #fit_g better

#Leave out English_L2_high_proficiency
fit_h <- glmer(Inflection ~ lag_response + cDutch_proficiency + cType_frequency_vowel + cTrial_number + cDutch_AoA + 
                 (1 + cDutch_proficiency|Verb) +  (1|ResponseId), family=binomial, data=d_L2)

anova(fit_f, fit_h) #fit_h better

#Leave out cDutch_AoA
fit_i <- glmer(Inflection ~ lag_response + cDutch_proficiency + cType_frequency_vowel + cTrial_number + 
                 (1 + cDutch_proficiency|Verb) +  (1|ResponseId), family=binomial, data=d_L2)

anova(fit_g, fit_i) #fit_i better

#AFTER REVIEWS: add age & gender
fit_i1 <- glmer(Inflection ~ lag_response + Age + cDutch_proficiency + cType_frequency_vowel + cTrial_number + 
                 (1 + cDutch_proficiency|Verb) +  (1|ResponseId), family=binomial, data=d_L2)
fit_i2 <- glmer(Inflection ~ lag_response + Gender + cDutch_proficiency + cType_frequency_vowel + cTrial_number + 
                  (1 + cDutch_proficiency|Verb) +  (1|ResponseId), family=binomial, data=d_L2)

#---Expected vs. unexpected strategy (strong verbs only)
#Filter dataset
strong <- droplevels(filter(d, Inflection=="strong"))
summary(strong$Vowel)
strong$Vowel_summary <- strong$Vowel
levels(strong$Vowel_summary) <- c("a", "other", "other", "ee", "other", "other",
                               "ie", "other", "o", "oe", "oo", "other", "other",
                               "other", "other")

strong$cTrial_number <- scale(strong$Trial_number, scale=TRUE, center=TRUE)
strong$cType_frequency_vowel <- scale(strong$Type_frequency_vowel, scale=TRUE, center=TRUE)

#Full model
fit_st <- glmer(Expected_vowel ~ Nativeness + lag_response + cTrial_number + cType_frequency_vowel +
                (1+Nativeness|Verb) + (1|ResponseId), family=binomial, data=strong)

strong$Nativeness_b <- model.matrix(fit_st)[,2]
#Drop correlated random slope nativeness
fit_st_1 <- glmer(Expected_vowel ~ Nativeness_b + lag_response + cTrial_number + cType_frequency_vowel +
                (Nativeness_b||Verb) + (1|ResponseId), family=binomial,data=strong)

anova(fit_st, fit_st_1) #fit_st better, but doesn't converge
#Add optimizer
fit_st_2 <- glmer(Expected_vowel ~ Nativeness + lag_response + cTrial_number + cType_frequency_vowel +
                  (1+Nativeness|Verb) + (1|ResponseId), family=binomial, data=strong, control=glmerControl(optimizer = "bobyqa"))
#does converge

#---OO-preterite (strong verbs only)

#full model
fit_oo <- glmer(oo_preterite ~ Nativeness + lag_response + cTrial_number + cType_frequency_vowel +
                    (1+Nativeness|Verb) + (1|ResponseId), family=binomial,
                  data=strong)

#Leave out correlated random slope Nativeness
strong$Nativeness_b <- model.matrix(fit_oo)[,2]

fit_oo_1 <- glmer(oo_preterite ~ Nativeness_b + lag_response + cTrial_number + cType_frequency_vowel +
                    (Nativeness_b||Verb) + (1|ResponseId), family=binomial,
                  data=strong)

anova(fit_oo, fit_oo_1) #fit_oo_1 better

#Leave out random slope Nativeness
fit_oo_2 <- glmer(oo_preteritum ~ Nativeness_b + lag_response + cTrial_number + cType_frequency_vowel +
                    (1|Verb) + (1|ResponseId), family=binomial,
                  data=strong)

anova(fit_oo_1, fit_oo_2) #fit_oo_1 better, but doesn't converge

#add optimizer
fit_oo_3 <- glmer(oo_preterite ~ Nativeness_b + lag_response + cTrial_number + cType_frequency_vowel +
                    (Nativeness_b||Verb) + (1|ResponseId), family=binomial,
                  data=strong, control=glmerControl(optimizer = "bobyqa"))

#Interaction present stem vowel
fit_oo_4 <- glmer(oo_preterite ~ Nativeness_b*Present_stem_vowel + lag_response + cTrial_number + 
                    (Nativeness_b||Verb) + (1|ResponseId), family=binomial,
                  data=strong, control=glmerControl(optimizer = "bobyqa"))



#=============================================
#---------------------------------------------
# 4. META-ANALYSIS
# --------------------------------------------
#=============================================

#---VIF-scores
#function VIF Zuur et al. 2009
corvif <- function(dataz) {
  dataz <- as.data.frame(dataz)
  
  #vif part
  form    <- formula(paste("fooy ~ ",paste(strsplit(names(dataz)," "),collapse=" + ")))
  dataz   <- data.frame(fooy=1 + rnorm(nrow(dataz)) ,dataz)
  lm_mod  <- lm(form,dataz)
  
  cat("\n\nVariance inflation factors\n\n")
  print(myvif(lm_mod))
}
#Support function for corvif. Will not be called by the user
myvif <- function(mod) {
  v <- vcov(mod)
  assign <- attributes(model.matrix(mod))$assign
  if (names(coefficients(mod)[1]) == "(Intercept)") {
    v <- v[-1, -1]
    assign <- assign[-1]
  } else warning("No intercept: vifs may not be sensible.")
  terms <- labels(terms(mod))
  n.terms <- length(terms)
  if (n.terms < 2) stop("The model contains fewer than 2 terms")
  if (length(assign) > dim(v)[1] ) {
    diag(tmp_cor)<-0
    if (any(tmp_cor==1.0)){
      return("Sample size is too small, 100% collinearity is present")
    } else {
      return("Sample size is too small")
    }
  }
  R <- cov2cor(v)
  detR <- det(R)
  result <- matrix(0, n.terms, 3)
  rownames(result) <- terms
  colnames(result) <- c("GVIF", "Df", "GVIF^(1/2Df)")
  for (term in 1:n.terms) {
    subs <- which(assign == term)
    result[term, 1] <- det(as.matrix(R[subs, subs])) * det(as.matrix(R[-subs, -subs])) / detR
    result[term, 2] <- length(subs)
  }
  if (all(result[, 2] == 1)) {
    result <- data.frame(GVIF=result[, 1])
  } else {
    result[, 3] <- result[, 1]^(1/(2 * result[, 2]))
  }
  invisible(result)
}

#VIF-scores model 1 (fit2)
vars <- cbind(d$Inflection, d$Nativeness, d$lag_response, d$cTrial_number, d$cType_frequency_vowel,
              d$Verb, d$ResponseId)
corvif(vars) #OK!

#VIF-scores model 2 (fit_D)
vars <- cbind(d$Inflection, d$Nativeness, d$lag_response, d$cTrial_number, d$Present_stem_vowel,
              d$Verb, d$ResponseId)
corvif(vars) #OK!

#VIF-scores model 3 (fit_i)
vars <- cbind(d_L2$Inflection, d_L2$cDutch_AoA, d_L2$lag_response, d_L2$cTrial_number, d_L2$cType_frequency_vowel,
              d_L2$Verb, d_L2$ResponseId, d_L2$cDutch_proficiency, d_L2$English_high_proficiency, d_L2$German_high_proficiency)
corvif(vars) #OK!

#VIF-scores model 4 (fit_st_2)
vars <- cbind(strong$Expected_vowel, strong$Nativeness, strong$lag_response, strong$cTrial_number, strong$cType_frequency_vowel,
              strong$Verb, strong$ResponseId)
corvif(vars) #OK!

#VIF-scores model 5 (fit_oo_3)
vars <- cbind(strong$oo_preterite, strong$Nativeness, strong$lag_response, strong$cRangorde, strong$cType_frequency_vowel,
              strong$Verb, strong$ResponseId)
corvif(vars) #OK!

#---R2
#model 1
r.squaredGLMM(fit2)
#model 2
r.squaredGLMM(fit_D)
#model 3
r.squaredGLMM(fit_i)
#model 4
r.squaredGLMM(fit_st_2)
#model 5
r.squaredGLMM(fit_oo_3)

#---C-value
#model 1
Preds <- predict(fit2, type="response")
auc(d$Inflection, Preds) 

#model 2
Preds <- predict(fit_D, type="response")
auc(d$Inflection, Preds)

#model 3
Preds <- predict(fit_i, type="response")
auc(d_L2$Inflection, Preds)

#model 4
Preds <- predict(fit_st_2, type="response")
auc(d$Expected_vowel, Preds)

#model 5
Preds <- predict(fit_oo_3, type="response")
auc(d$oo_preterite, Preds)

#=============================================
#---------------------------------------------
# 4. VISUALISATIONS
# --------------------------------------------
#=============================================

#Order present stem vowels by type frequency

d$Present_stem_vowel <- factor(d$Present_stem_vowel, levels=c("eu", "u", "oe", "o", "uu", "ou", "oo",
                                                                "a", "aa", "ie", "ee", "ui", "e", "i", "ij"))


#Figure 2
mosaic( ~ Inflection + Present_stem_vowel, data = d,
        highlighting = "Inflection", highlighting_fill = c("dark grey", "light grey"), labeling=labeling_values)


##Figure 8 
strong$Present_stem_vowel <- factor(strong$Present_stem_vowel, levels=c("eu", "u", "oe", "o", "uu", "ou", "oo",
                                                                      "a", "aa", "ie", "ee", "ui", "e", "i", "ij"))

t <- table(strong$Present_stem_vowel[strong$Nativeness=="native"], strong$oo_preterite[strong$Nativeness=="native"])
t <- as.data.frame(t)
t <- dcast(t, "Var1 ~Var2")
t$percentage_oo <- t$yes/(t$yes+t$no)
t$Nativeness <- "native"

t2 <- table(strong$Present_stem_vowel[strong$Nativeness=="non-native"], strong$oo_preterite[strong$Nativeness=="non-native"])
t2 <- as.data.frame(t2)
t2 <- dcast(t2, "Var1 ~Var2")
t2$percentage_oo <- t2$yes/(t2$yes+t2$no)
t2$Nativeness <- "non-native"
t3 <- rbind(t, t2)

ggplot(data=t3, aes(x=Var1, y=percentage_oo, fill=Nativeness)) + 
  geom_bar(stat="identity", position="dodge") +
  theme_classic() +
  scale_fill_manual(name="Nativeness", values=c("darkgrey", "lightgrey"), labels=c("native", "non-native")) + 
  labs(x = "Present stem vowel", y = "Proportion oo preterite",title = "", fill="") +
  theme(plot.title = element_text(size = 12), legend.text=element_text(size=12), axis.title.x=element_text(size=12),
        legend.title=element_text(size=12), axis.text.x = element_text(size=12), axis.title.y=element_text(size=12), axis.text.y=element_text(size=12))


#Figure 3
eff <- effect("Nativeness*cType_frequency_vowel", fit2)

plot(eff, type="response", main ="", xlab="Type frequency (centered and standardized)", 
     ylab="Probability of weak forms", lines=list(col="black", multiline=TRUE, lty=1:9))


#Figure 4
eff <- effect("Nativeness*Present_stem_vowel", fit_D)

eff <- as.data.frame(eff)
eff$Present_stem_vowel <- factor(eff$Present_stem_vowel, levels=c("eu", "u", "oe", "o", "uu", "ou", "oo",
                                                                  "a", "aa", "ie", "ee", "ui", "e", "i", "ij"))

ggplot()+
  geom_point(data=eff,aes(Present_stem_vowel, fit_D, linetype=Nativeness),size=2, position = position_dodge(width = -0.5))+
  ggtitle("") +
  geom_errorbar(data=eff,aes(x=Present_stem_vowel, ymin=lower, ymax=upper, linetype=Nativeness), position = position_dodge(width = -0.5), width=0.25) +
  xlab("Present stem vowel") +
  ylab ("Probability of weak forms") +
  theme_classic() +
  theme(plot.title = element_text(size = 12), legend.text=element_text(size=12), axis.title.x=element_text(size=12),
        legend.title=element_text(size=12), axis.text.x = element_text(size=12), axis.title.y=element_text(size=12), axis.text.y=element_text(size=12))


#Figure 5
eff <- effect("cDutch_proficiency", fit_i)
plot(eff, type="response", main ="", xlab="Self-rated proficiency (centered and standardized)", 
     ylab="Probability of weak forms", lines=list(col="black", multiline=TRUE, lty=1:9))


#Figure 6
strong <- strong %>%
  group_by(ResponseId) %>%
  mutate(unique_types = n_distinct(Vowel))

strong_b <- strong %>%
  select(ResponseId, Vowel, unique_types, Verb, Nativeness) %>%
  distinct(ResponseId, .keep_all=TRUE)

strong_b$unique_types <- as.factor(as.character(strong_b$unique_types))

ggplot(data=strong_b, aes(x=unique_types, fill=Nativeness, group=Nativeness)) + 
  geom_bar(position="dodge") +
  geom_text(stat = 'count',aes(label =..count.., group=Nativeness), position = position_dodge(width = 1),
            vjust = -0.5, size = 4) +
  scale_fill_manual(name="Nativeness", values=c("darkgrey", "lightgrey"), labels=c("native", "non-native")) + 
  theme_classic() + 
  xlab("Number of strong strategies") + 
  ylab("Number of participants") + 
  theme(plot.title = element_text(size = 12), legend.text=element_text(size=12), axis.title.x=element_text(size=12),
        legend.title=element_text(size=12), axis.text.x = element_text(size=12), axis.title.y=element_text(size=12), axis.text.y=element_text(size=12)) +
  scale_y_continuous(labels=scales::comma_format())


#Figure 7

strong_unexpected <- droplevels(filter(strong, Expected_vowel=="unexpected"))

ggplot(sterk_unexpected, aes(x= Vowel_summary,  group=Nativeness)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count", show.legend=FALSE) +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Vowel_summary") +
  xlab("Vowel preterite") +
  scale_fill_manual(name="", values=c("red3", "darkorange", "gold", "darkgreen", "darkblue", "darkorchid4", "mediumorchid"), labels=c("a", "other", "ee", "ie", "o", "oe", "oo")) + 
  theme_classic() + 
  theme(plot.title = element_text(size = 12), legend.text=element_text(size=12), axis.title.x=element_text(size=12),
        legend.title=element_text(size=12), axis.text.x = element_text(size=12), axis.title.y=element_text(size=12), axis.text.y=element_text(size=12)) +
  facet_grid(~Nativeness) +
  scale_y_continuous(labels = scales::percent)

#=============================================
#---------------------------------------------
# 4. Sj-measure Cuskley et al. 2015 (after reviews)
# --------------------------------------------
#=============================================

d2 <- strong %>%
  group_by(ResponseId) %>%
  mutate(unique_types = n_distinct(Vowel))

d2 <- d2 %>%
  add_count(ResponseId, name="number_irregular_verbs")


d2 <- d2 %>%
  group_by(ResponseId) %>%
  add_count(Vowel, name="number_irregular_verbs_type")


C_wide <- dcast(d2, ResponseId + Nativeness + number_irregular_verbs  ~ Vowel)

for(i in 1:nrow(C_wide)){
  for(j in names(C_wide)[-c(1:3)]){
    C_wide[[j]][i] <- C_wide[[j]][i]/C_wide$number_irregular_verbs[i]
  }
}

for(i in 1:nrow(C_wide)){
  for(j in names(C_wide)[-c(1:3)]){
    C_wide[[j]][i] <- C_wide[[j]][i]*log2(C_wide[[j]][i])
  }
}

C_wide$Sj_numerator <- NA
for(i in 1:nrow(C_wide)){
  sum_temp <- 0
  for(j in 4:(length(C_wide)-1)){
    if(!is.na(C_wide[[j]][i])){
      sum_temp <- sum_temp + C_wide[[j]][i]
    }
  }
  C_wide$Sj_numerator[i] <- -(sum_temp)
}

C_wide$Sj <- C_wide$Sj_numerator/log2(C_wide$number_irregular_verbs)

summary(C_wide$Sj)

Sj_df <- dplyr::select(C_wide, ResponseId, Nativeness, number_irregular_verbs, Sj)
head(Sj_df)

g <- ggplot(Sj_df, aes(x=number_irregular_verbs, y=Sj, color=Nativeness))
g <- g + geom_jitter()
g <- g + geom_smooth(method = "loess")
g 





