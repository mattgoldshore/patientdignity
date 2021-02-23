## library
library(expss)
library(readxl)
library(dplyr)
library(tidyverse)
library(summarytools)
library(ggpubr)
library(nnet)

## import of dataset
dignity <- read_excel("/Volumes/NO NAME/Dignity/mskcc with merged codes for analysis.xls")

## pdq response is dignity$DignityResponse

## dignity code
dignity$Code1
typeof(dignity$Code1)
dignity$code1 = factor(dignity$Code1)
dignity %>% count(code1)
ggplot(dignity) + geom_bar(mapping = aes(x = code1))

## create dignity category
dignity <- dignity %>% 
    mutate( 
      chochinov3 = case_when(code1 == "Fears" ~ 'Illness-Related Concerns',
                             code1 == "Symptoms" ~ 'Illness-Related Concerns',
                             code1 == "Coping strategies" ~ 'Dignity Conserving Repertoire',
                             code1 == "Goals" ~ 'Dignity Conserving Repertoire',
                             code1 == "Identity" ~ 'Dignity Conserving Repertoire',
                             code1 == "Interpersonal interaction" ~ 'Social Dignity Inventory',
                             code1 == "Other" ~ 'Other'))
dignity$chochinov3 <- factor(dignity$chochinov3,
                          levels=c('Illness-Related Concerns', 
                             'Dignity Conserving Repertoire', 
                             'Social Dignity Inventory', 
                             'Other'))
dignity %>% count(chochinov3)
dignity$chochinov3 = factor(dignity$chochinov3)

## create main dignity theme variable where family respondents to pdq are are recoded as NA
dignity <- dignity %>% 
    mutate( 
      code1nofam = case_when(code1 == "Fears" ~ 'Fears',
                             code1 == "Symptoms" ~ 'Symptoms',
                             code1 == "Coping strategies" ~ 'Coping Strategies',
                             code1 == "Goals" ~ 'Goals',
                             code1 == "Identity" ~ 'Identity',
                             code1 == "Interpersonal interaction" ~ 'Interpersonal Interaction',
                             code1 == "Other" ~ 'Other'))
dignity$code1nofam <- factor(dignity$code1nofam,
                               levels=c('Fears', 
                                        'Symptoms', 
                                        'Coping Strategies', 
                                        'Goals',
                                        'Identity',
                                        'Interpersonal Interaction',
                                        'Other'))
dignity %>% count(code1nofam)
  
## create dignity subtheme category
dignity$subcode1 = factor(dignity$Subcode1) # tranform Subcode1 to a factor variable
ggplot(dignity) + geom_bar(mapping = aes(x = subcode1)) + coord_flip() # bar graph and summary for subcode2
dignity %>% count(subcode1)
dignity <- dignity %>% 
    mutate( 
      code1fam = case_when(subcode1 == "Goals" ~ 'Goals',
                           subcode1 == "Identity" ~ 'Identity'))
dignity <- dignity %>% 
    mutate( 
      participant = case_when(Code1 == "Fears" ~ 'Patient',
                             Code1 == "Symptoms" ~ 'Patient',
                             Code1 == "Coping strategies" ~ 'Patient',
                             Code1 == "Goals" ~ 'Patient',
                             Code1 == "Identity" ~ 'Patient',
                             Code1 == "Interpersonal interaction" ~ 'Patient',
                             Code1 == "Other" ~ 'Patient',
                             Code1 == "Family" ~ 'Family'))
dignity$chochinov3 <- factor(dignity$chochinov3,
                               levels=c('Illness-Related Concerns', 
                                        'Dignity Conserving Repertoire', 
                                        'Social Dignity Inventory', 
                                        'Other'))
dignity %>% count(chochinov3)
  
## var4: Code2 dignity$Code2
## var5: Subcode2 dignity$Subcode2
## var6: AdmittingDiagnosis no changes at this time

## agegroup
dignity$age_n = as.numeric(dignity$Age)  # Transform Age into a numeric variable
ggplot(dignity) + geom_histogram(mapping = aes(x = age_n), binwidth = 1) # histogram and summary of age as a continuous metric
dignity_nonfamily <- dignity_nonfamily %>% 
    mutate( 
       agegroup = case_when(age_n >= 85  ~ '85 +',
                            age_n >= 75  & age_n <= 84 ~ '75 - 84',
                            age_n >= 65  & age_n <= 74 ~ '65 - 74',
                            age_n >= 55  & age_n <= 64 ~ '55 - 64',
                            age_n >= 45  & age_n <= 54 ~ '45 - 54',
                            age_n >= 35  & age_n <= 44 ~ '35 - 44',
                            age_n >= 20  & age_n <= 34 ~ '20 - 34')) 
dignity %>% count(agegroup)
dignity$agegroup= factor(dignity$agegroup)


## race
dignity$race = factor(dignity$Race)  # tranform Race to a factor variable
ggplot(dignity) + geom_bar(mapping = aes(x = Race)) # bar graph and summary for race
dignity %>% count(race)

## reclassification of race into groups
dignity <- dignity %>% 
  mutate(
    racegroup = case_when(race == "WHITE" ~ 'White',
                          race == "BLACK"  ~ 'Black',
                          race == "ASIAN"  ~ 'Asian',
                          race == "NATIVE AMERICAN" | Race == "OTHER" ~ 'Other',
                          race == "PT REFUSED TO ANSWER" | Race == "NO VALUE ENTERED" | Race == "Unknown" ~ 'Refused/Unknown'))  # create racegroup 
dignity$racegroup <- factor(dignity$racegroup, 
                            levels=c('White', 'Black', 'Asian', 'Other', 'Refused/Unknown'))
ggplot(dignity) + geom_bar(mapping = aes(x = racegroup)) # preferred data organization
dignity %>% count(racegroup)

## ethnicity
dignity %>% count(Ethnicity)
dignity$ethnicity = factor(dignity$Ethnicity)
dignity <- dignity %>% 
  mutate( 
    ethnicity = case_when(ethnicity == "HISPANIC OR LATINO" ~ 'Hispanic or Latino',
                          ethnicity == "NOT HISPANIC OR LATINO"  ~ 'Not Hispanic or Latino'))
dignity %>% count(ethnicity)
ggplot(dignity) + geom_bar(mapping = aes(x = ethnicity))
dignity <- within(dignity, 
                  ethnicity <- factor(ethnicity, 
                                      levels=c('Not Hispanic or Latino', 'Hispanic or Latino')))
ggplot(dignity) + geom_bar(mapping = aes(x = ethnicity))
dignity %>% count(ethnicity)


## recode the religion group variable
# bar graph and summary for religion
ggplot(dignity) + geom_bar(mapping = aes(x = Religion)) + coord_flip()
dignity %>% count(Religion)
dignity <- dignity %>% 
  mutate( 
    religiongroup = case_when(Religion == "Baptist" | 
                              Religion == "Catholic, Roman" | 
                              Religion == "Christian" | 
                              Religion == "Christian Orthodox" |
                              Religion == "Episcopalian" |
                              Religion == "Greek Orthodox" | 
                              Religion == "Lutheran" |
                              Religion == "Methodist" |
                              Religion == "Pentecostal" | 
                              Religion == "Presbyterian" |
                              Religion == "Prostestant, Other" | 
                              Religion == "Russian Orthodox" |
                              Religion == "Seventh Day Adventist" | 
                              Religion == "Reformed" | 
                              Religion == "Jehovah's Witness" ~ 'Christian',
                              Religion == "Jewish" ~ 'Jewish',
                              Religion == "Muslim" ~ 'Muslim',
                              Religion == "Buddhist" ~ 'Buddhist',
                              Religion == "Hindu" ~ 'Hindu',
                              Religion == "Other" |
                              Religion == "Sikh" |
                              Religion == "Unitarian" ~ 'Other',
                              Religion == "None" ~ 'None',
                              Religion == "NO VALUE ENTERED" |
                              Religion == "Unknown" |
                              Religion == "" ~ 'Unknown/Refused'))
dignity$religiongroup[is.na.data.frame(dignity$religiongroup)] <- "Unknown/Refused"
ggplot(dignity) + geom_bar(mapping = aes(x = religiongroup))
dignity %>% count(religiongroup)
dignity$religiongroup <- factor(dignity$religiongroup,
                             levels=c('Christian', 
                                      'Jewish', 
                                      'Muslim', 
                                      'Buddhist',
                                      'Hindu',
                                      'None',
                                      'Other',
                                      'Unknown/Refused'))
dignity %>% count(religiongroup)
dignity$religiongroup= factor(dignity$religiongroup)



## tumor histology not included in this analysis

## recode of tumor site
ggplot(dignity) + geom_bar(mapping = aes(x = TumorSite)) + coord_flip()
dignity %>% count(TumorSite)
dignity <- dignity %>% 
  mutate( 
    tumorsitegroup = case_when(   TumorSite == "C019-TONGUE, BASE" |
                                  TumorSite == "C022-TONGUE, VENTRUM" |
                                  TumorSite == "C023-TONGUE, ANTERIOR 2/3" |
                                  TumorSite == "C029-TONGUE, NOS" | 
                                  TumorSite == "C030-PET PET IMAG/BRAIN" |
                                  TumorSite == "C031-GUM, LOWER" | 
                                  TumorSite == "C040-MOUTH, ANTERIOR FLOOR" | 
                                  TumorSite == "C049-MOUTH, FLOOR, NOS" | 
                                  TumorSite == "C051-MUCOSA, SOFT PALATE" | 
                                  TumorSite == "C060-MUCOSA, BUCCAL" | 
                                  TumorSite == "C062-MUCOSA, RETROMOLAR" | 
                                  TumorSite == "C069-MUCOSA, ORAL, NOS" | 
                                  TumorSite == "C079-SALIVARY, PAROTID" |
                                  TumorSite == "C080-SALIVARY, SUBMAXILLARY" | 
                                  TumorSite == "C080-SALIVARY, SUBMAXILLARY" | 
                                  TumorSite == "C081-SALIVARY, SUBLINGUAL" |
                                  TumorSite == "C091-OROPHAR, TONSIL, PILLAR" | 
                                  TumorSite == "C099-OROPHAR, TONSIL" |
                                  TumorSite == "C108-OROPHAR, JUNCTIONAL" |
                                  TumorSite == "C109-OROPHAR, NOS" |
                                  TumorSite == "C111-NASOPHAR, POSTERIOR" |
                                  TumorSite == "C112-NASOPHAR, LATERAL"| 
                                  TumorSite == "C119-NASOPHAR, NOS" | 
                                  TumorSite == "C129-HYPOPHAR, PYRIFORM SINUS" | 
                                  TumorSite == "C139-HYPOPHAR, NOS" | 
                                  TumorSite == "C140-PHARYNX" |
                                  TumorSite == "C148-PHARYNX, OTHER PARTS" | 
                                  TumorSite == "C300-NASAL CAVITY" |
                                  TumorSite == "C310-MAXILLARY SINUS" | 
                                  TumorSite == "C311-ETHMOID SINUS" |
                                  TumorSite == "C313-SPHENOID SINUS" | 
                                  TumorSite == "C319-ACCESSORY SINUS, NOS" |
                                  TumorSite == "C490-CONN, HEAD/FACE/NECK" ~ 'Head and Neck',
                                  TumorSite == "C154-ESOPHAGUS, MIDDLE THIRD" | 
                                  TumorSite == "C155-ESOPHAGUS, LOWER THIRD" |
                                  TumorSite == "C160-STOMACH, CARDIA" |
                                  TumorSite == "C161-STOMACH, FUNDUS" | 
                                  TumorSite == "C162-STOMACH, BODY" | 
                                  TumorSite == "C163-STOMACH, ANTRUM" |
                                  TumorSite == "C164-STOMACH, PYLORUS" |
                                  TumorSite == "C165-STOMACH, LESSER CURVE" | 
                                  TumorSite == "C166-STOMACH, GREATER CURVE" |
                                  TumorSite == "C168-STOMACH, OTHER PARTS" |
                                  TumorSite == "C169-STOMACH, NOS" | 
                                  TumorSite == "C170-DUODENUM" | 
                                  TumorSite == "C171-JEJUNUM" | 
                                  TumorSite == "C172-ILEUM" | 
                                  TumorSite == "C179-SMALL INTESTINE, NOS" | 
                                  TumorSite == "C180-CECUM" | 
                                  TumorSite == "C181-APPENDIX" |
                                  TumorSite == "C182-ASCENDING COLON" | 
                                  TumorSite == "C183-COLON, HEPATIC FLEXURE" |
                                  TumorSite == "C184-TRANSVERSE COLON" | 
                                  TumorSite == "C185-SPLENIC FLEXURE OF COLON" | 
                                  TumorSite == "C186-DESCENDING COLON" | 
                                  TumorSite == "C187-SIGMOID COLON" | 
                                  TumorSite == "C189-COLON, NOS" | 
                                  TumorSite == "C199-RECTOSIGMOID JUNCTION" | 
                                  TumorSite == "C209-RECTUM, NOS" |
                                  TumorSite == "C210-ANUS, NOS" | 
                                  TumorSite == "C211-ANAL CANAL" |
                                  TumorSite == "C218-RECTUM, OTHER PARTS" |
                                  TumorSite == "C220-LIVER, NOS" |
                                  TumorSite == "C221-INTRAHEPATIC BILE DUCT" | 
                                  TumorSite == "C239-GALLBLADDER" | 
                                  TumorSite == "C240-EXTRAHEPATIC BILE DUCT" | 
                                  TumorSite == "C241-AMPULLA OF VATER" | 
                                  TumorSite == "C249-BILIARY TRACT, NOS" | 
                                  TumorSite == "C250-PANCREAS, HEAD" | 
                                  TumorSite == "C251-PANCREAS, BODY" | 
                                  TumorSite == "C252-PANCREAS, TAIL" | 
                                  TumorSite == "C257-OTHER SPECIFIED PARTS OF PANCREAS" | 
                                  TumorSite == "C258-PANCREAS, OVERLAPPING LESION" | 
                                  TumorSite == "C259-PANCREAS, NOS" |
                                  TumorSite == "C269-GASTROINTESTINAL TRACT, NOS" ~ 'Digestive/Gastrointestinal',
                                  TumorSite == "C321-LARYNX, SUPRAGLOTTIS" | 
                                  TumorSite == "C322-LARYNX, SUBGLOTTIS" |
                                  TumorSite == "C329-LARYNX, NOS" |
                                  TumorSite == "C340-MAIN BRONCHUS" |
                                  TumorSite == "C341-LUNG, UPPER LOBE" | 
                                  TumorSite == "C342-LUNG, MIDDLE LOBE" |
                                  TumorSite == "C343-LUNG, LOWER LOBE" | 
                                  TumorSite == "C348-LUNG, OTHER PARTS" |
                                  TumorSite == "C349-LUNG, NOS" | 
                                  TumorSite == "C379-THYMUS" | 
                                  TumorSite == "C380-HEART" | 
                                  TumorSite == "C381-ANTERIOR MEDIASTINUM" | 
                                  TumorSite == "C383-MEDIASTINUM, NOS" | 
                                  TumorSite == "C384-PARIETAL PLEURA" ~ 'Respiratory/Thoracic',
                                  TumorSite == "C400-BONE, ARM/SCAPULA" | 
                                  TumorSite == "C402-BONE, LEG" | 
                                  TumorSite == "C412-BONE, VERTEBRAL COLUMN" | 
                                  TumorSite == "C413-BONE, RIB/STERNUM/CLAVICLE & ASSOCIATED JOINTS" | 
                                  TumorSite == "C414-BONE, PELVIS" | 
                                  TumorSite == "C491-CONN, UPPER LIMB" |
                                  TumorSite == "C492-CONN, LOWER LIMB" ~ 'Other',
                                  TumorSite == "C420-BLOOD" | 
                                  TumorSite == "C421-BONE MARROW" | 
                                  TumorSite == "C422-SPLEEN" |
                                  TumorSite == "C712-PLANAR NUCLEAR MED IMAGING/SPLEEN" |
                                  TumorSite == "C713-PLANAR NUCLEAR MEDICINE IMAGING/BLD" | 
                                  TumorSite == "C770-LYMPH NODES, HEAD/NECK" | 
                                  TumorSite == "C771-INTRATHORACIC LYMPH NOES" | 
                                  TumorSite == "C772-INTRA-ABDOMINAL LYMPH NODES" | 
                                  TumorSite == "C773-LYMPH NODES, AXILLA/ARM" | 
                                  TumorSite == "C774-LYMPH NODES,INGUINAL/LEG" |
                                  TumorSite == "C778-LYMPH NODES, MULTIPLE REGIONS" | 
                                  TumorSite == "C779-LYMPH NODES, NOS" ~ 'Hematologic/Blood',
                                  TumorSite == "C440-SKIN, LIP" | 
                                  TumorSite == "C441-SKIN, EYELID" |
                                  TumorSite == "C443-SKIN, FACE" | 
                                  TumorSite == "C444-SKIN, SCALP/NECK" ~ 'Head and Neck',
                                  TumorSite == "C445-SKIN, TRUNK" | 
                                  TumorSite == "C446-SKIN, ARM/SHOULDER" |
                                  TumorSite == "C447-SKIN, LEG/HIP" | 
                                  TumorSite == "C449-SKIN, NOS" ~ 'Breast and Soft Tissue',
                                  TumorSite == "C480-RETROPERITONEUM" | 
                                  TumorSite == "C481-PERITONEUM, OTHER PARTS" | 
                                  TumorSite == "C482-PERITONEUM, NOS" | 
                                  TumorSite == "C493-CONN, THORAX" | 
                                  TumorSite == "C494-CONN, ABDOMEN" | 
                                  TumorSite == "C495-CONN, PELVIS" |
                                  TumorSite == "C495-CONN, PELVIS" |
                                  TumorSite == "C496-CONN, TRUNK" | 
                                  TumorSite == "C499-CONN, NOS" | 
                                  TumorSite == "C809-UNKNOWN PRIMARY SITE" ~ 'Unknown Primary',
                                  TumorSite == "C501-BREAST, CENTRAL PORTION" |
                                  TumorSite == "C503-BREAST, LIQ" | 
                                  TumorSite == "C504-BREAST, UOQ" | 
                                  TumorSite == "C505-BREAST, LOQ" | 
                                  TumorSite == "C508-BREAST, OVERLAPPING LESION OF BREAST" |
                                  TumorSite == "C509-BREAST, NOS" ~ 'Breast and Soft Tissue',
                                  TumorSite == "C510-LABIUM MAJUS" |
                                  TumorSite == "C519-VULVA, NOS" | 
                                  TumorSite == "C529-VAGINA, NOS" | 
                                  TumorSite == "C530-ENDOCERVIX" | 
                                  TumorSite == "C538-CERVIX, OTHER PARTS" | 
                                  TumorSite == "C539-CERVIX UTERI" | 
                                  TumorSite == "C540-ISTHMUS UTERI" | 
                                  TumorSite == "C541-ENDOMETRIUM" | 
                                  TumorSite == "C542-MYOMETRIUM" | 
                                  TumorSite == "C549-CORPUS UTERI" | 
                                  TumorSite == "C559-UTERUS, NOS" |
                                  TumorSite == "C569-OVARY" | 
                                  TumorSite == "C570-FALLOPIAN TUBE" | 
                                  TumorSite == "C574-ADNEXA, NOS" |
                                  TumorSite == "C578-UTERINE ADNEXA, OTHER PARTS" | 
                                  TumorSite == "C589-PLACENTA" ~ 'Gynecologic',
                                  TumorSite == "C621-DESCENDED TESTIS" | 
                                  TumorSite == "C629-TESTIS, NOS" |
                                  TumorSite == "C609-PENIS, NOS" | 
                                  TumorSite == "C619-PROSTATE" | 
                                  TumorSite == "C659-RENAL PELVIS" |
                                  TumorSite == "C669-URETER" | 
                                  TumorSite == "C670-BLADDER, TRIGONE" | 
                                  TumorSite == "C671-BLADDER, DOME" | 
                                  TumorSite == "C672-BLADDER, LATERAL WALL" | 
                                  TumorSite == "C673-BLADDER, ANTERIOR WALL" |
                                  TumorSite == "C674-BLADDER, POSTERIOR WALL" |
                                  TumorSite == "C677-URACHUS" | 
                                  TumorSite == "C678-BLADDER, OTHER PARTS" | 
                                  TumorSite == "C679-BLADDER, NOS" | 
                                  TumorSite == "C689-URINARY SYSTEM, NOS" ~ 'Genitourinary',
                                  TumorSite == "C690-CONJUNCTIVA" |
                                  TumorSite == "C693-CHOROID" | 
                                  TumorSite == "C695-LACRIMAL GLAND" | 
                                  TumorSite == "C699-EYE, NOS" | 
                                  TumorSite == "C700-CEREBRAL MENININGES" | 
                                  TumorSite == "C701-SPINAL MENINGES" | 
                                  TumorSite == "C711-FRONTAL LOBE" | 
                                  TumorSite == "C716-CEREBELLUM" |
                                  TumorSite == "C718-BRAIN, OTHER" |
                                  TumorSite == "C719-BRAIN, NOS" |
                                  TumorSite == "C720-SPINAL CORD" ~ 'Other',
                                  TumorSite == "C739-THYROID, NOS" ~ 'Head and Neck',
                                  TumorSite == "C749-ADRENAL GLAND, NOS" ~ 'Other'))
# this code renames all of the NA (blank) responses in the dignity$tumorsitegroup variable to "Unknown Primary"
dignity$tumorsitegroup[is.na.data.frame(dignity$tumorsitegroup)] <- "Unknown Primary"
# graph and quantify the distribution of tumor sites in the dataset
ggplot(dignity) + geom_bar(mapping = aes(x = tumorsitegroup)) + coord_flip()
dignity$tumorsitegroup <- factor(dignity$tumorsitegroup,
                                levels=c('Breast and Soft Tissue',
                                          'Digestive/Gastrointestinal',
                                          'Genitourinary',
                                          'Gynecologic',
                                          'Head and Neck',
                                          'Hematologic/Blood',
                                          'Respiratory/Thoracic',
                                          'Other',
                                          'Unknown Primary'))
dignity %>% count(tumorsitegroup)

## characterize the sample into alive versus dead
ggplot(dignity) + geom_bar(mapping = aes(x = Deceased)) + coord_flip()
dignity %>% count(Deceased)
dignity$deceased = factor(dignity$Deceased)  # tranform Code1 to a factor variable
dignity %>% count(deceased)

## for those who are dead, time between pdq administration and date of death
dignity$timetodeath_n = as.numeric(dignity$TimetoDeathDays)
ggplot(dignity) + geom_histogram(mapping = aes(x = timetodeath_n), binwidth = 10)
dignity %>% count(cut_width(timetodeath_n, 50))
dignity <- dignity %>% 
  mutate( 
    timetodeathgroup = case_when(timetodeath_n >= 365 ~  'More than 1 year',
                                 timetodeath_n >= 181  & timetodeath_n <= 365 ~  '181 - 365 Days',
                                 timetodeath_n >= 31  & timetodeath_n <= 180 ~   '30 - 180 Days',
                                 timetodeath_n >= 0  & timetodeath_n <= 30 ~     '0 - 30 Days')
        )
dignity$timetodeathgroup <- factor(dignity$timetodeathgroup,
                                 levels=c('0 - 30 Days',
                                          '30 - 180 Days',
                                          '181 - 365 Days',
                                          'More than 1 year'))
dignity %>% count(timetodeathgroup)
dignity$timetodeathgroup = factor(dignity$timetodeathgroup)

########################################
## Stratify the dataset by respondent ##
########################################

dignity_nonfamily <- subset(dignity, dignity$code1 != "Family") # subset of dignity for patient-respondents 
dignity_family <-  subset(dignity, dignity$code1 == "Family" & dignity$code1fam != 'NA') # subset of dignity for family-respondents

#########################
## Explore the dataset ##x
#########################

## Multinomial regression model, chochinov category as main dependent variable
library(nnet)
dignity_nonfamily$chochinov3
dignity_nonfamily$agegroup
dignity_nonfamily$religiongroup
dignity_nonfamily$timetodeathgroup

regression <- "chochinov3 ~ timetodeathgroup + agegroup"
mlogit <- nnet::multinom(regression, data = dignity_nonfamily)
output <- summary(mlogit)
print(output)
exp(coef(mlogit))
exp(confint(mlogit))

z <- output$coefficients/output$standard.errors
p <- (1 - pnorm(abs(z), 0, 1))*2 # we are using two-tailed z test
  
Pclass2 <- rbind(output$coefficients[1,],output$standard.errors[1,],z[1,],p[1,])
rownames(Pclass2) <- c("Coefficient","Std. Errors","z stat","p value")
knitr::kable(Pclass2)

Pclass3 <- rbind(output$coefficients[2,],output$standard.errors[2,],z[2,],p[2,])
rownames(Pclass3) <- c("Coefficient","Std. Errors","z stat","p value")
knitr::kable(Pclass3)

Pclass4 <- rbind(output$coefficients[3,],output$standard.errors[3,],z[3,],p[3,])
rownames(Pclass4) <- c("Coefficient","Std. Errors","z stat","p value")
knitr::kable(Pclass4)
  
mlr1 <- multinom(formula = chochinov3 ~ timetodeath_n, data = dignity_nonfamily)
summary(mlr1)
exp(coef(mlr1))

# Patient as respondent tables
mean(dignity_nonfamily$age_n)
sd(dignity_nonfamily$age_n)
group_by(dignity, chochinov3) %>%
  summarise(
    count = n(),
    mean = mean(age_n, na.rm = TRUE),
    sd = sd(age_n, na.rm = TRUE)
  )

table(dignity_nonfamily$agegroup)
prop.table(table(dignity_nonfamily$agegroup))*100
table(dignity_nonfamily$agegroup, dignity_nonfamily$chochinov3)
a <- prop.table(table(dignity_nonfamily$agegroup, dignity_nonfamily$chochinov3), margin=1)*100
format(round(a,1))

table(dignity_nonfamily$racegroup)
round(prop.table(table(dignity_nonfamily$racegroup)),3)*100
table(dignity_nonfamily$racegroup, dignity_nonfamily$chochinov3)
a <- prop.table(table(dignity_nonfamily$agegroup, dignity_nonfamily$chochinov3), margin=1)*100
format(round(a,1))

group_by(dignity_nonfamily, chochinov3) %>%
  summarise(
    count = n(racegrup),
    fre = mean(timetodeath_n, na.rm = TRUE),
    sd = sd(timetodeath_n, na.rm = TRUE)
  )

res.aov <- aov(age_n ~ chochinov3, data = dignity_nonfamily)
summary(res.aov)

freq(dignity_nonfamily$agegroup)
ctable(dignity_nonfamily$agegroup, dignity_nonfamily$chochinov3, chisq = TRUE)

freq(dignity_nonfamily$racegroup)
ctable(dignity_nonfamily$racegroup, dignity_nonfamily$chochinov3, chisq = TRUE)

freq(dignity_nonfamily$ethnicity)
ctable(dignity_nonfamily$ethnicity, dignity_nonfamily$chochinov3, chisq = TRUE)

freq(dignity_nonfamily$religiongroup)
ctable(dignity_nonfamily$religiongroup, dignity_nonfamily$chochinov3, chisq = TRUE)
fisher.test(dignity_nonfamily$religiongroup, dignity_nonfamily$chochinov3, simulate.p.value = TRUE)
 
freq(dignity_nonfamily$tumorsitegroup)
ctable(dignity_nonfamily$tumorsitegroup, dignity_nonfamily$chochinov3, chisq = TRUE)

ctable(dignity$ethnicity, dignity$religiongroup, chisq = TRUE)



group_by(dignity_nonfamily, code1nofam) %>%
  summarise(
    count = n(),
    mean = mean(age_n, na.rm = TRUE),
    sd = sd(age_n, na.rm = TRUE)
  )

freq(dignity_nonfamily$code1nofam)

res.aov <- aov(age_n ~ code1nofam, data = dignity_nonfamily)
summary(res.aov)

freq(dignity_nonfamily$agegroup)
ctable(dignity_nonfamily$agegroup, dignity_nonfamily$code1nofam, chisq = TRUE)

freq(dignity_nonfamily$racegroup)
ctable(dignity_nonfamily$racegroup, dignity_nonfamily$code1nofam, chisq = TRUE)

freq(dignity_nonfamily$ethnicity)
ctable(dignity_nonfamily$ethnicity, dignity_nonfamily$code1nofam, chisq = TRUE)

freq(dignity_nonfamily$religiongroup)
ctable(dignity_nonfamily$religiongroup, dignity_nonfamily$code1nofam, fisher.test = TRUE)
fisher.test(dignity_nonfamily$religiongroup, dignity_nonfamily$code1nofam, simulate.p.value = TRUE)

freq(dignity_nonfamily$tumorsitegroup)
ctable(dignity_nonfamily$tumorsitegroup, dignity_nonfamily$code1nofam, chisq = TRUE)

freq(dignity_nonfamily$deceased)
ctable(dignity_nonfamily$deceased, dignity_nonfamily$code1nofam, chisq = TRUE)

res.aov <- aov(timetodeath_n ~ code1nofam, data = dignity_nonfamily)
summary(res.aov)

wilcox.test(dignity_nonfamily$timetodeath_n, dignity_nonfamily$chochinov3)

group_by(dignity_nonfamily, code1nofam) %>%
  summarise(
    count = n(),
    mean = mean(timetodeath_n, na.rm = TRUE),
    sd = sd(timetodeath_n, na.rm = TRUE)
  )

## Table: family respondent

freq(dignity_family$agegroup)
ctable(dignity_family$agegroup, dignity_family$code1fam, chisq = TRUE, na.rm = TRUE)

freq(dignity_family$racegroup)
ctable(dignity_family$racegroup, dignity_family$code1fam, chisq = TRUE)

freq(dignity_family$ethnicity)
ctable(dignity_family$ethnicity, dignity_family$code1fam, chisq = TRUE)

freq(dignity_family$religiongroup)
ctable(dignity_family$religiongroup, dignity_family$code1fam, chisq = TRUE)

freq(dignity_family$tumorsitegroup)
ctable(dignity_family$tumorsitegroup, dignity_family$code1fam, chisq = TRUE)
fisher.test(dignity_family$tumorsitegroup, dignity_family$code1fam, simulate.p.value = TRUE)

freq(dignity_family$deceased)
ctable(dignity_family$deceased, dignity_family$code1fam, chisq = TRUE)

res.aov <- aov(timetodeath_n ~ code1fam, data = dignity_family)
summary(res.aov)



ctable(dignity_nonfamily$deceased, dignity_nonfamily$Subcode1, chisq = TRUE)





# Death analysis for patient-respondents by chochinov3
freq(dignity_family$deceased)
ctable(dignity_nonfamily$chochinov3, dignity_nonfamily$deceased, chisq = TRUE)
ctable(dignity_nonfamily$deceased, dignity_nonfamily$chochinov3, chisq = TRUE)

median(dignity_nonfamily$timetodeath_n, na.rm = TRUE)
IQR(dignity_nonfamily$timetodeath_n, na.rm = TRUE)
quantile(dignity_nonfamily$timetodeath_n, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

group_by(dignity_nonfamily, chochinov3) %>%
  summarise(
    count = n(),
    mean = mean(timetodeath_n, na.rm = TRUE),
    sd = sd(timetodeath_n, na.rm = TRUE),
    median = median(timetodeath_n, na.rm = TRUE),
    IQR = IQR(timetodeath_n, na.rm = TRUE),
    twentyfifth = quantile(timetodeath_n, probs = 0.25, na.rm = TRUE),
    seventyfifth = quantile(timetodeath_n, probs = 0.75, na.rm = TRUE)
  )
kruskal.test(timetodeath_n ~ chochinov3, data = dignity_nonfamily)

# Death analysis for patient-respondents by code1fam
median(dignity_nonfamily$timetodeath_n, na.rm = TRUE)
IQR(dignity_nonfamily$timetodeath_n, na.rm = TRUE)
quantile(dignity_nonfamily$timetodeath_n, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

group_by(dignity_nonfamily, code1nofam) %>%
  summarise(
    count = n(),
    mean = mean(timetodeath_n, na.rm = TRUE),
    sd = sd(timetodeath_n, na.rm = TRUE),
    median = median(timetodeath_n, na.rm = TRUE),
    IQR = IQR(timetodeath_n, na.rm = TRUE),
    twentyfifth = quantile(timetodeath_n, probs = 0.25, na.rm = TRUE),
    seventyfifth = quantile(timetodeath_n, probs = 0.75, na.rm = TRUE)
  )
kruskal.test(timetodeath_n ~ code1nofam, data = dignity_nonfamily)

# Present the median distributions as boxplots

boxplot1 <- ggplot(data = dignity_nonfamily) +
              geom_boxplot(mapping = aes(chochinov3, timetodeath_n, color = chochinov3)) +
              coord_flip()
dignity_nonfamily$chochinov3 <- factor(dignity_nonfamily$chochinov3 , levels=c("Other", 
                                                                               "Social Dignity Inventory",
                                                                               "Dignity Conserving Repertoire",
                                                                               "Illness-Related"))

boxplot1 +  scale_color_brewer(palette="RdYlBu") + 
            theme_classic() + 
            theme(legend.position="none") +
            labs(title="Time to Death (days) by Dignity Category",x="", y = "Time to death (days)") +
            theme(plot.title = element_text(hjust = 0.5)) +
            scale_x_discrete(labels = c("Other",
                                        "Social\nDignity\nInventory", 
                                        "Dignity\nConserving\nRepertoire",
                                        "Illness-\nRelates\nConcerns",
                                        "Other"))





dignity$code1nofam <- factor(dignity$code1nofam,
                             levels=c('Fears', 
                                      'Symptoms', 
                                      'Coping Strategies', 
                                      'Goals',
                                      'Identity',
                                      'Interpersonal Interaction',
                                      'Other'))

boxplot2 <- ggplot(data = dignity_nonfamily) +
  geom_boxplot(mapping = aes(chochinov3, timetodeath_n, color = code1nofam)) +
  coord_flip()

boxplot2 +  scale_color_brewer(palette="Paired") + 
  theme_classic() +
  labs(title="Time to Death (days) by Dignity Category and Main Dignity Theme",x="", y = "Time to death (days)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(labels = c("Other",
                              "Social\nDignity\nInventory", 
                              "Dignity\nConserving\nRepertoire",
                              "Illness-\nRelates\nConcerns",
                              "Other")) +
  theme(legend.position="bottom") +
  theme(legend.title = element_blank()) 
  







levels=c('Illness-Related Concerns', 
         'Dignity Conserving Repertoire', 
         'Social Dignity Inventory', 
         'Other'))



plot1 <- ggboxplot(dignity_nonfamily, x = "chochinov3", y = "timetodeath_n", 
                   color = "chochinov3", palette = c("#00AFBB", "#E7B800", "#FC4E07", "336600"),
                   ylab = "Time to Death (days)", xlab = "Dignity Category") +
  


boxplot1 + theme_classic()
boxplot1 + ordered(dignity_nonfamily$chochinov3,
                   levels = c("Illness-Related Concerns", 
                              "Dignity Conserving Repertoire", 
                              "Social Dignity Inventory",
                              "Other"))


# Create a table with the mean days to death for each categorical variables subgroups

group_by(dignity_nonfamily,agegroup) %>%
  summarise(
    count = n(),
    mean = mean(timetodeath_n, na.rm = TRUE),
    sd = sd(timetodeath_n, na.rm = TRUE),
    median = median(timetodeath_n, na.rm = TRUE),
    twentyfifth = quantile(timetodeath_n, probs = 0.25, na.rm = TRUE),
    seventyfifth = quantile(timetodeath_n, probs = 0.75, na.rm = TRUE)
  )

kruskal.test(timetodeath_n ~ racegroup, data = dignity_nonfamily)

group_by(dignity_nonfamily,racegroup) %>%
  summarise(
    count = n(),
    mean = mean(timetodeath_n, na.rm = TRUE),
    sd = sd(timetodeath_n, na.rm = TRUE),
    median = median(timetodeath_n, na.rm = TRUE),
    twentyfifth = quantile(timetodeath_n, probs = 0.25, na.rm = TRUE),
    seventyfifth = quantile(timetodeath_n, probs = 0.75, na.rm = TRUE)
  )

kruskal.test(timetodeath_n ~ racegroup, data = dignity_nonfamily)

group_by(dignity_nonfamily,ethnicity) %>%
  summarise(
    count = n(),
    mean = mean(timetodeath_n, na.rm = TRUE),
    sd = sd(timetodeath_n, na.rm = TRUE),
    median = median(timetodeath_n, na.rm = TRUE),
    twentyfifth = quantile(timetodeath_n, probs = 0.25, na.rm = TRUE),
    seventyfifth = quantile(timetodeath_n, probs = 0.75, na.rm = TRUE)
  )

kruskal.test(timetodeath_n ~ ethnicity, data = dignity_nonfamily)

group_by(dignity_nonfamily,ethnicity) %>%
  summarise(
    count = n(),
    mean = mean(timetodeath_n, na.rm = TRUE),
    sd = sd(timetodeath_n, na.rm = TRUE),
    median = median(timetodeath_n, na.rm = TRUE),
    twentyfifth = quantile(timetodeath_n, probs = 0.25, na.rm = TRUE),
    seventyfifth = quantile(timetodeath_n, probs = 0.75, na.rm = TRUE)
  )

kruskal.test(timetodeath_n ~ ethnicity, data = dignity_nonfamily)

group_by(dignity_nonfamily,religiongroup) %>%
  summarise(
    count = n(),
    mean = mean(timetodeath_n, na.rm = TRUE),
    sd = sd(timetodeath_n, na.rm = TRUE),
    median = median(timetodeath_n, na.rm = TRUE),
    twentyfifth = quantile(timetodeath_n, probs = 0.25, na.rm = TRUE),
    seventyfifth = quantile(timetodeath_n, probs = 0.75, na.rm = TRUE)
  )

kruskal.test(timetodeath_n ~ religiongroup, data = dignity_nonfamily)

group_by(dignity_nonfamily,tumorsitegroup) %>%
  summarise(
    count = n(),
    mean = mean(timetodeath_n, na.rm = TRUE),
    sd = sd(timetodeath_n, na.rm = TRUE),
    median = median(timetodeath_n, na.rm = TRUE),
    twentyfifth = quantile(timetodeath_n, probs = 0.25, na.rm = TRUE),
    seventyfifth = quantile(timetodeath_n, probs = 0.75, na.rm = TRUE)
  )

kruskal.test(timetodeath_n ~ tumorsitegroup, data = dignity_nonfamily)


group_by(dignity_nonfamily,chochinov3) %>%
  summarise(
    count = n(),
    mean = mean(timetodeath_n, na.rm = TRUE),
    sd = sd(timetodeath_n, na.rm = TRUE),
    median = median(timetodeath_n, na.rm = TRUE),
    twentyfifth = quantile(timetodeath_n, probs = 0.25, na.rm = TRUE),
    seventyfifth = quantile(timetodeath_n, probs = 0.75, na.rm = TRUE)
  )

kruskal.test(timetodeath_n ~ chochinov3, data = dignity_nonfamily)


lr1 = lm(timetodeath_n ~ agegroup + racegroup + tumorsitegroup + chochinov3, data = dignity_nonfamily)
summary(lr)

lr2 = lm(timetodeath_n ~ agegroup + racegroup + tumorsitegroup + code1nofam, data = dignity_nonfamily)
summary(lr2)

lr2 = lm(timetodeath_n ~ agegroup + racegroup + tumorsitegroup + code1nofam, data = dignity_nonfamily)
summary(lr2)

mlr1 <- multinom(formula = chochinov3 ~ timetodeath_n, data = dignity_nonfamily)
summary(mlr1)
exp(coef(mlr1))


group_by(dignity_nonfamily, racegroup) %>%
  summarise(
    count = n(),
    mean = mean(timetodeath_n, na.rm = TRUE),
    sd = sd(timetodeath_n, na.rm = TRUE)
  )



table(dignity$code1, dignity$deceased)
prop.table(table(dignity$code1, dignity$deceased), margin=1)*100

table(dignity_nonfamily$Code1, dignity_nonfamily$Deceased)
prop.table(table(dignity_nonfamily$Code1, dignity_nonfamily$Deceased), margin=1)*100
chisq.test(dignity_nonfamily$Code1, dignity_nonfamily$Deceased)

table(dignity_nonfamily$agegroup, dignity_nonfamily$code1)
prop.table(table(dignity_nonfamily$agegroup, dignity_nonfamily$code1), margin=1)*100
chisq.test(dignity_nonfamily$agegroup, dignity_nonfamily$code1)

table(dignity_nonfamily$racegroup, dignity_nonfamily$code1)
prop.table(table(dignity_nonfamily$racegroup, dignity_nonfamily$code1), margin=1)*100
chisq.test(dignity_nonfamily$racegroup, dignity_nonfamily$code1)

table(dignity_nonfamily$agegroup, dignity_nonfamily$chochinov3)
prop.table(table(dignity_nonfamily$agegroup, dignity_nonfamily$chochinov3), margin=1)*100
chisq.test(dignity_nonfamily$agegroup, dignity_nonfamily$chochinov3)

table(dignity_nonfamily$ethnicity, dignity_nonfamily$chochinov3)
prop.table(table(dignity_nonfamily$ethnicity, dignity_nonfamily$chochinov3), margin=1)*100
chisq.test(dignity_nonfamily$ethnicity, dignity_nonfamily$chochinov3)

table(dignity_nonfamily$, dignity_nonfamily$chochinov3)
prop.table(table(dignity_nonfamily$ethnicity, dignity_nonfamily$chochinov3), margin=1)*100
chisq.test(dignity_nonfamily$ethnicity, dignity_nonfamily$chochinov3)




table(dignity_nonfamily$chochinov3, dignity_nonfamily$Deceased)
prop.table(table(dignity_nonfamily$chochinov3, dignity_nonfamily$Deceased), margin=1)*100
chisq.test(dignity_nonfamily$chochinov3, dignity_nonfamily$Deceased)

levels(dignity_nonfamily$chochinov3)
table(dignity_nonfamily$chochinov3, dignity_nonfamily$timetodeath_n)


levels(dignity_nonfamily$chochinov3)
dignity_nonfamily$chochinov3 <- ordered(dignity_nonfamily$chochinov3,
                                    levels = c("Illness-Related Concerns", 
                                               "Dignity Conserving Repertoire", 
                                               "Social Dignity Inventory",
                                               "Other"))
group_by(dignity_nonfamily, chochinov3) %>%
  summarise(
    count = n(),
    mean = mean(timetodeath_n, na.rm = TRUE),
    sd = sd(timetodeath_n, na.rm = TRUE)
  )

plot1 <- ggboxplot(dignity_nonfamily, x = "chochinov3", y = "timetodeath_n", 
          color = "chochinov3", palette = c("#00AFBB", "#E7B800", "#FC4E07", "336600"),
          ylab = "Time to Death (days)", xlab = "Dignity Category") +
          scale_x_discrete(labels = c("Illness-\nRelates\nConcerns", "Dignity\nConserving\nRepertoire", 
                            "Social\nDignity\nInventory", "Other"))

plot1 + font("xy.text", size = 7, color = "black") +
        font("xlab", size = 10) +
        font("ylab", size = 10) +
        legend.title = element_blank()


font("title", size = 14, color = "red", face = "bold.italic")+
  font("subtitle", size = 10, color = "orange")+
  font("caption", size = 10, color = "orange")+
  font("xlab", size = 12, color = "blue")+
  font("ylab", size = 12, color = "#993333")+
  font("xy.text", size = 12, color = "gray", face = "bold")


last_plot() +  (axis.text.x = theme_text(size = 14))

res.aov <- aov(timetodeath_n ~ chochinov3, data = dignity_nonfamily)
summary(res.aov)

res.aov <- aov(timetodeath_n ~ code1, data = dignity_nonfamily)
summary(res.aov)


# Factor variables: code1, subcode1, agegroup, racegroup, ethnicity, religiongroup, tumorsitegroup, timetodeathgroup
# Numeric variuables: age_n, timetodeath_n

# mess around a bit with code
#ggplot(data = dignity) + geom_point(mapping = aes(x = code1, y=timetodeath_n))
#ggplot(data = dignity) + geom_point(mapping = aes(x = age_n, y=code1, color = subcode1))
#ggplot(data = dignity) + geom_point(mapping = aes(x = age_n, y=timetodeath_n))
#ggplot(data = dignity) + geom_point(mapping = aes(x = code1, y=subcode1, color = subcode1))
#ggplot(data = dignity) + geom_point(mapping = aes(x = age_n, y=timetodeath_n, color = code1))

# evaluate dignity code as a percentage of each of the factor variables for patients who responded
ggplot(data = dignity_nonfamily) + geom_bar(mapping = aes(x = agegroup,, fill = code1), position = "fill")
ggplot(data = dignity_nonfamily) + geom_bar(mapping = aes(x = racegroup, fill = code1), position = "fill")
ggplot(data = dignity_nonfamily) + geom_bar(mapping = aes(x = ethnicity, fill = code1), position = "fill")
ggplot(data = dignity_nonfamily) + geom_bar(mapping = aes(x = religiongroup, fill = code1), position = "fill")
ggplot(data = dignity_nonfamily) + geom_bar(mapping = aes(x = tumorsitegroup, fill = code1), position = "fill") + coord_flip()
ggplot(data = dignity_nonfamily) + geom_bar(mapping = aes(x = timetodeathgroup, fill = code1), position = "fill")

# evaluate dignity code as a percentage of each of the factor variables for family responses
ggplot(data = dignity_family) + geom_bar(mapping = aes(x = agegroup,, fill = subcode1), position = "fill")
ggplot(data = dignity_family) + geom_bar(mapping = aes(x = racegroup, fill = subcode1), position = "fill")
ggplot(data = dignity_family) + geom_bar(mapping = aes(x = ethnicity, fill = subcode1), position = "fill")
ggplot(data = dignity_family) + geom_bar(mapping = aes(x = religiongroup, fill = subcode1), position = "fill")
ggplot(data = dignity_family) + geom_bar(mapping = aes(x = tumorsitegroup, fill = subcode1), position = "fill") + coord_flip()
ggplot(data = dignity_family) + geom_bar(mapping = aes(x = timetodeathgroup, fill = subcode1), position = "fill")


# Look at the association between each independent variable and timetodeath_n as a function of code1
ggplot(data = dignity) + geom_point(mapping = aes(x = agegroup, y=timetodeath_n, color = code1))
ggplot(data = dignity) + geom_point(mapping = aes(x = racegroup, y=timetodeath_n, color = code1))

ggplot(data = dignity) + geom_bar(mapping = aes(x = agegroup, fill = code1))

freq(non)
ctable(dignity_nonfamily$ethnicity, dignity_nonfamily$chochinov3, chisq = TRUE)




























# EVERYTHING BELOW THIS LINE OF CODE IS SCRATCH WORK
 

Exploratory Data Analysis



ggplot(data = mpg) 

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = drv, y = class))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

library(readxl)
dignity1 <- read_excel("/Volumes/NO NAME/Dignity/mskcc with merged codes for analysis.xls")


ggplot(data = dignity1) + 
  geom_point(mapping = aes(x = Code1, y = TimetoDeathDays))

dignity1$days.f = factor(dignity1$TimetoDeathDays)
dignity1$days.n <- as.numeric(dignity1$days.f)

ggplot(data = dignity1) + 
  geom_point(mapping = aes(x = Code1, y = days.n, color = Subcode1))

dignity1$age.f = factor(dignity1$Age)
dignity1$age.n <- as.numeric(dignity1$age.f)

ggplot(data = dignity1) + 
  geom_point(mapping = aes(x = Code1, y = age.n, color = Religion))

ggplot(data = dignity1) + 
  geom_point(mapping = aes(x = Code1, y = age.n, color = Religion))

# 3-Way Frequency Table
mytable <- table(dignity1$Code1)
mytable

data(dignity1)
dignity1 = apply_labels(dignity1,
                      Code1 = "Primary Dignity Code",
                      Subcode1 = "Secondary Dignity Code",
                      Age = "Age (years)",
                      Race = "Race",
                      Ethnicity = "Ethnicity",
                      Religion = "Religion",
                      Deceased = "Vital Status",
                      TimetoDeathDays = "Duration from interview to death (days",
)

mytable <- table(dignity1$Code1, dignity1$Subcode1)
mytable
gt_tbl <- gt(data = mytable)
gt_tbl

library(expss)
data(mtcars)
mtcars = apply_labels(mtcars,
                      mpg = "Miles/(US) gallon",
                      cyl = "Number of cylinders",
                      disp = "Displacement (cu.in.)",
                      hp = "Gross horsepower",
                      drat = "Rear axle ratio",
                      wt = "Weight (1000 lbs)",
                      qsec = "1/4 mile time",
                      vs = "Engine",
                      vs = c("V-engine" = 0,
                             "Straight engine" = 1),
                      am = "Transmission",
                      am = c("Automatic" = 0,
                             "Manual"=1),
                      gear = "Number of forward gears",
                      carb = "Number of carburetors"
)





ggplot(data = dignity) + geom_point(mapping = aes(x = code1, y=timetodeath_n))
ggplot(data = dignity) + geom_point(mapping = aes(x = age_n, y=code1))
ggplot(data = dignity) + geom_point(mapping = aes(x = age_n, y=timetodeath_n))


ggplot(data = dignity_family) + geom_bar(mapping = aes(x = agegroup,, fill = subcode1), position = "fill")

ggplot(dignity) + geom_histogram(mapping = aes(x = age_n), binwidth = 5)
ggplot(dignity) + geom_histogram(mapping = aes(x = age_n, fill = racegroup), binwidth = 5)
dignity %>% count(cut_width(age_n, 10))

