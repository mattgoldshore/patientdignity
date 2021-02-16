#########################
## libraries to read in ##
##########################

library(expss)
library(readxl)
library(dplyr)
library(tidyverse)
library(summarytools)
library(ggpubr)

#######################
## import of dataset ##
#######################

dignity <- read_excel("/Volumes/NO NAME/Dignity/mskcc with merged codes for analysis.xls")

########################
## wrangle of dataset ##
########################

# var: pdq dignity response
  dignity$DignityResponse

# var: dignity code
  dignity$Code1
  typeof(dignity$Code1) # dignity$Code1 is a character variable
  dignity$code1 = factor(dignity$Code1)  # tranform Code1 to a factor variable
  dignity %>% count(code1) # 8 x 2 matrix of code1 and n
  # ggplot(dignity) + geom_bar(mapping = aes(x = code1))  # bar graph and summary for code1, horrible formating

# var: create the chochinov categorical variable based on code1
  dignity <- dignity %>% 
    mutate( 
      chochinov3 = case_when(code1 == "Coping strategies" ~ 'Dignity Conserving Repertoire',
                             code1 == "Goals" ~ 'Dignity Conserving Repertoire',
                             code1 == "Identity" ~ 'Dignity Conserving Repertoire',
                             code1 == "Fears" ~ 'Illness-Related Concerns',
                             code1 == "Symptoms" ~ 'Illness-Related Concerns',
                             code1 == "Interpersonal interaction" ~ 'Social Dignity Inventory',
                             code1 == "Other" ~ 'Other'))
  dignity$chochinov3 = factor(dignity$chochinov3)  # tranform chochinov3 to a factor variable
  dignity %>% count(chochinov3)

# var: Subcode1
  dignity$subcode1 = factor(dignity$Subcode1) # tranform Subcode1 to a factor variable
  # ggplot(dignity) + geom_bar(mapping = aes(x = subcode1)) + coord_flip() # bar graph and summary for subcode2
  dignity %>% count(subcode1)

# var4: Code2
  dignity$Code2

# var5: Subcode2
  dignity$Subcode2

# var6: AdmittingDiagnosis
  # no changes at this time

# var7: Age
  dignity$age_n = as.numeric(dignity$Age)  # Transform Age into a numeric variable
  # ggplot(dignity) + geom_histogram(mapping = aes(x = age_n), binwidth = 1) # histogram and summary of age as a continuous metric
  # for reference: dignity %>% count(cut_width(age_n, 10))
  dignity <- dignity %>% 
    mutate( 
       agegroup = case_when(age_n >= 90  & age_n <= 99 ~ '90 - 99',
                            age_n >= 80  & age_n <= 89 ~ '80 - 89',
                            age_n >= 70  & age_n <= 79 ~ '70 - 79',
                            age_n >= 60  & age_n <= 69 ~ '60 - 69',
                            age_n >= 50  & age_n <= 59 ~ '50 - 59',
                            age_n >= 40  & age_n <= 49 ~ '40 - 49',
                            age_n >= 30  & age_n <= 39 ~ '30 - 39',
                            age_n >= 20  & age_n <= 29 ~ '20 - 29')) # create categorical variable of age in groups of 10
  dignity %>% count(agegroup)

# var8: race
  
  dignity$race = factor(dignity$Race)  # tranform Race to a factor variable
  # ggplot(dignity) + geom_bar(mapping = aes(x = Race)) # bar graph and summary for race
  dignity %>% count(race)
  
  dignity <- dignity %>% 
  mutate(
    racegroup = case_when(race == "WHITE" ~ 'White',
                          race == "BLACK"  ~ 'Black',
                          race == "ASIAN"  ~ 'Asian',
                          race == "NATIVE AMERICAN" | Race == "OTHER" ~ 'Other',
                          race == "PT REFUSED TO ANSWER" | Race == "NO VALUE ENTERED" | Race == "Unknown" ~ 'Refused/Unknown'))  # create racegroup 
  
  dignity %>% count(racegroup)
  ggplot(dignity) + geom_bar(mapping = aes(x = racegroup)) # bar graph and summary for racegroup, alphabetical order by category
  
  dignity <- within(dignity, 
                  racegroup <- factor(racegroup, 
                                      levels=names(sort(table(racegroup), 
                                                        decreasing=TRUE)))) # descending frequency
  ggplot(dignity) + geom_bar(mapping = aes(x = racegroup)) # descending frequency
  
  dignity <- within(dignity, 
                  racegroup <- factor(racegroup, 
                                      levels=c('White', 'Black', 'Asian', 'Other', 'Refused/Unknown'))) # preferred data organization
  ggplot(dignity) + geom_bar(mapping = aes(x = racegroup)) # preferred data organization

# var9: ethnicity

  ggplot(dignity) + geom_bar(mapping = aes(x = Ethnicity)) # bar graph and summary for ethnicity  
  dignity$ethnicity = factor(dignity$Ethnicity)  # tranform Ethnicity to a factor variable
  dignity %>% count(ethnicity)
  dignity <- dignity %>% 
    mutate( 
      ethnicity = case_when(ethnicity == "HISPANIC OR LATINO" ~ 'Hispanic or Latino',
                          ethnicity == "NOT HISPANIC OR LATINO"  ~ 'Not Hispanic or Latino',
                          ethnicity == "NO VALUE ENTERED"  ~ 'Unknown')) # recode ethnicity
  dignity %>% count(ethnicity)
  ggplot(dignity) + geom_bar(mapping = aes(x = ethnicity))

  # this code to work through the way to order bars in the ggplot fuction
# this it the generic code
# theTable$Position <- factor(theTable$Position, levels = c(...))
# this is the code with order 'Hispanic or Latino' 'Not Hispanic or Latino' 'Unknown'
dignity <- within(dignity, 
                  ethnicity <- factor(ethnicity, 
                                      levels=c('Hispanic or Latino', 'Not Hispanic or Latino', 'Unknown')))
ggplot(dignity) + geom_bar(mapping = aes(x = ethnicity))
dignity <- within(dignity, 
                  ethnicity <- factor(ethnicity, 
                                      levels=c('Not Hispanic or Latino', 'Hispanic or Latino', 'Unknown')))
ggplot(dignity) + geom_bar(mapping = aes(x = ethnicity))



# var10: religion
# bar graph and summary for religion
ggplot(dignity) + geom_bar(mapping = aes(x = Religion)) + coord_flip()
dignity %>% count(Religion)
# recode religion into broader categories 
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
# this code renames all of the NA (blank) responses in the dignity$religiongroup variable to "Unknown/Refused"
dignity$religiongroup[is.na.data.frame(dignity$religiongroup)] <- "Unknown/Refused"
# graph and quantify the distribution of religions in the dataset
ggplot(dignity) + geom_bar(mapping = aes(x = religiongroup))
dignity %>% count(religiongroup)

# var11: TumorHistology
# bar graph and summary for TumorHistology
# ggplot(dignity) + geom_bar(mapping = aes(x = TumorHistology)) + coord_flip()
# dignity %>% count(TumorHistology)
# no changes at this time

# var12: TumorSite
# bar graph and summary for TumorHistology
ggplot(dignity) + geom_bar(mapping = aes(x = TumorSite)) + coord_flip()
dignity %>% count(TumorSite)
# recode tumorsite into broader categories 
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
                                  TumorSite == "C492-CONN, LOWER LIMB" ~ 'Musculoskeletal',
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
                                  TumorSite == "C444-SKIN, SCALP/NECK" |
                                  TumorSite == "C445-SKIN, TRUNK" | 
                                  TumorSite == "C446-SKIN, ARM/SHOULDER" |
                                  TumorSite == "C447-SKIN, LEG/HIP" | 
                                  TumorSite == "C449-SKIN, NOS" ~ 'Skin',
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
                                  TumorSite == "C509-BREAST, NOS" ~ 'Breast',
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
                                  TumorSite == "C629-TESTIS, NOS" ~ 'Germ cell',
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
                                  TumorSite == "C720-SPINAL CORD" ~ 'Neurologic',
                                  TumorSite == "C739-THYROID, NOS" |
                                  TumorSite == "C749-ADRENAL GLAND, NOS" ~ 'Endocrine and Neuroendocrine'))
# this code renames all of the NA (blank) responses in the dignity$tumorsitegroup variable to "Unknown Primary"
dignity$tumorsitegroup[is.na.data.frame(dignity$tumorsitegroup)] <- "Unknown Primary"
# graph and quantify the distribution of tumor sites in the dataset
ggplot(dignity) + geom_bar(mapping = aes(x = tumorsitegroup)) + coord_flip()
dignity %>% count(tumorsitegroup)

# var13: Deceased
# bar graph and summary for Deceased
ggplot(dignity) + geom_bar(mapping = aes(x = Deceased)) + coord_flip()
dignity %>% count(Deceased)
dignity$deceased = factor(dignity$Deceased)  # tranform Code1 to a factor variable
dignity %>% count(deceased)

# no changes at this time



# var14: TimetoDeathDays
# Transform TimetoDeathDays into a numeric variable
dignity$timetodeath_n = as.numeric(dignity$TimetoDeathDays)
# histogram and summary of n_timetodeath as a continuous metric
ggplot(dignity) + geom_histogram(mapping = aes(x = timetodeath_n), binwidth = 10)
dignity %>% count(cut_width(timetodeath_n, 50))
# create categorical variable of age in groups of 10
dignity <- dignity %>% 
  mutate( 
    timetodeathgroup = case_when(timetodeath_n >= 730  & timetodeath_n <= 999 ~  'More than 2 years',
                                 timetodeath_n >= 365  & timetodeath_n <= 730 ~  'More than 1 year',
                                 timetodeath_n >= 180  & timetodeath_n <= 365 ~  '180 - 365 Days',
                                 timetodeath_n >= 31  & timetodeath_n <= 180 ~   '30 - 180 Days',
                                 timetodeath_n >= 8  & timetodeath_n <= 30 ~     '7 - 30 Days',
                                 timetodeath_n >= 0  & timetodeath_n <= 7 ~      '<= 7 days')) 
dignity %>% count(timetodeathgroup)

########################################
## Stratify the dataset by respondent ##
########################################

dignity_nonfamily <- subset(dignity, dignity$code1 != "Family") # subset of dignity for patient-respondents 
dignity_family <-  subset(dignity, dignity$code1 == "Family") # subset of dignity for family-respondents

#########################
## Explore the dataset ##
#########################

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

