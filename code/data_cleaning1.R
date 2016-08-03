# Data cleaning part 1
# read original data, create list of names for multi-column questions
# output: cleaning1.RData (data, new_name, old_name)

rm(list = ls(all.names = TRUE))
library(dplyr)

## load surveydata.csv
data <- read.csv("data/surveydata.csv", na = c("#NULL!", "", "Refused", "NA"))

## rename sub-columns
data_rename <- data %>%
  # Q7
  rename("Q7_1_Bus" = Q7_1,
         "Q7_2_Carpool" = Q7_2,
         "Q7_3_Subway" = Q7_3,
         "Q7_4_Train" = Q7_4,
         "Q7_5_Taxi" = Q7_5,
         "Q7_6_Airplane" = Q7_6,
         "Q7_7_Other" = Q7_7,
         "Q7_8_Refused" = Q7_8) %>%
  # Q8
  rename("Q8_1_Work" = Q8_1,
         "Q8_2_School" = Q8_2,
         "Q8_3_Shopping" = Q8_3,
         "Q8_4_Visiting.people" = Q8_4,
         "Q8_5_Recreation" = Q8_5,
         "Q8_6_Other" = Q8_6,
         "Q8_7_Refused"= Q8_7) %>%
  # Q10
  rename("Q10_1_Bus" = Q10_1,
         "Q10_2_Carpool" = Q10_2,
         "Q10_3_Subway" = Q10_3,
         "Q10_4_Train" = Q10_4,
         "Q10_5_Taxi" = Q10_5,
         "Q10_6_Airplane" = Q10_6,
         "Q10_7_Don_t.know" = Q10_7,
         "Q10_8_Other" = Q10_8,
         "Q10_9_Refused" = Q10_9) %>%
  # Q11
  rename("Q11_1_Work" = Q11_1,
         "Q11_2_Schools" = Q11_2,
         "Q11_3_Day.care" = Q11_3,
         "Q11_4_Stores" = Q11_4,
         "Q11_5_Restaurants" = Q11_5,
         "Q11_6_Libraries" = Q11_6,
         "Q11_7_Hospitals" = Q11_7,
         "Q11_8_Doctor_s.office" = Q11_8,
         "Q11_9_Public.transportation" = Q11_9,
         "Q11_10_Family.or.friends" = Q11_10,
         "Q11_11_Other" = Q11_11) %>%
  # Q12
  rename("Q12_1_Avoid.touching.my.eyes" = Q12_1,
         "Q12_2_Avoid.touching.my.nose" = Q12_2,
         "Q12_3_Avoid.touching.my.mouth" =	Q12_3,
         "Q12_4_Wash.my.hands.with.soap.more.often"= Q12_4,
         "Q12_5_Use.hand.sanitizers" = Q12_5,
         "Q12_6_Clean.the.surfaces.in.my.home" =	Q12_6,
         "Q12_7_Clean.the.surfaces.at.work" = Q12_7,
         "Q12_8_Eat.nutritious.food" = Q12_8,
         "Q12_9_Get.adequate.rest" = Q12_9,
         "Q12_10_Get.recommended.vaccine" =	Q12_10,
         "Q12_11_Take.preventive.medicine" = Q12_11,
         "Q12_12_Cover.my.nose.and.mouth.with.a.surgical.mask" = Q12_12,
         "Q12_13_Avoid.contact.with.people.who.are.sick" = Q12_13,
         "Q12_14_Avoid.crowded.places" = Q12_14,
         "Q12_15_Other" = Q12_15) %>%
  # Q18
  rename("Q18_1_The.vaccine.costs.too.much" = Q18_1,
         "Q18_2_The.vaccine.is.not.very.effective.in.preventing.influenza" = Q18_2,
         "Q18_3_I.am.not.likely.to.get.influenza" = Q18_3,
         "Q18_4_Do.not.know.where.to.get.vaccine" = Q18_4,
         "Q18_5_The.side.effect.of.the.vaccine.are.too.risky" = Q18_5,
         "Q18_6_I.am.allergic.to.some.of.the.ingredients.in.the.vaccine" = Q18_6,
         "Q18_7_I.do.not.like.shots" = Q18_7,
         "Q18_8_I.just.don_t.get.around.to.doing.it" = Q18_8,
         "Q18_9_I.have.to.travel.too.far.to.get.vaccine" = Q18_9,
         "Q18_10_Other" = Q18_10,
         "Q18_11_Refused" = Q18_11) %>%
  # Q22
  rename("Q22_1_Go.to.a.doctor_s.office.or.medical.clinic" = Q22_1,
         "Q22_2_Decide.on.treatment.without.consulting.a.health.practitioner" = Q22_2,
         "Q22_3_Search.the.internet.for.a.treatment" = Q22_3,
         "Q22_4_Get.adequate.sleep" = Q22_4,
         "Q22_5_Eat.nutritious.food" = Q22_5,
         "Q22_6_Take-over-counter.medication.for.symptoms" = Q22_6,
         "Q22_7_Take.an.antiviral.medicine" = Q22_7,
         "Q22_8_Take.no.action.to.treat.the.illness" = Q22_8,
         "Q22_9_Other" = Q22_9) %>%
  # Q23
  rename("Q23_1_Stand.away.from.people" = Q23_1,
         "Q23_2_Avoid.public.places" = Q23_2,
         "Q23_3_Avoid.public.transportation" = Q23_3,
         "Q23_4_Stay.at.home" = Q23_4,
         "Q23_5_Wash.my.hands.with.soap.more.often" = Q23_5,
         "Q23_6_Use.hand.sanitizers" = Q23_6,
         "Q23_7_Clean.the.surfaces.in.my.home" = Q23_7,
         "Q23_8_Clean.the.surfaces.I.use.at.work" = Q23_8,
         "Q23_9_Cover.my.nose.and.mouth.with.a.surgical.mask" = Q23_9,
         "Q23_10_Cover.my.nose.and.mouth.when.I.sneeze.or.cough" = Q23_10,
         "Q23_11_Other" = Q23_11) %>%
  # Q24
  rename("Q24_1_Print.media.such.as.newspapers.and.magazines" = Q24_1,
         "Q24_2_Traditional.media.such.as.television.and.radio" = Q24_2,
         "Q24_3_Social.media.such.as.internet.and.blogs" = Q24_3,
         "Q24_4_Word.of.mouth" = Q24_4,
         "Q24_5_None" = Q24_5,
         "Q24_6_Other" = Q24_6,
         "Q24_7_Refused" = Q24_7) %>%
  # Q25
  rename("Q25_1_Stand.away.from.people" = Q25_1,
         "Q25_2_Avoid.public.places" = Q25_2,
         "Q25_3_Avoid.public.transportation" = Q25_3,
         "Q25_4_Stay.at.home" = Q25_4,
         "Q25_5_Wash.my.hands.with.soap.more.often" = Q25_5,
         "Q25_6_Use.hand.sanitizers" = Q25_6,
         "Q25_7_Clean.the.surfaces.in.my.home" = Q25_7,
         "Q25_8_Clean.the.surfaces.I.use.at.work" = Q25_8,
         "Q25_9_Cover.my.nose.and.mouth.with.a.surgical.mask" = Q25_9,
         "Q25_10_Cover.my.nose.and.mouth.when.I.sneeze.or.cough" = Q25_10,
         "Q25_11_Other" = Q25_11) %>%
  # Q27
  rename("Q27_1_Keep.the.child.away.from.the.others.in.the.residence" = Q27_1,
         "Q27_2_Keep.the.child.out.of.school-daycare" = Q27_2,
         "Q27_3_Stop.child_s.social.activities.like.play.dates" =	Q27_3,
         "Q27_4_Other" = Q27_4) %>%
  # Q29
  rename("Q29_1_A.parent.brings.the.child.to.work" = Q29_1,
         "Q29_2_A.parent.stays.home" = Q29_2,
         "Q29_3_Another.adult.stays.home" = Q29_3,
         "Q29_4_Send.the.child.to.school.sick" = Q29_4,
         "Q29_5_Take.the.child.to.a.relative.or.friends" =	Q29_5,
         "Q29_6_Other" = Q29_6) %>%
  # Q30
  rename("Q30_1_I.bring.the.child.to.work" = Q30_1,
         "Q30_2_I.stay.home" = Q30_2,
         "Q30_3_Another.adult.stays.home" = Q30_3,
         "Q30_4_Send.the.child.to.school.sick" = Q30_4,
         "Q30_5_Take.the.child.to.a.relative.or.friends" = Q30_5,
         "Q30_6_Other" = Q30_6)


# save lists of names
old_name <- names(data)
new_name <- names(data_rename)

save(data, old_name, new_name, file = "clean/cleaning1.RData")



