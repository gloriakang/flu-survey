# plotting.R
# ggplot guides

rm(list = ls(all.names = TRUE))
library(ggplot2)

load("data/cleaning2.RData")
rm(dat); rm(new_name); rm(old_name)  # just keep dataf

# temporarily called data2
data2 <- dataf

g1 <- ggplot(data2[!is.na(data2$Q1), ]) + theme(text = element_text(size = 10), axis.text.x = element_text(angle = 45, hjust = 1))
g2 <- ggplot(data2[!is.na(data2$Q2), ]) + theme(text = element_text(size = 10), axis.text.x = element_text(angle = 45, hjust = 1))
g3 <- ggplot(data2[!is.na(data2$Q3), ]) + theme(text = element_text(size = 10), axis.text.x = element_text(angle = 45, hjust = 1))
g4 <- ggplot(data2[!is.na(data2$Q4), ]) + theme(text = element_text(size = 10), axis.text.x = element_text(angle = 45, hjust = 1))
g5 <- ggplot(data2[!is.na(data2$Q5), ]) + theme(text = element_text(size = 10), axis.text.x = element_text(angle = 45, hjust = 1))
g6 <- ggplot(data2[!is.na(data2$Q6), ]) + theme(text = element_text(size = 10), axis.text.x = element_text(angle = 45, hjust = 1))
#
g9 <- ggplot(data2[!is.na(data2$Q9), ]) + theme(text = element_text(size = 10), axis.text.x = element_text(angle = 45, hjust = 1))
#
g13 <- ggplot(data2[!is.na(data2$Q13), ]) + theme(text = element_text(size = 10), axis.text.x = element_text(angle = 45, hjust = 1))
g14 <- ggplot(data2[!is.na(data2$Q14), ]) + theme(text = element_text(size = 10), axis.text.x = element_text(angle = 45, hjust = 1))
g15 <- ggplot(data2[!is.na(data2$Q15), ]) + theme(text = element_text(size = 10), axis.text.x = element_text(angle = 45, hjust = 1))
g16 <- ggplot(data2[!is.na(data2$Q16), ]) + theme(text = element_text(size = 10), axis.text.x = element_text(angle = 45, hjust = 1))
g17 <- ggplot(data2[!is.na(data2$Q17), ]) + theme(text = element_text(size = 10), axis.text.x = element_text(angle = 45, hjust = 1))
#
g19 <- ggplot(data2[!is.na(data2$Q19), ]) + theme(text = element_text(size = 10), axis.text.x = element_text(angle = 45, hjust = 1))
g20 <- ggplot(data2[!is.na(data2$Q20), ]) + theme(text = element_text(size = 10), axis.text.x = element_text(angle = 45, hjust = 1))
g21 <- ggplot(data2[!is.na(data2$Q21), ]) + theme(text = element_text(size = 10), axis.text.x = element_text(angle = 45, hjust = 1))
#
g26 <- ggplot(data2[!is.na(data2$Q26), ]) + theme(text = element_text(size = 10), axis.text.x = element_text(angle = 45, hjust = 1))
g28 <- ggplot(data2[!is.na(data2$Q28), ]) + theme(text = element_text(size = 10), axis.text.x = element_text(angle = 45, hjust = 1))
#
g31 <- ggplot(data2[!is.na(data2$Q31), ]) + theme(text = element_text(size = 10), axis.text.x = element_text(angle = 45, hjust = 1))
g32 <- ggplot(data2[!is.na(data2$Q32), ]) + theme(text = element_text(size = 10), axis.text.x = element_text(angle = 45, hjust = 1))
g33 <- ggplot(data2[!is.na(data2$Q33), ]) + theme(text = element_text(size = 10), axis.text.x = element_text(angle = 45, hjust = 1))



#save(list = ls(all.names = TRUE), file = "data/plotting.RData")
