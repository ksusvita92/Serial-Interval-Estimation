# Create estimated distribution of the swine flu outbreak in Quebec, Canada
# with and without outliers
#


library(ggplot2)
library(patchwork)
library(dplyr)




## *** Load estimate results ****
## ******************************
load("./Result/h1n1_res.rda")
source("./R/Real Outbreak Analysis/Quebec outliers.R")
load("./Data/si_h1n1.rda")

oldpar <- h1n1_res$quebec$par
newpar <- h1n1_res2$quebec2$par


dt_fit <- data.frame(x = seq(0, 16, length.out = 100)) %>%
  mutate(f = oldpar[4]*dcgg(x,oldpar[1],oldpar[2],oldpar[3]) +
           (1-oldpar[4])*dfgd(x,oldpar[1],oldpar[2]),
         fnew = newpar[4]*dcgg(x,newpar[1],newpar[2],newpar[3]) +
           (1-newpar[4])*dfgd(x,newpar[1],newpar[2]))


pl1 <- ggplot() + stat_boxplot(aes(y = h1n1$quebec), coef = cf) +
  labs(y = "") + coord_flip() + theme_light() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        axis.ticks.y = element_blank())

dt_hist <- data.frame(x=h1n1$quebec) %>%
  group_by(x) %>% summarise(count = n()) %>% ungroup() %>%
  mutate(d = count/length(h1n1$quebec), col = ifelse(x != outlier$out, "normal", "outlier"))

pl2 <- ggplot() + geom_bar(aes(x, d, fill = col), dt_hist, stat = "identity", col = "gray") +
  geom_line(aes(x, f, col = "with outlier"), dt_fit) +
  geom_line(aes(x, fnew, col = "without outlier"), dt_fit) +
  scale_y_continuous(sec.axis = sec_axis(~.*length(h1n1$quebec), name = "count")) +
  scale_fill_manual(values = c("normal"="white","outlier"="black")) +
  scale_color_manual(values = c("with outlier"="red", "without outlier"="blue")) +
  labs(x = "days", y="density") + theme_light() + theme(legend.position = "none")

(pl1 + pl2) + plot_layout(ncol = 1, heights = c(1,4), widths = c(1,1)) +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12))


