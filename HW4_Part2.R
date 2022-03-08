#------------------------------------------------------------------------------
# 4
#------------------------------------------------------------------------------
library(ggplot2)

#EXAMPLE
cars93 <- MASS::Cars93
ggplot(cars93, aes(x = Price, y = Fuel.tank.capacity)) +
  geom_point(color = "grey60") +
  geom_smooth(se = TRUE, method = "loess", formula = y ~ x, color = "#0072B2") +
  scale_x_continuous(
    name = "price (USD)",
    breaks = c(20, 40, 60),
    labels = c("$20,000", "$40,000", "$60,000")
  ) +
  scale_y_continuous(name = "fuel-tank capacity\n(US gallons)") +
  ggtitle("LOESS Smoothing Method") +
  theme(plot.title = element_text(size = 14, color = "#0072b2"))

#LM
ggplot(cars93, aes(x = Price, y = Fuel.tank.capacity)) +
  geom_point(color = "grey60") +
  geom_smooth(se = TRUE, method = "lm", formula = y ~ x, color = "#8fe388") +
  scale_x_continuous(
    name = "price (USD)",
    breaks = c(20, 40, 60),
    labels = c("$20,000", "$40,000", "$60,000")
  ) +
  scale_y_continuous(name = "fuel-tank capacity\n(US gallons)") +
  ggtitle("LM Smoothing Method") + 
  theme(plot.title = element_text(size = 14, color = "#8fe388"))

#GLM
ggplot(cars93, aes(x = Price, y = Fuel.tank.capacity)) +
  geom_point(color = "grey60") +
  geom_smooth(se = TRUE, method = "glm", formula = y ~ x, color = "#fe8d6d") +
  scale_x_continuous(
    name = "price (USD)",
    breaks = c(20, 40, 60),
    labels = c("$20,000", "$40,000", "$60,000")
  ) +
  scale_y_continuous(name = "fuel-tank capacity\n(US gallons)") +
  ggtitle("GLM Smoothing Method") + 
  theme(plot.title = element_text(size = 14, color = "#fe8d6d"))

#GAM
ggplot(cars93, aes(x = Price, y = Fuel.tank.capacity)) +
  geom_point(color = "grey60") +
  geom_smooth(se = TRUE, method = "gam", formula = y ~ x, color = "#7c6bea") +
  scale_x_continuous(
    name = "price (USD)",
    breaks = c(20, 40, 60),
    labels = c("$20,000", "$40,000", "$60,000")
  ) +
  scale_y_continuous(name = "fuel-tank capacity\n(US gallons)") +
  ggtitle("GAM Smoothing Method") +
  theme(plot.title = element_text(size = 14, color = "#7c6bea"))

#------------------------------------------------------------------------------
# 5
#------------------------------------------------------------------------------

load("D:/Users/Jeremy/Desktop/Online_Assignments/Winter 2022/CSC 302/Slides12Rscripts_Data/preprint_growth.rda") #please change the path if needed

head(preprint_growth)

preprint_growth %>% filter(archive == "bioRxiv") %>%
  filter(count > 0) -> biorxiv_growth

preprints<-preprint_growth %>% filter(archive %in%
  c("bioRxiv", "arXiv q-bio", "PeerJ Preprints")) %>%filter(count > 0) %>%
  mutate(archive = factor(archive, levels = c("bioRxiv", "arXiv q-bio", "PeerJ Preprints")))

preprints_final <- filter(preprints, date == ymd("2017-01-01"))

preprint_full<-preprint_growth %>% filter(archive %in% c("bioRxiv", "F1000Research")) %>% filter(count > 0) %>% filter(date > ymd("2014-01-01")) %>% drop_na()

ggplot(preprints) +
  aes(date, count, color = archive, fill = archive) +
  geom_line(size = 1) +
  scale_y_continuous(
    limits = c(0, 600), expand = c(0, 0),
    name = "preprints / month",
    sec.axis = dup_axis( #this part is for the second y axis
      breaks = preprints_final$count, #and we use the counts to position our labels
      labels = c("arXivq-bio", "PeerJPreprints", "bioRxiv"),
      name = NULL)
  ) +
  scale_x_date(name = "year",
               limits = c(min(biorxiv_growth$date), ymd("2017-01-01"))) +
  scale_color_manual(values = c("#0072b2", "#D55E00", "#009e73"),
                     name = NULL) +
  theme(legend.position = "none")

#preprint_full
ggplot(preprint_full) +
  aes(date, count, color = archive, fill = archive) +
  geom_line(size = 1) +
  labs(title = "Preprint Counts") +
  scale_x_date(name = "year",
               limits = c(min(preprint_full$date), ymd("2018-01-01"))) +
  scale_color_manual(values = c("#7c6bea", "#fe8d6d"), 
                     name = NULL) +
  theme(legend.position = "right")

