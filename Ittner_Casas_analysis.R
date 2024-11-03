library(haven)
library(dplyr)
library(readxl)
library(stringr)
library(ggplot2)
library(RColorBrewer)
library(cowplot)
library(plotly)
library(gapminder)
library(htmlwidgets)

rm(list = ls())

################################################################################
#import ITD
################################################################################

data <- read_excel("incarceration_trends.xlsx")

################################################################################
#create vars
################################################################################

data <- data %>%
  mutate(bw = black_jail_pop_rate/white_jail_pop_rate,
         jail_capacity_rate = jail_rated_capacity/total_pop_15to64*100000)

################################################################################
#Figure of jail population rate by urbanicity over time
################################################################################

rural <- as.data.frame(tapply(data[which(data$urbanicity=="rural"),]$total_jail_pop_rate,data[which(data$urbanicity=="rural"),]$year,median,na.rm=TRUE)) 
rural <- rural %>% 
  rename(pop_median = c("tapply(data[which(data$urbanicity == \"rural\"), ]$total_jail_pop_rate, ")) %>%
  mutate(year = 1970:2018,
         urbanicity = "rural")
rownames(rural) <- NULL

small <- as.data.frame(tapply(data[which(data$urbanicity=="small/mid"),]$total_jail_pop_rate,data[which(data$urbanicity=="small/mid"),]$year,median,na.rm=TRUE)) 
small <- small %>% 
  rename(pop_median = c("tapply(data[which(data$urbanicity == \"small/mid\"), ]$total_jail_pop_rate, ")) %>%
  mutate(year = 1970:2018,
         urbanicity = "small")
rownames(small) <- NULL

suburban <- as.data.frame(tapply(data[which(data$urbanicity=="suburban"),]$total_jail_pop_rate,data[which(data$urbanicity=="suburban"),]$year,median,na.rm=TRUE)) 
suburban <- suburban %>% 
  rename(pop_median = c("tapply(data[which(data$urbanicity == \"suburban\"), ]$total_jail_pop_rate, ")) %>%
  mutate(year = 1970:2018,
         urbanicity = "suburban")
rownames(suburban) <- NULL

urban <- as.data.frame(tapply(data[which(data$urbanicity=="urban"),]$total_jail_pop_rate,data[which(data$urbanicity=="urban"),]$year,median,na.rm=TRUE))
urban <- urban %>% 
  rename(pop_median = c("tapply(data[which(data$urbanicity == \"urban\"), ]$total_jail_pop_rate, ")) %>%
  mutate(year = 1970:2018,
         urbanicity = "urban")
rownames(urban) <- NULL

pop_data <- rbind(rural,small,suburban,urban)
rm(rural,small,suburban,urban)

pop_data <- pop_data[which(pop_data$year>=1978),] %>%
   mutate(year = as.character(year))

# Subsetting data to only be 2018
pop_data_2018 <- pop_data[which(pop_data$year==2018),]

# Original plot 
g1 <- ggplot(pop_data, aes(x=year, 
                           y = pop_median, 
                           color = urbanicity,
                           group = urbanicity)) +
  geom_line(size = 2) +
  ylab("Total Jail Population Rate") +
  ggtitle(element_blank()) +
  theme_bw() +
  theme(legend.text = element_text(size=10,
                                   family = "serif"),
        axis.text.x = element_text(size=12,
                                   family = "serif",
                                   angle = 45, 
                                   vjust = 1, 
                                   hjust=1,
                                   color = "black"),
        axis.text.y = element_text(size=12,
                                   family = "serif",
                                   color = "black"),
        axis.title.y = element_text(size=15,
                                    family = "serif"),
        legend.title=element_blank(),
        legend.position=c(0.1,0.9),
        axis.title.x = element_blank()) +
  scale_color_manual(values = c("#ffb000","#648fff","#dc267f","#cbc3e3"),
                     labels=c("Rural (N=1976)",
                              "Small/mid metro (N=730)",
                              "Suburban (N=368)",
                              "Urban (N=64)")) +
  scale_x_discrete(breaks=c(1978,1983,1988,1993,1998,2003,2008,2013,2018),
                   labels=c("1978","1983","1988","1993","1998","2003","2008","2013","2018"))

# Plot that would print in b/w
g1_bw <- ggplot(pop_data, aes(x=year, 
                           y = pop_median)) + 
  geom_line(size = 1, aes(group = urbanicity, linetype = urbanicity)) +
  ylab("Total Jail Population Rate") +
  ggtitle(element_blank()) +
  theme_bw() +
  theme(legend.text = element_text(size=10,
                                   family = "serif"),
        axis.text.x = element_text(size=12,
                                   family = "serif",
                                   angle = 45, 
                                   vjust = 1, 
                                   hjust=1,
                                   color = "black"),
        axis.text.y = element_text(size=12,
                                   family = "serif",
                                   color = "black"),
        axis.title.y = element_text(size=15,
                                    family = "serif"),
        legend.title=element_blank(),
        legend.position=c(0.1,0.9),
        axis.title.x = element_blank()) +
  scale_linetype_manual(values = c("dotdash", "solid", "longdash", "dotted"), 
                        labels=c("Rural (N=1976)",
                                 "Small/mid metro (N=730)",
                                 "Suburban (N=368)",
                                 "Urban (N=64)")) +
  scale_x_discrete(breaks=c(1978,1983,1988,1993,1998,2003,2008,2013,2018),
                   labels=c("1978","1983","1988","1993","1998","2003","2008","2013","2018"))

# Bar plot with 2018 data 
g1_2018 <- ggplot(pop_data_2018, aes(x=urbanicity, y=pop_median)) + 
  geom_bar(stat = "identity", width = .5, fill = "darkred") + 
  theme_classic() +
  scale_x_discrete(labels = c("Rural", "Small/mid metro", "Suburban", "Urban")) +
  labs(x = NULL, y = "Total Jail Population Rate, 2018") +
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.title.y = element_text(size=12,
                                    family = "serif",
                                    color = "black"),
        axis.text.y = element_blank(),      
        axis.ticks.y = element_blank()) + 
  geom_text(aes(label = round(pop_median, 0)),       # Adds labels to bars
            vjust = 1.5,                  # Adjusts vertical position
            color = "white",               # Text color
            size = 4) 

# Distorted original plot 
g1_distort <- ggplot(pop_data, aes(x=year, 
                           y = pop_median, 
                           color = urbanicity,
                           group = urbanicity)) +
  geom_line(size = 2) +
  ylab("Total Jail \n Population Rate") +
  ggtitle(element_blank()) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(size=12,
                                   family = "serif",
                                   angle = 45, 
                                   vjust = 1, 
                                   hjust=1,
                                   color = "black"),
        axis.text.y = element_text(size=12,
                                   family = "serif",
                                   color = "black"),
        axis.title.y = element_text(size=15,
                                    family = "serif"),
        axis.title.x = element_blank(),
        aspect.ratio = 7) +
  scale_color_manual(values = c("#ffb000","#648fff","#dc267f","#cbc3e3")) +
  scale_x_discrete(breaks=c(1978,1983,1988,1993,1998,2003,2008,2013,2018),
                   labels=c("1978","1983","1988","1993","1998","2003","2008","2013","2018"))

################################################################################
#Figure of jail admission rate by urbanicity over time
################################################################################

rural <- as.data.frame(tapply(data[which(data$urbanicity=="rural"),]$total_jail_adm_rate,data[which(data$urbanicity=="rural"),]$year,median,na.rm=TRUE)) 
rural <- rural %>% 
  rename(pop_median = c("tapply(data[which(data$urbanicity == \"rural\"), ]$total_jail_adm_rate, ")) %>%
  mutate(year = 1970:2018,
         urbanicity = "rural")
rownames(rural) <- NULL

small <- as.data.frame(tapply(data[which(data$urbanicity=="small/mid"),]$total_jail_adm_rate,data[which(data$urbanicity=="small/mid"),]$year,median,na.rm=TRUE)) 
small <- small %>% 
  rename(pop_median = c("tapply(data[which(data$urbanicity == \"small/mid\"), ]$total_jail_adm_rate, ")) %>%
  mutate(year = 1970:2018,
         urbanicity = "small")
rownames(small) <- NULL

suburban <- as.data.frame(tapply(data[which(data$urbanicity=="suburban"),]$total_jail_adm_rate,data[which(data$urbanicity=="suburban"),]$year,median,na.rm=TRUE)) 
suburban <- suburban %>% 
  rename(pop_median = c("tapply(data[which(data$urbanicity == \"suburban\"), ]$total_jail_adm_rate, ")) %>%
  mutate(year = 1970:2018,
         urbanicity = "suburban")
rownames(suburban) <- NULL

urban <- as.data.frame(tapply(data[which(data$urbanicity=="urban"),]$total_jail_adm_rate,data[which(data$urbanicity=="urban"),]$year,median,na.rm=TRUE))
urban <- urban %>% 
  rename(pop_median = c("tapply(data[which(data$urbanicity == \"urban\"), ]$total_jail_adm_rate, ")) %>%
  mutate(year = 1970:2018,
         urbanicity = "urban")
rownames(urban) <- NULL

adm_data <- rbind(rural,small,suburban,urban)
rm(rural,small,suburban,urban)

adm_data <- adm_data[which(adm_data$year>=1978),] %>%
  mutate(year = as.character(year))

# subsetting 2018 data 
adm_data_2018 <- adm_data[which(adm_data$year==2018),]

# Original plot 
g2 <- ggplot(adm_data, aes(x=year, 
                           y = pop_median, 
                           color = urbanicity,
                           group = urbanicity)) +
  geom_line(size = 2) +
  ylab("Total Jail Admission Rate") +
  ggtitle(element_blank()) +
  theme_bw() +
  theme(text = element_text(size = 15,
                            family = "serif",
                            color = "black"),
        legend.title=element_blank(),
        legend.position="none",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, color = "black"),
        axis.text.y = element_text(color = "black")) +
  scale_color_manual(values = c("#ffb000","#648fff","#dc267f","#cbc3e3"),
                     labels=c("Rural (N=1976)",
                              "Small/mid metro (N=730)",
                              "Suburban (N=368)",
                              "Urban (N=64)")) +
  scale_x_discrete(breaks=c(1978,1983,1988,1993,1998,2003,2008,2013,2018),
                   labels=c("1978","1983","1988","1993","1998","2003","2008","2013","2018"))


# plot that can be printed in b/w
g2_bw <- ggplot(adm_data, aes(x=year, 
                           y = pop_median)) +
  geom_line(size = 1, aes(group = urbanicity, linetype = urbanicity)) +
  ylab("Total Jail Admission Rate") +
  ggtitle(element_blank()) +
  theme_bw() +
  theme(text = element_text(size = 15,
                            family = "serif",
                            color = "black"),
        legend.title=element_blank(),
        legend.position="none",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, color = "black"),
        axis.text.y = element_text(color = "black")) +
  scale_linetype_manual(values = c("dotdash", "solid", "longdash", "dotted"), 
                        labels=c("Rural (N=1976)",
                                 "Small/mid metro (N=730)",
                                 "Suburban (N=368)",
                                 "Urban (N=64)")) +
  scale_x_discrete(breaks=c(1978,1983,1988,1993,1998,2003,2008,2013,2018),
                   labels=c("1978","1983","1988","1993","1998","2003","2008","2013","2018"))

# Bar plot with 2018 results 
g2_2018 <- ggplot(adm_data_2018, aes(x=urbanicity, y=pop_median)) + 
  geom_bar(stat = "identity", width = .5, fill = "darkred") + 
  theme_classic() +
  scale_x_discrete(labels = c("Rural", "Small/mid metro", "Suburban", "Urban")) +
  labs(x = NULL, y = "Total Jail Admission Rate, 2018") +
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.title.y = element_text(size=12,
                                    family = "serif",
                                    color = "black"),
        axis.text.y = element_blank(),      
        axis.ticks.y = element_blank()) + 
  geom_text(aes(label = round(pop_median, 0)),       # Adds labels to bars
            vjust = 1.5,                  # Adjusts vertical position
            color = "white",               # Text color
            size = 4) 

# Distorted original plot 
g2_distort <- ggplot(adm_data, aes(x=year, 
                           y = pop_median, 
                           color = urbanicity,
                           group = urbanicity)) +
  geom_line(size = 2) +
  ylab("Total Jail \n Admission Rate") +
  ggtitle(element_blank()) +
  theme_bw() +
  theme(text = element_text(size = 15,
                            family = "serif",
                            color = "black"),
        legend.title=element_blank(),
        legend.position="none",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, color = "black"),
        axis.text.y = element_text(color = "black"),
        aspect.ratio = 7) +
  scale_color_manual(values = c("#ffb000","#648fff","#dc267f","#cbc3e3"),
                     labels=c("Rural (N=1976)",
                              "Small/mid metro (N=730)",
                              "Suburban (N=368)",
                              "Urban (N=64)")) +
  scale_x_discrete(breaks=c(1978,1983,1988,1993,1998,2003,2008,2013,2018),
                   labels=c("1978","1983","1988","1993","1998","2003","2008","2013","2018"))

################################################################################
#Figure of pretrial jail population rate by urbanicity over time
################################################################################

rural <- as.data.frame(tapply(data[which(data$urbanicity=="rural"),]$total_jail_pretrial_rate,data[which(data$urbanicity=="rural"),]$year,median,na.rm=TRUE)) 
rural <- rural %>% 
  rename(pop_median = c("tapply(data[which(data$urbanicity == \"rural\"), ]$total_jail_pretrial_rate, ")) %>%
  mutate(year = 1970:2018,
         urbanicity = "rural")
rownames(rural) <- NULL

small <- as.data.frame(tapply(data[which(data$urbanicity=="small/mid"),]$total_jail_pretrial_rate,data[which(data$urbanicity=="small/mid"),]$year,median,na.rm=TRUE)) 
small <- small %>% 
  rename(pop_median = c("tapply(data[which(data$urbanicity == \"small/mid\"), ]$total_jail_pretrial_rate, ")) %>%
  mutate(year = 1970:2018,
         urbanicity = "small")
rownames(small) <- NULL

suburban <- as.data.frame(tapply(data[which(data$urbanicity=="suburban"),]$total_jail_pretrial_rate,data[which(data$urbanicity=="suburban"),]$year,median,na.rm=TRUE)) 
suburban <- suburban %>% 
  rename(pop_median = c("tapply(data[which(data$urbanicity == \"suburban\"), ]$total_jail_pretrial_rate, ")) %>%
  mutate(year = 1970:2018,
         urbanicity = "suburban")
rownames(suburban) <- NULL

urban <- as.data.frame(tapply(data[which(data$urbanicity=="urban"),]$total_jail_pretrial_rate,data[which(data$urbanicity=="urban"),]$year,median,na.rm=TRUE))
urban <- urban %>% 
  rename(pop_median = c("tapply(data[which(data$urbanicity == \"urban\"), ]$total_jail_pretrial_rate, ")) %>%
  mutate(year = 1970:2018,
         urbanicity = "urban")
rownames(urban) <- NULL

pretrial_data <- rbind(rural,small,suburban,urban)
rm(rural,small,suburban,urban)

pretrial_data <- pretrial_data[which(pretrial_data$year>=1978),] %>%
  mutate(year = as.character(year))

# Subsetting data for 2018 
pretrial_data_2018 <- pretrial_data[which(pretrial_data$year==2018),]

# original plot 
g3 <- ggplot(pretrial_data, aes(x=year, 
                                y = pop_median, 
                                color = urbanicity,
                                group = urbanicity)) +
  geom_line(size = 2) +
  ylab("Pretrial Jail Population Rate") +
  ggtitle(element_blank()) +
  theme_bw() +
  theme(text = element_text(size = 15,
                            family = "serif",
                            color = "black"),
        legend.title=element_blank(),
        legend.position="none",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, color = "black"),
        axis.text.y = element_text(color = "black")) +
  scale_color_manual(values = c("#ffb000","#648fff","#dc267f","#cbc3e3"),
                     labels=c("Rural (N=1976)",
                              "Small/mid metro (N=730)",
                              "Suburban (N=368)",
                              "Urban (N=64)")) +
  scale_x_discrete(breaks=c(1978,1983,1988,1993,1998,2003,2008,2013,2018),
                   labels=c("1978","1983","1988","1993","1998","2003","2008","2013","2018"))


# plot that can be printed in b/w
g3_bw <- ggplot(pretrial_data, aes(x=year, 
                                  y = pop_median)) +
  geom_line(size = 1, aes(group = urbanicity, linetype = urbanicity)) +
  ylab("Pretrial Jail Population Rate") +
  ggtitle(element_blank()) +
  theme_bw() +
  theme(text = element_text(size = 15,
                            family = "serif",
                            color = "black"),
        legend.title=element_blank(),
        legend.position="none",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, color = "black"),
        axis.text.y = element_text(color = "black")) +
  scale_linetype_manual(values = c("dotdash", "solid", "longdash", "dotted"), 
                        labels=c("Rural (N=1976)",
                                 "Small/mid metro (N=730)",
                                 "Suburban (N=368)",
                                 "Urban (N=64)")) +
  scale_x_discrete(breaks=c(1978,1983,1988,1993,1998,2003,2008,2013,2018),
                   labels=c("1978","1983","1988","1993","1998","2003","2008","2013","2018"))

# Bar plot with 2018 results 
g3_2018 <- ggplot(pretrial_data_2018, aes(x=urbanicity, y=pop_median)) + 
  geom_bar(stat = "identity", width = .5, fill = "darkred") + 
  theme_classic() +
  scale_x_discrete(labels = c("Rural", "Small/mid metro", "Suburban", "Urban")) +
  labs(x = NULL, y = "Pretrial Jail Population Rate, 2018") +
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.title.y = element_text(size=12,
                                    family = "serif",
                                    color = "black"),
        axis.text.y = element_blank(),      
        axis.ticks.y = element_blank()) + 
  geom_text(aes(label = round(pop_median, 0)),       # Adds labels to bars
            vjust = 1.5,                  # Adjusts vertical position
            color = "white",               # Text color
            size = 4) 

# Distorted original plot 
g3_distort <- ggplot(pretrial_data, aes(x=year, 
                                y = pop_median, 
                                color = urbanicity,
                                group = urbanicity)) +
  geom_line(size = 2) +
  ylab("Pretrial Jail \n Population Rate") +
  ggtitle(element_blank()) +
  theme_bw() +
  theme(text = element_text(size = 15,
                            family = "serif",
                            color = "black"),
        legend.title=element_blank(),
        legend.position="none",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, color = "black"),
        axis.text.y = element_text(color = "black"),
        aspect.ratio = 7) +
  scale_color_manual(values = c("#ffb000","#648fff","#dc267f","#cbc3e3"),
                     labels=c("Rural (N=1976)",
                              "Small/mid metro (N=730)",
                              "Suburban (N=368)",
                              "Urban (N=64)")) +
  scale_x_discrete(breaks=c(1978,1983,1988,1993,1998,2003,2008,2013,2018),
                   labels=c("1978","1983","1988","1993","1998","2003","2008","2013","2018"))
################################################################################
#Figure of black/white jail population rate ratio by urbanicity over time
################################################################################

rural <- as.data.frame(tapply(data[which(data$urbanicity=="rural"),]$bw,data[which(data$urbanicity=="rural"),]$year,median,na.rm=TRUE)) 
rural <- rural %>% 
  rename(bw = c("tapply(data[which(data$urbanicity == \"rural\"), ]$bw, data[which(data$urbanicity == ")) %>%
  mutate(year = 1970:2018,
         urbanicity = "rural")
rownames(rural) <- NULL

small <- as.data.frame(tapply(data[which(data$urbanicity=="small/mid"),]$bw,data[which(data$urbanicity=="small/mid"),]$year,median,na.rm=TRUE)) 
small <- small %>% 
  rename(bw = c("tapply(data[which(data$urbanicity == \"small/mid\"), ]$bw, data[which(data$urbanicity == ")) %>%
  mutate(year = 1970:2018,
         urbanicity = "small")
rownames(small) <- NULL

suburban <- as.data.frame(tapply(data[which(data$urbanicity=="suburban"),]$bw,data[which(data$urbanicity=="suburban"),]$year,median,na.rm=TRUE)) 
suburban <- suburban %>% 
  rename(bw = c("tapply(data[which(data$urbanicity == \"suburban\"), ]$bw, data[which(data$urbanicity == ")) %>%
  mutate(year = 1970:2018,
         urbanicity = "suburban")
rownames(suburban) <- NULL

urban <- as.data.frame(tapply(data[which(data$urbanicity=="urban"),]$bw,data[which(data$urbanicity=="urban"),]$year,median,na.rm=TRUE))
urban <- urban %>% 
  rename(bw = c("tapply(data[which(data$urbanicity == \"urban\"), ]$bw, data[which(data$urbanicity == ")) %>%
  mutate(year = 1970:2018,
         urbanicity = "urban")
rownames(urban) <- NULL

bw_data <- rbind(rural,small,suburban,urban)
rm(rural,small,suburban,urban)

bw_data <- bw_data[which(bw_data$year>=1990),] %>%
  mutate(year = as.character(year))

# Subsetting data for 2018 
bw_data_2018 <- bw_data[which(bw_data$year==2018),]

#Original plot 
g4 <- ggplot(bw_data, aes(x=year, 
                           y = bw, 
                           color = urbanicity,
                           group = urbanicity)) +
  geom_line(linewidth = 2) +
  ylab("Black/White Jail Pop. Rate Ratio") +
  ggtitle(element_blank()) +
  theme_bw() +
  theme(text = element_text(size = 15,
                            family = "serif",
                            color = "black"),
        legend.title=element_blank(),
        legend.position="none",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, color = "black"),
        axis.text.y = element_text(color = "black")) +
  scale_color_manual(values = c("#ffb000","#648fff","#dc267f","#cbc3e3"),
                     labels=c("Rural (N=1976)",
                              "Small/mid metro (N=730)",
                              "Suburban (N=368)",
                              "Urban (N=64)")) +
  scale_x_discrete(breaks=c(1990,1995,2000,2005,2010,2015,2018),
                   labels=c("1990","1995","2000","2005","2010","2015","2018"))

# Plot that can be printed in b/w
g4_bw <- ggplot(bw_data, aes(x=year, 
                          y = bw)) +
  geom_line(size = 1, aes(group = urbanicity, linetype = urbanicity)) +
  ylab("Black/White Jail Pop. Rate Ratio") +
  ggtitle(element_blank()) +
  theme_bw() +
  theme(text = element_text(size = 15,
                            family = "serif",
                            color = "black"),
        legend.title=element_blank(),
        legend.position="none",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, color = "black"),
        axis.text.y = element_text(color = "black")) +
  scale_linetype_manual(values = c("dotdash", "solid", "longdash", "dotted"), 
                        labels=c("Rural (N=1976)",
                                 "Small/mid metro (N=730)",
                                 "Suburban (N=368)",
                                 "Urban (N=64)")) +
  scale_x_discrete(breaks=c(1990,1995,2000,2005,2010,2015,2018),
                   labels=c("1990","1995","2000","2005","2010","2015","2018"))

# Bar plot with 2018 results 
g4_2018 <- ggplot(bw_data_2018, aes(x=urbanicity, y=bw)) + 
  geom_bar(stat = "identity", width = .5, fill = "darkred") + 
  theme_classic() +
  scale_x_discrete(labels = c("Rural", "Small/mid metro", "Suburban", "Urban")) +
  labs(x = NULL, y = "Black/White Jail Pop. Rate Ratio, 2018") +
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.title.y = element_text(size=12,
                                    family = "serif",
                                    color = "black"),
        axis.text.y = element_blank(),      
        axis.ticks.y = element_blank()) + 
  geom_text(aes(label = round(bw, 2)),       # Adds labels to bars
            vjust = 1.5,                  # Adjusts vertical position
            color = "white",               # Text color
            size = 4) 

# Distorted original plot 
g4_distort <- ggplot(bw_data, aes(x=year, 
                          y = bw, 
                          color = urbanicity,
                          group = urbanicity)) +
  geom_line(size = 2) +
  ylab("Black/White Jail \n Pop. Rate Ratio") +
  ggtitle(element_blank()) +
  theme_bw() +
  theme(text = element_text(size = 15,
                            family = "serif",
                            color = "black"),
        legend.title=element_blank(),
        legend.position="none",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, color = "black"),
        axis.text.y = element_text(color = "black"),
        aspect.ratio = 7) +
  scale_color_manual(values = c("#ffb000","#648fff","#dc267f","#cbc3e3"),
                     labels=c("Rural (N=1976)",
                              "Small/mid metro (N=730)",
                              "Suburban (N=368)",
                              "Urban (N=64)")) +
  scale_x_discrete(breaks=c(1990,1995,2000,2005,2010,2015,2018),
                   labels=c("1990","1995","2000","2005","2010","2015","2018"))
################################################################################
# Create plot
################################################################################

# Original plot 
 plot_grid(g1, g2, g3, g4, ncol = 2)

# Plot in b/w
 plot_grid(g1_bw, g2_bw, g3_bw, g4_bw,
           ncol = 2)

# 2018 data plot 
 plot_grid(g1_2018, g2_2018, g3_2018, g4_2018,
           ncol = 2)

# Distorted original plot 
plot_grid(g1_distort, g2_distort, g3_distort, g4_distort, nrow = 1, ncol = 4)


# Interactive plot
p <- ggplotly(g4)
saveWidget(p, file="interactive_ittner.html")



