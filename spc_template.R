
# Statistical Process Control chart template ####

library(easypackages)

libraries(c("readxl", "readr", "plyr", "dplyr", "ggplot2", "png", "tidyverse", "reshape2", "scales", 'jsonlite'))

# Using DToC data from NHS Digital ####

github_repo_dir <- '~/Documents/Repositories/spc'

test_df <- read_csv(paste0(github_repo_dir, '/test_df.csv'), col_types = cols(Date = col_date(format = '%d/%m/%Y'), Patients = col_double()))

mean(test_df$Patients)



dtoc_org <- read_csv(paste0(github_repo_dir, '/DToC_Days_Responsible_Organisation.csv'), col_types = cols(Name = col_character(),
  NHS = col_double(),`Social Care` = col_double(),Both = col_double(),  Period_year = col_character())) %>% 
  mutate(Month = paste("01", Period_year, sep = " ")) %>% 
  mutate(Month = as.Date(Month, format = "%d %b %Y")) %>% 
  arrange(Month) %>% 
  mutate(Total = NHS + `Social Care` + Both) %>% 
  mutate(Period_year = factor(Period_year, levels = unique(Period_year))) %>% 
  mutate(Perc_NHS = NHS/Total,
         Perc_Social_care = `Social Care`/Total,
         Perc_Both = Both/Total)

month_order <-  as.character(unique(dtoc_org$Period_year))

Chosen_DToC_days <- dtoc_org %>% 
  filter(Name == 'West Sussex')

# We need to put the data into long format format to plot a stacked bar chart
Chosen_DToC_days_long <- Chosen_DToC_days %>% 
  select(Name,Period_year,NHS,`Social Care`,Both) %>% 
  gather(key = "Organisation", value = "Days", NHS:Both) %>% 
  mutate(Organisation = factor(Organisation, levels = c("Both", "Social Care", "NHS"))) %>% 
  arrange(Organisation)

ggplot(data = Chosen_DToC_days_long, aes(x = Period_year, y = Days, fill = Organisation)) + 
  geom_bar(stat = "identity", width = 0.8) +
  scale_y_continuous(breaks = seq(0, round_any(max(Chosen_DToC_days$Total), 500, ceiling), ifelse(round_any(max(Chosen_DToC_days$Total), 500, ceiling) < 5000, 250, 500)), expand = c(0.01,0), limits = c(0, round_any(max(Chosen_DToC_days$Total), 500, ceiling)), labels = comma) +   scale_fill_manual(values = c("#4F81BD","#C0504D", "#000000"), limits = c("NHS","Social Care", "Both"), labels = c("NHS","Social Care", "Both NHS and Social Care"), name = "Delay attributed to") +  
  ylab("Delayed Days") +
  xlab("Month") +
  labs(caption = "Source: NHS England") +
  theme(legend.position = "top", 
        legend.text = element_text(size = 8), 
        legend.title = element_text(size = 8), 
        legend.key.size = unit(.75, "lines"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, size = 8), 
        plot.background = element_rect(fill = "white", colour = "#E2E2E3"), 
        panel.background = element_rect(fill = NA), 
        axis.title = element_text(colour = "#000000", face = "bold", size = 8),     
        panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_line(colour = "#E2E2E3", linetype = "dashed", size = 0.2))  







dtoc_ts <- read_csv(paste0(github_repo_dir, '/DToC_Days_Reason_for_Delay.csv'), col_types = cols(Code = col_character(),  Name = col_character(), `Awaiting completion of assessment` = col_double(),  `Awaiting public funding` = col_double(),  `Awaiting further NHS non-acute care` = col_double(),  `Awaiting residential home placement or availability` = col_double(),  `Awaiting nursing home placement or availability` = col_double(),  `Awaiting care package in own home` = col_double(),  `Awaiting community equipment and adaptations` = col_double(),  `Patient or family choice` = col_double(),  Disputes = col_double(),  `Housing - Patients not covered by NHS and Community Care Act` =  col_double(),  Other = col_double(),  `Total Delayed Transfers of Care` = col_double(),  Period = col_character())) %>% 
  mutate(Month = paste0("01 ", Period)) %>% 
  mutate(Month = as.Date(Month, '%d %B %Y')) %>% 
  select("Total Delayed Transfers of Care", "Period","Month", "Name") %>% 
  arrange(Month)

Org_DToC <- dtoc_ts %>% 
  filter(Name == 'West Sussex') %>% 
  group_by(Name) %>% 
  mutate(Period = factor(Period, levels = Period)) %>% 
  mutate(long_term_ave = round(mean(`Total Delayed Transfers of Care`, na.rm = TRUE),0)) %>% 
  mutate(LCL_3SD = round(long_term_ave- (3*sd(`Total Delayed Transfers of Care`, na.rm = FALSE)),0)) %>% 
  mutate(LCL_2SD = round(long_term_ave- (2*sd(`Total Delayed Transfers of Care`, na.rm = FALSE)),0)) %>% 
  mutate(UCL_2SD = round(long_term_ave + (2*sd(`Total Delayed Transfers of Care`, na.rm = FALSE)),0)) %>% 
  mutate(UCL_3SD = round(long_term_ave + (3*sd(`Total Delayed Transfers of Care`, na.rm = FALSE)),0))


# add 3 sigma figure out why this is diff to sd
# add trend rules around consective points to identify common cause and special cause

# WHAT ABOUT CALENDAR ADJUSTMENT ####
# Average daily days rather than totals per month as months do not have equal lengths #






ggplot(data = Org_DToC, aes(x = Period, y = `Total Delayed Transfers of Care`, group = 1)) +   geom_line(colour = "#61B5CD") +
  xlab("Month") +
  ylab("Total Delayed Days") +
  # ggtitle(paste("Total number of bed days lost due to delayed transfers of care during each reporting period, Acute and Non-Acute;\n", Org_name, " Local Authority; Monthly reporting (", months[1], " to ", months[length(months)], ")", sep = "")) +
  geom_hline(aes(yintercept=UCL_2SD),colour = "#F79646", linetype="dashed", lwd = .6) + 
  geom_hline(aes(yintercept=LCL_2SD),colour = "#F79646", linetype="dashed", lwd = .6) +
  geom_hline(aes(yintercept=UCL_3SD),colour = "#A8423F", linetype="dashed", lwd = .7) + 
  geom_hline(aes(yintercept=LCL_3SD),colour = "#A8423F", linetype="dashed", lwd = .7) +
  geom_hline(aes(yintercept = long_term_ave), colour = "#264852", lwd = .8) +
  geom_smooth(aes(),method='lm',  se = FALSE, colour = "black", lwd = .6) +
  scale_y_continuous(breaks = seq(1000, 4500, 250), limits = c(1000, 4500), labels = comma) + 
  geom_text(data= subset(Org_DToC, Period %in%c("January 2017", "August 2017")), aes(label=paste(format(`Total Delayed Transfers of Care`, big.mark = ","),sep="")), hjust = 1, vjust = -1, size = 3) +
  geom_text(data= subset(Org_DToC, Period %in%c("September 2017")), aes(label=paste(format(`Total Delayed Transfers of Care`, big.mark = ","),sep="")), hjust = 0, vjust = -1, size = 3) +
  geom_text(data = subset(Org_DToC, Period %in%c("April 2014")), aes(label=paste(format(`Total Delayed Transfers of Care`, big.mark = ","),sep="")), hjust = 1, vjust = 1.75, size = 3) +
  geom_point(aes(x = Period, y = `Total Delayed Transfers of Care`, fill =  "#B2DCE6"), colour="#61B5CD", size = 4, shape = 21, show.legend = FALSE) +
  scale_fill_manual(values = "#B2DCE6", label = "Delayed days", name = "") +
  labs(caption = "Note: Y axis does not start at zero.\nThe dashed inner and outer lines represent 95% (2 SD) and 99% (3 SD) control limits respectively\nThe solid line represents the long term average\nSource: NHS England: Monthly Situation Report")
