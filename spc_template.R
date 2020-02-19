
# Statistical Process Control chart template ####
library(easypackages)

libraries(c("readxl", "readr", "plyr", "dplyr", "ggplot2", "png", "tidyverse", "reshape2", "scales", 'jsonlite', 'zoo'))

# Using DToC data from NHS Digital ####

github_repo_dir <- '~/Documents/Repositories/spc'

# Brilliant post I found after a few hours trying to figure out the difference between SD and sigma. https://r-bar.net/xmr-control-chart-tutorial-examples/ this explains the constant value 1.128

# This is also a very very useful guide for when to use SPCs, which rules to apply and sensible tips for when to recalculate limits or produce multiple process SPCs.

# ‘Common cause variation’ is just that; common. Every normal process will exhibit natural (or common cause) variation. In healthcare, every patient is different, so every process will have inbuilt variation. Even simple processes exhibit variation. Just think about signing your name. Is your signature identical every time or just similar?
# ‘Special cause’ variation is something abnormal, the result of a specific change that has affected the process in a way beyond what would be expected under normal conditions.

# SPC using moving range not SD. The moving range is sequence sensitive and demphasizes systematic variation, allowing us to more clearly measure the inherent random process variation. The standard deviation is a measure of total variation (ie., systematic variation & random variation).

# The standard deviation, represents the total variation in our data. Total variation is comprised of random variation and systematic variation. When we make an XmR chart, our control limits should represent the random component to the variation in our process. We get this though the mR derived sequential deviation discussed above. The benefit of plotting the random variation window of our process (3 * sequential deviation), is that we can detect when systemic variation creeps into our process. And, hopefully do some corrective action to remove it.

# test_df <- read_csv(paste0(github_repo_dir, '/test_df.csv'), col_types = cols(Date = col_date(format = '%d/%m/%Y'), Patients = col_double()))

df <- data.frame(ScrewID = seq(1,20,1), Length	=c(2.92,	2.96,	2.86,	3.04,	3.07,	2.85,	3.00,	2.92,	2.97,	2.97,	3.09,	3.07,	2.99,	3.06,	3.05,	3.02,	3.07,	2.91,	3.07,	3.20)) %>% 
  mutate(mR = abs(Length - lag(Length)))

# Calculate the moving range, which is the absolute difference between each point and its predecessor 
process_mean <- mean(df$Length)

# Now calculate the mean(mR)
mean_mR <-  mean(df$mR, na.rm = TRUE)

# Convert mean(mR) to sequential deviation (or sigma)
seq_dev <- mean_mR / 1.128

# Use sequential deviation to calculate control limits
lower_control_limit <- process_mean - (seq_dev * 3)
upper_control_limit <- process_mean + (seq_dev * 3)

rm(df, lower_control_limit, upper_control_limit, seq_dev, mean_mR, process_mean)

# In one dataframe we create the metrics and then apply some rules for patterns in the processes.

# These are our conditions periods. We will want to highlight any consecutive data points in which the last five are above or below the process mean, in addition we want to highlight any periods of six or more values which are increasing or decreasing.

run_length <- 6
trend_length <- 7

df1 <- data.frame(ScrewID = seq(1,21,1), Length	= c(2.98,	2.96,	2.86,	2.74,	2.7,	2.5,	2.00,	2.92,	2.97,	2.97,	3.09,	3.07,	2.99,	3.06,	3.05,	3.02,	3.07,	3.06,	3.07,	3, 2)) %>%
  mutate(process_mean = mean(Length, na.rm = TRUE)) %>% 
  mutate(mR = abs(Length - lag(Length))) %>% 
  mutate(mean_mR = mean(mR, na.rm = TRUE)) %>% 
  mutate(seq_dev = mean_mR / 1.128) %>% 
  mutate(lower_control_limit = process_mean - (seq_dev * 3),
         upper_control_limit = process_mean + (seq_dev * 3)) %>% 
  mutate(one_sigma_lci = process_mean - seq_dev,
         one_sigma_uci = process_mean + seq_dev,
         two_sigma_lci = process_mean - (seq_dev * 2),
         two_sigma_uci = process_mean + (seq_dev * 2)) %>% 
  mutate(location = ifelse(Length > upper_control_limit, 'Outside +/- 3sigma', ifelse(Length < lower_control_limit, 'Outside +/- 3sigma', ifelse(Length > two_sigma_uci, 'Between +/- 2sigma and 3sigma', ifelse(Length < two_sigma_lci, 'Between +/- 2sigma and 3sigma', ifelse(Length > one_sigma_uci, 'Between +/- 1sigma and 2sigma', ifelse(Length < one_sigma_lci, 'Between +/- 1sigma and 2sigma', 'Within +/- 1sigma'))))))) %>% 
  mutate(rule_1 = ifelse(Length > upper_control_limit, 'Special cause concern', ifelse(Length < lower_control_limit, 'Special cause concern', 'Common cause variation'))) %>% # This highlights any values outside the control limits
  mutate(above_mean = ifelse(Length > process_mean, 1, 0)) %>% # above_mean is 1 if the value is above the mean and 0 if not.
  mutate(rule_2a = rollapplyr(above_mean, run_length, sum, align = 'right', partial = TRUE)) %>% # sum the last five (or whatever 'run_length' is) values for 'above_mean'. if the sum is 5 (or whatever 'run_length' is set to) then you know that there have been at least this many consecutive values above the process mean and this constitues a 'run'. 
  mutate(rule_2a_label = rollapply(rule_2a, run_length, function(x)if(any(x == run_length)) 'Run above (shift)' else 'No run', align = 'left', partial = TRUE)) %>% # Now we want to identify all values related to that above run which means we have to look forward (using align = 'left') to see if a value should be included in a run. Here, ScrewID 8 is at the beginning of a run. Although the rule_2 for ScrewID 8 = 1 (meaning that only ScrewID 8 was above the mean (and ScrewID 4-7 were not)), looking in the other direction you can see that for ScrewID 12 it has a rule_2 value of 5 (meaning that ScrewID 8-12 were all above the mean).
  mutate(below_mean = ifelse(Length < process_mean, 1, 0)) %>% # Now we do the same as above mean but for the below mean run.
  mutate(rule_2b = rollapplyr(below_mean, run_length, sum, partial = TRUE)) %>%
  mutate(rule_2b_label = rollapply(rule_2b, run_length, function(x)if(any(x == run_length)) 'Run below (shift)' else 'No run', align = 'left', partial = TRUE)) %>%
  mutate(rule_2 = ifelse(rule_2a_label == 'Run above (shift)', rule_2a_label, rule_2b_label)) %>% 
  select(-c(above_mean, below_mean, rule_2a, rule_2a_label, rule_2b, rule_2b_label)) %>% 
  mutate(trend_down = ifelse(Length < lag(Length, 1), 1, 0)) %>% # Now we say give 1 if the Length value is lower than the previous Length value, if not give 0
  mutate(trend_down = ifelse(is.na(trend_down), lead(trend_down, 1), trend_down)) %>% # As we were comparing value x to its predecessor, the first row has nothing to compare to and will be NA, so instead we'll use the value of row 2 to determine what this value should be.
  mutate(rule_3a = rollapplyr(trend_down, trend_length, sum, align = 'right', partial = TRUE)) %>% # Similarly to earlier (although we are using 'trend_length' rather than 'run_length') we can sum all the 1's to figure out which values are part of a downward trend. Note that this includes trends from above the mean to below the mean.
  mutate(rule_3a_label = rollapply(rule_3a, trend_length, function(x)if(any(x == trend_length)) 'Trend down (drift)' else 'No trend', align = 'left', partial = TRUE)) %>% # Equally, we use the rule_3a value but looking ahead six (or whatever) values to see if value x should be considered part of the trend.
  mutate(trend_up = ifelse(Length > lag(Length, 1), 1, 0)) %>% 
  mutate(trend_up = ifelse(is.na(trend_up), lead(trend_up, 1), trend_up)) %>% 
  mutate(rule_3b = rollapplyr(trend_up, trend_length, sum, align = 'right', partial = TRUE)) %>% 
  mutate(rule_3b_label = rollapply(rule_3b, trend_length, function(x)if(any(x == trend_length)) 'Trend up (drift)' else 'No trend', align = 'left', partial = TRUE)) %>%
  mutate(rule_3 = ifelse(rule_3a_label == 'Trend down (drift)', rule_3a_label, rule_3b_label)) %>% 
  select(-c(trend_down, trend_up, rule_3a, rule_3a_label, rule_3b, rule_3b_label)) %>%
  mutate(top_label = ifelse(rule_1 == 'Special cause concern', 'Special cause concern', ifelse(rule_2 %in% c('Run above (shift)', 'Run below (shift)'), rule_2, ifelse(rule_3 %in% c('Trend down (drift)', 'Trend up (drift)'), rule_3, 'Common cause variation')))) %>% 
  mutate(variation_label = ifelse(rule_1 == 'Special cause concern', 'Special cause variation', ifelse(rule_2 %in% c('Run above (shift)', 'Run below (shift)'), 'Special cause variation', ifelse(rule_3 %in% c('Trend down (drift)', 'Trend up (drift)'), 'Special cause variation', 'Common cause variation')))) # What is an improvement and what is concern will depend on the context. in a purely variation context (where you want things to stay within limits), trends (drift) and runs (shift) as well as points outside of limits may be of concern regardless of whether they are above or below the process mean. In cases where higher values are good then you may want to mark upward trends (drift) and above mean runs as improvement and below mean runs and downward drifts as special variation of concern.

# TO DO 

# Add a rule that highlights if two out of three consecutive points are in the sigma 2-3 zone
# Add a rule that highlights if 15 consecutive points are within +/- 1 sigma zone as 'huggers'

ggplot(data = df1, aes(x = ScrewID, y = Length, group = 1)) +
  geom_line(aes(colour = location)) +
  geom_point(aes(fill =  location), 
             colour="#61B5CD", 
             size = 4, 
             shape = 21) +
geom_hline(aes(yintercept = lower_control_limit),
           colour = "#A8423F",
           linetype="dotted",
           lwd = .7) +
geom_hline(aes(yintercept = upper_control_limit),
           colour = "#A8423F",
           linetype="dotted",
           lwd = .7) +
geom_hline(aes(yintercept = two_sigma_uci),
     colour = "#3d2b65",
     linetype="dashed",
     lwd = .7) +
geom_hline(aes(yintercept = two_sigma_lci),
     colour = "#3d2b65",
     linetype="dashed",
     lwd = .7) +
geom_hline(aes(yintercept = two_sigma_uci),
             colour = "#3d2b65",
             linetype="solid",
             lwd = .4) +
geom_hline(aes(yintercept = two_sigma_lci),
             colour = "#3d2b65",
             linetype="solid",
             lwd = .4) +  
  
geom_hline(aes(yintercept = process_mean),
colour = "#264852",
lwd = .8) +
scale_x_continuous(breaks = seq(0,nrow(df1), 1)) +
labs(caption = "Note: Y axis does not start at zero.\nThe red dotted lines represent 99% control limits (3σ, moving range) control limits respectively\nThe solid line represents the long term average.")




# What value rule should take prescedent? A single value might be a special cause of concern as well as in a run of above process mean and in a trend of increasing values.


# NHS Improvement use icons to signify overall performance of the whole data series. There are five outcomes - 1) common cause variation (random/natural variation only). 2) special cause variation in relation to high values (e.g. values above upper control limit, or consistent consecutive pattern of values above process mean/target). 3) special cause variation in relation to low values (e.g. values below lower control limit, or consistent consecutive pattern of values below process mean/target indicative of a shift). 4) special cause variation in relation to upward trends indicative of improvement (e.g. consecutive values increasing over time (usually 6 or 7 points)). 5) special cause variation in relation to downward trends indicative of improvement (e.g. consecutive values decreasing over time).

# Whether outcome 4 or 5 is relevant will depend on the polarity of the outcomes (is an icrease good (e.g. service uptake) or decrease good (non-elective admissions down)).

# What constitues special cause and common cause variation and improvement will depend on polarity of the data in question

# Further, the rules applied to the dataset will also depend on what you are trying to identify. You may want to apply different rules to different tasks. For example, you could use 1 and 2 sigma limits to show areas that may be of concern.

# Assurance
# For assessing likelihood of meeting a target you should expect values to consistently stay above the target.
# There are three outcomes - 1) process control chart indicates values consistently achieve target. 2) process control chart indicates values consistently do not achieve target. 3) process control chart indicates that the target is within common cause variation of values (e.g. values fall below and above target over time).

# Recalculation of limits part way through data when interventions or events occur that meaningfully change the dataset.

# I would try adding a new variable showing two or more distinct processes/interventions (e.g. time_1 may be 2017/18 - 2018/19 and time_2 may be 2019/20 onwards). By creating this and using group_by, you should be able to set mR and sigma limits for each process set.

# If you have several processes going on you may wish to split the dataset into those processes and produce an SPC for each one. An example of this can be daily admissions to hospital. Monday admissions may be very different to the rest of the working week and different still to weekend admissions. You may consider plotting a monday, a tuesday to friday and a saturday/sunday SPC.

# SPC charts can also enable you to see if a particular process can meet a specific target. The calculation used will determine the process capability. 
# The calculation used to determine process capability is:
# Capability = Target - Average / (3 * standard deviation)

# A value of 1 means the process is 100% capable of achieving the target. A negative figure means more than 50% of patients will not meet a given target.

# The procedure for calculating process capability is therefore:
# • Test for stability by plotting a control chart first
# • If unstable, gain control by identifying and controlling the main factors that affect the situation
# • Only if it is stable, calculate a process capability to determine if it is capable of meeting the target

# Real world data ####
# Delayed transfers of care.
# Unplanned hospital admissions.
# SSS or Health check numbers

dtoc_ts <- read_csv(paste0(github_repo_dir, '/DToC_Days_Reason_for_Delay.csv'), col_types = cols(Code = col_character(),  Name = col_character(), `Awaiting completion of assessment` = col_double(),  `Awaiting public funding` = col_double(),  `Awaiting further NHS non-acute care` = col_double(),  `Awaiting residential home placement or availability` = col_double(),  `Awaiting nursing home placement or availability` = col_double(),  `Awaiting care package in own home` = col_double(),  `Awaiting community equipment and adaptations` = col_double(),  `Patient or family choice` = col_double(),  Disputes = col_double(),  `Housing - Patients not covered by NHS and Community Care Act` =  col_double(),  Other = col_double(),  `Total Delayed Transfers of Care` = col_double(),  Period = col_character())) %>% 
  mutate(Month = paste0("01 ", Period)) %>% 
  mutate(Month = as.Date(Month, '%d %B %Y')) %>% 
  select("Total Delayed Transfers of Care", "Period","Month", "Name") %>% 
  arrange(Month)



