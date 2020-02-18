
# Unplanned admissions ####

download.file('https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/02/MAR_Comm-Web-file-Dec-19-9tun8.xls', './Dec.xls', mode = 'wb')


WSx_Month_non_elect <- WSx_Month %>%
  select(c(`Org Name`,`Total Non-elective G&A Admissions (FFCEs)`,Period_Year))