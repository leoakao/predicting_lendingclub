# Data Exploration --------------------------------------------------------------
# Employment Length Bar Chart
lc2 %>%
  group_by(emp_length) %>%
  count()

ggplot(data = lc2, aes(x = emp_length)) + geom_bar()

# Loan Purpose Bar Chart
ggplot(lc2, aes(x= purpose, fill = loan_status2)) +
  geom_bar() + xlab('Purpose') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

purposeg <- lc %>% 
  group_by(purpose) %>%
  count()
purposeg <- ungroup(purposeg)

ggplot(purposeg, aes(x= purpose, y= n, fill = purpose)) + 
  geom_bar(stat = "identity", col = 'black') + 
  xlab('Purpose') + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(lc2, aes(x= purpose, fill = loan_status2)) +
  geom_bar(col = 'black') + xlab('Purpose') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Payment Plan Stats (Only 80 Y's so not significant)
lc %>%
  group_by(pymnt_plan, loan_status) %>%
  count()

# dti Histogram
ggplot(lc2, aes(x = dti, fill = loan_status)) + 
  geom_histogram(col = 'white', binwidth = 1) + xlab('dti')

ggplot(lc2, aes(x = dti, fill = loan_status2)) + 
  geom_histogram(col = 'white', binwidth = 1) + xlab('dti')

# Distribution of Features about Negative Remarks
ggplot(lc2, aes(x = delinq_2yrs, fill = loan_status2)) + 
  geom_histogram(col = 'white', binwidth = 1) + xlab('Delinquincies within 2 Years')

ggplot(lc2, aes(x = inq_last_6mths, fill = loan_status2)) + 
  geom_histogram(col = 'white', binwidth = 1) + xlab('Inquiries in Last 6 months')

# Has NA in mths_since_last_delinq, mths_since_last_record, mths_since_last_major_deroj, 
#   mths_since_recent_bc_dlq, mo_sin_old_il_acct, mths_since_recent_inq, 
#   mths_since_recent_revolv_delinq, mths_since_recent_inq
ggplot(lc, aes(x = mths_since_last_delinq, fill = loan_status)) + 
  geom_histogram(col = 'white', binwidth = 6) + xlab('Months since last delinquincies')

lc2 %>% group_by(mths_since_last_delinq) %>% count()
lc2 %>% filter(is.na(mths_since_last_delinq)) %>% count()

ggplot(lc, aes(x = mths_since_last_record, fill = loan_status)) + 
  geom_histogram(col = 'white', binwidth = 12) + xlab('Months Since Last Record')
lc2 %>% group_by(mths_since_last_record) %>% count()

ggplot(lc, aes(x = mths_since_last_major_derog, fill = loan_status)) + 
  geom_histogram(col = 'white', binwidth = 12) + xlab('Months Since Last Major Derogatory')
lc2 %>% group_by(mths_since_last_major_derog) %>% count()

ggplot(lc, aes(x = mths_since_recent_bc_dlq, fill = loan_status)) + 
  geom_histogram(col = 'white', binwidth = 12) + xlab('Months Since Recent BC Delinquincy')

ggplot(lc, aes(x = mo_sin_old_il_acct, fill = loan_status)) + 
  geom_histogram(col = 'white', binwidth = 12) + xlab('Months Since Old Installation Account')

ggplot(lc, aes(x = mths_since_recent_inq, fill = loan_status)) + 
  geom_histogram(col = 'white', binwidth = 1) + xlab('Months Since Recent Inquiry')
ggplot(lc2, aes(x = mths_since_recent_inq, fill = loan_status)) + 
  geom_bar(col = 'white') + xlab('Months Since Recent Inquiry (Binned)')+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(lc, aes(x = mths_since_recent_revol_delinq, fill = loan_status)) + 
  geom_histogram(col = 'white', binwidth = 5) + xlab('Months Since Recent Revolving Delinquency')

# Loan Status & Loan Amt Histogram  
# A lot more fully paid loans, which is a good thing
lc2 %>% 
  group_by(loan_status) %>%
  count()

ggplot(lc2, aes(x = loan_amnt, fill = loan_status2)) + 
  geom_histogram(binwidth = 1000, col = 'white') + xlab('Loan Amt')

ggplot(lc2, aes(x = loan_amnt, fill = loan_status)) + 
  geom_histogram(binwidth = 1000, position = 'identity', col = 'black') + 
  xlab('Loan Amt')

# Percentage Loan Status/Interest Rate
ggplot(lc2, aes(x = int_rate, fill = loan_status)) + 
  geom_histogram(position = 'fill', binwidth = 0.01, col = 'black') +
  xlab('Interest Rate')

# Interest Rate Distribution
ggplot(lc2, aes(x = int_rate)) + 
  geom_histogram(position = 'identity', binwidth = 0.01, col = 'white') + xlab('Interest Rate')

# Income Distribution
ggplot(lc2, aes(x = annual_inc, fill = loan_status2)) + 
  geom_histogram(position = 'identity', binwidth = 5000, col = 'black') + 
  xlab('Income') + 
  coord_cartesian(xlim = c(0,500000))
max(lc2$annual_inc)
min(lc2$annual_inc)

ggplot(lc2, aes(x = annual_inc_joint, fill = loan_status2)) + 
  geom_histogram(position = 'identity', binwidth = 5000, col = 'black') + 
  xlab('Income') + 
  coord_cartesian(xlim = c(0,500000))

# Issue Date Distribution
ggplot(lc2,aes(x = issue_dsincelc, fill = loan_status2)) + 
  geom_histogram(bins = 50, col= 'black') +
  xlab('Issue Date (based on LC Start Yr')

# Number of Accounts Ever 120 Days Past Due Distribution
ggplot(lc2,aes(x = lc2$num_accts_ever_120_pd, fill = loan_status2)) + 
  geom_histogram(binwidth = 1, col= 'black') +
  xlab('Number of Accounts Ever 120 Days Past Due')

lc2 %>% group_by(num_accts_ever_120_pd) %>% count()

# Number of currently active bankcard accounts Distribution
ggplot(lc2,aes(x = lc2$num_actv_bc_tl, fill = loan_status2)) + 
  geom_histogram(binwidth = 1, col= 'black') +
  xlab('Number of Active Bank Card Distribution')

# Number of currently active bankcard accounts Distribution
ggplot(lc2,aes(x = lc2$num_actv_rev_tl, fill = loan_status2)) + 
  geom_histogram(binwidth = 1, col= 'black') +
  xlab('Number of Active Revolving Accounts Distribution')


# Loan Status
ggplot(lc2,aes(x = lc2$loan_status2)) + geom_bar()