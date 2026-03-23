install.packages('tidyverse')
library(tidyverse)
install.packages("dplyr")
library(dplyr)

install.packages('ggplot2')
library(ggplot2)
library(scales)


# Importing dataset
df <- read.csv("~/Documents/Intro to DS/Project/financial_transactions.csv")
View(df)   
dim(df)

# Question 1:	Are certain transaction types (e.g., deposit, transfer, and withdrawal) 
# more likely to be classified as high-risk classification?

df$Suspicious.Activity.Flag <- factor(df$Suspicious.Activity.Flag,
                                      levels = c(0, 1),
                                      labels = c("Not Suspicious", "Suspicious"))


# Dodge helps to draw the bars for each flag side by side (grouped) not stacked. 
ggplot(df, aes(x = Type, fill = Suspicious.Activity.Flag)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Suspicious Activity Flag by Transaction Type",
    x = "Transaction Type",
    y = 'No of transactions',
    fill = "Flag"
  )

# The bar chart displays the number of suspicious and non-suspicious transactions across the three transaction types: Deposit, Transfer, and Withdrawal. 
# Across all transaction types, the majority of transactions are classified as Not Suspicious, represented by the taller red bars. In comparison, suspicious transactions (shown in blue) account for a much smaller proportion of the total volume.
# Although all three types exhibit some level of suspicious activity, Deposits appear to have the highest count of suspicious transactions, followed closely by Transfers and Withdrawals. 
# This suggests that while suspicious behavior occurs across all transaction types, deposits may be slightly more prone to triggering the suspicious activity flag in this dataset. 
# However, the overall distribution indicates that suspicious transactions remain a small fraction relative to legitimate ones. 


# Question 2:	Does the timing of a transaction influence its risk category? 

# Convert Timestamp to POSIXct if not already
df$Timestamp <- as.POSIXct(df$Timestamp)

# Extract hour (0–23) as a categorical variable
df$Hour <- format(df$Timestamp, "%H")

# Summarize frequency by hour and risk flag
df_suspicious <- df %>%
  filter(Suspicious.Activity.Flag=="Suspicious")

suspicious_by_hour <- df_suspicious %>%
  group_by(Hour)%>%
  summarise(
    suspicious_count = n()
  )%>%
  arrange(Hour)

suspicious_by_hour

ggplot(suspicious_by_hour,
       aes(x = Hour, y = suspicious_count, group = 1)) +
  geom_line(color = "firebrick", linewidth = 1) +
  geom_point(color = "firebrick", size = 2) +
  labs(
    title = "Number of Suspicious Transactions by Year",
    x = "Year",
    y = "Number of Suspicious Transactions"
  ) +
  theme_minimal()

# This graph shows the percentage of suspicious and non-suspicious transactions for every hour of the day. 
# Most transactions at every hour are not suspicious, which is why the red portion of each bar is very large. 
# However, there are some hours where the teal section (suspicious transactions) is slightly higher, especially in the early morning and late afternoon. 
# This means that suspicious transactions do not happen randomly — they occur more often at certain times of the day. 
# Overall, the time of a transaction does seem to have some influence on whether it is marked as suspicious.


# Question 3: How is the transaction amount related to the risk level?
# For example, are very large or very small transactions more often flagged as risky?


ggplot(df, aes(x = Suspicious.Activity.Flag, y = Amount, fill = Suspicious.Activity.Flag))+
  geom_boxplot()+
  labs(
    title = "Transaction Amount by Risk Level",
    x = " Risk Classificaiton",
    y = "Transaction Amount",
    fill = "Flag"
  ) +
  theme_minimal() 

# This graph compares how much money is involved in suspicious and non-suspicious transactions. 
# Both types of transactions have very similar amounts. The middle lines in the boxes are almost the same, 
# which means the typical transaction amount is nearly equal for both groups. Suspicious transactions do show a little more variation, 
# but not enough to say that suspicious activity always involves bigger or smaller amounts.
# Thus, the amount of money does not seem to decide whether a transaction is suspicious. People can make large
# or small transactions that are both safe or suspicious, so the system is using other factors, not just the amount, 
# flag risky behavior. 



# Question 4: Do transaction locations show patterns, such as certain regions or areas being more prone to high-risk transactions?

location_summary <- df %>%
  group_by(Location, Suspicious.Activity.Flag) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(Location) %>%
  mutate(percent = count / sum(count) * 100)

ggplot(location_summary, aes(x= reorder(Location, percent), y= percent, fill = Suspicious.Activity.Flag)) +
  geom_col(position = 'fill') +
  scale_y_continuous(labels = percent_format()) +
  coord_flip() +
  labs(
    title = "Proportion of Suspicious Transactions by Location",
    x = "Location",
    y = "Percent of Transactions",
    fill = "Flag"
  )+
  theme_minimal()

df$Location <- factor(df$Location)
df$Suspicious.Activity.Flag <- factor(df$Suspicious.Activity.Flag)

location_risk_table <- table(df$Location, df$Suspicious.Activity.Flag)

chi_location <- chisq.test(location_risk_table)
chi_location


ggplot(df, aes(x = Suspicious.Activity.Flag, y = Amount, fill = Suspicious.Activity.Flag)) +
  geom_boxplot(outlier.alpha = 0.2) +
  coord_cartesian(ylim = quantile(df$Amount, c(0.01, 0.99), na.rm = TRUE)) +
  labs(
    title = "Transaction Amount by Risk Level",
    x = "Risk Classification",
    y = "Transaction Amount",
    fill = "Flag"
  ) +
  theme_minimal()






# This graph compares how many suspicious transactions happen in different states. 
# While most transactions in every state are safe, some places have a slightly higher share of suspicious ones than others. 
# For example, Florida shows a bit more suspicious activity compared to Texas, New York, and California.
# This means location does matter. Some areas seem to have more risky transactions, 
# suggesting that where a transaction happens can help identify potential fraud.


# Question 5: What is the average transaction amount for each risk category, 
# and how do these distributions compare? 

df %>%
  group_by(Suspicious.Activity.Flag) %>%
  summarise(
    Average_Amount = mean(Amount),
    Median_Amount = median(Amount),
    Count = n()
  )


ggplot(df, aes(x = Suspicious.Activity.Flag, y = Amount, fill = Suspicious.Activity.Flag))+
  geom_boxplot()+
  labs(
    title = "Distribution of Transaction Amounts by Risk Category",
    x = "Risk Category",
    y = "Transaction Amount",
    fill ='Flag'
  ) +
  theme_minimal()


df$Amount <- as.numeric(df$Amount)
df$Suspicious.Activity.Flag <- factor(df$Suspicious.Activity.Flag)
table(df$Suspicious.Activity.Flag)

t_test_result <- t.test(Amount ~ Suspicious.Activity.Flag, data = df)
t_test_result




# The average amount of money in suspicious transactions is about $5,052, and the average for non-suspicious transactions is about $4,997.
# These values are almost the same, which means suspicious transactions are not based on unusually high or low amounts. 
# The median amounts are also very close.
# However, even though the average amounts are similar, suspicious transactions make up only about 10% of all transactions (1,005 out of 9,995).
# This shows that most transactions are safe, and only a small portion are flagged as suspicious. 
# So, the amount of money does not seem to be the main reason a transaction becomes suspicious, the system must be using other factors to decide which ones to flag.





# Summarize suspicious percentage by location
location_suspicious <- df %>%
  group_by(Location) %>%
  summarise(
    suspicious_pct = mean(Suspicious.Activity.Flag == "Suspicious") * 100,
    count = n()
  ) %>%
  arrange(desc(suspicious_pct))

# Plot only suspicious percentage by location
ggplot(location_suspicious,
       aes(x = reorder(Location, suspicious_pct),
           y = suspicious_pct)) +
  geom_col(fill = "firebrick") +
  coord_flip() +
  labs(
    title = "Percentage of Suspicious Transactions by Location",
    x = "Location",
    y = "Suspicious Transaction Percentage"
  ) +
  theme_minimal()


# Make sure the flag is a factor (you already did this)
df$Suspicious.Activity.Flag <- factor(df$Suspicious.Activity.Flag,
                                      levels = c(0, 1),
                                      labels = c("Not Suspicious", "Suspicious"))

# Two-sample t-test: compare mean Amount between the two groups
t_test_result <- t.test(Amount ~ Suspicious.Activity.Flag, data = df)

# View results
t_test_result
