# loading data file on R
call_data <- read.csv(file.choose())

# check the structure of the data
str(call_data)

# Q1: communication channels ~ call duration
# Perform one-way ANOVA
anova <- aov(Call_Duration ~ Channel, data = call_data)

# Summarize the ANOVA result
summary(anova)

# create a boxplot of the anova results
boxplot(Call_Duration ~ Channel, data = call_data,
        main = "Call Duration by Channel",    
        xlab = "Channel",                     
        ylab = "Call Duration (Minutes)",     
        col = "violet",                      
        border = "black")    


# Q2 csat_score ~ reason
# use a linear regression model
lm_model <- lm(Csat_Score ~ Reason, data = call_data)

# View the summary of the model
summary(lm_model)

# Violin plot
library(ggplot2)
ggplot(call_data, aes(x = Reason, y = Csat_Score)) +
  geom_violin(fill = "violet", color = "black") +
  labs(title = "Violin Plot: Csat_Score by Reason", 
       x = "Reason", 
       y = "Csat_Score")

# Dot plot using ggplot2
ggplot(call_data, aes(x = Reason, y = Csat_Score)) +
  geom_jitter(width = 0.2, height = 0, color = "violet") +
  labs(title = "Dot Plot: Csat_Score by Reason", 
       x = "Reason", 
       y = "Csat_Score")

# Q3 call_center ~ response time
# create a contingency table
ct <- table(call_data$Call_Centres_City,
            call_data$Response_Time)
print(ct)

# chi square test
chi_test <- chisq.test(ct)
print(chi_test)

# Convert the contingency table to a data frame
ct_df <- as.data.frame(ct)
colnames(ct_df) <- c("Call_Centres_City", "Response_Time", "Count")

# clustered bar chart
ggplot(ct_df, aes(x = Call_Centres_City, y = Count, fill = Response_Time)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Distribution of Response Times by Center",
       x = "Call Center",
       y = "Count",
       fill = "Response Time") +
  theme_dark()

# stacked bar chart
ggplot(ct_df, aes(x = Call_Centres_City, y = Count, fill = Response_Time)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Proportion of Response Times by Center",
       x = "Call Center",
       y = "Proportion",
       fill = "Response Time") +
       theme_dark()


# stacked bar chart
ggplot(ct_df, aes(x = Call_Centres_City, y = Count, fill = Response_Time)) +
  geom_bar(stat = "identity", position = "stack") + 
  labs(
    title = "Response Times by Center, total calls",
    x = "Call Center",
    y = "Number of Calls",
    fill = "Response Time"
  ) +
  scale_fill_manual(values = c("Above SLA" = "coral", # Red
                               "Below SLA" = "green", # Green
                               "Within SLA" = "lightblue")) + # Blue
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )