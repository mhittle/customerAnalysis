# =============================================================================
# Customer Type Analysis: Residential vs Commercial/Professional
# Merges BigCommerce orders with Zoho CRM contacts to classify customers
# and produce buyer-ready metrics and visualizations for 2023-2025
# =============================================================================

# --- 1. Load Libraries -------------------------------------------------------
# Load individual tidyverse packages to avoid the 'fs' dependency issue
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(tidyr)
library(lubridate)
library(scales)
library(gridExtra)

# --- 2. Load Data ------------------------------------------------------------
bc_orders <- read_csv("orders-2026-03-25.csv", show_col_types = FALSE)
zoho_contacts <- read_csv("Contacts_2026_03_25.csv", show_col_types = FALSE)

cat("BigCommerce orders loaded:", nrow(bc_orders), "rows\n")
cat("Zoho contacts loaded:", nrow(zoho_contacts), "rows\n")

# --- 3. Clean & Prep BigCommerce Orders --------------------------------------

bc_clean <- bc_orders %>%
  mutate(
    order_date = mdy(`Order Date`),  # adjust parser if your date format differs
    order_year = year(order_date),
    order_total = as.numeric(gsub("[^0-9.]", "", `Order Total (inc tax)`)),
    customer_email = tolower(trimws(`Customer Email`)),
    billing_email = tolower(trimws(`Billing Email`)),
    shipping_company = tolower(trimws(`Shipping Company`)),
    billing_company = tolower(trimws(`Billing Company`)),
    customer_group = tolower(trimws(`Customer Group Name`)),
    customer_name = trimws(`Customer Name`),
    billing_name = trimws(`Billing Name`),
    shipping_name = trimws(`Shipping Name`)
  ) %>%
  filter(order_year %in% c(2023, 2024, 2025)) %>%
  # Use customer email as primary key; fall back to billing email

  mutate(
    match_email = case_when(
      !is.na(customer_email) & customer_email != "" ~ customer_email,
      !is.na(billing_email) & billing_email != ""   ~ billing_email,
      TRUE ~ NA_character_
    )
  )

cat("BigCommerce orders in 2023-2025:", nrow(bc_clean), "\n")

# --- 4. Clean & Prep Zoho Contacts -------------------------------------------

zoho_clean <- zoho_contacts %>%
  mutate(
    zoho_email = tolower(trimws(Email)),
    zoho_customer_type = tolower(trimws(`Customer Type`)),
    zoho_business_type = tolower(trimws(`Business Type`)),
    zoho_contact_type = tolower(trimws(`Contact Type`)),
    zoho_lead_source = tolower(trimws(`Lead Source`)),
    zoho_trade_status = tolower(trimws(`Trade Program Status`)),
    zoho_trade_interest = tolower(trimws(`Trade Program Interest`)),
    zoho_professional_type = tolower(trimws(ifelse(
      "professionalType" %in% names(.), `professionalType`, NA_character_
    ))),
    zoho_business_id = trimws(`Business ID`),
    zoho_business_entity = trimws(`Business Entity`),
    zoho_tax_exempt = tolower(trimws(`Tax Exempt?`)),
    zoho_company = tolower(trimws(`Account Name`)),
    zoho_first_name = trimws(`First Name`),
    zoho_last_name = trimws(`Last Name`),
    zoho_full_name = trimws(paste(zoho_first_name, zoho_last_name))
  )

# --- 5. Classify Zoho Contacts as Professional/Residential -------------------
# Strategy: sensitive to professional signals (catch as many as possible)

# Define professional email domain patterns (common business-ish domains excluded)
personal_email_domains <- c(
  "gmail.com", "yahoo.com", "hotmail.com", "outlook.com", "aol.com",
  "icloud.com", "me.com", "mac.com", "live.com", "msn.com",
  "comcast.net", "att.net", "verizon.net", "sbcglobal.net", "cox.net",
  "charter.net", "earthlink.net", "optonline.net", "frontier.com",
  "windstream.net", "centurylink.net", "embarqmail.com", "suddenlink.net",
  "mail.com", "email.com", "ymail.com", "rocketmail.com", "protonmail.com",
  "zoho.com", "fastmail.com", "hushmail.com", "inbox.com", "gmx.com"
)

zoho_classified <- zoho_clean %>%
  mutate(
    email_domain = str_extract(zoho_email, "(?<=@)[^@]+$"),
    has_business_email = !is.na(email_domain) & !(email_domain %in% personal_email_domains),

    # Professional signal flags
    sig_customer_type = zoho_customer_type %in% c(
      "professional", "commercial", "contractor", "builder", "designer",
      "architect", "trade", "dealer", "wholesale", "business", "other"
    ),
    sig_business_type = !is.na(zoho_business_type) & zoho_business_type != "" &
      zoho_business_type != "homeowner" & zoho_business_type != "residential",
    sig_contact_type = zoho_contact_type %in% c(
      "professional", "commercial", "contractor", "trade", "dealer", "business"
    ),
    sig_trade_status = !is.na(zoho_trade_status) & zoho_trade_status != "" &
      zoho_trade_status != "none" & zoho_trade_status != "n/a",
    sig_trade_interest = !is.na(zoho_trade_interest) & zoho_trade_interest != "" &
      zoho_trade_interest != "no" & zoho_trade_interest != "none" &
      zoho_trade_interest != "n/a",
    sig_professional_type = !is.na(zoho_professional_type) &
      zoho_professional_type != "" & zoho_professional_type != "homeowner",
    sig_business_id = !is.na(zoho_business_id) & zoho_business_id != "",
    sig_business_entity = !is.na(zoho_business_entity) & zoho_business_entity != "",
    sig_tax_exempt = !is.na(zoho_tax_exempt) & zoho_tax_exempt %in% c("yes", "true", "y"),
    sig_company = !is.na(zoho_company) & zoho_company != "",
    sig_email = has_business_email,

    # Explicit residential signals
    sig_residential = zoho_customer_type %in% c("homeowner", "residential") |
      zoho_business_type %in% c("homeowner", "residential") |
      zoho_contact_type %in% c("homeowner", "residential"),

    # Count professional signals
    pro_signal_count = sig_customer_type + sig_business_type + sig_contact_type +
      sig_trade_status + sig_trade_interest + sig_professional_type +
      sig_business_id + sig_business_entity + sig_tax_exempt +
      sig_company + sig_email,

    # Classification: sensitive to professional (any signal = professional)
    customer_class = case_when(
      pro_signal_count >= 1 & !sig_residential ~ "Professional",
      sig_residential & pro_signal_count == 0   ~ "Residential",
      sig_residential & pro_signal_count >= 1   ~ "Professional",  # pro signals override
      TRUE                                       ~ "Unknown"
    ),

    # Confidence level
    classification_confidence = case_when(
      pro_signal_count >= 3                      ~ "High",
      pro_signal_count == 2                      ~ "Medium",
      pro_signal_count == 1                      ~ "Low",
      sig_residential                            ~ "Medium",
      TRUE                                       ~ "No Signal"
    )
  )

# Summarize Zoho classification
cat("\n--- Zoho Contact Classification Summary ---\n")
zoho_classified %>%
  count(customer_class, classification_confidence) %>%
  arrange(customer_class, classification_confidence) %>%
  print(n = 20)

# --- 6. Merge BigCommerce Orders with Zoho Classifications -------------------

# Create a Zoho lookup: one row per email (take the one with most signals)
zoho_lookup <- zoho_classified %>%
  filter(!is.na(zoho_email) & zoho_email != "") %>%
  group_by(zoho_email) %>%
  slice_max(pro_signal_count, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(zoho_email, customer_class, classification_confidence, pro_signal_count,
         zoho_company, zoho_customer_type, zoho_business_type, zoho_trade_status,
         starts_with("sig_"))

# Primary merge: on email
orders_merged <- bc_clean %>%
  left_join(zoho_lookup, by = c("match_email" = "zoho_email"))

# For unmatched orders, try to classify from BigCommerce data alone
orders_merged <- orders_merged %>%
  mutate(
    # BigCommerce-only signals
    bc_has_company = !is.na(shipping_company) & shipping_company != "" &
      !shipping_company %in% c("n/a", "na", "none", "-"),
    bc_group_pro = customer_group %in% c(
      "professional", "commercial", "trade", "wholesale", "dealer",
      "contractor", "builder", "business"
    ),

    # Final classification: use Zoho if available, else BigCommerce signals
    final_class = case_when(
      !is.na(customer_class) ~ customer_class,
      bc_has_company | bc_group_pro ~ "Professional",
      TRUE ~ "Unknown"
    ),

    # For unknowns, assume Residential per instructions but track them
    display_class = ifelse(final_class == "Unknown", "Residential (assumed)", final_class),

    # Broad grouping for analysis
    broad_class = ifelse(final_class == "Professional", "Professional", "Residential")
  )

cat("\n--- Order Classification Summary ---\n")
orders_merged %>%
  count(final_class) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  print()

cat("\n--- Classification Detail ---\n")
orders_merged %>%
  count(display_class) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  print()

# --- 7. Analysis by Customer Type & Year -------------------------------------

# 7a. Summary table
analysis_summary <- orders_merged %>%
  group_by(order_year, broad_class) %>%
  summarise(
    order_count = n(),
    total_revenue = sum(order_total, na.rm = TRUE),
    avg_order_size = mean(order_total, na.rm = TRUE),
    median_order_size = median(order_total, na.rm = TRUE),
    unique_customers = n_distinct(match_email, na.rm = TRUE),
    .groups = "drop"
  )

# Add percentages within each year
analysis_summary <- analysis_summary %>%
  group_by(order_year) %>%
  mutate(
    pct_orders = round(order_count / sum(order_count) * 100, 1),
    pct_revenue = round(total_revenue / sum(total_revenue) * 100, 1)
  ) %>%
  ungroup()

cat("\n--- Analysis Summary by Year and Customer Type ---\n")
print(analysis_summary, n = 20)

# 7b. Repeat rate analysis
repeat_analysis <- orders_merged %>%
  group_by(broad_class, match_email) %>%
  summarise(
    order_count = n(),
    total_spent = sum(order_total, na.rm = TRUE),
    first_order = min(order_date, na.rm = TRUE),
    last_order = max(order_date, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(match_email)) %>%
  mutate(is_repeat = order_count > 1)

repeat_summary <- repeat_analysis %>%
  group_by(broad_class) %>%
  summarise(
    total_customers = n(),
    repeat_customers = sum(is_repeat),
    repeat_rate = round(repeat_customers / total_customers * 100, 1),
    avg_orders_per_customer = round(mean(order_count), 2),
    avg_ltv = round(mean(total_spent), 2),
    .groups = "drop"
  )

cat("\n--- Repeat Rate by Customer Type ---\n")
print(repeat_summary)

# Repeat rate by year (based on whether they've ordered before that year)
repeat_by_year <- orders_merged %>%
  filter(!is.na(match_email)) %>%
  arrange(match_email, order_date) %>%
  group_by(match_email) %>%
  mutate(
    cumulative_order_num = row_number(),
    is_repeat_order = cumulative_order_num > 1
  ) %>%
  ungroup() %>%
  group_by(order_year, broad_class) %>%
  summarise(
    total_orders = n(),
    repeat_orders = sum(is_repeat_order),
    repeat_order_rate = round(repeat_orders / total_orders * 100, 1),
    .groups = "drop"
  )

cat("\n--- Repeat Order Rate by Year and Customer Type ---\n")
print(repeat_by_year, n = 20)

# 7c. Certainty analysis (how many have no signal at all)
certainty_summary <- orders_merged %>%
  group_by(order_year) %>%
  summarise(
    total = n(),
    classified_pro = sum(final_class == "Professional"),
    classified_res = sum(final_class == "Residential"),
    no_signal = sum(final_class == "Unknown"),
    pct_no_signal = round(no_signal / total * 100, 1),
    matched_to_zoho = sum(!is.na(customer_class)),
    pct_matched = round(matched_to_zoho / total * 100, 1),
    .groups = "drop"
  )

cat("\n--- Classification Certainty by Year ---\n")
print(certainty_summary)

# --- 8. Visualizations (Buyer-Ready) -----------------------------------------

# Theme for clean, professional charts
theme_buyer <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray40", size = 12),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# Color palette
colors_class <- c("Professional" = "#2C5F8A", "Residential" = "#E8894A")
colors_detail <- c("Professional" = "#2C5F8A", "Residential" = "#E8894A",
                    "Residential (assumed)" = "#F5C285")

# --- Plot 1: Order Count by Customer Type & Year ---
p1 <- analysis_summary %>%
  ggplot(aes(x = factor(order_year), y = order_count, fill = broad_class)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = comma(order_count)),
            position = position_dodge(width = 0.7), vjust = -0.5, size = 4) +
  scale_fill_manual(values = colors_class) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Number of Orders by Customer Type",
    subtitle = "2023 - 2025",
    x = "Year", y = "Order Count", fill = "Customer Type"
  ) +
  theme_buyer

# --- Plot 2: Revenue by Customer Type & Year ---
p2 <- analysis_summary %>%
  ggplot(aes(x = factor(order_year), y = total_revenue, fill = broad_class)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = dollar(total_revenue, accuracy = 1)),
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = colors_class) +
  scale_y_continuous(labels = dollar, expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Revenue by Customer Type",
    subtitle = "2023 - 2025",
    x = "Year", y = "Total Revenue", fill = "Customer Type"
  ) +
  theme_buyer

# --- Plot 3: Revenue Share (Stacked %) ---
p3 <- analysis_summary %>%
  ggplot(aes(x = factor(order_year), y = pct_revenue, fill = broad_class)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(pct_revenue, "%")),
            position = position_stack(vjust = 0.5), size = 5, color = "white",
            fontface = "bold") +
  scale_fill_manual(values = colors_class) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "Revenue Share by Customer Type",
    subtitle = "2023 - 2025",
    x = "Year", y = "% of Revenue", fill = "Customer Type"
  ) +
  theme_buyer

# --- Plot 4: Average Order Size ---
p4 <- analysis_summary %>%
  ggplot(aes(x = factor(order_year), y = avg_order_size, fill = broad_class)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = dollar(avg_order_size, accuracy = 1)),
            position = position_dodge(width = 0.7), vjust = -0.5, size = 4) +
  scale_fill_manual(values = colors_class) +
  scale_y_continuous(labels = dollar, expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Average Order Size by Customer Type",
    subtitle = "2023 - 2025",
    x = "Year", y = "Avg Order Size", fill = "Customer Type"
  ) +
  theme_buyer

# --- Plot 5: Repeat Rate ---
p5 <- repeat_summary %>%
  ggplot(aes(x = broad_class, y = repeat_rate, fill = broad_class)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = paste0(repeat_rate, "%")), vjust = -0.5, size = 5,
            fontface = "bold") +
  scale_fill_manual(values = colors_class) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Repeat Customer Rate",
    subtitle = "Customers with 2+ orders (2023-2025)",
    x = "", y = "Repeat Rate", fill = "Customer Type"
  ) +
  theme_buyer

# --- Plot 6: Customer LTV ---
p6 <- repeat_summary %>%
  ggplot(aes(x = broad_class, y = avg_ltv, fill = broad_class)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = dollar(avg_ltv, accuracy = 1)), vjust = -0.5, size = 5,
            fontface = "bold") +
  scale_fill_manual(values = colors_class) +
  scale_y_continuous(labels = dollar, expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Average Customer Lifetime Value",
    subtitle = "Total spend per customer (2023-2025)",
    x = "", y = "Avg LTV", fill = "Customer Type"
  ) +
  theme_buyer

# --- Plot 7: Classification Certainty ---
certainty_long <- orders_merged %>%
  count(order_year, display_class) %>%
  group_by(order_year) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  ungroup()

p7 <- certainty_long %>%
  ggplot(aes(x = factor(order_year), y = n, fill = display_class)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(pct, "%")),
            position = position_stack(vjust = 0.5), size = 4, color = "white",
            fontface = "bold") +
  scale_fill_manual(values = colors_detail) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Classification Breakdown with Confidence",
    subtitle = "Shows assumed-residential orders (no signals) separately",
    x = "Year", y = "Order Count", fill = ""
  ) +
  theme_buyer

# --- Plot 8: Repeat Rate by Year ---
p8 <- repeat_by_year %>%
  ggplot(aes(x = factor(order_year), y = repeat_order_rate, fill = broad_class)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(repeat_order_rate, "%")),
            position = position_dodge(width = 0.7), vjust = -0.5, size = 4) +
  scale_fill_manual(values = colors_class) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Repeat Order Rate by Year",
    subtitle = "% of orders from returning customers",
    x = "Year", y = "Repeat Order %", fill = "Customer Type"
  ) +
  theme_buyer

# --- 9. Save All Plots -------------------------------------------------------

ggsave("plot_01_order_count.png", p1, width = 10, height = 6, dpi = 150)
ggsave("plot_02_revenue.png", p2, width = 10, height = 6, dpi = 150)
ggsave("plot_03_revenue_share.png", p3, width = 10, height = 6, dpi = 150)
ggsave("plot_04_avg_order_size.png", p4, width = 10, height = 6, dpi = 150)
ggsave("plot_05_repeat_rate.png", p5, width = 8, height = 6, dpi = 150)
ggsave("plot_06_customer_ltv.png", p6, width = 8, height = 6, dpi = 150)
ggsave("plot_07_classification_confidence.png", p7, width = 10, height = 6, dpi = 150)
ggsave("plot_08_repeat_rate_by_year.png", p8, width = 10, height = 6, dpi = 150)

cat("\nAll plots saved to working directory.\n")

# --- 10. Export Summary Tables ------------------------------------------------

write_csv(analysis_summary, "summary_by_year_and_type.csv")
write_csv(repeat_summary, "repeat_rate_summary.csv")
write_csv(repeat_by_year, "repeat_rate_by_year.csv")
write_csv(certainty_summary, "classification_certainty.csv")

# Export the full merged order-level data for further analysis
orders_export <- orders_merged %>%
  select(
    `Order ID`, order_date, order_year, order_total,
    customer_name = `Customer Name`, match_email,
    shipping_company, billing_company, customer_group,
    final_class, display_class, broad_class,
    pro_signal_count, classification_confidence,
    # Zoho signals
    zoho_customer_type, zoho_business_type, zoho_trade_status,
    starts_with("sig_")
  )

write_csv(orders_export, "orders_classified.csv")

cat("Summary tables and classified orders exported.\n")

# --- 11. Print Final Executive Summary ---------------------------------------

cat("\n")
cat("=============================================================\n")
cat("       EXECUTIVE SUMMARY: Customer Type Analysis\n")
cat("=============================================================\n\n")

total_orders <- nrow(orders_merged)
total_revenue <- sum(orders_merged$order_total, na.rm = TRUE)

cat(sprintf("Total Orders (2023-2025): %s\n", comma(total_orders)))
cat(sprintf("Total Revenue (2023-2025): %s\n\n", dollar(total_revenue)))

exec_summary <- orders_merged %>%
  group_by(broad_class) %>%
  summarise(
    orders = n(),
    revenue = sum(order_total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_orders = round(orders / sum(orders) * 100, 1),
    pct_revenue = round(revenue / sum(revenue) * 100, 1)
  )

for (i in 1:nrow(exec_summary)) {
  cat(sprintf("%s:\n", exec_summary$broad_class[i]))
  cat(sprintf("  Orders: %s (%s%%)\n",
              comma(exec_summary$orders[i]), exec_summary$pct_orders[i]))
  cat(sprintf("  Revenue: %s (%s%%)\n\n",
              dollar(exec_summary$revenue[i]), exec_summary$pct_revenue[i]))
}

no_signal_pct <- round(sum(orders_merged$final_class == "Unknown") / total_orders * 100, 1)
cat(sprintf("Classification Certainty: %s%% of orders had no signal\n", no_signal_pct))
cat("(These are assumed Residential in the analysis above)\n\n")

cat("Repeat Customer Rates:\n")
for (i in 1:nrow(repeat_summary)) {
  cat(sprintf("  %s: %s%% (%s avg orders/customer, %s avg LTV)\n",
              repeat_summary$broad_class[i],
              repeat_summary$repeat_rate[i],
              repeat_summary$avg_orders_per_customer[i],
              dollar(repeat_summary$avg_ltv[i])))
}
cat("\n=============================================================\n")
