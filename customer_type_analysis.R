# =============================================================================
# Customer Type Analysis: Residential vs Commercial/Professional
# Stripe as authority, Zoho CRM for classification, BigCommerce for yearly trends
# =============================================================================

# --- 1. Load Libraries -------------------------------------------------------
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
stripe_customers <- read_csv("stripe_unified_customers.csv", show_col_types = FALSE)

cat("BigCommerce orders loaded:", nrow(bc_orders), "rows\n")
cat("Zoho contacts loaded:", nrow(zoho_contacts), "rows\n")
cat("Stripe customers loaded:", nrow(stripe_customers), "rows\n")

# --- 3. Clean & Prep BigCommerce Orders --------------------------------------

bc_clean <- bc_orders %>%
  mutate(
    order_date = mdy(`Order Date`),
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
    zoho_trade_program = tolower(trimws(`Trade Program`)),
    zoho_professional_type = tolower(trimws(ifelse(
      "professionalType" %in% names(.), `professionalType`, NA_character_
    ))),
    zoho_customer_type_meta = tolower(trimws(ifelse(
      "customerType" %in% names(.), `customerType`, NA_character_
    ))),
    zoho_trade_interest_meta = tolower(trimws(ifelse(
      "tradeProgramInterest" %in% names(.), `tradeProgramInterest`, NA_character_
    ))),
    zoho_pro_project_type = tolower(trimws(ifelse(
      "proProjectType" %in% names(.), `proProjectType`, NA_character_
    ))),
    zoho_projects_per_year = tolower(trimws(`Projects per Year`)),
    zoho_business_id = trimws(`Business ID`),
    zoho_business_entity = trimws(`Business Entity`),
    zoho_tax_exempt = tolower(trimws(`Tax Exempt?`)),
    zoho_company = tolower(trimws(`Account Name`)),
    zoho_first_name = trimws(`First Name`),
    zoho_last_name = trimws(`Last Name`),
    zoho_full_name = trimws(paste(zoho_first_name, zoho_last_name))
  )

# --- 5. Classify Zoho Contacts -----------------------------------------------

personal_email_domains <- c(
  "gmail.com", "yahoo.com", "hotmail.com", "outlook.com", "aol.com",
  "icloud.com", "me.com", "mac.com", "live.com", "msn.com",
  "comcast.net", "att.net", "verizon.net", "sbcglobal.net", "cox.net",
  "charter.net", "earthlink.net", "optonline.net", "frontier.com",
  "windstream.net", "centurylink.net", "embarqmail.com", "suddenlink.net",
  "mail.com", "email.com", "ymail.com", "rocketmail.com", "protonmail.com",
  "zoho.com", "fastmail.com", "hushmail.com", "inbox.com", "gmx.com"
)

company_junk <- c(
  "", "n/a", "na", "none", "no", "self", "retired", "home", "homeowner",
  "yes", "kitchen", "bath", "bathroom", "bathroom remodel", "cabinet doors",
  "stock cabinets", "custom cabinets", "cabinets", "google", "bigcommerce",
  "reflectiz bigcommerce", "facebook", "instagram"
)

zoho_classified <- zoho_clean %>%
  mutate(
    email_domain = str_extract(zoho_email, "(?<=@)[^@]+$"),
    has_business_email = !is.na(email_domain) & !(email_domain %in% personal_email_domains),

    # STRONG signals (any one alone = Professional)
    sig_customer_type = zoho_customer_type %in% c(
      "professional", "commercial", "contractor", "builder", "designer",
      "architect", "trade", "dealer", "wholesale", "business"
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
    # Trade Program boolean field (true/false)
    sig_trade_program = !is.na(zoho_trade_program) & zoho_trade_program == "true",
    sig_professional_type = !is.na(zoho_professional_type) &
      zoho_professional_type != "" & zoho_professional_type != "homeowner",
    sig_business_id = !is.na(zoho_business_id) & zoho_business_id != "",
    sig_business_entity = !is.na(zoho_business_entity) & zoho_business_entity != "",
    sig_tax_exempt = !is.na(zoho_tax_exempt) & zoho_tax_exempt %in% c("yes", "true", "y"),
    # customerType metadata field (separate from Customer Type)
    sig_customer_type_meta = !is.na(zoho_customer_type_meta) &
      zoho_customer_type_meta %in% c("professional", "both"),
    # tradeProgramInterest metadata (separate from Trade Program Interest)
    sig_trade_interest_meta = !is.na(zoho_trade_interest_meta) &
      zoho_trade_interest_meta %in% c("yes", "yes!"),
    # professionalType has actual values (remodeler, home builder, etc.)
    # Already handled by sig_professional_type above
    # proProjectType — any value set indicates professional context
    sig_pro_project_type = !is.na(zoho_pro_project_type) &
      zoho_pro_project_type != "",
    # Projects per Year — any value indicates professional volume
    sig_projects_per_year = !is.na(zoho_projects_per_year) &
      zoho_projects_per_year != "",

    # WEAK signals (need 2+ together)
    sig_company_raw = !is.na(zoho_company) &
      !(zoho_company %in% company_junk) &
      !(zoho_company == tolower(paste(zoho_first_name, zoho_last_name))) &
      !(zoho_company == tolower(zoho_last_name)),
    sig_email_raw = has_business_email,

    # Explicit residential signals
    sig_residential = (zoho_customer_type %in% c("homeowner", "residential") |
      zoho_business_type %in% c("homeowner", "residential") |
      zoho_contact_type %in% c("homeowner", "residential") |
      zoho_customer_type_meta == "homeowner"),

    strong_signal_count = sig_customer_type + sig_business_type + sig_contact_type +
      sig_trade_status + sig_trade_interest + sig_trade_program +
      sig_professional_type + sig_business_id + sig_business_entity +
      sig_tax_exempt + sig_customer_type_meta + sig_trade_interest_meta +
      sig_pro_project_type + sig_projects_per_year,
    weak_signal_count = sig_company_raw + sig_email_raw,
    pro_signal_count = strong_signal_count + weak_signal_count,
    is_professional = (strong_signal_count >= 1) | (weak_signal_count >= 2),

    customer_class = case_when(
      is_professional & !sig_residential ~ "Professional",
      is_professional & sig_residential  ~ "Professional",
      sig_residential                    ~ "Residential",
      TRUE                               ~ "Unknown"
    ),
    classification_confidence = case_when(
      strong_signal_count >= 2 ~ "High",
      strong_signal_count == 1 ~ "Medium",
      weak_signal_count >= 2   ~ "Low",
      sig_residential          ~ "Medium",
      TRUE                     ~ "No Signal"
    )
  )

cat("\n--- Zoho Contact Classification Summary ---\n")
zoho_classified %>%
  count(customer_class, classification_confidence) %>%
  arrange(customer_class, classification_confidence) %>%
  print(n = 20)

# One row per email for lookups
zoho_lookup <- zoho_classified %>%
  filter(!is.na(zoho_email) & zoho_email != "") %>%
  group_by(zoho_email) %>%
  slice_max(pro_signal_count, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(zoho_email, customer_class, classification_confidence, pro_signal_count,
         zoho_company, zoho_customer_type, zoho_business_type, zoho_trade_status,
         starts_with("sig_"))

# ==========================================================================
# 6. STRIPE AS AUTHORITY: Build master customer list
# ==========================================================================

stripe_clean <- stripe_customers %>%
  mutate(
    stripe_email = tolower(trimws(Email)),
    stripe_name = trimws(Name),
    stripe_total_spend = as.numeric(gsub("[^0-9.]", "", `Total Spend`)),
    stripe_payment_count = as.numeric(`Payment Count`),
    stripe_avg_order = as.numeric(gsub("[^0-9.]", "", `Average Order`)),
    stripe_refunded = as.numeric(gsub("[^0-9.]", "", `Refunded Volume`)),
    stripe_created = ymd_hms(`Created (UTC)`, quiet = TRUE)
  ) %>%
  filter(!is.na(stripe_email) & stripe_email != "")

cat("\nStripe customers after cleaning:", nrow(stripe_clean), "\n")

# Consolidate to one row per email (Stripe can have duplicates)
stripe_deduped <- stripe_clean %>%
  group_by(stripe_email) %>%
  summarise(
    stripe_name = first(na.omit(stripe_name)),
    total_spend = sum(stripe_total_spend, na.rm = TRUE),
    total_orders = sum(stripe_payment_count, na.rm = TRUE),
    avg_order = ifelse(sum(stripe_payment_count, na.rm = TRUE) > 0,
                       sum(stripe_total_spend, na.rm = TRUE) / sum(stripe_payment_count, na.rm = TRUE),
                       0),
    total_refunded = sum(stripe_refunded, na.rm = TRUE),
    first_seen = min(stripe_created, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(total_spend > 0) %>%
  mutate(first_year = year(first_seen))

cat("Stripe paying customers (deduplicated, all time):", nrow(stripe_deduped), "\n")

# Filter to customers active in 2023-2025:
# 1. Created in 2023-2025 (definitely active in period), OR
# 2. Have a BigCommerce order in 2023-2025 (confirms activity in period)
bc_active_emails <- bc_clean %>%
  filter(!is.na(match_email)) %>%
  distinct(match_email) %>%
  pull()

customer_master <- stripe_deduped %>%
  filter(
    first_year >= 2023 |                         # Created during period
    stripe_email %in% bc_active_emails           # Has BC order in 2023-2025
  )

cat("Stripe paying customers active 2023-2025:", nrow(customer_master), "\n")
cat("  - Created 2023+:", sum(customer_master$first_year >= 2023), "\n")
cat("  - Pre-2023 with BC orders in period:", sum(customer_master$first_year < 2023), "\n")

# Classify from Zoho
customer_master <- customer_master %>%
  left_join(zoho_lookup %>% select(zoho_email, customer_class, classification_confidence,
                                    pro_signal_count),
            by = c("stripe_email" = "zoho_email"))

# Enrich from BigCommerce for unmatched
bc_company_lookup <- bc_clean %>%
  filter(!is.na(match_email)) %>%
  group_by(match_email) %>%
  summarise(
    bc_shipping_company = first(na.omit(shipping_company)),
    bc_customer_group = first(na.omit(customer_group)),
    .groups = "drop"
  ) %>%
  mutate(
    bc_has_company = !is.na(bc_shipping_company) & bc_shipping_company != "" &
      !bc_shipping_company %in% c("n/a", "na", "none", "-"),
    bc_group_pro = bc_customer_group %in% c(
      "professional", "commercial", "trade", "wholesale", "dealer",
      "contractor", "builder", "business"
    )
  )

customer_master <- customer_master %>%
  left_join(bc_company_lookup, by = c("stripe_email" = "match_email")) %>%
  mutate(
    final_class = case_when(
      !is.na(customer_class) ~ customer_class,
      bc_has_company | bc_group_pro ~ "Professional",
      TRUE ~ "Unknown"
    ),
    display_class = case_when(
      final_class == "Professional" ~ "Professional",
      final_class == "Residential"  ~ "Residential",
      TRUE                          ~ "Residential (assumed)"
    ),
    broad_class = ifelse(final_class == "Professional", "Professional", "Residential"),
    is_repeat = total_orders > 1
  )

# --- Stripe-authoritative summary ---
cat("\n--- Customer Master Summary (Stripe Authority) ---\n")
customer_master %>%
  group_by(broad_class) %>%
  summarise(
    customers = n(),
    revenue = sum(total_spend, na.rm = TRUE),
    orders = sum(total_orders, na.rm = TRUE),
    avg_order = round(sum(total_spend) / sum(total_orders), 2),
    avg_ltv = round(mean(total_spend), 2),
    repeat_rate = round(sum(is_repeat) / n() * 100, 1),
    .groups = "drop"
  ) %>%
  mutate(
    pct_customers = round(customers / sum(customers) * 100, 1),
    pct_revenue = round(revenue / sum(revenue) * 100, 1)
  ) %>%
  print()

cat("\n--- Classification Confidence ---\n")
customer_master %>%
  count(display_class) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  print()

# ==========================================================================
# 7. BigCommerce Yearly Trends (partial coverage)
# ==========================================================================

orders_merged <- bc_clean %>%
  left_join(zoho_lookup %>% select(zoho_email, customer_class),
            by = c("match_email" = "zoho_email")) %>%
  left_join(bc_company_lookup %>% select(match_email, bc_has_company, bc_group_pro),
            by = c("match_email" = "match_email")) %>%
  mutate(
    final_class = case_when(
      !is.na(customer_class) ~ customer_class,
      bc_has_company | bc_group_pro ~ "Professional",
      TRUE ~ "Unknown"
    ),
    display_class = case_when(
      final_class == "Professional" ~ "Professional",
      final_class == "Residential"  ~ "Residential",
      TRUE                          ~ "Residential (assumed)"
    ),
    broad_class = ifelse(final_class == "Professional", "Professional", "Residential")
  )

yearly_summary <- orders_merged %>%
  group_by(order_year, broad_class) %>%
  summarise(
    order_count = n(),
    total_revenue = sum(order_total, na.rm = TRUE),
    avg_order_size = mean(order_total, na.rm = TRUE),
    median_order_size = median(order_total, na.rm = TRUE),
    unique_customers = n_distinct(match_email, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(order_year) %>%
  mutate(
    pct_orders = round(order_count / sum(order_count) * 100, 1),
    pct_revenue = round(total_revenue / sum(total_revenue) * 100, 1)
  ) %>%
  ungroup()

cat("\n--- Yearly Summary (BigCommerce, partial coverage) ---\n")
print(yearly_summary, n = 20)

repeat_by_year <- orders_merged %>%
  filter(!is.na(match_email)) %>%
  arrange(match_email, order_date) %>%
  group_by(match_email) %>%
  mutate(cumulative_order_num = row_number(), is_repeat_order = cumulative_order_num > 1) %>%
  ungroup() %>%
  group_by(order_year, broad_class) %>%
  summarise(total_orders = n(), repeat_orders = sum(is_repeat_order),
            repeat_order_rate = round(repeat_orders / total_orders * 100, 1),
            .groups = "drop")

cat("\n--- Repeat Order Rate by Year ---\n")
print(repeat_by_year, n = 20)

# ==========================================================================
# 8. Data Coverage
# ==========================================================================

zoho_emails <- zoho_lookup %>% pull(zoho_email)
bc_emails <- bc_clean %>% filter(!is.na(match_email)) %>% distinct(match_email) %>% pull()
stripe_paying <- customer_master %>% pull(stripe_email)

cat("\n--- Data Source Coverage ---\n")
cat(sprintf("  Stripe paying customers:          %s\n", comma(length(stripe_paying))))
cat(sprintf("  Matched to Zoho CRM:              %s (%s%%)\n",
            comma(sum(stripe_paying %in% zoho_emails)),
            round(sum(stripe_paying %in% zoho_emails) / length(stripe_paying) * 100, 1)))
cat(sprintf("  Matched to BigCommerce:            %s (%s%%)\n",
            comma(sum(stripe_paying %in% bc_emails)),
            round(sum(stripe_paying %in% bc_emails) / length(stripe_paying) * 100, 1)))
cat(sprintf("  With any classification signal:    %s (%s%%)\n",
            comma(sum(customer_master$final_class != "Unknown")),
            round(sum(customer_master$final_class != "Unknown") / nrow(customer_master) * 100, 1)))
cat(sprintf("  No signal (assumed Residential):   %s (%s%%)\n",
            comma(sum(customer_master$final_class == "Unknown")),
            round(sum(customer_master$final_class == "Unknown") / nrow(customer_master) * 100, 1)))

# ==========================================================================
# 9. Executive Summary
# ==========================================================================

total_customers <- nrow(customer_master)
total_revenue <- sum(customer_master$total_spend, na.rm = TRUE)
total_orders_all <- sum(customer_master$total_orders, na.rm = TRUE)

cat("\n")
cat("=============================================================\n")
cat("       EXECUTIVE SUMMARY (Stripe-Authoritative)\n")
cat("=============================================================\n\n")

cat(sprintf("Total Paying Customers: %s\n", comma(total_customers)))
cat(sprintf("Total Revenue: %s\n", dollar(total_revenue)))
cat(sprintf("Total Orders: %s\n\n", comma(total_orders_all)))

type_summary <- customer_master %>%
  group_by(broad_class) %>%
  summarise(
    customers = n(), revenue = sum(total_spend, na.rm = TRUE),
    orders = sum(total_orders, na.rm = TRUE),
    avg_ltv = round(mean(total_spend), 2),
    repeat_rate = round(sum(is_repeat) / n() * 100, 1),
    .groups = "drop"
  ) %>%
  mutate(pct_cust = round(customers / sum(customers) * 100, 1),
         pct_rev = round(revenue / sum(revenue) * 100, 1))

for (i in 1:nrow(type_summary)) {
  cat(sprintf("%s:\n", type_summary$broad_class[i]))
  cat(sprintf("  Customers: %s (%s%%)\n", comma(type_summary$customers[i]), type_summary$pct_cust[i]))
  cat(sprintf("  Revenue:   %s (%s%%)\n", dollar(type_summary$revenue[i]), type_summary$pct_rev[i]))
  cat(sprintf("  Orders:    %s\n", comma(type_summary$orders[i])))
  cat(sprintf("  Avg LTV:   %s\n", dollar(type_summary$avg_ltv[i])))
  cat(sprintf("  Repeat:    %s%%\n\n", type_summary$repeat_rate[i]))
}

no_signal_pct <- round(sum(customer_master$final_class == "Unknown") / total_customers * 100, 1)
cat(sprintf("Classification Certainty: %s%% of customers had no signal\n", no_signal_pct))
cat("(These are assumed Residential in the analysis above)\n")
cat("\n=============================================================\n")

# ==========================================================================
# 10. Export
# ==========================================================================

write_csv(customer_master, "customer_master_classified.csv")
write_csv(yearly_summary, "yearly_summary_bc.csv")
write_csv(repeat_by_year, "repeat_rate_by_year.csv")

cat("\nExports saved.\n")
