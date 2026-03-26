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
zoho_accounts <- read_csv("Accounts_2025_03_25.csv", show_col_types = FALSE)
bc_customers <- read_csv("BC_customers.csv", show_col_types = FALSE)
stripe_customers <- read_csv("stripe_unified_customers.csv", show_col_types = FALSE)

cat("BigCommerce orders loaded:", nrow(bc_orders), "rows\n")
cat("BigCommerce customers loaded:", nrow(bc_customers), "rows\n")
cat("Zoho contacts loaded:", nrow(zoho_contacts), "rows\n")
cat("Zoho accounts loaded:", nrow(zoho_accounts), "rows\n")
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
    zoho_disaster_recovery = tolower(trimws(`Disaster Recovery?`)),
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
    # Tax Exempt — any entry counts
    sig_tax_exempt = !is.na(zoho_tax_exempt) & zoho_tax_exempt != "",
    # customerType metadata field (separate from Customer Type)
    sig_customer_type_meta = !is.na(zoho_customer_type_meta) &
      zoho_customer_type_meta %in% c("professional", "both"),
    # tradeProgramInterest metadata — any entry counts
    sig_trade_interest_meta = !is.na(zoho_trade_interest_meta) &
      zoho_trade_interest_meta != "",
    # proProjectType — any value set indicates professional context
    sig_pro_project_type = !is.na(zoho_pro_project_type) &
      zoho_pro_project_type != "",
    # Projects per Year — any value indicates professional volume
    sig_projects_per_year = !is.na(zoho_projects_per_year) &
      zoho_projects_per_year != "",
    # Disaster Recovery — any entry counts
    sig_disaster_recovery = !is.na(zoho_disaster_recovery) &
      zoho_disaster_recovery != "" & zoho_disaster_recovery != "no",

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
      sig_pro_project_type + sig_projects_per_year + sig_disaster_recovery,
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

# --- 5b. Zoho Accounts: additional classification source -----------------
# Accounts have many of the same fields as Contacts and can catch customers
# that the Contacts export missed. We join via Account Name to Contact's email.

acct_classified <- zoho_accounts %>%
  mutate(
    acct_name = tolower(trimws(`Account Name`)),
    acct_email = tolower(trimws(ifelse("Email 1" %in% names(.), `Email 1`, NA_character_))),
    acct_customer_type = tolower(trimws(`Customer Type`)),
    acct_business_type = tolower(trimws(`Business Type`)),
    acct_contact_type = tolower(trimws(`Contact Type`)),
    acct_trade_status = tolower(trimws(ifelse(
      "Trade Program Status" %in% names(.), `Trade Program Status`, NA_character_))),
    acct_trade_interest = tolower(trimws(`Trade Program Interest`)),
    acct_trade_program = tolower(trimws(`Trade Program`)),
    acct_projects_per_year = tolower(trimws(`Projects per Year`)),
    acct_disaster_recovery = tolower(trimws(`Disaster Recovery?`)),
    acct_account_type = tolower(trimws(`Account Type`)),
    acct_industry = tolower(trimws(Industry))
  ) %>%
  mutate(
    acct_sig_customer_type = acct_customer_type %in% c(
      "professional", "commercial", "contractor", "builder", "designer",
      "architect", "trade", "dealer", "wholesale", "business"),
    acct_sig_business_type = !is.na(acct_business_type) & acct_business_type != "" &
      !acct_business_type %in% c("homeowner", "residential"),
    acct_sig_contact_type = acct_contact_type %in% c(
      "professional", "commercial", "contractor", "trade", "dealer", "business"),
    acct_sig_trade_interest = !is.na(acct_trade_interest) & acct_trade_interest != "" &
      !acct_trade_interest %in% c("no", "none", "n/a"),
    acct_sig_trade_program = !is.na(acct_trade_program) & acct_trade_program == "true",
    acct_sig_projects = !is.na(acct_projects_per_year) & acct_projects_per_year != "",
    acct_sig_disaster = !is.na(acct_disaster_recovery) & acct_disaster_recovery != "" &
      acct_disaster_recovery != "no",
    acct_sig_industry = !is.na(acct_industry) & acct_industry != "",
    acct_sig_type = !is.na(acct_account_type) & acct_account_type != "" &
      !acct_account_type %in% c("customer", "other", ""),
    acct_is_pro = acct_sig_customer_type | acct_sig_business_type | acct_sig_contact_type |
      acct_sig_trade_interest | acct_sig_trade_program | acct_sig_projects |
      acct_sig_disaster | acct_sig_industry | acct_sig_type
  )

# Build account lookup by email (where email exists)
acct_lookup <- acct_classified %>%
  filter(!is.na(acct_email) & acct_email != "" & acct_is_pro) %>%
  distinct(acct_email) %>%
  mutate(acct_is_professional = TRUE)

cat("Zoho accounts flagged as professional:", nrow(acct_lookup), "\n")

# --- 5c. BigCommerce address signal: different ship-to = professional ----
# Different shipping vs billing name + address indicates ordering for a jobsite/client

bc_address_signal <- bc_clean %>%
  filter(!is.na(match_email)) %>%
  mutate(
    ship_name = tolower(trimws(`Shipping Name`)),
    bill_name = tolower(trimws(`Billing Name`)),
    ship_addr = tolower(trimws(paste(`Shipping Street 1`, `Shipping Suburb`,
                                      `Shipping State`, `Shipping Zip`))),
    bill_addr = tolower(trimws(paste(`Billing Street 1`, `Billing Suburb`,
                                      `Billing State`, `Billing Zip`))),
    diff_name = !is.na(ship_name) & !is.na(bill_name) & ship_name != "" &
      bill_name != "" & ship_name != bill_name,
    diff_addr = !is.na(ship_addr) & !is.na(bill_addr) & ship_addr != bill_addr,
    diff_name_and_addr = diff_name & diff_addr
  ) %>%
  group_by(match_email) %>%
  summarise(
    has_diff_ship_bill = any(diff_name_and_addr, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(has_diff_ship_bill)

cat("Customers with different billing/shipping name+address:", nrow(bc_address_signal), "\n")

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

# --- Classify from Zoho (email match first, then name fallback) ---

# Step 1: Email match
customer_master <- customer_master %>%
  left_join(zoho_lookup %>% select(zoho_email, customer_class, classification_confidence,
                                    pro_signal_count),
            by = c("stripe_email" = "zoho_email"))

email_matched <- sum(!is.na(customer_master$customer_class))
cat("\nZoho classification - email match:", email_matched, "customers\n")

# Step 2: Name-based fallback for unmatched customers
# Build a name lookup from Zoho contacts (only classified ones, deduplicated)
zoho_name_lookup <- zoho_classified %>%
  filter(!is.na(zoho_full_name) & zoho_full_name != "" &
         zoho_full_name != "NA NA" & nchar(zoho_full_name) > 3) %>%
  mutate(match_name = tolower(zoho_full_name)) %>%
  group_by(match_name) %>%
  slice_max(pro_signal_count, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  # Exclude very common/ambiguous names (appearing too many times = unreliable)
  add_count(match_name, name = "name_freq") %>%
  filter(name_freq == 1) %>%
  select(match_name,
         name_customer_class = customer_class,
         name_confidence = classification_confidence,
         name_signal_count = pro_signal_count)

# Standardize Stripe names for matching
customer_master <- customer_master %>%
  mutate(stripe_match_name = tolower(trimws(stripe_name)))

# Apply name match only where email didn't match
name_join <- customer_master %>%
  filter(is.na(customer_class) & !is.na(stripe_match_name) & stripe_match_name != "") %>%
  select(stripe_email, stripe_match_name) %>%
  inner_join(zoho_name_lookup, by = c("stripe_match_name" = "match_name"))

# Merge name matches back
customer_master <- customer_master %>%
  left_join(name_join %>% select(stripe_email, name_customer_class, name_confidence, name_signal_count),
            by = "stripe_email") %>%
  mutate(
    customer_class = ifelse(is.na(customer_class), name_customer_class, customer_class),
    classification_confidence = ifelse(is.na(classification_confidence), name_confidence, classification_confidence),
    pro_signal_count = ifelse(is.na(pro_signal_count), name_signal_count, pro_signal_count)
  ) %>%
  select(-name_customer_class, -name_confidence, -name_signal_count)

name_matched <- sum(!is.na(customer_master$customer_class)) - email_matched
cat("Zoho classification - name match (fallback):", name_matched, "additional customers\n")

# --- Also build name lookup from BigCommerce for company/group signals ---
bc_name_lookup <- bc_clean %>%
  filter(!is.na(customer_name) & customer_name != "") %>%
  mutate(match_name = tolower(trimws(customer_name))) %>%
  group_by(match_name) %>%
  summarise(
    bc_shipping_company_name = first(na.omit(shipping_company)),
    bc_billing_company_name = first(na.omit(billing_company)),
    bc_customer_group_name = first(na.omit(customer_group)),
    .groups = "drop"
  ) %>%
  add_count(match_name, name = "name_freq") %>%
  filter(name_freq == 1) %>%  # Only unique names
  mutate(
    bc_name_has_company = (!is.na(bc_shipping_company_name) & bc_shipping_company_name != "" &
      !bc_shipping_company_name %in% c("n/a", "na", "none", "-")) |
      (!is.na(bc_billing_company_name) & bc_billing_company_name != "" &
      !bc_billing_company_name %in% c("n/a", "na", "none", "-")),
    bc_name_group_pro = bc_customer_group_name %in% c(
      "professional", "commercial", "trade", "wholesale", "dealer",
      "contractor", "builder", "business"
    ),
    bc_name_is_pro = bc_name_has_company | bc_name_group_pro
  ) %>%
  filter(bc_name_is_pro) %>%
  select(match_name, bc_name_is_pro)

cat("BC name-based pro signals:", nrow(bc_name_lookup), "unique names\n")

# --- BC Customers file: Company field = professional ---
bc_cust_lookup <- bc_customers %>%
  mutate(
    bc_cust_email = tolower(trimws(Email)),
    bc_cust_company = tolower(trimws(Company)),
    bc_cust_name = tolower(trimws(paste(`First Name`, `Last Name`)))
  ) %>%
  filter(!is.na(bc_cust_email) & bc_cust_email != "") %>%
  mutate(
    bc_cust_has_company = !is.na(bc_cust_company) & bc_cust_company != "" &
      !bc_cust_company %in% c("n/a", "na", "none", "-", "self", "home", "retired")
  ) %>%
  filter(bc_cust_has_company) %>%
  distinct(bc_cust_email) %>%
  mutate(bc_cust_is_pro = TRUE)

cat("BC customers with Company field:", nrow(bc_cust_lookup), "\n")

# --- Business name keywords in personal email accounts ---
# If someone uses gmail/yahoo but their name contains business keywords, flag as pro
biz_keywords <- c(
  "cabinetry", "cabinets", "cabinet", "painting", "painters", "paint",
  "handyman", "design", "designs", "designer", "designing",
  "homes", "home builder", "homebuilder", "builders", "building",
  "construction", "contracting", "contractor", "contractors",
  "remodel", "remodeling", "remodelers", "renovation", "renovations",
  "woodwork", "woodworking", "millwork", "carpentry", "carpenter",
  "plumbing", "plumber", "electric", "electrical", "electrician",
  "roofing", "roofer", "flooring", "tile", "tiling",
  "restoration", "restorations", "maintenance",
  "property", "properties", "real estate", "realty",
  "interiors", "interior", "staging", "decor",
  "kitchen", "kitchens", "bath",
  "llc", "inc", "corp", "co\\.", "& sons", "& son",
  "enterprises", "services", "solutions", "group", "associates",
  "custom", "pro ", "professional"
)
biz_pattern <- paste(biz_keywords, collapse = "|")

# Enrich from BigCommerce orders
# Signals: Shipping Company (any entry), Billing Company, Customer Group
bc_company_lookup <- bc_clean %>%
  filter(!is.na(match_email)) %>%
  group_by(match_email) %>%
  summarise(
    bc_shipping_company = first(na.omit(shipping_company)),
    bc_billing_company = first(na.omit(billing_company)),
    bc_customer_group = first(na.omit(customer_group)),
    bc_customer_name = first(na.omit(customer_name)),
    .groups = "drop"
  ) %>%
  mutate(
    # Shipping Company: ANY entry = professional
    bc_has_shipping_co = !is.na(bc_shipping_company) & bc_shipping_company != "" &
      !bc_shipping_company %in% c("n/a", "na", "none", "-"),
    bc_has_billing_co = !is.na(bc_billing_company) & bc_billing_company != "" &
      !bc_billing_company %in% c("n/a", "na", "none", "-"),
    bc_has_company = bc_has_shipping_co | bc_has_billing_co,
    # Customer Group: Contractors and Trade Program - Level 1 = professional
    bc_group_pro = bc_customer_group %in% c(
      "professional", "commercial", "trade", "wholesale", "dealer",
      "contractor", "contractors", "builder", "business",
      "trade program - level 1"
    ),
    bc_group_residential = bc_customer_group == "residential",
    # Business name in customer name (even with personal email)
    bc_name_has_biz = !is.na(bc_customer_name) &
      grepl(biz_pattern, tolower(bc_customer_name), ignore.case = TRUE)
  )

# Also check if Stripe name itself has business keywords
stripe_name_has_biz <- customer_master %>%
  filter(!is.na(stripe_match_name) & stripe_match_name != "") %>%
  mutate(has_biz = grepl(biz_pattern, stripe_match_name, ignore.case = TRUE)) %>%
  filter(has_biz) %>%
  distinct(stripe_email) %>%
  mutate(stripe_name_is_biz = TRUE)

cat("Stripe customers with business keywords in name:", nrow(stripe_name_has_biz), "\n")

customer_master <- customer_master %>%
  left_join(bc_company_lookup, by = c("stripe_email" = "match_email")) %>%
  left_join(acct_lookup, by = c("stripe_email" = "acct_email")) %>%
  left_join(bc_address_signal, by = c("stripe_email" = "match_email")) %>%
  left_join(bc_name_lookup, by = c("stripe_match_name" = "match_name")) %>%
  left_join(bc_cust_lookup, by = c("stripe_email" = "bc_cust_email")) %>%
  left_join(stripe_name_has_biz, by = "stripe_email") %>%
  mutate(
    acct_is_professional = ifelse(is.na(acct_is_professional), FALSE, acct_is_professional),
    has_diff_ship_bill = ifelse(is.na(has_diff_ship_bill), FALSE, has_diff_ship_bill),
    bc_name_is_pro = ifelse(is.na(bc_name_is_pro), FALSE, bc_name_is_pro),
    bc_cust_is_pro = ifelse(is.na(bc_cust_is_pro), FALSE, bc_cust_is_pro),
    bc_name_has_biz = ifelse(is.na(bc_name_has_biz), FALSE, bc_name_has_biz),
    stripe_name_is_biz = ifelse(is.na(stripe_name_is_biz), FALSE, stripe_name_is_biz),

    # Professional always wins: if ANY source says pro, they're pro
    final_class = case_when(
      customer_class == "Professional"       ~ "Professional",
      acct_is_professional                   ~ "Professional",
      bc_has_company | bc_group_pro          ~ "Professional",
      bc_cust_is_pro                         ~ "Professional",
      bc_name_is_pro                         ~ "Professional",
      bc_name_has_biz                        ~ "Professional",
      stripe_name_is_biz                     ~ "Professional",
      has_diff_ship_bill                     ~ "Professional",
      customer_class == "Residential"        ~ "Residential",
      TRUE                                   ~ "Unknown"
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
  mutate(bc_match_name = tolower(trimws(customer_name))) %>%
  left_join(zoho_lookup %>% select(zoho_email, customer_class),
            by = c("match_email" = "zoho_email")) %>%
  # Name fallback for Zoho classification
  left_join(zoho_name_lookup %>% select(match_name, name_customer_class),
            by = c("bc_match_name" = "match_name")) %>%
  mutate(customer_class = ifelse(is.na(customer_class), name_customer_class, customer_class)) %>%
  select(-name_customer_class) %>%
  left_join(bc_company_lookup %>% select(match_email, bc_has_company, bc_group_pro, bc_name_has_biz),
            by = c("match_email" = "match_email")) %>%
  left_join(acct_lookup, by = c("match_email" = "acct_email")) %>%
  left_join(bc_address_signal, by = c("match_email" = "match_email")) %>%
  left_join(bc_name_lookup, by = c("bc_match_name" = "match_name")) %>%
  left_join(bc_cust_lookup, by = c("match_email" = "bc_cust_email")) %>%
  mutate(
    acct_is_professional = ifelse(is.na(acct_is_professional), FALSE, acct_is_professional),
    has_diff_ship_bill = ifelse(is.na(has_diff_ship_bill), FALSE, has_diff_ship_bill),
    bc_name_is_pro = ifelse(is.na(bc_name_is_pro), FALSE, bc_name_is_pro),
    bc_cust_is_pro = ifelse(is.na(bc_cust_is_pro), FALSE, bc_cust_is_pro),
    bc_name_has_biz = ifelse(is.na(bc_name_has_biz), FALSE, bc_name_has_biz),
    # Professional always wins
    final_class = case_when(
      customer_class == "Professional"       ~ "Professional",
      acct_is_professional                   ~ "Professional",
      bc_has_company | bc_group_pro          ~ "Professional",
      bc_cust_is_pro                         ~ "Professional",
      bc_name_is_pro                         ~ "Professional",
      bc_name_has_biz                        ~ "Professional",
      has_diff_ship_bill                     ~ "Professional",
      customer_class == "Residential"        ~ "Residential",
      TRUE                                   ~ "Unknown"
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
