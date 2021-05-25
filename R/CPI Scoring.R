library(httr)
library(tidyverse)
library(RPostgres)

con = dbConnect(odbc::odbc(),"postgreSAKIRP")
district = dbReadTable(con,"districts")
ward = dbReadTable(con,"wards")
faab = dbReadTable(con,"faab_coach")
group = dbReadTable(con,"groups2020-2021")
village = dbReadTable(con,"villages")

kobo_server_url = "https://kc.humanitarianresponse.info/"
form_id = "275634"
url = paste0(kobo_server_url,"api/v1/data/",form_id,".csv")
rawdata = GET(url,authenticate(Sys.getenv("sakirp_user"),Sys.getenv("sakirp_pw")))
content = content(rawdata,"raw",encoding="UTF-8")
cpi_data = read_csv(content)

cpi_2021 = cpi_data %>% 
  #furaha %>% 
  filter(lubridate::year(endtime) == 2021) %>% 
  left_join(group, by =c("group_name"="id")) %>% 
  left_join(village, by = c("village"="id")) %>% 
  left_join(ward, by = c("ward_id"="id")) %>% 
  left_join(district, by = c("district_id" = "id")) %>%
  mutate(faab_id = as.numeric(`survey_info/faab`)) %>% 
  left_join(faab,by = c("faab_id"="id")) %>% 
  select(
    district,
    ward,
    village = village.y,
    faab = name,
    group,
    ######################### ACCOUNTABILITY ###################################
    registered = ends_with("registered"),
    constitution = ends_with("constitution"),
    constitution_access = ends_with("constitution_access"),
    exec_committee_num = `accountability/EC`,
    evaluation_EC = ends_with("evaluation_EC"),
    fin_reports = ends_with("financial_rep"),
    fin_reports_eval = ends_with("financial_rep_adequate"),
    participatory_decisions = ends_with("participatory_decision"),
    satisfaction = ends_with("satisfaction"),
    information_access = ends_with("information_access"),
    conflict_mgt = ends_with("conflict_mgmt"),
    registration_records = ends_with("Encroachment_problems"),
    meeting_records = ends_with("Total"),
    production_records = ends_with("/Male_Adult"),
    transaction_records = ends_with("Female_Adult"),
    financial_records = ends_with("/Male_Youth"),
    bank = ends_with("bank"),
    
    #################### PARTICIPATION #########################################
    meet_per_month = ends_with("meetings"),
    youth = ends_with("/youth"),
    women = ends_with("women"),
    annual_meeting = ends_with("annual_meeting"),
    annual_information_sharing = ends_with("annual_information_sharing"),
    annual_financial_report = ends_with("annual_financial_report"),
    attendance = ends_with("attendance"),
    member_activity_info = ends_with("member_activity_info"),
    ################### PROFESSIONAL CAPACITY ##################################
    member_training = `professionality/_26_Je_kikundi_huwa_afunzo_kwa_wanachama`,
    EC_training = ends_with("EC_training"),
    EC_GAP_FAAB = ends_with("EC_GAP_FAAB"),
    EC_organization = ends_with("EC_organize"),
    backstopping = ends_with("backstopping"),
    field_reports = ends_with("field_reports"),
    field_visits = ends_with("field_visits"),
    ################### INCOME DIVERSIFICATION #################################
    dependency = ends_with("dependency"),
    joining_fees = ends_with("membership_fees"),
    membership_paid = ends_with("membership_paid"),
    ever_credited = ends_with("ever_credited"),
    loan_status = ends_with("loan_status"),
    membership_size = ends_with("membership_size"),
    crops = ends_with("crops"),
    livestock = ends_with("livestock"),
    own_income = ends_with("own_income"),
    assets = ends_with("assets"),
    finance_access = ends_with("finance_access"),
    ################### STRATEGIC PLANNING POTENTIAL ###########################
    vision = ends_with("vision"),
    prod_plan = ends_with("prod_plan"),
    marketing_plan = ends_with("marketing_plan"),
    investment_plan = ends_with("investment_plan"),
    financial_plan = ends_with("financial_plan"),
    prioritize = ends_with("prioritize"),
    long_term_plans = ends_with("long_term_plans"),
    selfOrganization = ends_with("selfOrganization"),
    ################## PRODUCTION MANAGEMENT ###################################
    inputs_collectively = ends_with("inputs_collectively"),
    evaluate_inputs = ends_with("evaluate_inputs"),
    prod_mgt_commitee = `production/mgmt_committee`,
    prod_plan_collective = ends_with("prod_plan_collective"),
    monitor_quality_prod = ends_with("monitor_quality_prod"),
    ################## MOTIVATION FOR FARMERS TO ORGANIZE ######################
    market_collectively = ends_with("market_collectively"),
    storage = ends_with("storage"),
    aggregation_centre = ends_with("aggregation_centre"),
    transport = ends_with("transport"),
    market_info = ends_with("market_info"),
    traders = ends_with("traders"),
    ################# ADVOCACY #################################################
    network = ends_with("network"),
    lobbying = ends_with("lobbying"),
    LGA_relationship = ends_with("LGA_relationship"),
    exchange = ends_with("exchange")
  ) %>% 
  mutate(
    fin_reports = as.integer(fin_reports),
    fin_reports_eval = as.integer(fin_reports_eval),
    member_training = as.integer(member_training),
    dependency = as.integer(dependency),
    ever_credited = as.integer(ever_credited),
    financial_plan = as.integer(financial_plan),
    prioritize = as.integer(prioritize),
    selfOrganization = as.integer(selfOrganization),
    loan_status = case_when(
      loan_status == "n/a" ~ "0",
      TRUE ~ (loan_status)
    ),
    loan_status = as.integer(loan_status)
  )

cpi = cpi_2021 %>% 
  pivot_longer(cols = registered:exchange,names_to = "attribute",
               values_to = "actual") %>% 
  mutate(section = case_when(
    attribute %in% c(
      'registered' ,
      'constitution' ,
      'constitution_access' ,
      'exec_committee_num' ,
      'evaluation_EC' ,
      'fin_reports' ,
      'fin_reports_eval' ,
      'participatory_decisions' ,
      'satisfaction' ,
      'information_access' ,
      'conflict_mgt' ,
      'registration_records' ,
      'meeting_records' ,
      'production_records' ,
      'transaction_records' ,
      'financial_records' ,
      'bank' ) ~ "Accountability",
    attribute %in% c(
      'meet_per_month' ,
      'youth' ,
      'women' ,
      'annual_meeting' ,
      'annual_information_sharing' ,
      'annual_financial_report' ,
      'attendance' ,
      'member_activity_info' 
    ) ~ "Participation",
    attribute %in% c(
      'member_training' ,
      'EC_training' ,
      'EC_GAP_FAAB' ,
      'EC_organization' ,
      'backstopping' ,
      'field_reports' ,
      'field_visits' 
    ) ~ "Professional Capacity",
    attribute %in% c(
      'dependency' ,
      'joining_fees' ,
      'membership_paid' ,
      'ever_credited' ,
      'membership_size' ,
      'crops' ,
      'livestock' ,
      'own_income' ,
      'assets' ,
      'finance_access',
      'loan_status'
    ) ~ "Income Diversification",
    attribute %in% c(
      'vision' ,
      'prod_plan' ,
      'marketing_plan' ,
      'investment_plan' ,
      'financial_plan' ,
      'prioritize' ,
      'long_term_plans' ,
      'selfOrganization' 
    ) ~ "Strategic Planning",
    attribute %in% c(
      'inputs_collectively' ,
      'evaluate_inputs' ,
      'prod_mgt_commitee' ,
      'prod_plan_collective' ,
      'monitor_quality_prod' 
    ) ~ "Production Management",
    attribute %in% c(
      'market_collectively' ,
      'storage' ,
      'aggregation_centre' ,
      'transport' ,
      'market_info' ,
      'traders'
    ) ~ "Motivation to Organize",
    attribute %in% c(
      'network',
      'lobbying',
      'LGA_relationship',
      'exchange'
    ) ~ "Advocacy"
  ),
  weight = case_when(
    attribute %in% c(
      'registered','constitution','meet_per_month',
      'annual_financial_report','attendance','member_training','EC_organization',
      'joining_fees','membership_paid','loan_status','own_income',
      'prod_plan_collective','prod_records','monitor_quality_prod',
      'market_collectively','traders','fin_reports_eval'
    ) ~ 3,
    attribute %in% c(
      'evaluation_EC','information_access','registration_records','meeting_records',
      'production_records','transaction_records','financial_records','bank','annual_meeting',
      'annual_information_sharing','field_reports','field_visits','ever_credited',
      'membership_size','prod_plan','marketing_plan','investment_plan','financial_plan',
      'prioritize','long_term_plans','mgmt_committee','storage','market_info','network',
      'lobbying','LGA_relationship','exchange','prod_mgt_commitee'
    ) ~ 2,
    attribute %in% c(
      'constitution_access','participatory_decisions','conflict_mgt','women',
      'EC_training','EC_GAP_FAAB','backstopping','crops','assets','finance_access',
      'inputs_collectively','evaluate_inputs','aggregation_centre'
    ) ~ 2.5,
    attribute %in% c(
      'youth','livestock'
    ) ~ 1.5,
    attribute %in% c(
      'inputs_collectively','member_activity_info','dependency','vision',
      'selfOrganization','transport','satisfaction'
    ) ~ 1,
    attribute == 'fin_reports' ~ 2.25,
    attribute == 'exec_committee_num' ~ 0
  ),
  max_score = case_when(
    weight == 1 ~ 4,
    weight == 1.5 ~ 6,
    weight == 2 ~ 8,
    weight == 2.5 ~ 10,
    weight == 2.25 ~ 9,
    weight == 3 ~ 12,
    weight == 0 ~ 0
  ),
  weighted_score = weight*actual
  )

cpi_final = cpi %>% 
  group_by(district,ward,village,faab,group,section,attribute) %>% 
  summarise(actual = mean(actual)) %>% 
  ungroup() %>% 
  mutate(
    weight = case_when(
      attribute %in% c(
        'registered','constitution','meet_per_month',
        'annual_financial_report','attendance','member_training','EC_organization',
        'joining_fees','membership_paid','loan_status','own_income',
        'prod_plan_collective','prod_records','monitor_quality_prod',
        'market_collectively','traders','fin_reports_eval'
      ) ~ 3,
      attribute %in% c(
        'evaluation_EC','information_access','registration_records','meeting_records',
        'production_records','transaction_records','financial_records','bank','annual_meeting',
        'annual_information_sharing','field_reports','field_visits','ever_credited',
        'membership_size','prod_plan','marketing_plan','investment_plan','financial_plan',
        'prioritize','long_term_plans','mgmt_committee','storage','market_info','network',
        'lobbying','LGA_relationship','exchange','prod_mgt_commitee'
      ) ~ 2,
      attribute %in% c(
        'constitution_access','participatory_decisions','conflict_mgt','women',
        'EC_training','EC_GAP_FAAB','backstopping','crops','assets','finance_access',
        'inputs_collectively','evaluate_inputs','aggregation_centre'
      ) ~ 2.5,
      attribute %in% c(
        'youth','livestock'
      ) ~ 1.5,
      attribute %in% c(
        'inputs_collectively','member_activity_info','dependency','vision',
        'selfOrganization','transport','satisfaction'
      ) ~ 1,
      attribute == 'fin_reports' ~ 2.25,
      attribute == 'exec_committee_num' ~ 0
    ),
    max_score = case_when(
      weight == 1 ~ 4,
      weight == 1.5 ~ 6,
      weight == 2 ~ 8,
      weight == 2.5 ~ 10,
      weight == 2.25 ~ 9,
      weight == 3 ~ 12,
      weight == 0 ~ 0
    ),
    weighted_score = weight*actual
  )

max_accountability = 143
max_participation = 72
max_prof_cap = 70
max_inc_diverse = 104
max_strat_plan = 56
max_prod_mgt = 64
max_motivation = 54
max_advocacy = 32

sum_max_score = sum(max_accountability,max_advocacy,max_participation,
                    max_prof_cap,max_inc_diverse,max_strat_plan,max_prod_mgt,
                    max_motivation)

cpi_final_1 = cpi_final %>% 
  group_by(district,ward,village,faab,group,section) %>% 
  summarise(section_total = sum(weighted_score)) %>% 
  mutate(section_score = case_when(
    section == "Accountability" ~ section_total/max_accountability,
    section == "Advocacy" ~ section_total/max_advocacy,
    section == "Income Diversification" ~ section_total/max_inc_diverse,
    section == "Motivation to Organize" ~ section_total/max_motivation,
    section == "Participation" ~ section_total/max_participation,
    section == "Production Management" ~ section_total/max_prod_mgt,
    section == "Professional Capacity" ~ section_total/max_prof_cap,
    section == "Strategic Planning" ~ section_total/max_strat_plan
  ),
  
  section_weight = case_when(
    section == "Accountability" ~ max_accountability/sum_max_score,
    section == "Advocacy" ~ max_advocacy/sum_max_score,
    section == "Income Diversification" ~ max_inc_diverse/sum_max_score,
    section == "Motivation to Organize" ~ max_motivation/sum_max_score,
    section == "Participation" ~ max_participation/sum_max_score,
    section == "Production Management" ~ max_prod_mgt/sum_max_score,
    section == "Professional Capacity" ~ max_prof_cap/sum_max_score,
    section == "Strategic Planning" ~ max_strat_plan/sum_max_score
  ),
  
  weighted_section_score = section_score*section_weight
  ) %>% 
  group_by(district,ward,village,group) %>% 
  summarise(
    CPI = round(sum(weighted_section_score*100),0),
    `CPI Score` = glue::glue("{round(sum(weighted_section_score*100),0)}%")
    )

