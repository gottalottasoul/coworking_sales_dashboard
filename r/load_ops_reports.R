# ./load.r
# This file builds the necessary dataframes for the report and saves them as an r data file
# Blake Abbenante
# 10/1/19

#########Build the reporting dfs############################



fill_deal_summary<-sfdc_deals_edited %>% 
#  left_join(RiHana::get_hana_locations() %>% select(hs_locations,hana_unit),by=c("location_id"="hs_locations")) %>% 
  mutate(total_deal_amount=monthly_amount*lease_term) %>%
#  mutate(pipeline_stage=factor(pipeline_stage,levels=c("Closed Won","Closed Lost","Contract Sent", "Negotiation/Proposal","Touring", "Qualified - Local Fill Opportunity", "Upper Funnel"))) %>%
  mutate(pipeline_stage=factor(pipeline_stage,levels=c("Closed Won","Closed Lost","Verbal Award / Contract Sent", "Proposal", "Negotiation", "Touring / Solutioning", "Qualified", "Exploratory"))) %>% 
  select(deal_name,hana_unit,hana_product,pipeline_stage,create_week,seats,total_deal_amount,sales_channel,acquisition_channel)

summ_all<-fill_deal_summary %>% #build an aggregated view to append to the report
  mutate(hana_unit='All') %>% 
  group_by(hana_unit,pipeline_stage) %>% 
  summarise(me_deals=scales::comma(n()),
            me_seats=scales::comma(sum(seats,na.rm=TRUE)),
            me_revenue=scales::dollar(sum(total_deal_amount,na.rm = TRUE))) %>% 
  pivot_longer(cols=starts_with('me_'),
               names_to = 'measure',
               names_prefix = 'me_',
               values_to = 'count') %>% 
  ungroup(.) %>% 
  arrange(desc(pipeline_stage)) %>% 
  pivot_wider(names_from=pipeline_stage,values_from = count) %>% 
  ungroup(.) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) 

summ_location<-fill_deal_summary %>% #group by location
  mutate(hana_unit=ifelse(is.na(hana_unit),'Other',hana_unit)) %>% 
  group_by(hana_unit,pipeline_stage) %>% 
  summarise(me_deals=scales::comma(n()),
            me_seats=scales::comma(sum(seats,na.rm=TRUE)),
            me_revenue=scales::dollar(sum(total_deal_amount,na.rm = TRUE)),
            sort_revenue=sum(total_deal_amount[pipeline_stage=='Closed Won'],na.rm=TRUE)) %>% 
  pivot_longer(cols=starts_with('me_'),
               names_to = 'measure',
               names_prefix = 'me_',
               values_to = 'count') %>% 
  ungroup(.) %>% 
  arrange(desc(sort_revenue),desc(pipeline_stage)) %>% 
  select(-sort_revenue) %>% 
#  arrange(desc(pipeline_stage)) %>% 
  pivot_wider(names_from=pipeline_stage,values_from = count) %>% 
  ungroup(.) %>% 
  mutate_if(is.character, ~replace(., is.na(.), 0))  

#bind the aggregate and location specific
pipeline_summary<-rbind(summ_all,summ_location)

last_week_status<-sfdc_deal_history_edited %>% 
  filter(stage_date<=hana_dates$past_7_days) %>% 
  arrange(desc(stage_date)) %>% 
  group_by(opportunity_id) %>% 
  slice(1) %>% 
  select(opportunity_id,last_pipeline_stage=pipeline_stage_detail)


pipeline_deals<-sfdc_deals_edited %>%
#left_join(RiHana::get_hana_locations(),by=c("location_id"="hs_locations")) %>%
mutate(close_week=floor_date(close_date,unit="week",week_start=1),
close_month=floor_date(close_date,unit="month"),
close_quarter=floor_date(close_date,unit="quarter"),
cohort_week=floor_date(contact_create_date,unit="week",week_start=1),
cohort_month=floor_date(contact_create_date,unit="month"),
cohort_quarter=floor_date(contact_create_date,unit="quarter"),
lease_term=case_when(#for missing data, we'll default to 3 months for Share and 12 months for Team
    is.na(lease_term) & hana_product=='Hana Share'~3,
    is.na(lease_term) & hana_product=='Hana Team'~12,
    is.na(lease_term) & hana_product=='Hana Meet'~1,
    TRUE~lease_term
),
total_deal_amount=lease_term*monthly_amount
) %>%
  mutate(pipeline_stage=factor(pipeline_stage,levels=c("Closed Won","Closed Lost","Verbal Award / Contract Sent", "Proposal", "Negotiation", "Touring / Solutioning", "Qualified", "Exploratory")), 
  #customer_name=coalesce(customer_name,'Not Provided'))%>%
customer_name=case_when(
  grepl("unknown",customer_name,ignore.case = TRUE)~associated_contacts,
  is.na(customer_name)~associated_contacts,
  TRUE~customer_name)) %>% 
arrange(pipeline_stage,anticipated_start_date) %>%
#mutate(monthly_amount=scales::dollar(monthly_amount)) %>%
left_join(last_week_status,by=c("deal_id"="opportunity_id")) %>%
  mutate_if(str_detect(names(.),"(hana_unit|hana_city|hana_region)"), ~replace(., is.na(.), "Not Specified")) %>%   
#  left_join(hs_deal_notes) %>%
select(deal_name,customer_name,hana_region,hana_unit,hana_city,hana_product,pipeline_stage,create_date,create_week,create_month,create_quarter,seats,lease_term,monthly_amount,total_deal_amount,anticipated_start_date,close_date,close_week,close_month,close_quarter,cohort_week,cohort_month,cohort_quarter,closed_lost_reason,acquisition_channel,acquisition_source,digital_channel,sales_channel,pipeline_stage_detail,last_pipeline_stage,deal_link,notes)



#calculate the conversion percentage for all closed deals
#by stage of the pipeline
historical_conversion_by_stage<-sfdc_deal_history_edited %>%
  filter(pipeline_stage %in% c('Closed Lost','Closed Won')) %>%
  select(opportunity_id,final_status=pipeline_stage) %>%
  inner_join(sfdc_deal_history_edited) %>%
  select(opportunity_id,final_status,pipeline_stage) %>%
  unique(.) %>%
  group_by(final_status,pipeline_stage) %>%
  summarise(deals=n()) %>%
  pivot_wider(names_from=c(final_status),
              values_from = c(deals),
              values_fill = list(deals=0)) %>%
  janitor::clean_names(.)  %>%
  ungroup(.) %>%
  mutate(conversion=closed_won/(closed_won+closed_lost),
         pipeline_stage=factor(pipeline_stage,levels=c("Closed Won","Closed Lost","Verbal Award / Contract Sent", "Proposal", "Negotiation", "Touring / Solutioning", "Qualified", "Exploratory"))) %>% 
  arrange(pipeline_stage) 


historical_deal_stage<-sfdc_deals_edited %>% 
  mutate(Pipeline_resolution = ifelse(pipeline_stage %in% c('Closed Lost','Closed Won'),pipeline_stage,'In Progress')) %>% 
  select(deal_id,Pipeline_resolution,location_id,hana_product,create_date,acquisition_channel,sales_channel) %>% 
  inner_join(sfdc_deal_history_edited,by=c("deal_id"="opportunity_id")) %>% 
  inner_join(RiHana::get_hana_locations(),by=c("location_id"="hs_locations")) %>% 
  arrange(stage_date) %>% 
  group_by(deal_id) %>% 
  mutate(next_date=coalesce(lead(stage_date),now()),
         time_in_stage= round(difftime(next_date,stage_date,units="days"),1)) %>% 
  rename(hana_unit=hana_name)
#         stage_date=as_date(date_created)) %>% 
#  select(-name,-date_created,-source_id,-source,-next_date,-nex_locations,-open_date,-unit_local_tz)
  
  
  
  


 # save(pipeline_summary,us_deals,
 #      file="inc/us_ops_report.RData")
 # 
 # save(pipeline_summary,uk_deals,
 #      file="inc/uk_ops_report.RData")
 
 save(pipeline_summary,
      pipeline_deals,
#      fill_deal_details,
      historical_deal_stage,
      historical_conversion_by_stage,
      file="inc/local_fill_dashboard.RData")
 

  