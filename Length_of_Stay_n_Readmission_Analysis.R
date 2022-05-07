library(readxl)
library(data.table)
library(tidyverse)
library(xml2)

LoS_Readmit_func <- function(df,...) {
  vars <- enquos(...)
    df %>%
    select(!!!vars) %>%
    group_by(BENE_MBI_ID) %>%
    arrange(BENE_MBI_ID, CLM_FROM_DT, CLM_THRU_DT) %>%
    mutate(
      CLM_FROM_DT = as.Date(CLM_FROM_DT),
      CLM_THRU_DT = as.Date(CLM_THRU_DT),
      CLM_EFCTV_DT = as.Date(CLM_EFCTV_DT),
      counts = n(),
      rcd_cnt = row_number(BENE_MBI_ID),
      admit_flag = case_when(
        counts == 1 |
          rcd_cnt == min(rcd_cnt) |
          (counts > 1 &
            (CLM_FROM_DT - lag(CLM_THRU_DT, 1) > 1)) ~ "1",
        (counts > 1 &
          (CLM_FROM_DT - lag(CLM_THRU_DT, 1) <= 1) ~ "0")
      ),
      dsch_flag = case_when(
        (max(counts) == 1 |
           rcd_cnt == max(counts) |
           (counts > 1 &
            (lead(CLM_FROM_DT, 1) - CLM_THRU_DT) > 1)) & BENE_PTNT_STUS_CD != "30" |
          (rcd_cnt == max(counts) & BENE_PTNT_STUS_CD == "30" &
            (CLM_EFCTV_DT - CLM_THRU_DT) > 30) ~ "1",


        (counts > 1 &
          (lead(CLM_FROM_DT, 1) - CLM_THRU_DT) <= 1) |
          (max(counts) >= 1 & BENE_PTNT_STUS_CD == "30") |
          (rcd_cnt == max(counts) & BENE_PTNT_STUS_CD == "30" &
          (CLM_EFCTV_DT - CLM_THRU_DT) <= 30) ~ "0"),

      starts_id = rep(accelerometry::rle2(as.numeric(dsch_flag), indices = TRUE)[, 2], accelerometry::rle2(as.numeric(dsch_flag), indices = TRUE)[, 4]),
      starts = if_else(admit_flag == 0 & dsch_flag == 1, lag(starts_id), starts_id),
      ADMIT_FROM_DT = if_else(admit_flag == 1, CLM_FROM_DT, CLM_FROM_DT[starts]),
      

      ip_dsch = if_else(dsch_flag == "0", 0, cumsum(as.numeric(dsch_flag))),
      days = ifelse(dsch_flag == "0" & rcd_cnt != max(rcd_cnt), 0, ifelse(dsch_flag == "1" & BENE_PTNT_STUS_CD != "30",
        as.numeric(CLM_THRU_DT - ADMIT_FROM_DT),
        as.numeric(CLM_THRU_DT - ADMIT_FROM_DT) + 1
      )),
      uniq_rcd = ifelse(rcd_cnt==1,1,
                       ifelse(
                         rcd_cnt >1 &
                           as.Date(CLM_FROM_DT)==lag(as.Date(CLM_FROM_DT)) &
                           as.Date(CLM_THRU_DT)==lag(as.Date(CLM_THRU_DT)) &
                           CLM_TYPE_CD ==lag(CLM_TYPE_CD),0,1)),
      
      dsch_run = rep(rle(ip_dsch)$values == "1" ,rle(ip_dsch)$lengths),
      
      dsch_sum = cumsum(dsch_run),
      
      
      dsch_ind = rep(accelerometry::rle2(as.numeric(dsch_run),indices = TRUE)[,2],accelerometry::rle2(as.numeric(dsch_run),indices = TRUE)[,4]),
      
      dsch_start = ifelse(dsch_ind == 1,1,dsch_ind-1),
      
      dsch_len    =  rep(rle(dsch_run)$lengths,rle(dsch_run)$lengths),
      
     DSCH_FROM_DT = if_else(dsch_flag == 1, CLM_THRU_DT, CLM_THRU_DT[dsch_start]),
      
      
      readmit = ifelse(uniq_rcd == "1" & dsch_sum == 1 & as.Date(CLM_THRU_DT) - as.Date(DSCH_FROM_DT) >= 1 &
                       as.Date(CLM_THRU_DT) - as.Date(DSCH_FROM_DT) <= 30 |                 
                       
                       uniq_rcd ==  "1" & dsch_sum > 1 & as.Date(CLM_THRU_DT) - lag(as.Date(DSCH_FROM_DT)) >= 0  &  
                       as.Date(CLM_THRU_DT) - lag(as.Date(DSCH_FROM_DT)) <= 30 ,1,0)
    )
}
 

fac_util <- map_df(dflist[c(3:4,6,17)],
                   days_fn,BENE_MBI_ID, CLM_TYPE_CD,CLM_FROM_DT,CLM_THRU_DT,CLM_PMT_AMT,CLM_EFCTV_DT, BENE_PTNT_STUS_CD,
                   .id = "groups") 
