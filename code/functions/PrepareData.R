# this function prepares Peron's data "Final_10v4.dta" for further analysis
# by Jiaru Zhong,04.09.2023


PrepareData <- function(){
  # Record execution time
  start_time <- Sys.time()
  
  
  # LOADING DATA
  data <- read_dta("input/Final_10v4.dta")
  
  
  # Dropping some observations
  data <- data[data$Year >= 1980, ]
  data <- data[!is.na(data$Year), ]
  data <- data[data$percentile != "", ]
  data <- data[!(data$aptinc > 0 & is.na(data$PWMtemp_cru_v4)), ]
  data <- data[!(data$CountryCode %in% c("SYC", "MDV", "VEN", "SSD", "PRK")), ]
  
  # Creating new variables
  
  data$IGSWB <- as.integer(factor(data$WB_Incomegroup))
  data$REGS5 <- as.integer(factor(data$`_regions`))
  data$REMIND <- as.integer(factor(data$RegionCode))
  
  data$gini_index1 <- data$gini_index * 100
  data$hcpi_a1 <- data$hcpi_a / 100
  data$KOFTrGIdf1 <- data$KOFTrGIdf / 100
  data$KOFFiGIdf1 <- data$KOFFiGIdf / 100
  data$KOFIpGIdf1 <- data$KOFIpGIdf / 100
  data$KOFPoGIdf1 <- data$KOFPoGIdf / 100
  data$KOFCuGIdf1 <- data$KOFCuGIdf / 100
  data$GDP_grwth1 <- data$GDP_grwth / 100
  data$Pop_grwth1 <- data$Pop_grwth / 100
  data$Edu_exp1 <- data$Edu_exp / 100
  
  
  # Generating new variables
  data <- data %>%
    mutate(PWMRainfall_cru_v41 = PWMRainfall_cru_v4 * 0.001,
           AWMRainfall_cru_v41 = AWMRainfall_cru_v4 * 0.001)
  
  
  # Calculate the mean temperature by state
  data <- data %>%
    group_by(state) %>%
    mutate(TemP_Mean1 = mean(PWMtemp_cru_v4)) %>%
    ungroup()
  
  # Create a dummy variable 'KCC' based on 'TemP_Mean1'
  data$KCC <- ifelse(data$TemP_Mean1 < 18, 0, ifelse(data$TemP_Mean1 >= 18, 1, NA))
  
  
  
  # Estimating counterfactual country-level mean temperature
  data <- data %>%
    group_by(state) %>%
    mutate(CTemP_Mean_gswp3 = mean(ctemp_gswp3,na.rm = TRUE),
           CTemP_Mean_20crv3 = mean(ctemp_20crv3,na.rm = TRUE)) %>%
    ungroup()
  
  # Calculate panelid using ave()
  data <- data %>%
    group_by(state, percentile) %>%
    mutate(panelid = cur_group_id())
  
  
  # Setting data as panel data
  data <- data %>%
    arrange(panelid, Year) %>%
    mutate(panelid = as.factor(panelid)) %>%
    mutate_at(vars(Year), as.integer)
  
  pdata <- pdata.frame(data, index=c("panelid", "Year"))
  
  # Estimating income growth and creating income lag variables
  unique_panels <- unique(pdata$panelid)
  
  pdata$Growth_income=NA
  pdata$Lag_income=NA
  for (panel in unique_panels) {
    panel_data <- pdata %>% dplyr::filter(panelid == panel)
    panel_data <- panel_data %>%
      arrange(Year) %>%
      mutate(Growth_income = c(NA,diff(Ln_income_ppp05))) 
    pdata[pdata$panelid == panel, ] <- panel_data
  }
  
  
  # Creating new ID variables for analysis
  pdata <- pdata %>%
    group_by(state, panelid) %>%
    mutate(clusid = cur_group_id()) 
  
  pdata <- pdata %>%
    mutate(temp = PWMtemp_cru_v4,
           temp2 = PWMtemp_cru_v4 * PWMtemp_cru_v4,
           prec = PWMRainfall_cru_v41,
           prec2 = PWMRainfall_cru_v41 * PWMRainfall_cru_v41)
  
  pdata$Year=as.character(pdata$Year)
  pdata$IG_hetero=relevel(as.factor(pdata$IG_hetero),ref=4)
  pdata$state=as.factor(pdata$state)
  
  
  # Sorting data for the regression analysis
  pdata <- pdata %>%
    arrange(panelid, Year)
  
  
  return(pdata)
  
  # Record the end time
  end_time <- Sys.time()
  
  # Calculate the execution time
  execution_time <- end_time - start_time
  
  # Print the execution time
  cat("Execution time:", execution_time, "seconds\n")
  
}
