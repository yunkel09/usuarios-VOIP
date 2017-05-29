#   ____________________________________________________________________________
#   VOIP USERS ANALYSIS                                                     ####


##  ............................................................................
##  Step 0: Packages and options                                            ####


      pkgs <- c('dplyr',
                'data.table',     # 
                'scales',
                'magrittr')

      lapply(pkgs, library, character.only = TRUE)
      options(scipen = 999)
      

##  ............................................................................
##  Step 1: Load Dataset                                                   ####


      myurl <- 'http://talent.com.gt/IntroR/UsersVoIP.csv'
      var_1 <- as.tibble(fread(input = myurl,
                               data.table = FALSE))
      
      save(var_1, file = 'voip_users_raw_df.rda')
      load('voip_users_raw_df.rda')
      
      
##  ............................................................................
##  Step 2: Data Carpentry                                                  ####

      colnames(var_1) %<>% tolower
      
##  ............................................................................
##  Step 3: Calculations                                                    ####

            
      # 2. Determine cuántos registros totales tiene el data frame resultante (cada registro representa información de un usuario)
      var_2 <- nrow(var_1)
      
      # 3. Determine cuántos usuarios son PREPAGO, cuátntos POSTPAGO y cuántos FACTURA FIJA (usar la variable CST_TYPE)
      var_3 <- var_1 %>%
               count(cst_type, sort = TRUE) %>%
               rename(users = 'n')
      
      # 4. Determine qué porcentaje de los usuarios totales son PREPAGO, POSTPAGO y FACTURA FIJA
      var_4 <- var_1 %>%
               group_by(cst_type) %>%
               summarise(users = n()) %>%
               mutate(user_proportion = users / sum(users))
      
      # 5. Determine qué porcentaje del revenue total viene de usuarios PREPAGO, POSTPAGO y FACTURA FIJA (usar la variable REVENUE_TOTAL)
      var_5 <- var_1 %>%
               group_by(cst_type) %>%
               summarise(revenue = sum(REVENUE_TOTAL)) %>%
               mutate(porcentaje_revenue = revenue / sum(revenue))
      
      # 6. Determine qué porcentaje del revenue de broadband viene de usuarios PREPAGO, POSTPAGO y FACTURA FIJA (usar la variable REVENUE_INF)
      var_6 <- var_1 %>%
               group_by(cst_type) %>%
               summarise(revenue = sum(REVENUE_INF)) %>%
               mutate(porcentaje_revenue_broadband = revenue / sum(revenue))
      
      # Lazy approach      
      # var_7 <- tibble(CST_TYPE = levels(as.factor(var_1$CST_TYPE)),
      #                 USERS = var_4$cantidad_usuarios,
      #                 USERS_PROPORTION = var_4$porcentaje_usuarios_por_tipo_cliente,
      #                 REVENUE_PROPORTION = var_5$porcentaje_revenue,
      #                 REVENUE_INF_PROPORTION = var_6$porcentaje_revenue_broadband)
      
      refcols <- c('CST_TYPE',
                   'USERS',                    
                   'USERS_PROPORTION',
                   'REVENUE_PROPORTION',
                   'REVENUE_INF_PROPORTION')
      
      var_7 <-    var_1 %>%
                  select(cst_type, REVENUE_TOTAL, REVENUE_INF) %>%
                  group_by(cst_type) %>%
                  summarise(USERS = n(),
                            REVENUE = sum(REVENUE_TOTAL),
                            REVENUE_BROADBAND = sum(REVENUE_INF)) %>%
                  mutate(USERS_PROPORTION = percent(USERS / sum(USERS)),
                         REVENUE_PROPORTION = percent(REVENUE / sum(REVENUE)),
                         REVENUE_INF_PROPORTION = percent(REVENUE_BROADBAND  / sum(REVENUE_BROADBAND ))) %>%
                  select(-REVENUE) %>%
                  arrange(desc(USERS))
      
      var_7 <-    var_7[refcols]
                  
      
      # 8. Determine qué porcentaje del revenue de broadband (variable REVENUE_INF) 
      # proviene de cada tipo de device distinto (clasificados según la variable DVC_TP_NM)
        
      var_8 <-    var_1 %>%
                  select(REVENUE_INF, DVC_TP_NM) %>%
                  group_by(DVC_TP_NM) %>%
                  summarise(REVENUE_BROADBAND = sum(REVENUE_INF)) %>%
                  mutate(REVENUE_INF_PROPORTION = percent(REVENUE_BROADBAND / sum(REVENUE_BROADBAND))) %>%
                  arrange(desc(as.numeric(sub("%", "", REVENUE_INF_PROPORTION))))
            
      # 9. Agregue una nueva variable al data frame original para determinar el
      # revenue outgoing de cada usuario (REVENUE_TOTAL - REVENUE_INC_TOTAL)
      
      var_9 <-    var_1 %>%
                  mutate(revenue_outgoing = revenue_total - revenue_inc_total)
      

      # 10. Determine el ARPU outgoing total (utilizando la nueva variable construída en el punto anterior)      
      
      var_10 <-   dollar(sum(var_9$revenue_outgoing)/n_distinct(var_9$msisdn_dd))
      
      # 11. Determine el ARPU outgoing para los siguientes tipos de usuarios
      
      var_11a <-  var_9 %>%
                  select(revenue_outgoing, msisdn_dd, uservoip_max) %>%
                  group_by(uservoip_max) %>%
                  summarise(arpu_uservoip = sum(revenue_outgoing) / n_distinct(msisdn_dd))
                        
            
      var_11b <-  var_9 %>%
                  select(revenue_outgoing, msisdn_dd, whatsappvoip_max) %>%
                  group_by(whatsappvoip_max) %>%
                  summarise(arpu_whatsappvoip = sum(revenue_outgoing) / n_distinct(msisdn_dd))
            
      
      var_11c <-  var_9 %>%
                  select(uservoip_max, mb_totales, voip_trffc_sum) %>%
                  filter(mb_totales > 0,
                         voip_trffc_sum == 0) %>%
                  mutate_at('mb_totales', round, digits = 2) %>%
                  arrange(desc(mb_totales))
      
      var_11d <-  var_9 %>%
                  filter(mb_totales == 0)
                  
            
      var_11e <-  var_9 %>%
                  filter(user_inc_intl > 0)

      # 12. Determine el ARPU outgoing de los usuarios con cada DVC_TP_NM
                  
      var_12 <-   var_9 %>%
                  group_by(dvc_tp_nm) %>%
                  summarise(arpu_by_dvc_tp = sum(revenue_outgoing) / n_distinct(msisdn_dd)) %>%
                  arrange(desc(arpu_by_dvc_tp)) %>%
                  mutate_at('arpu_by_dvc_tp', round, digits = 2)
      

            
            
            
            
            
            
            
            
        
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
               
      