#   ____________________________________________________________________________
#   Usuarios VOIP                                                           ####

      library(dplyr)
      library(data.table)
      library(scales)
      
      # Leer una vez
      myurl <- 'http://talent.com.gt/IntroR/UsersVoIP.csv'
      var_1 <- as.tibble(fread(input = myurl,
                               data.table = FALSE))
      
      # Guardar en formato binario
      save(var_1, file = 'voip_users_raw_df.rda')
      load('voip_users_raw_df.rda')
      
      
      # 2. Determine cuántos registros totales tiene el data frame resultante (cada registro representa información de un usuario)
      var_2 <- nrow(var_1)
      
      # 3. Determine cuántos usuarios son PREPAGO, cuátntos POSTPAGO y cuántos FACTURA FIJA (usar la variable CST_TYPE)
      var_3 <- var_1 %>%
               count(CST_TYPE, sort = TRUE) %>%
               rename(cantidad_usuarios = 'n')
      
      # 4. Determine qué porcentaje de los usuarios totales son PREPAGO, POSTPAGO y FACTURA FIJA
      var_4 <- var_1 %>%
               group_by(CST_TYPE) %>%
               summarise(cantidad_usuarios = n()) %>%
               mutate(porcentaje_usuarios_por_tipo_cliente = cantidad_usuarios / sum(cantidad_usuarios))
      
      # 5. Determine qué porcentaje del revenue total viene de usuarios PREPAGO, POSTPAGO y FACTURA FIJA (usar la variable REVENUE_TOTAL)
      var_5 <- var_1 %>%
               group_by(CST_TYPE) %>%
               summarise(revenue = sum(REVENUE_TOTAL)) %>%
               mutate(porcentaje_revenue = revenue / sum(revenue))
      
      # 6. Determine qué porcentaje del revenue de broadband viene de usuarios PREPAGO, POSTPAGO y FACTURA FIJA (usar la variable REVENUE_INF)
      var_6 <- var_1 %>%
               group_by(CST_TYPE) %>%
               summarise(revenue = sum(REVENUE_INF)) %>%
               mutate(porcentaje_revenue_broadband = revenue / sum(revenue))
      
      # Lazy approach      
      # var_7 <- tibble(tipo_cliente = levels(as.factor(var_1$CST_TYPE)),
      #                     cantidad_usuarios = var_4$cantidad_usuarios,
      #                     porcentaje_usuarios_por_tipo = var_4$porcentaje_usuarios_por_tipo_cliente,
      #                     porcentaje_revenue_total = var_5$porcentaje_revenue,
      #                     porcentaje_revenue_broadband = var_6$porcentaje_revenue_broadband)
      
      refcols <- c('CST_TYPE',
                   'USERS',
                   'USERS_PROPORTION',
                   'REVENUE_PROPORTION',
                   'REVENUE_INF_PROPORTION')
      
      var_7 <-    var_1 %>%
                  select(CST_TYPE, REVENUE_TOTAL, REVENUE_INF) %>%
                  group_by(CST_TYPE) %>%
                  summarise(USERS = n(),
                            REVENUE = sum(REVENUE_TOTAL),
                            REVENUE_BROADBAND = sum(REVENUE_INF)) %>%
                  mutate(USERS_PROPORTION = percent(USERS / sum(USERS)),
                         REVENUE_PROPORTION = percent(REVENUE / sum(REVENUE)),
                         REVENUE_INF_PROPORTION = percent(REVENUE_BROADBAND  / sum(REVENUE_BROADBAND ))) %>%
                  select(-REVENUE) %>%
                  arrange(desc(USERS))
      
      var_7 <- var_7[refcols]
                  
             
            
            
            
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
               
      