#   ____________________________________________________________________________
#   Usuarios VOIP                                                           ####

      # Paquetesa a utilizar
      pkgs <- c('data.table', 'tidyverse')
      
      # Cargar paquetes
      inst <- lapply(pkgs, library, character.only = TRUE)
      
      myurl <- 'http://talent.com.gt/IntroR/UsersVoIP.csv'
      var_1 <- fread(myurl, data.table = FALSE)
      
      
      # 2. Determine cuántos registros totales tiene el data frame resultante (cada registro representa información de un usuario)
      var_2 <- nrow(var_1)
      
      # 3. Determine cuántos usuarios son PREPAGO, cuátntos POSTPAGO y cuántos FACTURA FIJA (usar la variable CST_TYPE)
      var_3 <- var_1 %>%
               group_by(CST_TYPE) %>%
               summarise(cantidad_usuarios = n())
      
      # 4. Determine qué porcentaje de los usuarios totales son PREPAGO, POSTPAGO y FACTURA FIJA
      var_4 <- var_1 %>%
               group_by(CST_TYPE) %>%
               summarise(cantidad_usuarios = n()) %>%
               mutate(porcentaje = cantidad_usuarios / sum(cantidad_usuarios))
      
      # 5. Determine qué porcentaje del revenue total viene de usuarios PREPAGO, POSTPAGO y FACTURA FIJA (usar la variable REVENUE_TOTAL)
      var_5 <- var_1 %>%
               group_by(CST_TYPE) %>%
               summarise(revenue = sum(REVENUE_TOTAL)) %>%
               mutate(porcentaje_revenue = revenue / sum(revenue))
      
      # 6. Determine qué porcentaje del revenue de broadband viene de usuarios PREPAGO, POSTPAGO y FACTURA FIJA (usar la variable REVENUE_INF)
      var_6 <- var_1 %>%
               group_by(CST_TYPE) %>%
               summarise(revenue = sum(REVENUE_INF)) %>%
               mutate(porcentaje_revenue_broadband = revenue / sum(REVENUE_INF))
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
               
      