shinyServer(function(input, output) {
   
  
  
  #------------------------HALAMAN 1
  
  # INFO BOX OUTPUT
  output$info_1 <- renderText({
     info_1 <- upah.df %>% 
      #menghubungkan dengan filter
      filter(tahun %in% input$input_tahun) %>% 
      filter(provinsi %in% input$input_provinsi)
     glue("Rp",formatC(info_1$upah, big.mark = ".", decimal.mark = ","))
     
  })
  
  output$info_2 <- renderText({
    info_2 <- ump.df %>% 
      #menghubungkan dengan filter
      filter(tahun %in% input$input_tahun) %>% 
      filter(provinsi %in% input$input_provinsi)
    glue("Rp",formatC(info_2$ump, big.mark = ".", decimal.mark = ","))
    
  })
  
  output$info_3 <- renderText({
    info_3 <- peng.df %>% 
      #menghubungkan dengan filter
      filter(tahun %in% input$input_tahun) %>% 
      filter(provinsi %in% input$input_provinsi) %>%
      filter(daerah %in% input$input_daerah) %>%
      filter(jenis %in% input$input_jenis)
    glue("Rp",formatC(info_3$peng, big.mark = ".", decimal.mark = ","))
    
  })
  
  output$info_4 <- renderText({
    info_4 <- gk.df %>% 
      #menghubungkan dengan filter
      filter(tahun %in% input$input_tahun) %>% 
      filter(provinsi %in% input$input_provinsi) %>%
      filter(daerah %in% input$input_daerah) %>%
      filter(jenis %in% input$input_jenis) %>%
      filter(periode %in% input$input_periode)
    glue("Rp",formatC(info_4$gk, big.mark = ".", decimal.mark = ","))
    
  })
  
  #VISUALISASI HALAMAN 1
  
  
  #HEADING HALAMAN 1
  output$heading_1a <- renderText({
    glue("Diagram Peta {input$input_data1} Menurut Provinsi")
  })
  
  output$heading_1b <- renderText({
    glue("15 Provinsi {input$input_data1} Tertinggi")
  })
  
  
  #LEGENDA MAP CHART DAN BARPLOT HALAMAN 1
  output$legend_img <- renderImage({
    
    list(src = "assets/legend1.png",
         width = "100%")
    
  }, deleteFile = F)
  
  
  #DIAGRAM PETA (MAP CHART) HALAMAN 1
  output$mapchart_upah <- renderLeaflet({
    
    if(input$input_data1 == "Upah Pekerja Per Jam"){
      
      case_pilih <- upah.df %>%
        filter(tahun %in% input$input_tahun) %>%
        mutate(nilai = upah)
      
    } else {
      if(input$input_data1 == "Upah Minimum Provinsi"){
        
        case_pilih <- ump.df %>%
          filter(tahun %in% input$input_tahun) %>%
          mutate(nilai = ump)
        
      } else {
        if(input$input_data1 == "Pengeluaran Per Kapita"){
          
          case_pilih <- peng.df %>%
            filter(tahun %in% input$input_tahun) %>%
            filter(daerah %in% input$input_daerah) %>%
            filter(jenis %in% input$input_jenis) %>%
            mutate(nilai = peng)
          
        } else {
          
          case_pilih <- gk.df %>%
            filter(tahun %in% input$input_tahun) %>%
            filter(daerah %in% input$input_daerah) %>%
            filter(jenis %in% input$input_jenis) %>%
            filter(periode %in% input$input_periode) %>%
            mutate(nilai = gk)
          
        }
      }
    }
    
    
    
    data.map <- left_join(map_indo, case_pilih, by = "provinsi")
    
    mybins <- c()
    for(i in 0:20){
      mybins <- c(mybins,quantile(data.map$nilai,i/20))
    }
    
    mypalette <- colorBin(palette=c("#EC7063","#EB984E","#F1C40F","#58D68D","#148F77"), 
                          domain=data.map$nilai, 
                          na.color="transparent", 
                          bins=mybins)
    
    
    leaflet(data.map) %>%
      addTiles() %>%
      #fitBounds(~min(94), ~min(-13), ~max(145), ~max(5)) %>%
      setView( lat=-3, lng=118 , zoom=4.4) %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
                  fillColor = ~mypalette(nilai),
                  label = ~paste0(provinsi, ": Rp", formatC(nilai, big.mark = ".", decimal.mark = ","))) %>%
      #addLegend("bottomright", pal = mypalette, values = round(upah,0), opacity = 1.0, title="Upah Pekerja") %>% 
      addProviderTiles(providers$CartoDB.Positron) 
  })
  
  
  #DIAGRAM BATANG HALAMAN 1
  output$barchart_upah <- renderPlotly({
    
    if(input$input_data1 == "Upah Pekerja Per Jam"){
      
      case_pilih <- upah.df %>%
        filter(tahun %in% input$input_tahun) %>%
        mutate(nilai = upah)
      
    } else {
      if(input$input_data1 == "Upah Minimum Provinsi"){
        
        case_pilih <- ump.df %>%
          filter(tahun %in% input$input_tahun) %>%
          mutate(nilai = ump)
        
      } else {
        if(input$input_data1 == "Pengeluaran Per Kapita"){
          
          case_pilih <- peng.df %>%
            filter(tahun %in% input$input_tahun) %>%
            filter(daerah %in% input$input_daerah) %>%
            filter(jenis %in% input$input_jenis) %>%
            mutate(nilai = peng)
          
        } else {
          
          case_pilih <- gk.df %>%
            filter(tahun %in% input$input_tahun) %>%
            filter(daerah %in% input$input_daerah) %>%
            filter(jenis %in% input$input_jenis) %>%
            filter(periode %in% input$input_periode) %>%
            mutate(nilai = gk)
          
        }
        
        
      }
    }
    
    barchart_upah1 <- case_pilih %>%
      mutate(text = glue("Provinsi: {provinsi}
                          {input$input_data1}: Rp{nilai}")) %>%
      arrange(-nilai) %>%
      head(15) %>%
      ggplot(mapping = aes(x = nilai/1000,
                           y = reorder(provinsi, nilai/1000),
                           text = text)) +
      # layer 1
      geom_col(mapping = aes(fill = nilai/1000)) +
      
      # mengubah gradasi warna
      scale_fill_gradientn(colours = c("#CECB47","#92D274","#4FCA8A","#279D7C","#148F77"))+
      
      # memberikan label text
      # geom_text(mapping = aes(label = glue("Rp{upah}")), 
      #           size = 3, 
      #           hjust = 3) + 
      labs(y = "Provinsi",
           x = paste(input$input_data1," (ribu rupiah)")
      ) +
      # memberikan tema
      theme_classic() +
      theme(legend.position = "none",
            text = element_text(size = 8))
    
    ggplotly(p = barchart_upah1, 
             tooltip = "text",
             height = 420)
    
  })
  
  
  
  #--------------------------HALAMAN 3
  
  
  # PLOT LINE CHART
  
  output$linechart_gab <- renderPlotly({
    
    
    case_upah <- upah.df %>%
      filter(provinsi %in% input$input_provinsi3) %>%
      filter(tahun %in% input$input_tahun3[1]:input$input_tahun3[2]) %>%
      mutate(upah_bulan = upah * input$input_jamkerja * 4)
      
    case_ump <- ump.df %>%
      filter(provinsi %in% input$input_provinsi3) %>%
      filter(tahun %in% input$input_tahun3[1]:input$input_tahun3[2])
    
    case_peng <- peng.df %>%
      filter(provinsi %in% input$input_provinsi3) %>%
      filter(daerah %in% input$input_daerah3) %>%
      filter(jenis %in% "TOTAL") %>%
      filter(tahun %in% input$input_tahun3[1]:input$input_tahun3[2]) %>%
      mutate(peng_ruta = peng * input$input_tanggungan)
    
    case_gk <- gk.df %>%
      filter(provinsi %in% input$input_provinsi3) %>%
      filter(daerah %in% input$input_daerah3) %>%
      filter(jenis %in% "TOTAL") %>%
      filter(periode %in% "MARET") %>%
      filter(tahun %in% input$input_tahun3[1]:input$input_tahun3[2]) %>%
      mutate(gk_ruta = gk * input$input_tanggungan)
    
      
    
    
    case_linechart <- case_upah %>%
      left_join(case_ump, by = "tahun") %>%
      left_join(case_peng, by = "tahun") %>%
      left_join(case_gk, by = "tahun") 
    # 
    if(input$input_data3 == "Upah Pekerja Sebulan"){
      case_linechart <- case_linechart %>%
        mutate(upah_pilih = upah_bulan) %>%
        dplyr::select(tahun, upah_pilih, peng_ruta, gk_ruta)%>%
        mutate(text = glue("<b>{input$input_data3}</b>
                            Rp{upah_pilih}")) %>%
        mutate(text1 = glue("<b>Pengeluaran Ruta</b>
                             Rp{peng_ruta}")) %>%
        mutate(text2 = glue("<b>Garis Kemiskinan Ruta</b>
                             Rp{gk_ruta}"))
        
    } else {
      case_linechart <- case_linechart %>%
        mutate(upah_pilih = ump) %>%
        dplyr::select(tahun, upah_pilih, peng_ruta, gk_ruta) %>%
        mutate(text = glue("<b>Tahun</b>: {tahun}
                          <b>UMP</b>: Rp{upah_pilih}")) %>%
        mutate(text1 = glue("<b>Pengeluaran Ruta</b>
                             Rp{peng_ruta}")) %>%
        mutate(text2 = glue("<b>Garis Kemiskinan Ruta</b>
                             Rp{gk_ruta}"))
    }
    
    
    chart_linechart <- case_linechart %>% 
      ggplot(aes(x = tahun)) +
      geom_line(aes(y = peng_ruta),
                colour = "orange") +
      geom_point(aes(y=peng_ruta,
                     size = 1,
                     text = text1),
                 colour = "orange")+
      geom_area(aes(y = peng_ruta),
                alpha=0.6 , 
                linewidth=.5, 
                colour= NA,
                fill="#FAD7A0") +
      geom_line(aes(y = gk_ruta),
                colour = "maroon") +
      geom_point(aes(y=gk_ruta,
                     size = 1,
                     text = text2),
                 colour = "maroon")+
      geom_area(aes(y = gk_ruta),
                alpha=0.6 , 
                linewidth=.5, 
                colour= NA,
                fill="#F1948A") +
      geom_line(aes(y = upah_pilih),
                colour = "black") +
      geom_point(aes(y = upah_pilih,
                     size = upah_pilih,
                     text = text),
                 colour = "#17A589") +
      geom_segment(aes(x = 2022,
                       xend = 2022,
                       y = 0,
                       yend = input$upah_sendiri),
                   color = "#21618C")+
      geom_point(aes(x = 2022,
                     y = input$upah_sendiri,
                     text = glue("<b>Pendapatan Saya</b>
                                  Rp{input$upah_sendiri}")),
                 size = 5,
                 color = "#21618C",
                 fill = alpha("#5DADE2", 0.3),
                 alpha = 0.7,
                 shape = 21,
                 stroke = 2)+
      
      
      
      labs(x = "Tahun",
           y = "Pendapatan vs Pengeluaran Pekerja (rupiah)",
           caption = "Sumber: Badan Pusat Statistik (BPS)") +
      
      theme_minimal()+
      theme(legend.position = "right",
            text = element_text(size = 8))
    
    
    ggplotly(p = chart_linechart, tooltip="text", height = 350)
    
  })
  

  
  # INFO BOX OUTPUT HALAMAN 3
  
  output$info_1b <- renderText({
    case_upah2 <- upah.df %>%
      filter(provinsi %in% input$input_provinsi3) %>%
      filter(tahun %in% 2022) %>%
      mutate(upah_bulan = upah * input$input_jamkerja * 4)
    
    glue("Rp",format(case_upah2$upah_bulan, 
                     scientific = FALSE,
                     big.mark = ".", decimal.mark = ","))
    
  })
  
  output$info_2b <- renderText({
    case_ump2 <- ump.df %>%
      filter(provinsi %in% input$input_provinsi3) %>%
      filter(tahun %in% 2022)
    glue("Rp",formatC(case_ump2$ump, big.mark = ".", decimal.mark = ","))
    
  })
  
  output$info_3b <- renderText({
    case_peng2 <- peng.df %>%
      filter(provinsi %in% input$input_provinsi3) %>%
      filter(daerah %in% input$input_daerah3) %>%
      filter(jenis %in% "TOTAL") %>%
      filter(tahun %in% 2022) %>%
      mutate(peng_ruta = peng * input$input_tanggungan)
    glue("Rp",formatC(case_peng2$peng_ruta, big.mark = ".", decimal.mark = ","))
    
  })
  
  output$info_4b <- renderText({
    case_gk2 <- gk.df %>%
      filter(provinsi %in% input$input_provinsi3) %>%
      filter(daerah %in% input$input_daerah3) %>%
      filter(jenis %in% "TOTAL") %>%
      filter(periode %in% "MARET") %>%
      filter(tahun %in% 2022) %>%
      mutate(gk_ruta = gk * input$input_tanggungan)
    glue("Rp",formatC(case_gk2$gk_ruta, big.mark = ".", decimal.mark = ","))
    
  })
  
  # VALUE BOX STATUS KESEJAHTERAAN HALAMAN 3
  
  output$info_1c <- renderValueBox({
    #UPAH PEKERJA
    case_upah2 <- upah.df %>%
      filter(provinsi %in% input$input_provinsi3) %>%
      filter(tahun %in% 2022) %>%
      mutate(upah_bulan = upah * input$input_jamkerja * 4)
    
    #RATA2 PENGELUARAN
    case_peng2 <- peng.df %>%
      filter(provinsi %in% input$input_provinsi3) %>%
      filter(daerah %in% input$input_daerah3) %>%
      filter(jenis %in% "TOTAL") %>%
      filter(tahun %in% 2022) %>%
      mutate(peng_ruta = peng * input$input_tanggungan)
    
    #GARIS KEMISKINAN
    case_gk2 <- gk.df %>%
      filter(provinsi %in% input$input_provinsi3) %>%
      filter(daerah %in% input$input_daerah3) %>%
      filter(jenis %in% "TOTAL") %>%
      filter(periode %in% "MARET") %>%
      filter(tahun %in% 2022) %>%
      mutate(gk_ruta = gk * input$input_tanggungan)
    
    if(case_upah2$upah_bulan > case_peng2$peng_ruta){
      status_sejahtera <- "SEJAHTERA"
    } else {
      if(case_upah2$upah_bulan > case_gk2$gk_ruta){
        status_sejahtera <- "KURANG SEJAHTERA"
      } else {
        status_sejahtera <- "TIDAK SEJAHTERA"
      }
    }
    
    if(status_sejahtera == "SEJAHTERA"){
      valueBox(
        value =  status_sejahtera,
        icon = icon("city"),
        color = "green",
        width = 4,
        subtitle = div(icon("question-circle"), "STATUS KESEJAHTERAAN PEKERJA")
      )
    } else {
      if(status_sejahtera == "KURANG SEJAHTERA"){
        valueBox(
          value =  status_sejahtera,
          icon = icon("house"),
          color = "orange",
          width = 4,
          subtitle = div(icon("question-circle"), "STATUS KESEJAHTERAAN PEKERJA")
        )
      } else {
        valueBox(
          value =  status_sejahtera,
          icon = icon("house-flood-water"),
          color = "maroon",
          width = 4,
          subtitle = div(icon("question-circle"), "STATUS KESEJAHTERAAN PEKERJA")
        )
      }
    }
    
    
    
    
  })

  
  # VALUE BOX PERKIRAAN TABUNGAN HALAMAN 3
  
  output$info_2c <- renderValueBox({
    #UPAH PEKERJA
    case_upah2 <- upah.df %>%
      filter(provinsi %in% input$input_provinsi3) %>%
      filter(tahun %in% 2022) %>%
      mutate(upah_bulan = upah * input$input_jamkerja * 4)
    
    #RATA2 PENGELUARAN
    case_peng2 <- peng.df %>%
      filter(provinsi %in% input$input_provinsi3) %>%
      filter(daerah %in% input$input_daerah3) %>%
      filter(jenis %in% "TOTAL") %>%
      filter(tahun %in% 2022) %>%
      mutate(peng_ruta = peng * input$input_tanggungan)
    
    
    tabungan <- case_upah2$upah_bulan - case_peng2$peng_ruta
    
    tabungan_text <- glue("Rp",format(abs(tabungan),
                                 scientific = FALSE,
                                 big.mark = ".", decimal.mark = ",",
                                 ))
    
    if(tabungan > 0){
      valueBox(
        value = tabungan_text,
        width = 4,
        icon = icon("piggy-bank"),
        color = "green",
        subtitle = div(icon("question-circle"),"PERKIRAAN TABUNGAN PEKERJA")
      )
    } else {
      valueBox(
        value = paste("-",tabungan_text),
        width = 4,
        icon = icon("person-drowning"),
        color = "maroon",
        subtitle = div(icon("question-circle"),"PERKIRAAN HUTANG PEKERJA")
      )
    }
    
    
    
    
  })
  
  
  #LEGENDA HALAMAN 3
  
  output$legend_img2 <- renderUI({
    
    renderImage({list(src = "assets/legend2.png",
                      width = "100%",
                      id = "imghover")},
                deleteFile = F)
    #style ="hover img{transform: scale(1.5);}")
    
  })
  
  
  #EDIT UPAH PEKERJA HALAMAN 3
  
  output$info_1d <- renderText({
    
    #RATA2 PENGELUARAN
    case_peng2 <- peng.df %>%
      filter(provinsi %in% input$input_provinsi3) %>%
      filter(daerah %in% input$input_daerah3) %>%
      filter(jenis %in% "TOTAL") %>%
      filter(tahun %in% 2022) %>%
      mutate(peng_ruta = peng * input$input_tanggungan)
    
    #GARIS KEMISKINAN
    case_gk2 <- gk.df %>%
      filter(provinsi %in% input$input_provinsi3) %>%
      filter(daerah %in% input$input_daerah3) %>%
      filter(jenis %in% "TOTAL") %>%
      filter(periode %in% "MARET") %>%
      filter(tahun %in% 2022) %>%
      mutate(gk_ruta = gk * input$input_tanggungan)
    
    if(input$upah_sendiri > case_peng2$peng_ruta){
      status_sejahtera <- "SEJAHTERA"
    } else {
      if(input$upah_sendiri > case_gk2$gk_ruta){
        status_sejahtera <- "KURANG SEJAHTERA"
      } else {
        status_sejahtera <- "TIDAK SEJAHTERA"
      }
    }
    
    status_sejahtera
  })
  
  output$info_2d <- renderText({
    
    #RATA2 PENGELUARAN
    case_peng2 <- peng.df %>%
      filter(provinsi %in% input$input_provinsi3) %>%
      filter(daerah %in% input$input_daerah3) %>%
      filter(jenis %in% "TOTAL") %>%
      filter(tahun %in% 2022) %>%
      mutate(peng_ruta = peng * input$input_tanggungan)
    
    
    tabungan <- input$upah_sendiri - case_peng2$peng_ruta
    
    glue("Rp",format(tabungan,
                     scientific = FALSE,
                     big.mark = ".", decimal.mark = ",",
    ))
  })
  
  
  #HEADING HALAMAN 3
  output$heading_2 <- renderText({
    glue("{input$input_data3} vs Pengeluaran dan Garis Kemiskinan Rumah Tangga")
  })
  
  
  
  
  
  
  #--------------------------------HALAMAN 2
  
  #HEADING HALAMAN 2
  output$heading_1 <- renderText({
    glue("{input$input_data} vs Pengeluaran dan Garis Kemiskinan")
  })
  
  
  #SCATTER PLOT
  
  output$scatter_upah1 <- renderPlotly({
    
    if(input$input_data == "Upah Pekerja Per Jam"){
      data1 <- upah.df %>%
        filter(tahun %in% input$input_tahun2)
    } else {
      data1 <- ump.df %>%
        filter(tahun %in% input$input_tahun2)
    }
    
    
    data2 <- peng.df %>%
      filter(tahun %in% input$input_tahun2) %>%
      filter(daerah %in% input$input_daerah2) %>%
      filter(jenis %in% input$input_jenis2)
    
    data_a <- data1 %>%
      left_join(data2, by="provinsi")
    
    data_a <- data_a[,c(1,3,7)]
    colnames(data_a) <- c("provinsi","upah","peng")
    
    data_a <- data_a %>%
      mutate(quadrant = case_when(data_a$upah <= median(data_a$upah, na.rm = TRUE) & data_a$peng >= median(data_a$peng, na.rm = TRUE) ~ "Q2",
                                  data_a$upah >= median(data_a$upah, na.rm = TRUE) & data_a$peng <= median(data_a$peng, na.rm = TRUE) ~ "Q4",
                                  TRUE ~ "MID")) %>%
      mutate(text = glue("<b>Provinsi</b>: {provinsi}
                         <b>{input$input_data}</b>: Rp{upah}
                         <b>Pengeluaran Per Kapita</b>: Rp{peng}"))
    
    scatterplot_upah1 <- data_a %>%
      ggplot(aes(x = upah,
                 y = peng,
                 color = quadrant,
                 text = text)) +
      geom_vline(xintercept = median(data_a$upah, na.rm=TRUE))+
      geom_hline(yintercept = median(data_a$peng, na.rm=TRUE))+
      geom_point(aes(size = upah))+
      geom_text(data=data_a[data_a$provinsi %in% input$input_provinsi2,],
                aes(x= upah-0.05*upah,
                    y= peng-0.05*peng,
                    label = glue("<b>{provinsi}</b>")))+
      scale_color_manual(values = c("orange","maroon","#008080"))+
      labs(x = glue("{input$input_data} (rupiah)"),
           y = "Rata-Rata Pengeluaran Per Kapita (rupiah)",
           caption = "Sumber: Badan Pusat Statistik (BPS)") +
      theme_minimal()+
      theme(legend.position = "none",
            text = element_text(size = 8))
    
    
    ggplotly(p = scatterplot_upah1, tooltip = "text", height = 300, width = 450)
    
  })
  
  output$scatter_upah2 <- renderPlotly({
    
    if(input$input_data == "Upah Pekerja Per Jam"){
      data3 <- upah.df %>%
        filter(tahun %in% input$input_tahun2)
    } else {
      data3 <- ump.df %>%
        filter(tahun %in% input$input_tahun2)
    }
    
    data4 <- gk.df %>%
      filter(tahun %in% input$input_tahun2) %>%
      filter(daerah %in% input$input_daerah2) %>%
      filter(jenis %in% input$input_jenis2) %>%
      filter(periode %in% "MARET")
    
    data_b <- data3 %>%
      left_join(data4, by="provinsi")
    
    data_b <- data_b[,c(1,3,8)]
    colnames(data_b) <- c("provinsi","upah","gk")
    
    data_b <- data_b %>%
      mutate(quadrant = case_when(data_b$upah <= median(data_b$upah, na.rm = TRUE) & data_b$gk >= median(data_b$gk, na.rm = TRUE) ~ "Q2",
                                  data_b$upah >= median(data_b$upah, na.rm = TRUE) & data_b$gk <= median(data_b$gk, na.rm = TRUE) ~ "Q4",
                                  TRUE ~ "MID")) %>%
      mutate(text = glue("<b>Provinsi</b>: {provinsi}
                         <b>{input$input_data}</b>: Rp{upah}
                         <b>Garis Kemiskinan</b>: Rp{gk}"))
    
    scatterplot_upah2 <- data_b %>%
      ggplot(aes(x = upah,
                 y = gk,
                 color = quadrant,
                 text = text)) +
      geom_vline(xintercept = median(data_b$upah, na.rm = TRUE))+
      geom_hline(yintercept = median(data_b$gk, na.rm = TRUE))+
      geom_point(aes(size = upah))+
      geom_text(data=data_b[data_b$provinsi %in% input$input_provinsi2,],
                aes(x= upah-0.05*upah,
                    y= gk-0.05*gk,
                    label = glue("<b>{provinsi}</b>")))+
      scale_color_manual(values = c("orange","maroon","#008080"))+
      labs(x = glue("{input$input_data} (rupiah)"),
           y = "Garis Kemiskinan (rupiah)",
           caption = "Sumber: Badan Pusat Statistik (BPS)") +
      theme_minimal()+
      theme(legend.position = "none",
            text = element_text(size = 8))
    
    ggplotly(p = scatterplot_upah2, tooltip = "text", height = 300, width = 450)
    
  })

  
  #------------------------------HALAMAN 4
  
  #HEADING HALAMAN 4
  output$heading_5 <- renderText({
    glue("{input$input_data5} vs Pengeluaran dan Garis Kemiskinan Rumah Tangga")
  })
  
  
  #SCATTER PLOT PERBANDINGAN ANTAR PROVINSI HALAMAN 4
  
  output$scatter_upah3 <- renderPlotly({
    
    if(input$input_data5 == "Upah Pekerja Per Jam"){
      data1 <- upah.df %>%
        filter(tahun %in% input$input_tahun5) %>%
        mutate(upah = upah * input$input_jamkerja5 * 4)
    } else {
      data1 <- ump.df %>%
        filter(tahun %in% input$input_tahun5) %>%
        mutate(upah = ump)
    }
    
    
    data2 <- peng.df %>%
      filter(tahun %in% input$input_tahun5) %>%
      filter(daerah %in% input$input_daerah5) %>%
      filter(jenis %in% "TOTAL") %>%
      mutate(peng = peng * input$input_tanggungan5)
    
    data3 <- gk.df %>%
      filter(tahun %in% input$input_tahun5) %>%
      filter(daerah %in% input$input_daerah5) %>%
      filter(jenis %in% "TOTAL") %>%
      filter(periode %in% "MARET") %>%
      mutate(gk = gk * input$input_tanggungan5)
    
    data_a <- data1 %>%
      left_join(data2, by="provinsi") %>%
      left_join(data3, by="provinsi")
    
    data_a <- data_a[,c("provinsi","upah","peng","gk")]
    #colnames(data_a) <- c("provinsi","upah","peng")
    
    data_a <- data_a %>%
      
      mutate(katg = case_when(upah <= gk & upah < peng ~ "TIDAK SEJAHTERA",
                              upah >= peng & upah > gk ~ "SEJAHTERA",
                              upah < peng & upah > gk ~ "KURANG SEJAHTERA")) %>%
      mutate(katg = as.factor(katg)) %>%
      
      filter(provinsi %in% input$input_provinsi5)
    
    data_a <- data_a %>%
      
      mutate(text1 = glue("<b>Provinsi</b>: {provinsi}
                         <b>{input$input_data5}</b>: Rp{upah}
                         <b>Pengeluaran Ruta</b>: Rp{peng}
                         <b>Status</b>: {katg}")) %>%
      
      mutate(text2 = glue("<b>Provinsi</b>: {provinsi}
                         <b>{input$input_data5}</b>: Rp{upah}
                         <b>Garis Kemiskinan Ruta</b>: Rp{gk}
                         <b>Status</b>: {katg}"))
    #polys <- ggplot()
    
    #img <- png::readPNG("assets/bg.png")
    
    scatterplot_upah3 <- data_a %>%
      ggplot(aes(x = upah)) +
      #background_image(img) +
      #geom_area(aes(x = 1000000,
      #              y = 1000000))+
      #geom_ribbon(aes(ymin=0,ymax=1500000),
      #            fill="#A3E4D7")+
      #           color = colorFactor(c("#148F77","#D68910","#B03A2E"), factor(katg)),
      #           fill = colorFactor(c("#148F77","#D68910","#B03A2E"), factor(katg)))) +
      
      #geom_ribbon(aes(x = c(0,max(data_a$upah)+500000), ymin=0, ymax=max(data_a$peng)+500000), fill = "#A3E4D7")+
      #geom_ribbon(aes(x = c(0,max(data_a$upah)+500000), ymin=c(0,max(data_a$upah)+500000), ymax=max(data_a$peng)+500000), fill = "#F1948A")+
      geom_abline(intercept=0,
                  slope = 1)+
      #geom_polygon(aes(x = c(0, Inf, 0), y = c(0, Inf, Inf), fill = "Sejahera"))+
      #geom_polygon(aes(x = c(0, Inf, Inf), y = c(0, Inf, 0), fill = "Kurang Sejahtera"))+
      #scale_fill_manual(values = c("#A3E4D7","#F1948A"))+
      
      #geom_polygon(aes(x=x,y=y),data=data.frame(x=c(0,0,500000),y=c(0,500000,500000)),fill="#A3E4D7")+
      geom_segment(aes(x = upah,
                       xend = upah,
                       y = gk,
                       yend = peng),
                   color = "black")+
      geom_point(aes(x = upah,
                     y = peng,
                     color = katg,
                     fill = katg,
                     text = text1),
                 size = 5,
                 alpha = 0.7,
                 shape = 21,
                 stroke = 2)+

      geom_point(aes(x = upah,
                     y = gk,
                     color = katg,
                     fill = katg,
                     text = text2),
                 size = 5,
                 alpha = 0.7,
                 shape = 21,
                 stroke = 2)+
      geom_text(data=data_a[data_a$provinsi %in% input$input_provinsi5,],
                aes(x= upah-0.05*upah,
                    y= peng-0.05*peng,
                    color = katg,
                    label = glue("<b>{provinsi}</b>")))+
      scale_color_manual(values = c("SEJAHTERA" = "#148F77",
                                    "KURANG SEJAHTERA" = "#D68910",
                                    "TIDAK SEJAHTERA" = "#B03A2E"))+
      scale_fill_manual(values = c("SEJAHTERA" = "#48C9B0",
                                   "KURANG SEJAHTERA" = "#F5B041",
                                   "TIDAK SEJAHTERA" = "#EC7063"))+
      labs(x = glue("{input$input_data} (rupiah)"),
           y = "Pengeluaran/GK Rumah Tangga (rupiah)",
           caption = "Sumber: Badan Pusat Statistik (BPS)") +
      theme_minimal()+
      theme(legend.position = "none",
            text = element_text(size = 8))+
      xlim(0,max(data_a$upah)+500000)+
      ylim(0,max(data_a$upah)+500000)
    
    
    ggplotly(p = scatterplot_upah3, tooltip = "text", height = 300)
    
  })
  
  # REKOMENDASI PROVINSI HALAMAN 4
  output$rekomendasi = renderValueBox({
    
    if(input$input_data5 == "Upah Pekerja Per Jam"){
      data1 <- upah.df %>%
        filter(tahun %in% input$input_tahun5) %>%
        mutate(upah = upah * input$input_jamkerja5 * 4)
    } else {
      data1 <- ump.df %>%
        filter(tahun %in% input$input_tahun5) %>%
        mutate(upah = ump)
    }
    
    
    data2 <- peng.df %>%
      filter(tahun %in% input$input_tahun5) %>%
      filter(daerah %in% input$input_daerah5) %>%
      filter(jenis %in% "TOTAL") %>%
      mutate(peng = peng * input$input_tanggungan5)
    
    data3 <- gk.df %>%
      filter(tahun %in% input$input_tahun5) %>%
      filter(daerah %in% input$input_daerah5) %>%
      filter(jenis %in% "TOTAL") %>%
      filter(periode %in% "MARET") %>%
      mutate(gk = gk * input$input_tanggungan5)
    
    data_a <- data1 %>%
      left_join(data2, by="provinsi") %>%
      left_join(data3, by="provinsi")
    
    data_a <- data_a[,c("provinsi","upah","peng","gk")]
    
    data_a <- data_a %>%
      filter(provinsi %in% input$input_provinsi5) %>%
      mutate(tabungan = upah - peng) %>%
      arrange(-tabungan) %>%
      head(1)
    
    valueBox(
      value = data_a$provinsi,
      subtitle = "REKOMENDASI PILIHAN PROVINSI",
      icon = icon("globe")
    )
    
  })
  
  #STATUS KESEJAHTERAAN PROVINSI PILIHAN HALAMAN 4
  
  output$reko_status = renderValueBox({
    
    if(input$input_data5 == "Upah Pekerja Per Jam"){
      data1 <- upah.df %>%
        filter(tahun %in% input$input_tahun5) %>%
        mutate(upah = upah * input$input_jamkerja5 * 4)
    } else {
      data1 <- ump.df %>%
        filter(tahun %in% input$input_tahun5) %>%
        mutate(upah = ump)
    }
    
    
    data2 <- peng.df %>%
      filter(tahun %in% input$input_tahun5) %>%
      filter(daerah %in% input$input_daerah5) %>%
      filter(jenis %in% "TOTAL") %>%
      mutate(peng = peng * input$input_tanggungan5)
    
    data3 <- gk.df %>%
      filter(tahun %in% input$input_tahun5) %>%
      filter(daerah %in% input$input_daerah5) %>%
      filter(jenis %in% "TOTAL") %>%
      filter(periode %in% "MARET") %>%
      mutate(gk = gk * input$input_tanggungan5)
    
    data_a <- data1 %>%
      left_join(data2, by="provinsi") %>%
      left_join(data3, by="provinsi")
    
    data_a <- data_a[,c("provinsi","upah","peng","gk")]
    
    data_a <- data_a %>%
      filter(provinsi %in% input$input_provinsi5) %>%
      mutate(tabungan = upah - peng) %>%
      arrange(-tabungan) %>%
      head(1) %>%
      mutate(katg = case_when(upah <= gk & upah < peng ~ "TIDAK SEJAHTERA",
                              upah >= peng & upah > gk ~ "SEJAHTERA",
                              upah < peng & upah > gk ~ "KURANG SEJAHTERA"))
    
    status_sejahtera = data_a$katg
    
    if(status_sejahtera == "SEJAHTERA"){
      valueBox(
        value =  status_sejahtera,
        icon = icon("city"),
        color = "green",
        width = 12,
        subtitle = div(icon("question-circle"), "STATUS KESEJAHTERAAN PEKERJA")
      )
    } else {
      if(status_sejahtera == "KURANG SEJAHTERA"){
        valueBox(
          value =  status_sejahtera,
          icon = icon("house"),
          color = "orange",
          width = 12,
          subtitle = div(icon("question-circle"), "STATUS KESEJAHTERAAN PEKERJA")
        )
      } else {
        valueBox(
          value =  status_sejahtera,
          icon = icon("house-flood-water"),
          color = "maroon",
          width = 12,
          subtitle = div(icon("question-circle"), "STATUS KESEJAHTERAAN PEKERJA")
        )
      }
    }
    
  })
  
  #PERKIRAAN TABUNGAN PROVINSI PILIHAN HALAMAN 4
  
  output$reko_tabungan = renderValueBox({
    
    if(input$input_data5 == "Upah Pekerja Per Jam"){
      data1 <- upah.df %>%
        filter(tahun %in% input$input_tahun5) %>%
        mutate(upah = upah * input$input_jamkerja5 * 4)
    } else {
      data1 <- ump.df %>%
        filter(tahun %in% input$input_tahun5) %>%
        mutate(upah = ump)
    }
    
    
    data2 <- peng.df %>%
      filter(tahun %in% input$input_tahun5) %>%
      filter(daerah %in% input$input_daerah5) %>%
      filter(jenis %in% "TOTAL") %>%
      mutate(peng = peng * input$input_tanggungan5)
    
    data3 <- gk.df %>%
      filter(tahun %in% input$input_tahun5) %>%
      filter(daerah %in% input$input_daerah5) %>%
      filter(jenis %in% "TOTAL") %>%
      filter(periode %in% "MARET") %>%
      mutate(gk = gk * input$input_tanggungan5)
    
    data_a <- data1 %>%
      left_join(data2, by="provinsi") %>%
      left_join(data3, by="provinsi")
    
    data_a <- data_a[,c("provinsi","upah","peng","gk")]
    
    data_a <- data_a %>%
      filter(provinsi %in% input$input_provinsi5) %>%
      mutate(tabungan = upah - peng) %>%
      arrange(-tabungan) %>%
      head(1) 
    
    tabungan = data_a$tabungan
    
    tabungan_text <- glue("Rp",format(abs(tabungan),
                                      scientific = FALSE,
                                      big.mark = ".", decimal.mark = ",",
    ))
    
    if(tabungan > 0){
      valueBox(
        value = tabungan_text,
        icon = icon("piggy-bank"),
        color = "green",
        subtitle = div(icon("question-circle"),"PERKIRAAN TABUNGAN PEKERJA")
      )
    } else {
      valueBox(
        value = paste("-",tabungan_text),
        icon = icon("person-drowning"),
        color = "maroon",
        subtitle = div(icon("question-circle"),"PERKIRAAN HUTANG PEKERJA")
      )
    }
    
  })
  
  #LEGENDA HALAMAN 4
  
  output$legend_img3 <- renderUI({
    
    renderImage({list(src = "assets/legend3.png",
                      width = "100%",
                      id = "imghover2")},
                deleteFile = F)
    #style ="hover img{transform: scale(1.5);}")
    
  })
  
  #-----------------------------HALAMAN 5
  
  # renderDataTable() untuk menyiapkan render dataframe pada dashboard
  
  ## OUTPUT TABEL 1
  datasetInput1 <- reactive({
    switch(input$dataset1,
           "Long Format" = upah.df,
           "Wide Format" = upah)
  })
  
  
  output$downloadData1 <- downloadHandler(
    filename = function() {
      paste("Upah Pekerja Per Jam (",input$dataset1, ").csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput1(), file, row.names = FALSE)
    }
  )
  
   output$table_1 <- renderDataTable({
     
     datatable(
       data = upah.df,
       colnames = c("PROVINSI","TAHUN","RATA-RATA UPAH PEKERJA PER JAM"),
       filter = "top",
       options = list(
         scrollx = TRUE,
         pageLength = 10
       )
     )
   })
   
   ## OUTPUT TABEL 2
   datasetInput2 <- reactive({
     switch(input$dataset2,
            "Long Format" = ump.df,
            "Wide Format" = ump)
   })
   
   
   output$downloadData2 <- downloadHandler(
     filename = function() {
       paste("Upah Minimum Provinsi (",input$dataset2, ").csv", sep = "")
     },
     content = function(file) {
       write.csv(datasetInput2(), file, row.names = FALSE)
     }
   )
   
   output$table_2 <- renderDataTable({
     datatable(
       data = ump.df,
       colnames = c("PROVINSI","TAHUN","UPAH MINIMUM PROVINSI"),
       filter = "top",
       options = list(
         scrollx = TRUE,
         pageLength = 10
       )
     )
   })
   
   ## OUTPUT TABEL 3
   datasetInput3 <- reactive({
     switch(input$dataset3,
            "Long Format" = peng.df,
            "Wide Format" = peng)
   })
   
   
   output$downloadData3 <- downloadHandler(
     filename = function() {
       paste("Pengeluaran Per Kapita (",input$dataset3, ").csv", sep = "")
     },
     content = function(file) {
       write.csv(datasetInput3(), file, row.names = FALSE)
     }
   )
   
   output$table_3 <- renderDataTable({
     datatable(
       data = peng.df,
       colnames = c("PROVINSI","DAERAH","JENIS PENGELUARAN","TAHUN","RATA-RATA PENGELUARAN PER KAPITA"),
       filter = "top",
       options = list(
         scrollx = TRUE,
         pageLength = 10
       )
     )
   })
   
   ##OUTPUT TABEL 4
   datasetInput4 <- reactive({
     switch(input$dataset4,
            "Long Format" = ump.df,
            "Wide Format" = ump)
   })
   
   
   output$downloadData4 <- downloadHandler(
     filename = function() {
       paste("Garis Kemiskinan (",input$dataset4, ").csv", sep = "")
     },
     content = function(file) {
       write.csv(datasetInput4(), file, row.names = FALSE)
     }
   )
   
   output$table_4 <- renderDataTable({
     datatable(
       data = gk.df,
       colnames = c("PROVINSI","JENIS PENGELUARAN","DAERAH","TAHUN","PERIODE","GARIS KEMISKINAN PER KAPITA"),
       filter = "top",
       options = list(
         scrollx = TRUE,
         pageLength = 10
       )
     )
   })

})