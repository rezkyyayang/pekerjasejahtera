# Fungsi dashboardPage() diperuntuhkan untuk membuat ketiga bagian pada Shiny

fluidPage(
  
tags$head(
  tags$link(rel="icon", 
            href="favicon-16x16.png",
            type="image/png",
            sizes="16x16"),
  tags$link(rel="icon", 
            href="favicon-32x32.png",
            type="image/png",
            sizes="32x32"),
  tags$link(rel="apple-touch-icon", 
            href="apple-touch-icon.png",
            type="image/png",
            sizes="180x180"),
  tags$link(rel="manifest",
            href="www/site.webmanifest"),
  tags$style("html{zoom:70%;}")
  #tags$meta(name="viewport", content="width=device-width, initial-scale=1.0")
  ),

dashboardPage(skin = "green",
              
              
              # Fungsi dashboardHeader() adalah bagian untuk membuat header
              dashboardHeader(title = "Pekerja Sejahtera"),
              
              # Fungsi dashboardSidebar() adalah bagian untuk membuat sidebar
              dashboardSidebar(
                
                # sidebarMenu() untuk membuat dan mengatur menu
                sidebarMenu(
                  
                  # menuItem untuk menambahkan sebuah tab menu pada sidebar
                  menuItem(
                    # text untuk memeberikan tampilan nama pada tab
                    text = "Gambaran Umum",
                    # tabName untuk memberikan identitas yang mewakili tab tersebut
                    tabName = "menu_1",
                    icon = icon("house")
                  ),
                  
                  menuItem(
                    text = "Kesejahteraan Pekerja",
                    tabName = "menu_2",
                    icon = icon("person-walking-luggage")
                  ),
                  
                  menuItem(
                    text = "Perbandingan Tahun",
                    tabName = "menu_3",
                    icon = icon("timeline")
                  ),
                  
                  menuItem(
                    text = "Perbandingan Provinsi",
                    tabName = "menu_4",
                    icon = icon("globe")
                  ),
                  
                  menuItem(
                    text = "Datasets",
                    tabName = "menu_5",
                    icon = icon("table"),
                    
                    menuSubItem(
                      text = "Upah Pekerja Per Jam",
                      tabName = "submenu_1",
                      icon = icon("database")
                    ),
                    
                    menuSubItem(
                      text = "Upah Minimum Provinsi",
                      tabName = "submenu_2",
                      icon = icon("database")
                    ),
                    
                    menuSubItem(
                      text = "Pengeluaran Per Kapita",
                      tabName = "submenu_3",
                      icon = icon("database")
                    ),
                    
                    menuSubItem(
                      text = "Garis Kemiskinan",
                      tabName = "submenu_4",
                      icon = icon("database")
                    )
                    
                  ),
                  
                  menuItem(
                    text = "Source Code",
                    icon = icon("code"),
                    href = "https://github.com/rezkyyayang/"
                  )
                  

                )
              ),
              
              # Fungsi dashboardBody() adalah bagian untuk membuat isi body
              dashboardBody(
                
                setSliderColor(rep("Teal",3),
                               sliderId = 1:3),
                
                # tabItems() digunakan untuk mengumpulkan isi body dari setiap tab menu
                tabItems(
                  
                  # ----- Menu 1: GAMBARAN UMUM
                  tabItem(
                    tabName = "menu_1",
                    fluidPage(
                      h2(tags$b("Dashboard Pekerja Sejahtera")),
                      br(),
                      div(style = "text-align:justify;font-size:18px", 
                          p("Sebagai seseorang yang baru saja memasuki dunia kerja, mungkin kita semua akan mengalami kebingungan 
                            dimulai dari di daerah mana kita akan bekerja, berapa upah yang akan didapatkan, serta apakah pendapatan 
                            tersebut akan mencukupi untuk kebutuhan sehari-hari.", 
                            "Melalui beberapa data yang bersumber dari Badan Pusat Statistik (BPS), dashboard ini mencoba membantu 
                            pekerja dalam memilih tempat bekerja berdasarkan beberapa variabel seperti pendapatan dan pengeluaran.",
                            "Beberapa hal tersebut menjadikan Dashboard Pekerja Sejahtera ini sangat penting dan relevan sebab 
                            kondisi persaingan dunia kerja yang sangat ketat sehingga bagi pekerja tentu dibutuhkan perencanaan 
                            yang matang termasuk memikirkan kesejahteraannya berdasarkan pilihan-pilihan yang telah dibuat."),
                          br()
                      )
                      
                    ),
                    
                    fluidRow(
                      
                      # infoBox() adalah fungsi untuk membuat kotak berisi nilai
                      infoBox(
                        # parameter title memberi judul pada infoBox
                        title = "Rata-Rata Upah Per Jam",
                        # parameter value memasukkan nilai pada infoBox
                        value = textOutput(outputId = "info_1"),
                        # icon mengatur simbol yang ditampilkan pada infoBox
                        icon = icon("clock"),
                        color = "aqua",
                        width = 3,
                        subtitle = tags$a(icon("question-circle", id="q_upah"))
                      ),
                      
                      infoBox(
                        title = "Upah Minimum Provinsi",
                        value = textOutput(outputId = "info_2"),
                        icon = icon("earth-asia"),
                        color = "aqua",
                        width = 3,
                        subtitle = tags$a(icon("question-circle", id="q_ump"))
                      ),
                      
                      infoBox(
                        # parameter title memberi judul pada infoBox
                        title = "Pengeluaran Per Kapita",
                        # parameter value memasukkan nilai pada infoBox
                        value = textOutput(outputId = "info_3"),
                        # icon mengatur simbol yang ditampilkan pada infoBox
                        icon = icon("coins"),
                        color = "teal",
                        width = 3,
                        subtitle = tags$a(icon("question-circle", id="q_peng"))
                      ),
                      
                      infoBox(
                        title = "Garis Kemiskinan",
                        value = textOutput(outputId = "info_4"),
                        icon = icon("chart-line"),
                        color = "teal",
                        width = 3,
                        subtitle = tags$a(icon("question-circle", id="q_gk"))
                      )
                    ),
                    
                    # untuk input/filter & visualisasi
                    fluidRow( 
                      
                      # box() digunakan untuk membuat kotak kosong yang fungsinya menampung visualisasi & input agar rapi
                      box(
                        
                        #mengatur panjang horizontal box
                        width = 2,
                        #mengatur panjang vertikal box
                        height = 650,
                        
                        img(src = "logo.png",
                            width = "100%"),
                        br(),
                        br(),
                        # PILIH DATA
                        selectInput(
                          # inputId adalah identitas input
                          inputId = "input_data1",
                          # label untuk memberi judul/nama pada input
                          label = "Pilih Visualisasi Data",
                          # choices untuk memberikan opsi yang bisa dipilih pada input
                          choices = c("Upah Pekerja Per Jam", 
                                      "Upah Minimum Provinsi",
                                      "Pengeluaran Per Kapita",
                                      "Garis Kemiskinan"),
                          selected = "Upah Pekerja Per Jam"
                        ),
                        
                        # PILIH TAHUN
                        
                        sliderInput(
                          # inputId adalah identitas input
                          inputId = "input_tahun",
                          # label untuk memberi judul/nama pada input
                          label = "Pilih Tahun",
                          # choices untuk memberikan opsi yang bisa dipilih pada input
                          min = 2002,
                          max = 2022,
                          value = 2022,
                          sep = ""
                          
                        ),
                        
                        # PILIH PROVINSI
                        selectInput(
                          # inputId adalah identitas input
                          inputId = "input_provinsi",
                          # label untuk memberi judul/nama pada input
                          label = "Pilih Provinsi",
                          # choices untuk memberikan opsi yang bisa dipilih pada input
                          choices = unique(upah.df$provinsi),
                          selected = "INDONESIA"
                        ),
                        
                        # PILIH DAERAH
                        selectInput(
                          inputId = "input_daerah",
                          label = "Pilih Daerah",
                          choices = c("PERDESAAN","PERKOTAAN","PERDESAANPERKOTAAN"),
                          #nilai default selected
                          selected = "PERDESAANPERKOTAAN"
                        ),
                        
                        # PILIH JENIS PENGELUARAN
                        selectInput(
                          inputId = "input_jenis",
                          label = "Pilih Jenis Pengeluaran",
                          choices = c("MAKANAN","NONMAKANAN","TOTAL"),
                          #nilai default selected
                          selected = "TOTAL"
                        ),
                        
                        selectInput(
                          inputId = "input_periode",
                          label = "Pilih Periode Survei",
                          choices = c("MARET","SEPTEMBER"),
                          #nilai default selected
                          selected = "MARET"
                        )
                        
                      ),
                      
                      box(
                        width = 7,
                        height = 650,
                        h4(tags$b(textOutput(outputId = "heading_1a"))),
                        br(),
                        leafletOutput(outputId = "mapchart_upah",
                                      height = 525), #DIAGRAM PETA
                        br(),
                        imageOutput("legend_img")
                      ),
                      
                      box(
                        h4(tags$b(textOutput(outputId = "heading_1b"))),
                        width = 3,
                        height = 650,
                        plotlyOutput(outputId = "barchart_upah") #DIAGRAM BATANG
                      )
                      
                    ),
                    
                    
                    
                    
                  ),
                  
                  # ----- Menu 2: KESEJAHTERAAN PEKERJA
                  tabItem(
                    tabName = "menu_2",
                    fluidPage(
                      h2(tags$b("Pekerja Sejahtera: Pendapatan vs Pengeluaran")),
                      br(),
                      div(style = "text-align:justify;font-size:18px", 
                          p("Kondisi persaingan dunia kerja yang semakin ketat menjadikan pekerja membutuhkan perencanaan 
                            yang matang termasuk memikirkan kesejahteraannya berdasarkan pilihan-pilihan yang telah dibuat, 
                            salah satunya pilihan daerah tempat berkerja.",
                            " Kesejahteraan pekerja dapat diperkirakan dengan membandingkan pendapatan dengan pengeluaran.",
                            "Pendapatan pekerja dapat ditaksir melalui data upah per jam pekerja dan Upah Minimum Provinsi. 
                            Sedangkan pengeluaran pekerja dapat ditaksir melalui data pengeluaran per kapita dan garis kemiskinan per kapita."),
                          br()
                      ),
                      
                      
                      
                    ),
                    
                    
                    fluidRow(
                      # box() digunakan untuk membuat kotak kosong yang fungsinya menampung visualisasi & input agar rapi
                      box(
                        #mengatur panjang horizontal box
                        width = 2,
                        #mengatur panjang vertikal box
                        height = 750,
                        
                        img(src = "logo.png",
                            width = "100%"),
                        br(),
                        br(),
                        
                        # PILIH DATA
                        selectInput(
                          # inputId adalah identitas input
                          inputId = "input_data",
                          # label untuk memberi judul/nama pada input
                          label = "Pilih Data",
                          # choices untuk memberikan opsi yang bisa dipilih pada input
                          choices = c("Upah Pekerja Per Jam", "Upah Minimum Provinsi"),
                          selected = "Upah Pekerja Per Jam"
                        ),
                        
                        
                        # PILIH TAHUN
                        sliderInput(
                          # inputId adalah identitas input
                          inputId = "input_tahun2",
                          # label untuk memberi judul/nama pada input
                          label = "Pilih Tahun",
                          # choices untuk memberikan opsi yang bisa dipilih pada input
                          min = 2015,
                          max = 2022,
                          value = 2022,
                          sep = "",
                          animate = animationOptions(interval = 5000, loop = TRUE)
                          
                        ),
                        
                        # PILIH DAERAH
                        selectInput(
                          inputId = "input_provinsi2",
                          label = "Pilih Provinsi",
                          choices = unique(upah.df$provinsi),
                          #nilai default selected
                          selected = c("INDONESIA","DKI JAKARTA","DI YOGYAKARTA"),
                          multiple = TRUE
                        ),
                        
                        # PILIH DAERAH
                        selectInput(
                          inputId = "input_daerah2",
                          label = "Pilih Daerah",
                          choices = c("PERDESAAN","PERKOTAAN","PERDESAANPERKOTAAN"),
                          #nilai default selected
                          selected = "PERDESAANPERKOTAAN"
                        ),
                        
                        # PILIH JENIS PENGELUARAN
                        selectInput(
                          inputId = "input_jenis2",
                          label = "Pilih Jenis Pengeluaran",
                          choices = c("MAKANAN","NONMAKANAN","TOTAL"),
                          #nilai default selected
                          selected = "TOTAL"
                        ),
                        
                      ),
                      
                      textOutput(outputId = "heading_1",
                                 container = h2),
                      
                      
                      br(),
                      
                      box(
                        h4(tags$b("vs PENGELUARAN PER KAPITA")),
                        div(icon("question-circle"),tags$b("PENGELUARAN PER KAPITA:")," Biaya yang dikeluarkan untuk konsumsi per kapita (orang) selama sebulan baik yang berasal dari pembelian, pemberian, maupun produksi sendiri."),
                        width = 5,
                        plotlyOutput(outputId = "scatter_upah1")
                      ),
                      
                      box(
                        h4(tags$b("vs GARIS KEMISKINAN")),
                        div(icon("question-circle"),tags$b("GARIS KEMISKINAN:")," Nilai rupiah pengeluaran minimum yang diperlukan seseorang untuk memenuhi kebutuhan pokok hidupnya selama sebulan baik kebutuhan makanan maupun non makanan."),
                        width = 5,
                        plotlyOutput(outputId = "scatter_upah2")
                      ),
                      
                      box(
                        width = 10,
                      infoBox(
                        title = h4(tags$b("Upah Rendah, Pengeluaran/GK Tinggi")),
                        subtitle = div(style = "text-align:justify;font-size:16px;", "Pekerja di kelompok provinsi ini memiliki pendapatan yang cenderung kurang dari pengeluaran dan kebutuhan hidup sehingga pekerja berpotensi memiliki status",tags$b("KURANG SEJAHTERA.")),
                        icon = icon("caret-down"),
                        color = "maroon",
                        width = 4,
                        fill = TRUE
                        
                      ),
                      
                      infoBox(
                        title = h4(tags$b("Upah Sebanding Pengeluaran/GK")),
                        subtitle = div(style = "text-align:justify;font-size:16px;", "Pekerja di kelompok provinsi ini memiliki pendapatan atau upah yang cenderung sebanding dengan pengeluaran dan kebutuhan hidup sehingga pekerja masih berpotensi memiliki status ",tags$b("SEJAHTERA.")),
                        icon = icon("caret-right"),
                        color = "orange",
                        width = 4,
                        fill = TRUE
                      ),
                      
                      infoBox(
                        title = h4(tags$b("Upah Tinggi, Pengeluaran/GK Rendah")),
                        subtitle = div(style = "text-align:justify;font-size:16px;", "Pekerja di kelompok provinsi ini memiliki pendapatan atau upah yang cenderung lebih tinggi dari pengeluaran dan kebutuhan hidup sehingga pekerja cenderung ",tags$b("SEJAHTERA.")),
                        icon = icon("caret-up"),
                        color = "teal",
                        width = 4,
                        fill = TRUE
                        
                      )
                      
                      
                      
                    )
                    )
                  ),
                  
                  # ----- Menu 3
                  tabItem(
                    tabName = "menu_3",
                    fluidPage(
                      h2(tags$b("Pekerja Sejahtera: Dari Masa Ke Masa ")),
                      br(),
                      div(style = "text-align:justify;font-size:18px", 
                          p("Kondisi persaingan dunia kerja yang semakin ketat menjadikan pekerja membutuhkan perencanaan 
                            yang matang termasuk memikirkan kesejahteraannya berdasarkan pilihan-pilihan yang telah dibuat, 
                            salah satunya pilihan daerah tempat berkerja.",
                            " Kesejahteraan pekerja dapat diperkirakan dengan membandingkan pendapatan dengan pengeluaran.",
                            "Pendapatan pekerja dapat ditaksir melalui data upah per jam pekerja dan Upah Minimum Provinsi. 
                            Sedangkan pengeluaran pekerja dapat ditaksir melalui data pengeluaran per kapita dan garis kemiskinan per kapita."),
                          br()
                      ),
                      
                      
                      
                    ),
                    
                    fluidRow(
                      
                      # infoBox() adalah fungsi untuk membuat kotak berisi nilai
                      infoBox(
                        # parameter title memberi judul pada infoBox
                        title = "Rata-Rata Upah Sebulan 2022",
                        # parameter value memasukkan nilai pada infoBox
                        value = textOutput(outputId = "info_1b"),
                        # icon mengatur simbol yang ditampilkan pada infoBox
                        icon = icon("clock"),
                        color = "aqua",
                        width = 3,
                        subtitle = div(icon("question-circle"),"Upah pekerja per jam dikali jumlah jam kerja dalam sebulan")
                      ),
                      
                      infoBox(
                        title = "Upah Minimum Provinsi 2022",
                        value = textOutput(outputId = "info_2b"),
                        icon = icon("earth-asia"),
                        color = "aqua",
                        width = 3,
                        subtitle = div(icon("question-circle"),"Upah Minimum Provinsi (UMP) dalam sebulan yang ditetapkan pemerintah")
                      ),
                      
                      infoBox(
                        # parameter title memberi judul pada infoBox
                        title = "Pengeluaran Rumah Tangga 2022",
                        # parameter value memasukkan nilai pada infoBox
                        value = textOutput(outputId = "info_3b"),
                        # icon mengatur simbol yang ditampilkan pada infoBox
                        icon = icon("coins"),
                        color = "teal",
                        width = 3,
                        subtitle = div(icon("question-circle"),"Pengeluaran per kapita dikali jumlah orang yang ditanggung pekerja")
                      ),
                      
                      infoBox(
                        title = "Garis Kemiskinan Ruta 2022",
                        value = textOutput(outputId = "info_4b"),
                        icon = icon("chart-line"),
                        color = "teal",
                        width = 3,
                        subtitle = div(icon("question-circle"),"Garis Kemiskinan (GK) per kapita dikali jumlah tanggungan pekerja")
                      )
                    ),
                    
                    fluidRow(
                      # box() digunakan untuk membuat kotak kosong yang fungsinya menampung visualisasi & input agar rapi
                      box(
                        #mengatur panjang horizontal box
                        width = 2,
                        #mengatur panjang vertikal box
                        height = 650,
                        
                        img(src = "logo.png",
                            width = "100%"),
                        br(),
                        br(),
                        # PILIH DATA
                        selectInput(
                          # inputId adalah identitas input
                          inputId = "input_data3",
                          # label untuk memberi judul/nama pada input
                          label = "Pilih Data",
                          # choices untuk memberikan opsi yang bisa dipilih pada input
                          choices = c("Upah Pekerja Sebulan", "Upah Minimum Provinsi"),
                          selected = "Upah Pekerja Sebulan"
                        ),
                        
                        # PILIH TAHUN
                        sliderInput(
                          # inputId adalah identitas input
                          inputId = "input_tahun3",
                          # label untuk memberi judul/nama pada input
                          label = "Pilih Rentang Tahun",
                          # choices untuk memberikan opsi yang bisa dipilih pada input
                          min = 2002,
                          max = 2022,
                          value = c(2015,2022),
                          sep = ""
                          
                        ),
                        
                        # PILIH DATA
                        selectInput(
                          # inputId adalah identitas input
                          inputId = "input_provinsi3",
                          # label untuk memberi judul/nama pada input
                          label = "Pilih Provinsi",
                          # choices untuk memberikan opsi yang bisa dipilih pada input
                          choices = unique(upah.df$provinsi),
                          selected = "INDONESIA"
                        ),
                        
                        # PILIH DAERAH
                        selectInput(
                          inputId = "input_daerah3",
                          label = "Pilih Daerah",
                          choices = c("PERDESAAN","PERKOTAAN","PERDESAANPERKOTAAN"),
                          #nilai default selected
                          selected = "PERDESAANPERKOTAAN"
                        ),
                        
                        # INPUT JAM KERJA SEMINGGU
                        numericInput(
                          inputId = "input_jamkerja",
                          label = "Jam Kerja Seminggu",
                          value = 40,
                          min = 1,
                          max = 100
                        ),
                        
                        # INPUT JUMLAH TANGGUNGAN
                        numericInput(
                          inputId = "input_tanggungan",
                          label = "Jumlah Tanggungan",
                          value = 1,
                          min = 1,
                          max = 10
                        )
                        
                      ),
                      
                      box(
                        textOutput(outputId = "heading_2",
                                   container = h4),
                        div(icon("question-circle"),tags$b("UPAH PEKERJA SEBULAN"),"  didapatkan dari Upah Pekerja Per Jam yang dikalikan dengan jumlah jam kerja selama sebulan."),
                        div(tags$b("PENGELUARAN dan GARIS KEMISKINAN RUMAH TANGGA")," didapatkan dari Pengeluaran/GK Per Kapita dikalikan dengan jumlah orang yang ditanggung pekerja."),
                        width = 8,
                        height = 650,
                        plotlyOutput(outputId = "linechart_gab"),
                        br(),
                        
                        valueBoxOutput(
                          outputId = "info_1c",
                          width = 6
                        ),
                        
                        valueBoxOutput(
                          outputId = "info_2c",
                          width = 6
                        ),
                      ),
                      
                      
                      
                      
                      box(
                        #mengatur panjang horizontal box
                        width = 2,
                        #mengatur panjang vertikal box
                        height = 650,
                        
                        h3("Edit Upah Pekerja"),
                        
                        numericInput(
                          inputId = "upah_sendiri",
                          label = "Pendapatan Saat Ini",
                          min = 0,
                          step = 100000,
                          value = 5000000
                        ),
                        
                        h4("Status: ",tags$b(textOutput(outputId = "info_1d"))),
                        div(tags$a(icon("question-circle", id="q_sejahtera")), "STATUS KESEJAHTERAAN PEKERJA"),
                        h4("Tabungan: ",tags$b(textOutput(outputId = "info_2d"))),
                        div(tags$a(icon("question-circle", id="q_tabungan")), "PERKIRAAN TABUNGAN PEKERJA"),
                        
                        h3(tags$b("Legenda")),
                        #tags$img(src = "assets/legend2.png")
                        fluidPage(
                          tags$style(make_css(list(c('img#imghover'),
                                                   c('transition'),
                                                   c('transform 0.25s ease;')))),
                          tags$style(make_css(list(c('img#imghover:hover'),
                                                   c('transform'),
                                                   c('scale(1.5) translate(-50%, -50%)')))),
                          uiOutput("legend_img2")
                        ),
                        #tags$style('div#image:hover {
                        #            transform: scale(1.5);
                        #            transform-origin: top left;
                        #           }')
                        
                      )
                      
                      
                      
                    )
                  ),
                  
                  # ----- Menu 4
                  # ----- Menu 4: PERBANDINGAN PROVINSI
                  tabItem(
                    tabName = "menu_4",
                    fluidPage(
                      h2(tags$b("Pekerja Sejahtera: Antar Provinsi")),
                      br(),
                      div(style = "text-align:justify;font-size:18px", 
                          p("Kondisi persaingan dunia kerja yang semakin ketat menjadikan pekerja membutuhkan perencanaan 
                            yang matang termasuk memikirkan kesejahteraannya berdasarkan pilihan-pilihan yang telah dibuat, 
                            salah satunya pilihan provinsi tempat berkerja.",
                            " Kesejahteraan pekerja dapat diperkirakan dengan membandingkan pendapatan dengan pengeluaran.",
                            "Pendapatan pekerja dapat ditaksir melalui data upah per jam pekerja dan Upah Minimum Provinsi. 
                            Sedangkan pengeluaran pekerja dapat ditaksir melalui data pengeluaran per kapita dan garis kemiskinan per kapita.",
                            "Visualisasi berikut berusaha membandingkan estimasi status kesejahteraan pekerja antar provinsi yang dipilih"),
                          br()
                      ),
                      
                      
                      
                    ),
                    
                    
                    fluidRow(
                      # box() digunakan untuk membuat kotak kosong yang fungsinya menampung visualisasi & input agar rapi
                      box(
                        #mengatur panjang horizontal box
                        width = 2,
                        #mengatur panjang vertikal box
                        height = 780,
                        
                        # PILIH DATA
                        selectInput(
                          # inputId adalah identitas input
                          inputId = "input_data5",
                          # label untuk memberi judul/nama pada input
                          label = "Pilih Data",
                          # choices untuk memberikan opsi yang bisa dipilih pada input
                          choices = c("Upah Pekerja Per Jam", "Upah Minimum Provinsi"),
                          selected = "Upah Pekerja Per Jam"
                        ),
                        
                        
                        # PILIH TAHUN
                        sliderInput(
                          # inputId adalah identitas input
                          inputId = "input_tahun5",
                          # label untuk memberi judul/nama pada input
                          label = "Pilih Tahun",
                          # choices untuk memberikan opsi yang bisa dipilih pada input
                          min = 2015,
                          max = 2022,
                          value = 2022,
                          sep = "",
                          animate = animationOptions(interval = 5000, loop = TRUE)
                          
                        ),
                        
                        # PILIH DAERAH
                        selectInput(
                          inputId = "input_provinsi5",
                          label = "Pilih Provinsi",
                          choices = unique(upah.df$provinsi),
                          #nilai default selected
                          selected = c("INDONESIA"),
                          multiple = TRUE
                        ),
                        
                        # PILIH DAERAH
                        selectInput(
                          inputId = "input_daerah5",
                          label = "Pilih Daerah",
                          choices = c("PERDESAAN","PERKOTAAN","PERDESAANPERKOTAAN"),
                          #nilai default selected
                          selected = "PERDESAANPERKOTAAN"
                        ),
                        
                        # INPUT JAM KERJA SEMINGGU
                        numericInput(
                          inputId = "input_jamkerja5",
                          label = "Jam Kerja Seminggu",
                          value = 40,
                          min = 1,
                          max = 100
                        ),
                        
                        # INPUT JUMLAH TANGGUNGAN
                        numericInput(
                          inputId = "input_tanggungan5",
                          label = "Jumlah Tanggungan",
                          value = 1,
                          min = 1,
                          max = 10
                        ),
                        
                        h3(tags$b("Legenda")),
                        #tags$img(src = "assets/legend2.png")
                        fluidPage(
                          
                          tags$style(make_css(list(c('img#imghover2'),
                                                   c('transition'),
                                                   c('transform 0.25s ease;')))),
                          tags$style(make_css(list(c('img#imghover2:hover'),
                                                   #c('position relative'),
                                                   #c('z-index 9999'),
                                                   #c('9999'),
                                                   c('transform','position','z-index'),
                                                   c('scale(2) translate(50%, -50%)','relative','9999')))),
                          uiOutput("legend_img3")
                        )
                      ),
                      
                      textOutput(outputId = "heading_5",
                                 container = h2),
                      div(tags$a(icon("question-circle", id="q_peng_gk")), "Pengeluaran dan Garis Kemiskinan Rumah Tangga"),
                      
                      br(),
                      
                      box(
                        width = 7,
                        height = 500,
                        br(),
                        plotlyOutput(outputId = "scatter_upah3")
                      ),
                      
                      box(
                        width = 3,
                        height = 500,
                        h3(tags$b("Rekomendasi Pekerja")),
                        br(),
                        valueBoxOutput(outputId = "rekomendasi",
                                       width = 12),
                        valueBoxOutput(outputId = "reko_status",
                                       width = 12),
                        valueBoxOutput(outputId = "reko_tabungan",
                                       width = 12)
                      ),
                      
                      box(
                        width = 10,
                        infoBox(
                          title = h4(style="font-size=20px;",tags$b("TIDAK SEJAHTERA")),
                          subtitle = div(style = "text-align:justify;font-size=16px;", "Pekerja di kelompok provinsi ini memiliki pendapatan kurang dari pengeluaran rata-rata dan garis kemiskinan rumah tangga"),
                          icon = icon("caret-down"),
                          color = "maroon",
                          width = 4,
                          fill = TRUE
                          
                        ),
                        
                        infoBox(
                          title = h4(style="font-size=20px;",tags$b("KURANG SEJAHTERA")),
                          subtitle = div(style = "text-align:justify;font-size=16px;", "Pekerja di kelompok provinsi ini memiliki pendapatan kurang dari pengeluaran rata-rata namun masih di atas garis kemiskinan rumah tangga."),
                          icon = icon("caret-right"),
                          color = "orange",
                          width = 4,
                          fill = TRUE
                        ),
                        
                        infoBox(
                          title = h4(style="font-size=20px;",tags$b("SEJAHTERA")),
                          subtitle = div(style = "text-align:justify;font-size=16px;", "Pekerja di kelompok provinsi ini memiliki pendapatan lebih tinggi dari pengeluaran rata-rata maupun garis kemiskinan rumah tangga."),
                          icon = icon("caret-up"),
                          color = "teal",
                          width = 4,
                          fill = TRUE
                          
                        )
                        
                        
                        
                      )
                    )
                  ),
                  
                  
                  
                  
                  
                  # ----- Menu 5
                  #DATASET
                  tabItem(
                    tabName = "menu_5"
                  ),
                  
                  tabItem(
                    tabName = "submenu_1",
                    fluidPage(
                      h2(tags$b("Dataset: Upah Pekerja Per Jam ")),
                      br(),
                      div(style = "text-align:justify;font-size:16px", 
                          p("Sumber: Badan Pusat Statistik (BPS)"),
                          br()
                      ),
                      
                      
                      
                    ),
                    
                    fluidRow(
                      # box() digunakan untuk membuat kotak kosong yang fungsinya menampung visualisasi & input agar rapi
                      box(
                        #mengatur panjang horizontal box
                        width = 2,
                        #mengatur panjang vertikal box
                        height = 600,
                        
                        
                        # Input: Choose dataset ----
                        selectInput(inputId = "dataset1", 
                                    label = "Pilih Format Data:",
                                    choices = c("Long Format","Wide Format"),
                                    selected = "Long Format"),
                        
                        # Button
                        downloadButton("downloadData1", "Download")
                        
                      ),
                      box(width = 10,
                          height = 600,
                          dataTableOutput(outputId = "table_1")
                      )
                      ),
                    
                  ),
                  
                  tabItem(
                    tabName = "submenu_2",
                    fluidPage(
                      h2(tags$b("Dataset: Upah Minimum Provinsi ")),
                      br(),
                      div(style = "text-align:justify;font-size:16px", 
                          p("Sumber: Badan Pusat Statistik (BPS)"),
                          br()
                      ),
                      
                      
                      
                    ),
                    
                    fluidRow(
                      # box() digunakan untuk membuat kotak kosong yang fungsinya menampung visualisasi & input agar rapi
                      box(
                        #mengatur panjang horizontal box
                        width = 2,
                        #mengatur panjang vertikal box
                        height = 600,
                        
                        
                        # Input: Choose dataset ----
                        selectInput(inputId = "dataset2", 
                                    label = "Pilih Format Data:",
                                    choices = c("Long Format","Wide Format"),
                                    selected = "Long Format"),
                        
                        # Button
                        downloadButton("downloadData2", "Download")
                        
                      ),
                      box(width = 10,
                          height = 600,
                          dataTableOutput(outputId = "table_2")
                      )
                    )
                  ),
                  
                  tabItem(
                    tabName = "submenu_3",
                    fluidPage(
                      h2(tags$b("Dataset: Pengeluaran Per Kapita ")),
                      br(),
                      div(style = "text-align:justify;font-size:16px", 
                          p("Sumber: Badan Pusat Statistik (BPS)"),
                          br()
                      ),
                      
                      
                      
                    ),
                    
                    fluidRow(
                      # box() digunakan untuk membuat kotak kosong yang fungsinya menampung visualisasi & input agar rapi
                      box(
                        #mengatur panjang horizontal box
                        width = 2,
                        #mengatur panjang vertikal box
                        height = 600,
                        
                        
                        # Input: Choose dataset ----
                        selectInput(inputId = "dataset3", 
                                    label = "Pilih Format Data:",
                                    choices = c("Long Format","Wide Format"),
                                    selected = "Long Format"),
                        
                        # Button
                        downloadButton("downloadData3", "Download")
                        
                      ),
                      box(width = 10,
                          height = 600,
                          dataTableOutput(outputId = "table_3")
                      )
                    )
                  ),
                  
                  tabItem(
                    tabName = "submenu_4",
                    fluidPage(
                      h2(tags$b("Dataset: Garis Kemiskinan ")),
                      br(),
                      div(style = "text-align:justify;font-size:16px", 
                          p("Sumber: Badan Pusat Statistik (BPS)"),
                          br()
                      ),
                      
                      
                      
                    ),
                    
                    fluidRow(
                      # box() digunakan untuk membuat kotak kosong yang fungsinya menampung visualisasi & input agar rapi
                      box(
                        #mengatur panjang horizontal box
                        width = 2,
                        #mengatur panjang vertikal box
                        height = 600,
                        
                        
                        # Input: Choose dataset ----
                        selectInput(inputId = "dataset4", 
                                    label = "Pilih Format Data:",
                                    choices = c("Long Format","Wide Format"),
                                    selected = "Long Format"),
                        
                        # Button
                        downloadButton("downloadData4", "Download")
                        
                      ),
                      box(width = 10,
                          height = 600,
                          dataTableOutput(outputId = "table_4")
                      )
                    )
                 )
               ),
               
               # memberikan keterangan singkat setiap variabel
               bsPopover(id="q_upah",
                         title = "Upah Pekerja Per Jam",
                         content = "Pendapatan atau upah yang diterima pekerja dari hasil pekerjaannya selama seminggu terakhir dibagi dengan jumlah jam kerja.",
                         trigger = "hover",
                         placement = "right",
                         options = list(container = "body")),
               
               bsPopover(id="q_ump",
                         title = "Upah Minimum Provinsi",
                         content = "Standar minimum yang digunakan para pekerja atau pelaku industri untuk memberikan upah kepada para pekerjanya. UMP berlaku di seluruh kabupaten/kota dalam satu provinsi.",
                         trigger = "hover",
                         placement = "right",
                         options = list(container = "body")),
               
               bsPopover(id="q_peng",
                         title = "Rata-Rata Pengeluaran Per Kapita",
                         content = "Biaya yang dikeluarkan untuk konsumsi per kapita (orang) selama sebulan baik yang berasal dari pembelian, pemberian, maupun produksi sendiri.",
                         trigger = "hover",
                         placement = "right",
                         options = list(container = "body")),
               
               bsPopover(id="q_gk",
                         title = "Garis Kemiskinan Per Kapita",
                         content = "Nilai rupiah pengeluaran minimum yang diperlukan seseorang untuk memenuhi kebutuhan pokok hidupnya selama sebulan baik kebutuhan makanan maupun non makanan.",
                         trigger = "hover",
                         placement = "right",
                         options = list(container = "body")),
               
               bsPopover(id="q_sejahtera",
                         title = "Status Kesejahteraan",
                         content = "<b>SEJAHTERA</b> artinya pendapatan berada <u>di atas</u> garis rata-rata pengeluaran ruta,<br> <b>KURANG SEJAHTERA</b> artinya pendapatan berada <u>di antara</u> rata-rata pengeluaran ruta dan garis kemiskinan, sedangkan<br> <b>TIDAK SEJAHTERA</b> artinya pendapatan <u>di bawah</u> garis kemiskinan.",
                         trigger = "hover",
                         placement = "right",
                         options = list(container = "body")),
               
               bsPopover(id="q_tabungan",
                         title = "Perkiraan Tabungan",
                         content = "<b>PERKIRAAN TABUNGAN atau HUTANG PEKERJA</b> didapatkan dari pendapatan dikurangi dengan rata-rata pengeluaran rumah tangga.",
                         trigger = "hover",
                         placement = "right",
                         options = list(container = "body")),
               
               bsPopover(id="q_peng_gk",
                         title = "Pengeluaran/GK Rumah Tangga",
                         content = "<b>PENGELUARAN RUMAH TANGGA</b> dihasilkan dari pengeluaran per kapita dikali jumlah orang yang ditanggung pekerja. Pengeluaran Per Kapita adalah biaya yang dikeluarkan untuk konsumsi per kapita (orang) selama sebulan baik yang berasal dari pembelian, pemberian, maupun produksi sendiri.<br> <b>GARIS KEMISKINAN RUMAH TANGGA</b> dihasilkan dari garis kemiskinan dikali jumlah orang yang ditanggung pekerja. Garis kemiskinan adalah nilai rupiah pengeluaran minimum yang diperlukan seseorang untuk memenuhi kebutuhan pokok hidupnya selama sebulan baik kebutuhan makanan maupun non makanan.",
                         trigger = "hover",
                         placement = "right",
                         options = list(container = "body"))
               
               
             )
          )
              
)