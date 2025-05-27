library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(shinycssloaders)
library(shinyWidgets)
library(shinyjs)
library(fontawesome)
library(dplyr)
library(igraph)

users_db <- reactiveVal(data.frame(
  username = character(),
  password = character(), # Plain password untuk demo (jangan di produksi)
  name = character(),
  age = numeric(),
  gender = character(),
  stringsAsFactors = FALSE
))

evaluate_blood_pressure <- function(systolic, diastolic, family_history, family_count) {
  risk_factor <- ifelse(family_history, 1.5 + (family_count * 0.2), 1)
  
  if (systolic < 120 && diastolic < 80) {
    return(list(
      category = "NORMAL",
      risk = "Rendah",
      color = "green",
      explanation = "Tekanan darah dalam batas normal."
    ))
  } else if (systolic < 130 || diastolic < 85) {
    return(list(
      category = "NORMAL TINGGI",
      risk = ifelse(family_history, "Sedang", "Rendah"),
      color = ifelse(family_history, "yellow", "green"),
      explanation = ifelse(
        family_history,
        paste("Perlu pemantauan (", family_count, "anggota keluarga memiliki hipertensi)."),
        "Perhatikan pola hidup sehat."
      )
    ))
  } else if (systolic < 140 || diastolic < 90) {
    return(list(
      category = "PRA-HIPERTENSI",
      risk = ifelse(family_history, "Tinggi", "Sedang"),
      color = ifelse(family_history, "orange", "yellow"),
      explanation = "Perubahan gaya hidup diperlukan."
    ))
  } else {
    return(list(
      category = "HIPERTENSI",
      risk = "Sangat Tinggi",
      color = "red",
      explanation = "Segera konsultasi ke dokter!"
    ))
  }
}

evaluate_glucose <- function(glucose, age, family_history, family_count) {
  risk_factor <- ifelse(family_history, 1.7 + (family_count * 0.3), 1)
  
  status <- dplyr::case_when(
    glucose < 100 ~ "NORMAL",
    glucose < 126 ~ "PRA-DIABETES",
    TRUE ~ "DIABETES"
  )
  
  explanation <- switch(
    status,
    "NORMAL" = "Gula darah normal.",
    "PRA-DIABETES" = ifelse(
      family_history,
      paste("Risiko tinggi (", family_count, "keluarga memiliki diabetes)."),
      "Perlu pemantauan."
    ),
    "DIABETES" = "Segera konsultasi ke dokter!"
  )
  
  return(list(
    category = status,
    risk = ifelse(family_history & status != "NORMAL", "Tinggi", "Sedang"),
    color = dplyr::case_when(
      status == "NORMAL" ~ "green",
      status == "PRA-DIABETES" ~ "orange",
      TRUE ~ "red"
    ),
    explanation = explanation
  ))
}

calculate_family_risk <- function(hypertension_count, diabetes_count, heart_disease) {
  h_risk <- min(1, hypertension_count * 0.15)
  d_risk <- min(1, diabetes_count * 0.2)
  hd_risk <- ifelse(heart_disease, 0.3, 0)
  total_risk <- min(1, (h_risk + d_risk + hd_risk) * 1.2)
  return(round(total_risk * 100, 1))
}

youtube_videos <- data.frame(
  id = c("VcLbbAbnJ_w", "EsVLl_bEcXw", "vG72o1BpPpM", "3B7toL_s9oE", "hIHJgB18AB8"),
  title = c(
    "Diet Untuk Penderita Hipertensi",
    "The Best Workout For High Blood Pressure",
    "Edukasi Gizi pada Diabetes Mellitus",
    "Senam Aerobik Turunkan Gula Darah",
    "Cara Cerdik Cegah dan Kendalikan Hipertensi"
  ),
  category = c("Edukasi", "Olahraga", "Edukasi", "Olahraga", "Edukasi"),
  duration = c("01:05:00", "17:44", "16:29", "32:37", "02:26")
)

educational_resources <- data.frame(
  title = c(
    "Cara Mencegah Hipertensi Sejak Dini",
    "Cara Mencegah Diabetes Melitus Sejak Dini",
    "Hubungan Diabetes dengan Hipertensi",
    "Hipertensi Penyakit Paling Banyak Diidap Masyarakat",
    "Pengelolaan dan Pencegahan Diabetes Melitus Tipe 2",
    "Diabetes Bisa Sebabkan Hipertensi",
    "Buku Pedomen Hipertensi"
  ),
  type = c("Artikel", "Artikel", "Artikel", "Artikel", "Ebook", "Artikel", "Ebook"),
  link = c(
    "https://www.alodokter.com/10-cara-mencegah-hipertensi-sejak-dini",
    "https://www.siloamhospitals.com/en/informasi-siloam/artikel/cara-mencegah-diabetes",
    "https://www.halodoc.com/artikel/adakah-hubungan-diabetes-dengan-hipertensi-begini-penjelasannya?srsltid=AfmBOooivAJpU-GYbMNM7POvEwuktvXEYfqB_Zfd1YHPAand1MHHE8aJ",
    "https://sehatnegeriku.kemkes.go.id/baca/umum/20190517/5130282/hipertensi-penyakit-paling-banyak-diidap-masyarakat/",
    "https://pbperkeni.or.id/wp-content/uploads/2021/11/22-10-21-Website-Pedoman-Pengelolaan-dan-Pencegahan-DMT2-Ebook.pdf",
    "https://www.halodoc.com/artikel/diabetes-bisa-sebabkan-hipertensi-apa-kaitannya",
    "https://diskes.badungkab.go.id/storage/diskes/file/Buku%20Pedoman%20Hipertensi%202024.pdf"
  ),
  description = c(
    "Cara Mencegah Hipertensi Sejak Dini - Tips mencegah hipertensi secara alami dan efektif",
    "Cara Mencegah Diabetes Melitus Sejak Dini - Panduan pencegahan diabetes dengan gaya hidup sehat",
    "Hubungan Diabetes dengan Hipertensi - Penjelasan keterkaitan diabetes dan hipertensi secara ilmiah",
    "Hipertensi Penyakit Paling Banyak Diidap Masyarakat - Statistik dan dampak hipertensi pada masyarakat Indonesia",
    "Pengelolaan dan Pencegahan Diabetes Melitus Tipe 2 - Buku pedoman pengelolaan diabetes tipe 2 secara praktis",
    "Diabetes Bisa Sebabkan Hipertensi - Hubungan langsung antara diabetes dan hipertensi",
    "Buku Pedomen Hipertensi - Buku pedoman pencegahan dan pengelolaan hipertensi 2024"
  )
)

medical_faqs <- data.frame(
  question = c(
    "Apa perbedaan antara diabetes tipe 1 dan tipe 2?",
    "Berapa tekanan darah yang dianggap normal?",
    "Apakah hipertensi bisa sembuh?",
    "Apa gejala awal diabetes yang perlu diwaspadai?",
    "Bagaimana cara menurunkan tekanan darah tanpa obat?",
    "Apakah penderita diabetes boleh makan buah?",
    "Seberapa sering saya harus memeriksa tekanan darah?"
  ),
  answer = c(
    "Diabetes tipe 1 disebabkan oleh kerusakan sel pankreas yang memproduksi insulin, sementara tipe 2 disebabkan oleh resistensi insulin. Tipe 1 biasanya muncul sejak kecil, tipe 2 lebih terkait dengan gaya hidup.",
    "Tekanan darah normal adalah di bawah 120/80 mmHg. 120-129/<80 dianggap elevated, 130-139/80-89 adalah hipertensi stage 1, dan ≥140/90 adalah hipertensi stage 2.",
    "Hipertensi adalah kondisi kronis yang bisa dikontrol tetapi umumnya tidak bisa disembuhkan sepenuhnya. Dengan pengobatan dan gaya hidup sehat, tekanan darah bisa normal tanpa gejala.",
    "Gejala awal diabetes antara lain sering haus, sering buang air kecil, lemas, penglihatan kabur, dan luka yang lambat sembuh.",
    "Beberapa cara alami menurunkan tekanan darah: diet rendah garam, olahraga teratur, kurangi stres, hindari alkohol dan rokok, serta konsumsi makanan kaya kalium.",
    "Ya, penderita diabetes boleh makan buah tetapi perlu memperhatikan porsi dan indeks glikemik. Buah segar lebih baik daripada jus, dan sebaiknya dikonsumsi sebagai pengganti camilan manis.",
    "Untuk orang sehat, setahun sekali cukup. Jika memiliki hipertensi atau risiko tinggi, sebaiknya 1-2 kali seminggu atau sesuai anjuran dokter."
  ),
  category = c("Diabetes", "Hipertensi", "Hipertensi", "Diabetes", "Hipertensi", "Diabetes", "Hipertensi")
)

hospital_coordinates <- list(
  "RS Cipto Mangunkusumo" = list(lat = -6.1764, lng = 106.8446),
  "RS Harapan Kita" = list(lat = -6.2090, lng = 106.7946),
  "RS Sardjito" = list(lat = -7.7679, lng = 110.3748),
  "RS Bethesda" = list(lat = -7.7825, lng = 110.3839),
  "RS Dr. Soetomo" = list(lat = -7.2892, lng = 112.7348),
  "RS Siloam" = list(lat = -7.2756, lng = 112.7903),
  "RS Hasan Sadikin" = list(lat = -6.9018, lng = 107.6186),
  "RS Sanglah" = list(lat = -8.6705, lng = 115.2126)
)

medical_facilities <- list(
  "Jakarta" = list(
    "RS Cipto Mangunkusumo" = list(
      coordinates = hospital_coordinates[["RS Cipto Mangunkusumo"]],
      doctors = list(
        list(
          name = "Dr. Ahmad Fauzi, Sp.PD",
          specialization = "Penyakit Dalam",
          schedule = "Senin & Kamis, 09:00-15:00",
          contact = "konsultasi@rscm.id"
        ),
        list(
          name = "Dr. Siti Rahayu, Sp.GK",
          specialization = "Gizi Klinik",
          schedule = "Selasa & Jumat, 10:00-16:00",
          contact = "gizi@sardjitohospital.com"
        )
      )
    ),
    "RS Harapan Kita" = list(
      coordinates = hospital_coordinates[["RS Harapan Kita"]],
      doctors = list(
        list(
          name = "Dr. Bambang Wijaya, Sp.JP",
          specialization = "Kardiologi",
          schedule = "Rabu & Sabtu, 08:00-14:00",
          contact = "kardiologi@harapankita.id"
        )
      )
    )
  ),
  "Yogyakarta" = list(
    "RS Sardjito" = list(
      coordinates = hospital_coordinates[["RS Sardjito"]],
      doctors = list(
        list(
          name = "Dr. Maya Indah, Sp.PD-KEMD",
          specialization = "Endokrinologi",
          schedule = "Senin & Rabu, 13:00-17:00",
          contact = "endokrin@dharmais.com"
        )
      )
    ),
    "RS Bethesda" = list(
      coordinates = hospital_coordinates[["RS Bethesda"]],
      doctors = list(
        list(
          name = "Dr. Budi Santoso, Sp.PD",
          specialization = "Penyakit Dalam",
          schedule = "Selasa & Jumat, 08:00-12:00",
          contact = "poli_penyakit_dalam@bethesda.id"
        )
      )
    )
  ),
  "Surabaya" = list(
    "RS Dr. Soetomo" = list(
      coordinates = hospital_coordinates[["RS Dr. Soetomo"]],
      doctors = list(
        list(
          name = "Dr. Rina Wijayanti, Sp.PD-KEMD",
          specialization = "Endokrinologi",
          schedule = "Senin & Kamis, 10:00-16:00",
          contact = "endokrin@soetomo.id"
        )
      )
    ),
    "RS Siloam" = list(
      coordinates = hospital_coordinates[["RS Siloam"]],
      doctors = list(
        list(
          name = "Dr. Agus Setiawan, Sp.JP",
          specialization = "Kardiologi",
          schedule = "Rabu & Sabtu, 09:00-15:00",
          contact = "kardiologi@siloam.id"
        )
      )
    )
  ),
  "Bandung" = list(
    "RS Hasan Sadikin" = list(
      coordinates = hospital_coordinates[["RS Hasan Sadikin"]],
      doctors = list(
        list(
          name = "Dr. Dian Permatasari, Sp.GK",
          specialization = "Gizi Klinik",
          schedule = "Selasa & Jumat, 08:00-14:00",
          contact = "gizi@rshs.id"
        )
      )
    )
  ),
  "Bali" = list(
    "RS Sanglah" = list(
      coordinates = hospital_coordinates[["RS Sanglah"]],
      doctors = list(
        list(
          name = "Dr. Komang Adiarta, Sp.PD",
          specialization = "Penyakit Dalam",
          schedule = "Senin & Kamis, 09:00-15:00",
          contact = "poli_penyakit_dalam@sanglah.id"
        )
      )
    )
  )
)

get_facilities <- function(city) {
  if (city %in% names(medical_facilities)) {
    return(names(medical_facilities[[city]]))
  } else {
    return(NULL)
  }
}

get_doctors <- function(city, facility) {
  if (city %in% names(medical_facilities) && facility %in% names(medical_facilities[[city]])) {
    return(medical_facilities[[city]][[facility]]$doctors)
  } else {
    return(NULL)
  }
}

get_facility_coordinates <- function(city, facility) {
  if (city %in% names(medical_facilities) && facility %in% names(medical_facilities[[city]])) {
    return(medical_facilities[[city]][[facility]]$coordinates)
  } else {
    return(NULL)
  }
}

login_ui <- fluidPage(
  useShinyjs(),
  div(style = "max-width: 400px; margin: 50px auto;",
      h2("Login INYONG HEALTHY TRACK"),
      textInput("login_username", "Username"),
      passwordInput("login_password", "Password"),
      actionButton("login_btn", "Login", class = "btn-primary"),
      br(), br(),
      p("Belum punya akun? Daftar dulu!"),
      actionButton("show_register_btn", "Registrasi", class = "btn-success")
  )
)

register_ui <- fluidPage(
  useShinyjs(),
  div(style = "max-width: 500px; margin: 50px auto;",
      h2("Registrasi Akun Baru"),
      textInput("reg_name", "Nama Lengkap"),
      numericInput("reg_age", "Umur", value = 20, min = 1, max = 120),
      prettyRadioButtons("reg_gender", "Jenis Kelamin",
                         choices = c("Laki-laki" = "male", "Perempuan" = "female"),
                         icon = icon("venus-mars"), status = "primary"),
      textInput("reg_username", "Username"),
      passwordInput("reg_password", "Password"),
      passwordInput("reg_password_confirm", "Konfirmasi Password"),
      actionButton("register_btn", "Daftar", class = "btn-success"),
      br(), br(),
      actionButton("show_login_btn", "Kembali ke Login", class = "btn-link")
  )
)

ui_main <- function() {
  dashboardPage(
    skin = "blue", 
    dashboardHeader(
      title = div(
        style = "display: flex; align-items: center;",
        icon("heart-pulse", class = "fa-2x", style = "color: #FF6B6B; margin-right: 10px;"),
        span("INYONG HEALTHY TRACK", style = "font-family: 'Arial Black'; font-size: 22px; color: white;")
      ),
      titleWidth = 300
    ),
    
    dashboardSidebar(
      width = 300,
      sidebarMenu(
        id = "tabs",
        menuItem("Beranda", tabName = "home", icon = icon("home")),
        menuItem("Input Data", tabName = "input", icon = icon("pen-to-square")),
        menuItem("Hasil Analisis", tabName = "results", icon = icon("chart-simple")),
        menuItem("Riwayat Keluarga", tabName = "family", icon = icon("family")),
        menuItem("Edukasi Kesehatan", tabName = "education", icon = icon("graduation-cap")),
        menuItem("FAQ Medis", tabName = "faq", icon = icon("question-circle")),
        menuItem("Pengaturan", tabName = "settings", icon = icon("gear")),
        div(
          style = "position: absolute; bottom: 20px; left: 20px; right: 20px;",
          actionBttn(
            "submit",
            "ANALISIS SEKARANG",
            style = "gradient",
            color = "danger",
            block = TRUE,
            size = "md"
          )
        )
      )
    ),
    
    dashboardBody(
      id = "dashboard_body",
      useShinyjs(),
      tags$head(
        tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Poppins:wght@400;600&display=swap"),
        tags$style(HTML("
          body {
            font-family: 'Poppins', sans-serif;
          }
          .box {
            border-radius: 10px;
            box-shadow: 0 4px 8px rgba(0,0,0,0.1);
            transition: transform 0.3s;
          }
          .box:hover {
            transform: translateY(-5px);
          }
          .value-box {
            border-radius: 10px;
          }
          .progress-bar {
            border-radius: 20px;
          }
          .family-node {
            fill: #6a3093;
            stroke: #fff;
            stroke-width: 2px;
          }
          .family-link {
            stroke: #999;
            stroke-width: 2px;
          }
          .risk-high {
            fill: #FF6B6B;
          }
          .risk-medium {
            fill: #FFD166;
          }
          .risk-low {
            fill: #4ECDC4;
          }
          .welcome-card {
            background: linear-gradient(135deg, #6a3093 0%, #a044ff 100%);
            color: white;
            padding: 20px;
            border-radius: 10px;
            margin-bottom: 20px;
          }
          .feature-card {
            background-color: white;
            padding: 15px;
            border-radius: 10px;
            margin-bottom: 15px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
          }
          .social-media a {
            color: #6a3093;
            margin-right: 15px;
            font-size: 24px;
          }
          .social-media a:hover {
            color: #a044ff;
          }
          .video-card {
            margin-bottom: 20px;
            border-radius: 10px;
            overflow: hidden;
          }
          .resource-card {
            padding: 15px;
            margin-bottom: 15px;
            border-left: 4px solid #6a3093;
            background-color: #f9f9f9;
          }
          .faq-item {
            margin-bottom: 15px;
            border-bottom: 1px solid #eee;
            padding-bottom: 15px;
          }
          .doctor-card {
            padding: 15px;
            margin-bottom: 15px;
            border-radius: 10px;
            background-color: white;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
          }
          .selection-card {
            padding: 15px;
            margin-bottom: 15px;
            border-radius: 10px;
            background-color: #f0f0f0;
          }
          .selection-title {
            font-weight: bold;
            margin-bottom: 10px;
            color: #6a3093;
          }
          .map-btn {
            margin-top: 5px;
            margin-bottom: 10px;
            background-color: #4285F4;
            color: white;
            border: none;
            padding: 5px 10px;
            border-radius: 4px;
            cursor: pointer;
            font-size: 12px;
          }
          .map-btn:hover {
            background-color: #3367D6;
          }
          .map-btn i {
            margin-right: 5px;
          }
          /* DARK MODE STYLE */
          body.dark-mode {
            background-color: #121212 !important;
            color: #e0e0e0 !important;
          }
          body.dark-mode .skin-blue .main-header .navbar {
            background-color: #1f1f1f !important;
            border-bottom: 1px solid #333 !important;
          }
          body.dark-mode .skin-blue .main-sidebar {
            background-color: #1f1f1f !important;
            color: #e0e0e0 !important;
          }
          body.dark-mode .skin-blue .main-sidebar .sidebar-menu > li > a {
            color: #e0e0e0 !important;
          }
          body.dark-mode .skin-blue .main-sidebar .sidebar-menu > li.active > a,
          body.dark-mode .skin-blue .main-sidebar .sidebar-menu > li:hover > a {
            background: #333 !important;
            color: #fff !important;
          }
          body.dark-mode .content-wrapper, 
          body.dark-mode .right-side {
            background-color: #181818 !important;
          }
          body.dark-mode .box {
            background: #242424 !important;
            border-color: #333 !important;
            color: #ddd !important;
          }
          body.dark-mode .box-header {
            color: #ddd !important;
          }
          body.dark-mode .value-box {
            background: #242424 !important;
            color: #ddd !important;
          }
          body.dark-mode .btn-primary {
            background-color: #3a3a3a !important;
            border-color: #555 !important;
            color: #eee !important;
          }
          /* Scrollbar for dark mode */
          body.dark-mode ::-webkit-scrollbar {
            width: 8px;
          }
          body.dark-mode ::-webkit-scrollbar-track {
            background: #1f1f1f;
          }
          body.dark-mode ::-webkit-scrollbar-thumb {
            background: #555;
          }
        "))
      ),
      
      tabItems(
        tabItem(
          tabName = "home",
          div(class = "welcome-card",
              h2("Selamat Datang di INYONG HEALTHY TRACK", style = "font-weight: 600;"),
              p("Aplikasi analisis kesehatan personal untuk memantau risiko hipertensi, diabetes, dan penyakit terkait berdasarkan gaya hidup dan riwayat keluarga.", 
                style = "font-size: 16px;")
          ),
          fluidRow(
            box(
              title = "📋 Tentang Aplikasi", width = 12, status = "primary", solidHeader = TRUE,
              p("INYONG HEALTHY TRACK adalah platform komprehensif untuk memantau dan menganalisis kesehatan Anda. Aplikasi ini membantu Anda:"),
              tags$ul(
                tags$li("Memahami kondisi tekanan darah dan gula darah Anda"),
                tags$li("Menilai risiko kesehatan berdasarkan riwayat keluarga"),
                tags$li("Mendapatkan rekomendasi gaya hidup dan nutrisi yang personal"),
                tags$li("Melacak perkembangan kesehatan dari waktu ke waktu"),
                tags$li("Mengakses konten edukasi kesehatan terpercaya"),
                tags$li("Konsultasi dengan tenaga medis profesional")
              ),
              p("Dikembangkan dengan algoritma medis terbaru, INYONG HEALTHY TRACK memberikan analisis yang akurat dan mudah dipahami.")
            )
          ),
          fluidRow(
            box(
              title = "📌 Petunjuk Penggunaan", width = 6, status = "info", solidHeader = TRUE,
              tags$ol(
                tags$li("Isi data kesehatan Anda di tab 'Input Data'"),
                tags$li("Klik tombol 'Analisis Sekarang' untuk memproses data"),
                tags$li("Lihat hasil analisis di tab 'Hasil Analisis'"),
                tags$li("Periksa risiko keturunan di tab 'Riwayat Keluarga'"),
                tags$li("Akses materi edukasi di tab 'Edukasi Kesehatan'"),
                tags$li("Temukan jawaban di 'FAQ Medis' atau konsultasi langsung")
              )
            ),
            box(
              title = "📱 Hubungi Kami", width = 6, status = "success", solidHeader = TRUE,
              p("Untuk pertanyaan atau masukan, silakan hubungi kami melalui:"),
              div(class = "social-media",
                  a(href = "https://www.instagram.com/inyong_health/?igsh=NzF4YTBxZWU0ZWZt#", target = "_blank", icon("instagram")),
                  a(href = "mailto:info.inyonghealthy@gmail.com", icon("envelope"))
              ),
              br(),
              p("Email: info.inyonghealthy@gmail.com"),
              p("Telepon: 0895-3261-81255"),
              p("Alamat: Jalan Sosio Humaniora No:1, Bulaksumur, Caturtunggal, Depok, Sleman Regency, Special Region of Yogyakarta 55281")
            )
          ),
          fluidRow(
            box(
              title = "✨ Fitur Unggulan", width = 12, status = "warning", solidHeader = TRUE,
              fluidRow(
                column(width = 4,
                       div(class = "feature-card",
                           h4(icon("heart-pulse"), "Analisis Tekanan Darah"),
                           p("Pemantauan tekanan darah dengan klasifikasi risiko yang akurat")
                       )
                ),
                column(width = 4,
                       div(class = "feature-card",
                           h4(icon("vial"), "Analisis Gula Darah"),
                           p("Penilaian risiko diabetes berdasarkan level gula darah")
                       )
                ),
                column(width = 4,
                       div(class = "feature-card",
                           h4(icon("family"), "Riwayat Keluarga"),
                           p("Analisis risiko keturunan berdasarkan riwayat kesehatan keluarga")
                       )
                )
              ),
              fluidRow(
                column(width = 4,
                       div(class = "feature-card",
                           h4(icon("graduation-cap"), "Edukasi Kesehatan"),
                           p("Video dan materi edukasi tentang hipertensi dan diabetes")
                       )
                ),
                column(width = 4,
                       div(class = "feature-card",
                           h4(icon("dumbbell"), "Rekomendasi Gaya Hidup"),
                           p("Saran personal untuk meningkatkan kebiasaan hidup sehat")
                       )
                ),
                column(width = 4,
                       div(class = "feature-card",
                           h4(icon("apple-whole"), "Rekomendasi Nutrisi"),
                           p("Panduan asupan gizi yang sesuai dengan kondisi kesehatan Anda")
                       )
                )
              ),
              fluidRow(
                column(width = 4,
                       div(class = "feature-card",
                           h4(icon("question-circle"), "FAQ Medis"),
                           p("Jawaban atas pertanyaan umum seputar hipertensi dan diabetes")
                       )
                ),
                column(width = 4,
                       div(class = "feature-card",
                           h4(icon("user-md"), "Konsultasi Medis"),
                           p("Informasi untuk berkonsultasi dengan tenaga medis profesional")
                       )
                ),
                column(width = 4,
                       div(class = "feature-card",
                           h4(icon("map-marked-alt"), "Lokasi Rumah Sakit"),
                           p("Panduan lokasi rumah sakit dengan integrasi Google Maps")
                       )
                )
              )
            )
          )
        ),
        
        tabItem(
          tabName = "input",
          h2("📋 Data Kesehatan", style = "font-weight: 600; color: #333;"),
          fluidRow(
            box(
              title = span(icon("user"), " Identitas"), width = 4, status = "primary", solidHeader = TRUE,
              textInput("name", "Nama Lengkap:", placeholder = "Contoh: Budi Santoso", value = ""),
              numericInput("age", "Umur:", value = 30, min = 1, max = 120),
              prettyRadioButtons(
                "gender", "Jenis Kelamin:",
                choices = c("Laki-laki" = "male", "Perempuan" = "female"),
                icon = icon("venus-mars"), status = "primary"
              )
            ),
            
            box(
              title = span(icon("heart"), " Tekanan Darah & Gula Darah"), width = 4, status = "danger", solidHeader = TRUE,
              sliderInput("systolic", "Tekanan Sistolik (mmHg):", min = 50, max = 300, value = 120),
              sliderInput("diastolic", "Tekanan Diastolik (mmHg):", min = 30, max = 200, value = 80),
              sliderInput("glucose", "Gula Darah Puasa (mg/dL):", min = 20, max = 500, value = 90)
            ),
            
            box(
              title = span(icon("running"), " Gaya Hidup"), width = 4, status = "info", solidHeader = TRUE,
              sliderInput("exercise_freq", "Frekuensi Olahraga (kali/minggu):", min = 0, max = 7, value = 3),
              sliderInput("exercise_duration", "Durasi Olahraga (menit/sesi):", min = 0, max = 120, value = 30),
              sliderInput("sleep", "Durasi Tidur (jam/hari):", min = 1, max = 12, value = 7),
              awesomeRadio(
                "smoking", "Kebiasaan Merokok:",
                choices = c("Tidak" = "none", "Jarang" = "rare", "Sering" = "often"),
                selected = "none"
              )
            )
          ),
          
          fluidRow(
            box(
              title = span(icon("utensils"), " Asupan Gizi"), width = 6, status = "success", solidHeader = TRUE,
              sliderInput("carbs", "Karbohidrat (gram/hari):", min = 0, max = 500, value = 200),
              sliderInput("protein", "Protein (gram/hari):", min = 0, max = 300, value = 70),
              sliderInput("fat", "Lemak (gram/hari):", min = 0, max = 200, value = 50),
              sliderInput("water", "Air (liter/hari):", min = 0, max = 10, value = 2)
            ),
            
            box(
              title = span(icon("notes-medical"), " Riwayat Kesehatan"), width = 6, status = "warning", solidHeader = TRUE,
              materialSwitch("hypertension_history", "Riwayat Hipertensi Keluarga?", status = "danger"),
              conditionalPanel(
                condition = "input.hypertension_history",
                sliderInput("hypertension_count", "Jumlah Keluarga dengan Hipertensi:", min = 1, max = 10, value = 1)
              ),
              materialSwitch("diabetes_history", "Riwayat Diabetes Keluarga?", status = "warning"),
              conditionalPanel(
                condition = "input.diabetes_history",
                sliderInput("diabetes_count", "Jumlah Keluarga dengan Diabetes:", min = 1, max = 10, value = 1)
              ),
              materialSwitch("heart_disease_history", "Riwayat Penyakit Jantung Keluarga?", status = "primary")
            )
          )
        ),
        
        tabItem(
          tabName = "results",
          h2("📊 Hasil Analisis Kesehatan", style = "font-weight: 600; color: #333;"),
          fluidRow(
            valueBoxOutput("bp_box", width = 6) %>% withSpinner(color = "#FF6B6B"),
            valueBoxOutput("glucose_box", width = 6) %>% withSpinner(color = "#4ECDC4")
          ),
          
          fluidRow(
            box(
              title = span(icon("chart-line"), " Grafik Risiko Hipertensi"), width = 6, status = "danger",
              plotlyOutput("bp_plot") %>% withSpinner()
            ),
            box(
              title = span(icon("chart-pie"), " Grafik Risiko Diabetes"), width = 6, status = "warning",
              plotlyOutput("glucose_plot") %>% withSpinner()
            )
          ),
          
          fluidRow(
            box(
              title = span(icon("dumbbell"), " Rekomendasi Gaya Hidup"), width = 6, status = "info",
              uiOutput("lifestyle_rec") %>% withSpinner()
            ),
            box(
              title = span(icon("apple-whole"), " Rekomendasi Gizi"), width = 6, status = "success",
              uiOutput("nutrition_rec") %>% withSpinner()
            )
          ),
          
          fluidRow(
            box(
              title = span(icon("table"), " Ringkasan Data"), width = 12, status = "primary",
              DTOutput("summary_table") %>% withSpinner()
            )
          )
        ),
        
        tabItem(
          tabName = "family",
          h2("👪 Riwayat Kesehatan Keluarga", style = "font-weight: 600; color: #333;"),
          fluidRow(
            box(
              title = "Riwayat Hipertensi dan Diabetes Keluarga", width = 12, status = "primary", solidHeader = TRUE,
              selectInput("father_hypertension", "Apakah Ayah Anda menderita Hipertensi?", choices = c("Tidak", "Ya")),
              selectInput("mother_hypertension", "Apakah Ibu Anda menderita Hipertensi?", choices = c("Tidak", "Ya")),
              numericInput("sibling_hypertension", "Jumlah Saudara yang menderita Hipertensi:", value = 0, min = 0, max = 10),
              selectInput("father_diabetes", "Apakah Ayah Anda menderita Diabetes?", choices = c("Tidak", "Ya")),
              selectInput("mother_diabetes", "Apakah Ibu Anda menderita Diabetes?", choices = c("Tidak", "Ya")),
              numericInput("sibling_diabetes", "Jumlah Saudara yang menderita Diabetes:", value = 0, min = 0, max = 10),
              actionBttn("submit_family", "Analisis Risiko Keturunan", style = "gradient", color = "success", block = TRUE)
            )
          ),
          fluidRow(
            box(
              title = "Hasil Analisis Risiko Keturunan", width = 12, status = "info", solidHeader = TRUE,
              htmlOutput("family_risk_info")
            )
          )
        ),
        
        tabItem(
          tabName = "education",
          h2("🎥 Edukasi Kesehatan", style = "font-weight: 600; color: #333;"),
          fluidRow(
            box(
              title = span(icon("youtube"), " Video Edukasi Kesehatan"), width = 12, status = "danger", solidHeader = TRUE,
              p("Tonton video edukasi berikut untuk mempelajari lebih lanjut tentang pengelolaan hipertensi dan diabetes:"),
              fluidRow(
                lapply(1:nrow(youtube_videos), function(i) {
                  column(
                    width = 6,
                    div(class = "video-card",
                        h4(youtube_videos$title[i]),
                        p(paste("Kategori:", youtube_videos$category[i], "| Durasi:", youtube_videos$duration[i])),
                        tags$iframe(
                          width = "100%",
                          height = "315",
                          src = paste0("https://www.youtube.com/embed/", youtube_videos$id[i]),
                          frameborder = "0",
                          allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture",
                          allowfullscreen = NA
                        )
                    )
                  )
                })
              )
            )
          ),
          fluidRow(
            box(
              title = span(icon("book"), " Sumber Edukasi Lainnya"), width = 12, status = "info", solidHeader = TRUE,
              p("Akses berbagai sumber edukasi berikut untuk informasi lebih lanjut:"),
              lapply(1:nrow(educational_resources), function(i) {
                div(
                  class = "resource-card",
                  h4(educational_resources$title[i]),
                  p(paste("Tipe:", educational_resources$type[i])),
                  p(educational_resources$description[i]),
                  a(href = educational_resources$link[i], target = "_blank", 
                    "Buka Sumber", class = "btn btn-primary")
                )
              })
            )
          )
        ),
        
        tabItem(
          tabName = "faq",
          h2("❓ FAQ Medis", style = "font-weight: 600; color: #333;"),
          fluidRow(
            box(
              title = span(icon("question-circle"), " Pertanyaan Umum"), width = 8, status = "primary", solidHeader = TRUE,
              lapply(1:nrow(medical_faqs), function(i) {
                div(
                  class = "faq-item",
                  h4(medical_faqs$question[i], style = "color: #6a3093;"),
                  p(medical_faqs$answer[i]),
                  span(paste("Kategori:", medical_faqs$category[i]), style = "font-size: 12px; color: #777;")
                )
              })
            ),
            box(
              title = span(icon("user-md"), " Konsultasi dengan Tenaga Medis"), width = 4, status = "success", solidHeader = TRUE,
              p("Jika Anda memiliki pertanyaan spesifik yang tidak terjawab di FAQ, Anda dapat berkonsultasi dengan tenaga medis profesional berikut:"),
              div(class = "selection-card",
                  div(class = "selection-title", "1. Pilih Kota:"),
                  selectInput("city_select", NULL, 
                              choices = c("Pilih kota...", names(medical_facilities))),
                  conditionalPanel(
                    condition = "input.city_select != 'Pilih kota...'",
                    div(class = "selection-title", "2. Pilih Fasilitas Kesehatan:"),
                    uiOutput("facility_ui"),
                    conditionalPanel(
                      condition = "input.facility_select != null",
                      div(class = "selection-title", "3. Dokter yang Tersedia:"),
                      uiOutput("doctors_ui")
                    )
                  )
              ),
              hr(),
              h4("Formulir Pertanyaan"),
              textAreaInput("question_text", "Tulis pertanyaan Anda:", rows = 3),
              selectInput("question_category", "Kategori Pertanyaan:", 
                          choices = c("Hipertensi", "Diabetes", "Gizi", "Lainnya")),
              actionButton("submit_question", "Kirim Pertanyaan", class = "btn-primary")
            )
          )
        ),
        
        tabItem(
          tabName = "settings",
          h2("⚙️ Pengaturan Aplikasi", style = "font-weight: 600; color: #333;"),
          fluidRow(
            box(
              title = "Preferensi Tampilan", width = 6, status = "primary", solidHeader = TRUE,
              selectInput("theme", "Tema Warna:", choices = c("Light", "Dark"), selected = "Light"),
              materialSwitch("notifications", "Aktifkan Notifikasi", status = "success", value = TRUE)
            ),
            box(
              title = "Informasi Akun", width = 6, status = "info", solidHeader = TRUE,
              textInput("email", "Alamat Email:"),
              passwordInput("password", "Kata Sandi:"),
              actionButton("save_settings", "Simpan Perubahan", class = "btn-primary")
            )
          )
        )
      )
    )
  )
}

server <- function(input, output, session) {
  
  logged_in <- reactiveVal(FALSE)
  current_user <- reactiveVal(NULL)
  
  output$dashboard_ui <- renderUI({
    req(logged_in())
    ui_main()
  })
  
  observe({
    if (logged_in()) {
      shinyjs::hide("login_ui")
      shinyjs::hide("register_ui")
      shinyjs::show("dashboard_ui")
    } else {
      shinyjs::show("login_ui")
      shinyjs::hide("register_ui")
      shinyjs::hide("dashboard_ui")
    }
  })
  
  observeEvent(input$login_btn, {
    req(input$login_username, input$login_password)
    users <- users_db()
    user_row <- users[users$username == input$login_username, ]
    if (nrow(user_row) == 1 && user_row$password == input$login_password) {
      logged_in(TRUE)
      current_user(input$login_username)
      showNotification("Login berhasil!", type = "message")
      
      updateTextInput(session, "name", value = user_row$name)
      updateNumericInput(session, "age", value = user_row$age)
      updatePrettyRadioButtons(session, "gender", selected = user_row$gender)
      
      shinyjs::removeClass(selector = "body", class = "dark-mode")
      updateSelectInput(session, "theme", selected = "Light")
      
    } else {
      showNotification("Username atau password salah.", type = "error")
    }
  })
  
  observeEvent(input$show_register_btn, {
    shinyjs::hide("login_ui")
    shinyjs::show("register_ui")
  })
  
  observeEvent(input$show_login_btn, {
    shinyjs::hide("register_ui")
    shinyjs::show("login_ui")
  })
  
  observeEvent(input$register_btn, {
    if (nchar(input$reg_username) < 4) {
      showNotification("Username minimal 4 karakter.", type = "warning")
      return()
    }
    if (input$reg_password != input$reg_password_confirm) {
      showNotification("Password dan konfirmasi tidak cocok.", type = "warning")
      return()
    }
    if (nchar(input$reg_password) < 6) {
      showNotification("Password minimal 6 karakter.", type = "warning")
      return()
    }
    if (nchar(input$reg_name) < 3) {
      showNotification("Nama lengkap minimal 3 karakter.", type = "warning")
      return()
    }
    
    users <- users_db()
    if (input$reg_username %in% users$username) {
      showNotification("Username sudah digunakan.", type = "warning")
      return()
    }
    
    new_user <- data.frame(
      username = input$reg_username,
      password = input$reg_password,
      name = input$reg_name,
      age = input$reg_age,
      gender = input$reg_gender,
      stringsAsFactors = FALSE
    )
    users_db(rbind(users, new_user))
    
    showNotification("Registrasi berhasil! Silakan login.", type = "message")
    shinyjs::hide("register_ui")
    shinyjs::show("login_ui")
  })
  
  observeEvent(input$theme, {
    req(input$theme)
    if (input$theme == "Dark") {
      shinyjs::addClass(selector = "body", class = "dark-mode")
    } else {
      shinyjs::removeClass(selector = "body", class = "dark-mode")
    }
  })
  
  user_data <- reactive({
    req(logged_in())
    list(
      name = input$name,
      age = input$age,
      gender = input$gender,
      systolic = input$systolic,
      diastolic = input$diastolic,
      glucose = input$glucose,
      exercise_freq = input$exercise_freq,
      exercise_duration = input$exercise_duration,
      sleep = input$sleep,
      smoking = input$smoking,
      carbs = input$carbs,
      protein = input$protein,
      fat = input$fat,
      water = input$water,
      hypertension_history = input$hypertension_history,
      hypertension_count = ifelse(input$hypertension_history, input$hypertension_count, 0),
      diabetes_history = input$diabetes_history,
      diabetes_count = ifelse(input$diabetes_history, input$diabetes_count, 0),
      heart_disease_history = input$heart_disease_history
    )
  })
  
  bp_result <- reactive({
    evaluate_blood_pressure(
      user_data()$systolic,
      user_data()$diastolic,
      user_data()$hypertension_history,
      user_data()$hypertension_count
    )
  })
  
  glucose_result <- reactive({
    evaluate_glucose(
      user_data()$glucose,
      user_data()$age,
      user_data()$diabetes_history,
      user_data()$diabetes_count
    )
  })
  
  output$bp_box <- renderValueBox({
    req(logged_in())
    result <- bp_result()
    valueBox(
      value = tags$p(result$category, style = "font-size: 24px; font-weight: bold;"),
      subtitle = tags$p(
        paste("Risiko:", result$risk),
        br(),
        result$explanation
      ),
      color = result$color,
      icon = icon("heart-pulse"),
      width = 12
    )
  })
  
  output$glucose_box <- renderValueBox({
    req(logged_in())
    result <- glucose_result()
    valueBox(
      value = tags$p(result$category, style = "font-size: 24px; font-weight: bold;"),
      subtitle = tags$p(
        paste("Risiko:", result$risk),
        br(),
        result$explanation
      ),
      color = result$color,
      icon = icon("vial"),
      width = 12
    )
  })
  
  output$bp_plot <- renderPlotly({
    req(logged_in())
    data <- data.frame(
      Faktor = c("Tekanan Sistolik", "Tekanan Diastolik", "Riwayat Keluarga"),
      Nilai = c(
        user_data()$systolic / 200,
        user_data()$diastolic / 150,
        min(1, user_data()$hypertension_count * 0.2)
      )
    )
    plot_ly(
      data,
      x = ~Faktor,
      y = ~Nilai,
      type = "bar",
      marker = list(
        color = c("#FF6B6B", "#FF8E8E", "#FFC1C1"),
        line = list(color = "#FFFFFF", width = 1.5)
      )
    ) %>%
      layout(
        title = "Faktor Risiko Hipertensi",
        yaxis = list(title = "Skala Risiko", range = c(0, 1)),
        xaxis = list(title = ""),
        plot_bgcolor = "#F9F9F9",
        paper_bgcolor = "#F9F9F9"
      )
  })
  
  output$glucose_plot <- renderPlotly({
    req(logged_in())
    data <- data.frame(
      Faktor = c("Gula Darah", "Usia", "Riwayat Keluarga"),
      Nilai = c(
        user_data()$glucose / 300,
        user_data()$age / 100,
        min(1, user_data()$diabetes_count * 0.15)
      )
    )
    plot_ly(
      data,
      labels = ~Faktor,
      values = ~Nilai,
      type = "pie",
      marker = list(colors = c("#4ECDC4", "#A2E8E0", "#D6F5F2")),
      textposition = "inside",
      textinfo = "label+percent"
    ) %>%
      layout(
        title = "Komposisi Risiko Diabetes",
        plot_bgcolor = "#F9F9F9",
        paper_bgcolor = "#F9F9F9"
      )
  })
  
  output$lifestyle_rec <- renderUI({
    req(logged_in())
    rec <- c()
    if (user_data()$exercise_freq < 3) {
      rec <- c(rec, paste0(
        "<div style='background: #E3F2FD; padding: 10px; border-radius: 5px; margin-bottom: 5px;'>",
        "<b>🏋️‍♂️ Olahraga:</b> Tingkatkan frekuensi olahraga minimal 3x/minggu (Saat ini: ",
        user_data()$exercise_freq, "x/minggu)</div>"
      ))
    }
    if (user_data()$exercise_duration < 30) {
      rec <- c(rec, paste0(
        "<div style='background: #E3F2FD; padding: 10px; border-radius: 5px; margin-bottom: 5px;'>",
        "<b>⏱ Durasi Olahraga:</b> Targetkan 30+ menit/sesi (Saat ini: ",
        user_data()$exercise_duration, " menit)</div>"
      ))
    }
    if (user_data()$sleep < 7) {
      rec <- c(rec, paste0(
        "<div style='background: #E3F2FD; padding: 10px; border-radius: 5px; margin-bottom: 5px;'>",
        "<b>😴 Tidur:</b> Kurang dari rekomendasi 7-9 jam/hari (Saat ini: ",
        user_data()$sleep, " jam)</div>"
      ))
    }
    if (user_data()$smoking != "none") {
      rec <- c(rec,
               "<div style='background: #FFEBEE; padding: 10px; border-radius: 5px; margin-bottom: 5px;'>",
               "<b>🚭 Merokok:</b> Pertimbangkan untuk berhenti merokok</div>"
      )
    }
    if (length(rec) == 0) {
      HTML(
        "<div style='background: #E8F5E9; padding: 15px; border-radius: 5px; text-align: center;'>
        <h4 style='color: #2E7D32;'>🎉 Gaya hidup Anda sudah baik!</h4>
        <p>Pertahankan kebiasaan sehat ini.</p>
        </div>"
      )
    } else {
      HTML(paste(rec, collapse = ""))
    }
  })
  
  output$nutrition_rec <- renderUI({
    req(logged_in())
    rec <- c()
    if (user_data()$carbs > 250) {
      rec <- c(rec, paste0(
        "<div style='background: #E8F5E9; padding: 10px; border-radius: 5px; margin-bottom: 5px;'>",
        "<b>🍚 Karbohidrat:</b> Kurangi asupan (Saat ini: ", user_data()$carbs, "g/hari)</div>"
      ))
    }
    if (user_data()$protein < 50) {
      rec <- c(rec, paste0(
        "<div style='background: #E8F5E9; padding: 10px; border-radius: 5px; margin-bottom: 5px;'>",
        "<b>🍗 Protein:</b> Tingkatkan asupan (Saat ini: ", user_data()$protein, "g/hari)</div>"
      ))
    }
    if (user_data()$fat > 70) {
      rec <- c(rec, paste0(
        "<div style='background: #E8F5E9; padding: 10px; border-radius: 5px; margin-bottom: 5px;'>",
        "<b>🥑 Lemak:</b> Kurangi asupan lemak jenuh (Saat ini: ", user_data()$fat, "g/hari)</div>"
      ))
    }
    if (user_data()$water < 2) {
      rec <- c(rec, paste0(
        "<div style='background: #E3F2FD; padding: 10px; border-radius: 5px; margin-bottom: 5px;'>",
        "<b>💧 Air:</b> Minum lebih banyak (Saat ini: ", user_data()$water, "L/hari)</div>"
      ))
    }
    if (length(rec) == 0) {
      HTML(
        "<div style='background: #E8F5E9; padding: 15px; border-radius: 5px; text-align: center;'>
        <h4 style='color: #2E7D32;'>🥗 Pola makan Anda seimbang!</h4>
        <p>Pertahankan asupan gizi yang baik.</p>
        </div>"
      )
    } else {
      HTML(paste(rec, collapse = ""))
    }
  })
  
  output$summary_table <- renderDT({
    req(logged_in())
    data <- data.frame(
      Kategori = c(
        "Nama", "Umur", "Jenis Kelamin",
        "Tekanan Darah", "Gula Darah",
        "Olahraga", "Durasi Olahraga",
        "Tidur", "Merokok",
        "Karbohidrat", "Protein", "Lemak", "Air",
        "Riwayat Hipertensi Keluarga",
        "Riwayat Diabetes Keluarga"
      ),
      Nilai = c(
        user_data()$name,
        paste(user_data()$age, "tahun"),
        ifelse(user_data()$gender == "male", "Laki-laki", "Perempuan"),
        paste(user_data()$systolic, "/", user_data()$diastolic, "mmHg"),
        paste(user_data()$glucose, "mg/dL"),
        paste(user_data()$exercise_freq, "kali/minggu"),
        paste(user_data()$exercise_duration, "menit/sesi"),
        paste(user_data()$sleep, "jam/hari"),
        switch(
          user_data()$smoking,
          "none" = "Tidak",
          "rare" = "Jarang",
          "often" = "Sering"
        ),
        paste(user_data()$carbs, "g/hari"),
        paste(user_data()$protein, "g/hari"),
        paste(user_data()$fat, "g/hari"),
        paste(user_data()$water, "L/hari"),
        ifelse(
          user_data()$hypertension_history,
          paste("Ya (", user_data()$hypertension_count, "orang)"),
          "Tidak"
        ),
        ifelse(
          user_data()$diabetes_history,
          paste("Ya (", user_data()$diabetes_count, "orang)"),
          "Tidak"
        )
      )
    )
    datatable(
      data,
      rownames = FALSE,
      options = list(
        dom = 't',
        pageLength = 15,
        scrollX = TRUE,
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#6a3093', 'color': '#fff'});",
          "}"
        )
      )
    ) %>%
      formatStyle(
        columns = names(data),
        backgroundColor = "#F9F9F9"
      )
  })
  
  output$family_risk_info <- renderUI({
    req(input$submit_family, logged_in())
    
    hypertension_count <- sum(c(
      input$father_hypertension == "Ya",
      input$mother_hypertension == "Ya",
      input$sibling_hypertension
    ))
    
    diabetes_count <- sum(c(
      input$father_diabetes == "Ya",
      input$mother_diabetes == "Ya",
      input$sibling_diabetes
    ))
    
    risk <- calculate_family_risk(
      hypertension_count,
      diabetes_count,
      user_data()$heart_disease_history
    )
    
    risk_category <- ifelse(risk > 60, "Tinggi",
                            ifelse(risk > 30, "Sedang", "Rendah"))
    
    color <- ifelse(risk > 60, "#FF6B6B",
                    ifelse(risk > 30, "#FFD166", "#4ECDC4"))
    
    HTML(paste0(
      "<div style='padding: 15px;'>",
      "<h4>📊 Estimasi Risiko Keturunan</h4>",
      "<div style='background-color: ", color, "; color: white; padding: 10px; border-radius: 5px; text-align: center;'>",
      "<p style='font-size: 18px; margin: 0;'>Berdasarkan riwayat keluarga, risiko keturunan Anda memiliki penyakit terkait adalah <b>",
      risk, "%</b> (", risk_category, ")</p>",
      "</div>",
      "<p style='margin-top: 10px;'>Faktor yang mempengaruhi:</p>",
      "<ul>",
      "<li>", hypertension_count, " anggota keluarga dengan hipertensi</li>",
      "<li>", diabetes_count, " anggota keluarga dengan diabetes</li>",
      "<li>", ifelse(user_data()$heart_disease_history, "Ada", "Tidak ada"), " riwayat penyakit jantung</li>",
      "</ul>",
      "</div>"
    ))
  })
  
  output$facility_ui <- renderUI({
    req(input$city_select)
    if (input$city_select != "Pilih kota...") {
      facilities <- get_facilities(input$city_select)
      selectInput("facility_select", NULL, choices = c("Pilih fasilitas...", facilities))
    }
  })
  
  output$doctors_ui <- renderUI({
    req(input$city_select, input$facility_select)
    if (input$city_select != "Pilih kota..." && input$facility_select != "Pilih fasilitas...") {
      doctors <- get_doctors(input$city_select, input$facility_select)
      coords <- get_facility_coordinates(input$city_select, input$facility_select)
      if (!is.null(doctors) && length(doctors) > 0) {
        map_url <- paste0("https://www.google.com/maps?q=", coords$lat, ",", coords$lng, "&hl=id&z=17")
        div(
          a(href = map_url, target = "_blank",
            class = "map-btn",
            icon("map-marked-alt"),
            "Lihat di Google Maps"
          ),
          lapply(doctors, function(doc) {
            div(
              class = "doctor-card",
              h4(doc$name),
              p(strong("Spesialisasi:"), doc$specialization),
              p(strong("Jadwal:"), doc$schedule),
              p(strong("Kontak:"), doc$contact)
            )
          })
        )
      } else {
        p("Tidak ada dokter yang tersedia di fasilitas ini.")
      }
    }
  })
  
  observeEvent(input$submit, {
    req(logged_in())
    updateTabItems(session, "tabs", "results")
    showNotification("Analisis berhasil dilakukan!", type = "message")
  })
  
  observeEvent(input$submit_question, {
    req(logged_in())
    if (nchar(input$question_text) > 10) {
      showNotification("Pertanyaan Anda telah terkirim! Tenaga medis akan menghubungi Anda via email.", type = "message")
      reset("question_text")
    } else {
      showNotification("Silakan tulis pertanyaan yang lebih lengkap.", type = "warning")
    }
  })
  
}

shinyApp(
  ui = fluidPage(
    useShinyjs(),
    hidden(div(id = "login_ui", login_ui)),
    hidden(div(id = "register_ui", register_ui)),
    hidden(div(id = "dashboard_ui", uiOutput("dashboard_ui")))
  ),
  server = server
)
