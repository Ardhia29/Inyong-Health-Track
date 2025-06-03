# Inyong-Health-Track
# Tentang Program Aplikasi
INYONG HEALTHY TRACK adalah aplikasi berbasis R Shiny yang dikembangkan untuk mendeteksi risiko hipertensi dan diabetes berdasarkan data kesehatan individu, riwayat keturunan, dan pola hidup. Aplikasi ini memanfaatkan algoritma evaluasi medis untuk memberikan analisis risiko secara personal dan menyajikan visualisasi interaktif yang mudah dipahami. Selain itu, aplikasi ini memberikan edukasi kesehatan dan rekomendasi gaya hidup berdasarkan data input pengguna. 

# Fitur Utama 
Aplikasi ini dirancang dengan berbasis R Shiny dan menyediakan fitur-fitur berikut:

- 🔎 Analisis Tekanan Darah: Deteksi klasifikasi tekanan darah berdasarkan input pengguna.
- 🔍 Analisis Gula Darah: Menilai status gula darah dan risiko diabetes.
- 👨‍👩‍👧‍👦 Analisis Riwayat Keluarga: Mengukur risiko keturunan untuk hipertensi dan diabetes.
- 📊 Visualisasi Interaktif: Grafik risiko untuk mempermudah pemahaman hasil.
- 🧘 Rekomendasi Gaya Hidup: Saran olahraga, tidur, dan kebiasaan sehat.
- 🥗 Rekomendasi Nutrisi: Estimasi kebutuhan karbohidrat, protein, lemak, dan air.
- 📚 Edukasi Kesehatan: Video YouTube dan artikel kesehatan terpercaya.
- ❓ FAQ Medis: Jawaban atas pertanyaan umum mengenai hipertensi dan diabetes.
- 🏥 Konsultasi Medis & Lokasi RS: Direktori dokter spesialis dan rumah sakit lengkap dengan jadwal dan lokasi.
- ⚙️ Pengaturan Tampilan & Akun: Kustomisasi preferensi pengguna.

# Cara Penggunaan Aplikasi
1. Buka aplikasi melalui browser (R Shiny App).
2. Isi data pribadi dan kesehatan di tab "Input Data".
3. Klik tombol "Analisis Sekarang".
4. Lihat hasil analisis di tab "📊 Hasil Analisis", berupa:
    - Kategori tekanan darah dan risiko (NORMAL, PRA-HIPERTENSI, HIPERTENSI)
    - Status gula darah (NORMAL, PRA-DIABETES, DIABETES)
    - Rekomendasi gaya hidup dan gizi
    - Grafik faktor risiko interaktif (bar dan pie chart)
5. Masuk ke tab "Riwayat Keluarga" untuk menilai risiko genetik berdasarkan data keluarga (ayah, ibu, saudara).
6. Akses "Edukasi Kesehatan" untuk menonton video atau membaca artikel/ebook tentang hipertensi dan diabetes.
7. Kunjungi tab "FAQ Medis" jika ingin melihat pertanyaan umum atau konsultasi dengan dokter berdasarkan kota dan rumah sakit.
8. Gunakan tab "Pengaturan" untuk menyesuaikan tema tampilan, email, notifikasi, dan preferensi lainnya.

# Teknologi yang Digunakan 
- Bahasa Pemrograman: R (R Language)
- Framework UI: R Shiny, Shiny Dashboard
- Visualisasi Interaktif: plotly, DT (DataTable)
- Fitur UX/UI tambahan: shinyjs, shinyWidgets, fontawesome
- Data Input: Slider, Radio Button, Select Input, Conditional Panel
- Reaktivitas Data: Fungsi reactive() dan render*() pada Shiny
- Visualisasi Map dan Koordinat: Integrasi lokasi dokter dan rumah sakit dengan Google Maps link & koordinat manual
- Struktur Data: data.frame, list, dan conditionalPanel
- Edu-content: Integrasi video dari YouTube dan sumber eksternal (Alodokter, Halodoc, Siloam, dll.)

# Lisensi
Open Source / Free-to-use.
- Aplikasi ini dikembangkan tanpa biaya lisensi tinggi dan didesain dapat digunakan oleh masyarakat luas, terutama sebagai media edukasi dan pencegahan penyakit kronis. 

# Tim Pengembang
Disusun oleh Kelompok 10 dari Program Studi Akuntansi, Fakultas Ekonomika dan Bisnis, Universitas Gadjah Mada:

1. Ibnu Zidan Alfarij [24/541345/EK/25149]
2. Tiara Anindita Sukmana [24/541371/EK/25151]
3. Raffli Taufika Fiqri [24/545667/EK/25350]
4. Nizam Novyanda Wibowo [24/544505/EK/25322]
5. Raihani Khairunnisa [24/542943/EK/25269]
6. Qeisha Mayliana Gusmita [24/542473/EK/25247]
7. Gabriela Ardhia Himasari [24/542359/EK/25236]

Dosen pembimbing:
Dr. Lukman Heryawan, S.T., M.T.
