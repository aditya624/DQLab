library(stringdist)
library(openxlsx)

#Membaca file staging.final.xlsx
data.pelanggan <- read.xlsx("/home/pika/dqlab/staging.final.xlsx")

#Inisialisai variable untuk hasil.akhir
hasil.akhir <- NULL

#Inisialiasi variable grouping_no dengan nilai 1
grouping_no <- 1

while(length(data.pelanggan$nama)>0)
{
  #Variable referensi nama dan alamat diambil dari item pertama
  referensi.nama <- data.pelanggan$nama[1]
  referensi.alamat <- data.pelanggan$alamat[1]
  
  #Menghitung jarak antara referensi dengan item-item nama dan alamat
  #gunakan method "cosine" untuk nama, dan method "lv" untuk alamat
  jarak.teks.nama <- stringdist(referensi.nama, data.pelanggan$nama, method="cosine")
  jarak.teks.alamat <- stringdist(referensi.alamat, data.pelanggan$alamat, method="lv")
  
  #Hasil filter jarak dengan threshold 
  # - lebih kecil sama dengan angka 0.15 untuk nama
  # - lebih kecil dari angka 15 untuk alamat
  #disimpan ke variable filter.jarak
  filter.jarak <- (jarak.teks.nama <= 0.15 & jarak.teks.alamat < 15)
  
  #Melakukan filtering pada variable data.pelanggan, dan mengambil tiga kolom 
  #untuk disimpan ke tiga variable 
  kode_pelanggan.temp <- data.pelanggan[filter.jarak,]$kode_pelanggan
  nama.temp <- data.pelanggan[filter.jarak,]$nama
  alamat.temp <- data.pelanggan[filter.jarak,]$alamat
  
  #Konstruksi temporary variable
  var.temp <- data.frame(grouping=grouping_no, kode_pelanggan=kode_pelanggan.temp, nama=nama.temp, alamat=alamat.temp, jumlah_record=length(kode_pelanggan.temp))
  
  #Menggabungkan temporary variable dengan hasil sebelumnya
  hasil.akhir <- rbind(hasil.akhir, var.temp)
  
  #Menggabungkan hasil sebelumnya
  data.pelanggan <- data.pelanggan[!filter.jarak,]
  
  #Menambahkan nilai grouping untuk diambil pada iterasi selanjutnya
  grouping_no <- grouping_no + 1
}

#Menulis hasil ke file staging.duplikat.standarisasi.xlsx
write.xlsx(hasil.akhir, file="/home/pika/dqlab/staging.duplikat.standarisasi.xlsx")

