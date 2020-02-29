#Load library stringdist
library("stringdist")

#Membuat variable vector nama
nama.pelanggan <- c("Agus Cahyono", "Justin Bieber", "Agus Tjahyono", "Cahyono Agus")

#Inisialisai variable untuk hasil.akhir
hasil.akhir <- NULL

#Inisialiasi variable grouping_no dengan nilai 1
grouping_no <- 1

#Melakukan perulangan proses pencarian dengan perintah while, sampai akhirnya isi vector menjadi kosong (panjang = 0)
while(length(nama.pelanggan)>0)
{
  #Variable referensi diisi dengan item pertama variable nama.pelanggan
  referensi <- nama.pelanggan[1]
  
  #Menghitung jarak antara referensi dengan item-item nama.pelanggan
  jarak.teks <- stringdist(referensi, nama.pelanggan, method="cosine")
  
  #Hasil filter jarak dengan threshold 0.15 disimpan ke variable nama.hasil
  nama.hasil <- nama.pelanggan[jarak.teks <= 0.15]
  
  #Hasil filter jarak dengan threshold 0.15 disimpan ke variable nama.hasil
  var.temp = data.frame(grouping=grouping_no, nama=nama.hasil)
  
  #Menggabungkan hasil sebelumnya 
  hasil.akhir <- rbind(hasil.akhir, var.temp)  
  
  #Mengambil porsi data yang bukan di dalam threshold dengan menggunakan simbol ! yang mewakili operator not (bukan)
  nama.pelanggan <- nama.pelanggan[!(jarak.teks <= 0.15)]
  
  #Menambahkan nilai grouping untuk diambil pada iterasi selanjutnya
  grouping_no <- grouping_no + 1
}
#Menampilkan hasil akhir
hasil.akhir
