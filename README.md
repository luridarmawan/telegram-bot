

# Simple Telegram Bot - Desktop

## What is it?

**Telegram Bot** ini adalah bot telegram sederhana yang dibangun dengan menggunakan bahasa **Pascal** dengan framework [**FastPlaz**](https://fastplaz.com). Dijalankan di desktop, bukan sebagai _services_.

Pengoperasiannya sederhana, cukup menjalankan aplikasinya di laptop/pc saja, selama jaringan internet telah terhubung.

Di telegram-bot ini **sudah** disediakan [**NLP**](https://medium.com/@luridarmawan/natural-language-processing-nlp-sederhana-dari-carik-bot-78952b618695) untuk handle pesan yang masuk, sama seperti yang digunakan oleh [**CarikBot**](https://carik.id).

[![Telegram Bot Video](assets/video.png)](https://youtu.be/SgcHuqY9dSM "Telegram Bot Video")

## Why use it?

**Ringan Tanpa Beban**

Sebagian teman-teman mendapatkan kesulitan saat mengimplementasikan telegram bot dalam method hook. Mengingat harus menyediakan server/vps/hosting untuk menangkap hit dari telegram, dan juga perlu seting SSL juga. Memang ada yang free, tapi tetap sebagian diantara kita merasakan kesulitan.

Nahh...
Dengan telegram-bot ini, anda cukup menjalankan aplikasi ini di laptop/pc. Cukup menyediakan koneksi internet yang baik.

Aplikasi ini ringan, bahkan ringan tanpa beban. Bisa dikompilasi antar platform untuk dijalankan baik di Windows, Linux maupun Mac.


## How to use it


### Requirements

- [**FastPlaz**_runtime](http://www.fastplaz.com/)

### Instalasi

**install requirement**

Download kode sumber aplikasi Telegram Bot dan _package_ FastPlaz dengan cara berikut ini (menggunakan git):

```bash
$ mkdir -p TelegramBot/source/vendors
$ cd TelegramBot/source
$ git clone -b development https://github.com/luridarmawan/telegram-bot.git

# install vendors
#   change to branch development if needed

$ cd vendors
$ git clone -b development https://github.com/fastplaz/fastplaz.git
$ git clone -b development https://github.com/luridarmawan/SimpleAI.git

```

Anda boleh mengunakan cara lain untuk mendapatkan kode sumber ini, misalkan dengan download langsung dari link github yg tersedia.

Kemudian _compile_ (tidak perlu install) paket ini:

- FastPlaz: tools/fastplaz_runtime.lpk
- FastPlaz: tools/fastplaz_integration.lpk
- SimpleAI: source/packages/simpleai_package.lpk
- SimpleAI: source/packages/simplebot_package.lpk

**Build Telegram Bot dari IDE**

Melalui Lazarus, buka file `"src/bot.lpi"` dan *compile* file tersebut. 

Akan terbentuk file binary di 'bin/bot*'

***Configuration***

Konfigurasi menggunakan telegram bot ini sangat sederhana, cukup buka file `bin/config.json`, kemudian cantumkan telegram token anda.

```json
{
  "telegram": {
    "default": {
      "token": "your-telegram-here"
    }
  }
}
```

Token bisa anda dapatkan melalui chat ke [BotFather](https://telegram.me/botfather) yang sudah disediakan oleh Telegram.

Jika kompilasi berjalan baik dan konfigurasi telah benar, coba jalankan aplikasi telegram bot tadi.
Kira-kira tampilannya akan seperti ini.

![Telegram Bot](assets/app.png "Telegram Bot")

Jika tombol start diaktifkan, Bot akan secara periodik mengambil data pesan dari server telegram, dan mengirimkan pesan balik berupa **echo** dari pesan yang dikirim sebelumnya.

**NLP (Natural Language Processing)**

Bot ini menggunakan NLP yang disediakan oleh [Carik Bot](https://carik.id?ref=github). Konfigurasi konteks bisa dilakukan melalui file yang tersedia di folder `files/nlp/`. Informasi tentang NLP bisa dibaca melalui situs [Medium Luri Darmawan](https://medium.com/@luridarmawan/natural-language-processing-nlp-sederhana-dari-carik-bot-78952b618695).

**Telegram Bot Installation**

Tidak ada instalasi secara khusus dari aplikasi ini. Selama jaringan internet tersedia, Anda bisa menjalankan aplikasi ini di laptop/pc anda.

**Executable File**

Kami menyediakan file binary di halaman [Release](https://github.com/luridarmawan/telegram-bot/releases) untuk anda yang tidak mau direportkan dengan proses kompilasi . Tersedia untuk:

- [Linux 64 bit](https://github.com/luridarmawan/telegram-bot/releases/download/0.0.0/TelegramBot-binary-linux64.zip)
- [Windows 64 bit](https://github.com/luridarmawan/telegram-bot/releases/download/0.0.0/TelegramBot-binary-win64.zip)

Silakan download dan dijalankan.


## USAGE

Aplikasi secara periodik mengambil informasi pesan dari Telegram. Interval waktu bisa ditentukan sendiri melalui aplikasi ini, disarankan cukup 2000 atau 3000 mili detik saja.

![Telegram Bot](assets/app-interval.png "Interval Telegram Bot")

Disediakan fitur untuk mengirimkan pesan secara manual.

![Telegram Bot](assets/app-sendmessage.png "Send Message Telegram Bot")

Masukkan telegram-ID yang dituju dan pesan yang akan dikirimkan, kemudian tekan tombol 'Send'. Log hasil pengiriman ditampilkan di Page Log #2

### Custom Message

Tentu anda ingin membuat pesan balasan yang _custom_ khan? Mudah kok.

Dari IDE/Editor favorit anda, buka file project `bot.lpi`, nama berkas `main.pas`, lalu cari prosedur `onMessageHandler`. Prosedur inilah yang akan menangani arus pesan yang masuk. Pesan yang masuk disematkan di dalam parameter `AMessage`.

![Telegram Bot Custom Message](assets/basic-code.png "Basic Code Telegram Bot")

Jika pesan akan diabaikan dan tidak mengirimkan perlu balasan, cukup langsung `Exit` saja atau dengan memberikan nilai `False` ke variable `AHandled`.

### Catatan

Aplikasi ini adalah contoh membuat bot telegram yang sederhana, tersedia NLP sederhana pula, namun bukan untuk kebutuhan trafik yang tinggi.

Untuk penggunaan dengan trafik yang tinggi, saya menyarankan untuk menggunakan telegram bot yang memanfaatkan method webhook.
 
Untuk kebutuhan ini, anda bisa menggunakan [**Carik Bot**](https://carik.id) yang ada di mana-mana, di banyak layanan pesan singkat seperti [Telegram](https://t.me/carikbot), [Facebook Messenger](https://m.me/Carik.Bot), [Line](https://line.me/ti/p/~@carik), [Instagram](https://www.instagram.com/carikbot/), bahkan ada pula untuk [Android App](https://carik.id/app).

## Video Compilasi Source

_*belum tersedia_

## Referensi

- [Carik Telegram Bot - Webhook](https://github.com/luridarmawan/carik)
- [Stemming Word dalam Carik](https://medium.com/@luridarmawan/stemming-word-dalam-carik-da3b802038c8)
- [Natural Language Processing (NLP) sederhana dari Carik Bot](https://medium.com/@luridarmawan/natural-language-processing-nlp-sederhana-dari-carik-bot-78952b618695)

