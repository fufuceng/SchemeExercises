#lang racket
;--------------------------------------------
; Verilen satır ve sutun değerleriyle tablo oluşturur.
(define satir-uret
  (lambda (sutun-sayisi satir-liste)
    (if (= sutun-sayisi 0) satir-liste (satir-uret (- sutun-sayisi 1) (append satir-liste (cons "X" '()))))))
(define tahta-uret-helper
  (lambda (satir sutun ana-liste)
    (if (= satir 0) ana-liste (tahta-uret-helper (- satir 1) sutun (append ana-liste (cons (satir-uret sutun '()) '()))))))
(define tahta-uret
  (lambda (satir sutun)
    (tahta-uret-helper satir sutun '())
    ))
; ------------------------------------------
;; Listeden elemanın sağ tarafını sil.
(define sag-taraf-silici-helper
  (lambda (liste-1 liste-2 index sayac)
    (cond ((null? liste-1) liste-2)
          ((or (> sayac index)(= sayac index))(sag-taraf-silici-helper (cdr liste-1) (append liste-2 '("_")) index (+ sayac 1)))
          (else(sag-taraf-silici-helper (cdr liste-1) (append liste-2 (cons (car liste-1) '())) index (+ sayac 1))))))

(define sag-taraf-silici
  (lambda (liste-1 index)
    (sag-taraf-silici-helper liste-1 '() index 1)))

;; Satır Sutunla Eleman Sil
(define satiri-temizle
  (lambda (satir sutun sayac tahta yeni-tahta)
    (cond ((null? tahta) yeni-tahta)
          ((or (= sayac satir) (> sayac satir))
           (satiri-temizle satir sutun (+ sayac 1) (cdr tahta) (append yeni-tahta (cons(sag-taraf-silici (car tahta) sutun) '()))))
          (else(satiri-temizle satir sutun (+ sayac 1) (cdr tahta) (append yeni-tahta (cons(car tahta) '())))))))
;; Verilen koordinatlarla tahta guncelle. Burada, koordinat alınan inputtur aslında.
; Kullanıcıdan alınan input ve bilgisayarın oynamaya karar verdiği hamleler liste tipindedir '(1 2) gibi
; car kordinat -> satır , cadr kordinat -> sutun u ifade eder.
(define tahta-guncelle
  (lambda (kordinat tahta)
    (satiri-temizle (car kordinat) (cadr kordinat) 1 tahta '())))

(define tahta-yazdir-helper
  (lambda (tahta proc-1 proc-2)
    (if (null? tahta)(newline) (tahta-yazdir-helper (cdr tahta)(display (car tahta)) (newline)))))

(define tahta-yazdir
  (lambda (tahta)(tahta-yazdir-helper tahta 1 1)))
;; --- AI --- ;;
; 2,2 var mı kontrol et. Eğer 2,2 varsa, 1,1 in yanındaki ve altındaki seçilmemişleri say. Eğer bunlar birbirine eşitse, 2,2 seçip sonrasında oyunu kazanabiliriz.
; Eğer eşit değillerse, Eşitlemek için hamleler yap. Nasıl? Fazla olan taraftan 2 sinin farkı kadarını silmeye oyna. Eşitledikten sonra; kullanıcı 2 2 seçmemesi durumunda
; 2,2 seçip sonrasında oyunu kazanabiliriz.
(define iki-iki-var-mi
  (lambda (tahta)
    (if (eq? (cadadr tahta) "X") true false)))
;; ----------------------
;; Zehirlinin yanındaki ve altındaki cookieleri say.
(define bir-bir-yani-helper
  (lambda (satir sayac)
    (if (null?  satir) sayac (if (eq? (car satir) "X") (bir-bir-yani-helper (cdr satir) (+ sayac 1)) (bir-bir-yani-helper (cdr satir) sayac)))))
(define bir-bir-yani
  (lambda (tahta)
    (bir-bir-yani-helper (cdar tahta) 0)
    ))
(define bir-bir-alti-helper
  (lambda (tahta sayac)
    (if (null? tahta) sayac
        (if (eq? (caar tahta) "X") (bir-bir-alti-helper (cdr tahta) (+ sayac 1)) (bir-bir-alti-helper (cdr tahta) sayac)))))
(define bir-bir-alti
  (lambda (tahta)
    (bir-bir-alti-helper (cdr tahta) 0)))
;; ------------------------------------------------------------
(define bilgisayar-hamlesi-tahta-guncelle
  (lambda (tahta kordinat)
    (tahta-guncelle kordinat tahta)))
; Bu kısım gerçek hamlenin döndüğü kısımdır. 1,1 sağı ve solu farkını alır.
; Olan farka göre hamleleri gerçekleştirir.
; Fark 0'dan büyükse hamle kordinatları satir = 1, sutun = ((1,1 sağındaki X sayısı) - fark) + 2
; Fark 0'dan küçükse hamle kordinatları satir = ((1,1 altındaki X sayısı) + fark) + 2, sutun = 1
; Fark 0'a eşitse    hamle kordinatları satir = 1, ((1,1 sağındaki X sayısı) - fark) + 1
(define bilgisayar-hamlesi-fark
  (lambda (tahta)
    (- (bir-bir-yani tahta) (bir-bir-alti tahta))))
(define bilgisayar-hamlesi-helper
  (lambda (tahta fark)
    (cond ((> fark 0)
           (cons 1 (cons (+ (- (bir-bir-yani tahta) fark) 2) '())))
          ((< fark 0)
           (cons (+ (+ (bir-bir-alti tahta) fark) 2) (cons 1 '())))
          ((= fark 0)
            (cons 1 (cons (+ (- (bir-bir-yani tahta) fark) 1) '())))
          )))
; Bilgisayar hamlesi ana fonksiyonu.
(define bilgisayar-hamlesi
  (lambda (tahta)
    (if (iki-iki-var-mi tahta)
        (if (= (bilgisayar-hamlesi-fark tahta) 0)
            (tahta-guncelle '(2 2) tahta)
            (bilgisayar-hamlesi-tahta-guncelle tahta (bilgisayar-hamlesi-helper tahta (bilgisayar-hamlesi-fark tahta))))
        (bilgisayar-hamlesi-tahta-guncelle tahta (bilgisayar-hamlesi-helper tahta (bilgisayar-hamlesi-fark tahta))))))
;; ----------------------------------------------------------------------------
; SONUÇ KONTROL MEKANİZMALARI
; Board da sadece 1,1 deki elemanın kalıp kalmadığını kontrol ediyor.
; Sadece 1,1 deki eleman kalmışsa oyun bitmiş demektir.
(define satir-yenmis-mi
  (lambda (satir)
    (if (null? satir) true (if (eq? (car satir) "X") false (satir-yenmis-mi (cdr satir))))))

(define tek-zehir-mi-var-helper
  (lambda (tahta sayac)
    (if (null? tahta) true (if (= sayac 1) (if (satir-yenmis-mi (cdar tahta)) (tek-zehir-mi-var-helper (cdr tahta) (+ sayac 1)) false)
                               (if (satir-yenmis-mi (car tahta)) (tek-zehir-mi-var-helper (cdr tahta) (+ sayac 1)) false)))))
(define tek-zehir-mi-var
  (lambda (tahta)
    (and (eq? (caar tahta) "X")(tek-zehir-mi-var-helper tahta 1))))
; 1,1 de bulunan elemanın seçilip seçilmediğini kontrol ediyor.
; Seçilmişse oyun bitmiş demektir.
(define zehir-yendi-mi
  (lambda (tahta)
    (if (eq? "_" (caar tahta)) true false)))
;---------------------------------
; İnput alan fonksiyon.
(define (input-al)
  (display "Please enter Row Column like '1 2' : ")
    (map (lambda (n) (string->number n)) (string-split (read-line))))
;; ------------------------------
; Hamlenin uygunluğunun kontrol edildiği bölüm. hamle-uygun-mu fonksiyonuna "tahta hamle-list"
; Parametre olarak verilir ve bu fonksiyon içinde "satiri-al" "elemani-al" "sinir-kontrol"
; fonksiyonları sayesinde hamlenin uygunluğunu kontrol eder.
; Kontrol ettiği durumlar;
; 1- Daha önceki hamlelerde seçilmiş bir cookie seçmiş mi?
; 2- Matrisin sınırları dışında bir değer seçmiş mi?
; 3- Alınan input, sayılardan mı oluşuyor?
; 4- Alınan input, satir ve sutun değerlerini içeriyor mu?

(define satiri-al
  (lambda (satir tahta)
    (if (= satir 1) (car tahta) (satiri-al (- satir 1) (cdr tahta)))))
(define elemani-al
  (lambda (sutun-sayi satir-liste)
    (if (= sutun-sayi 1) (car satir-liste) (elemani-al (- sutun-sayi 1) (cdr satir-liste)))))
(define sinir-kontrol
  (lambda (tahta hamle-list)
    (if (null? hamle-list) false
        (if (not (= (length hamle-list) 2))
            false
            (if (and (number? (car hamle-list)) (number? (cadr hamle-list)))
                (if (and (or (> (car hamle-list) (length tahta)) (< (car hamle-list) 1) ) (or (> (cadr hamle-list) (length (car tahta))) (< (cadr hamle-list) 1)) )
                    false
                    true)
                false )))))
(define hamle-uygun-mu
  (lambda(tahta hamle-list)
    (if (sinir-kontrol tahta hamle-list) (if (eq? (elemani-al (cadr hamle-list) (satiri-al (car hamle-list) tahta)) "X") true false) false)
  ))
;; ------------------------------
; Oyun sırasına göre sonucu yazdır.
(define sonuc-yazdir
  (lambda (oyun-sirasi)
    (if (= oyun-sirasi -1) (display "You Won!") (display "I won! See u later!"))))
; ----------------------------------------------------------------------------
; Parametre olarak, tahta-yazdir fonksiyonunu alır.
; Bu sayede hem tahtayı yazdırıp, hemnde güncel tahtayı
; ana-oyun fonksiyonu için geri döndürür.
(define tahta-dondur-yazdir-helper
  (lambda (guncel-tahta yazdir-proc)
    guncel-tahta))
(define tahta-dondur-yazdir
  (lambda (guncel-tahta)
    (tahta-dondur-yazdir-helper guncel-tahta (tahta-yazdir guncel-tahta))))
; ----------------------------------------------------------------------------
; Parametre olarak, tahta oyun-sirasi ve hamle-list alır. Hamle list, sıra kullanıcıya
; geçtiğinde input-al fonksiyonu olur, sıra bilgisayara geçtiğinde ise 0 olur. Bu da onu
; etkisiz kılar. Parametre olarak (input-al) fonksiyonunun verilmesinin amacı şudur;
; Alınan inputu kontrol etmek için, inputu
; bir parametre gibi vererek, ana-oyun fonksiyonunun içinde 2 defa kullanılması gerektiğinden
; dolayıdır. Kontrol etmek istenmezse, ana-oyun'un sadece "tahta oyun-sirasi" parametrelerini
; alması yeterlidir. Böyle bir durumda kullanıcı hamlesinin gerçekleşmesi için tahta-guncelle
; fonksiyonuna "(input-al) tahta" parametrelerinin yollanması gerekir.
(define ana-oyun
  (lambda (tahta oyun-sirasi hamle-list)
    (cond ((tek-zehir-mi-var tahta)
           (sonuc-yazdir (* oyun-sirasi -1)))
          ((zehir-yendi-mi tahta)(sonuc-yazdir oyun-sirasi))
          ((= oyun-sirasi -1)
           (if (hamle-uygun-mu tahta hamle-list)
               (ana-oyun (tahta-dondur-yazdir (tahta-guncelle hamle-list tahta)) (* oyun-sirasi -1) 0)
               (ana-oyun tahta oyun-sirasi (input-al))))
          ((= oyun-sirasi 1)
           (ana-oyun (tahta-dondur-yazdir(bilgisayar-hamlesi tahta)) (* oyun-sirasi -1) (input-al))))))

(define tahta (tahta-uret 4 5))
(tahta-yazdir tahta)
(ana-oyun tahta -1 (input-al))
