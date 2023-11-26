(define (encode/data-processing-immediate t c op s rd rn imm)
  ;; 1 30 9 8   7 6 5   4 3 2 1 20   9 8 7 6   5 4 3 2   1 10 9 8 7 6 5 4 3 2 1 0
  ;; cond     | 0 0 1 | op       s | rn      | rd      | imm12
  (let ((out 0))
    (set! out (b+  out            c))
    (set! out (b+ (b<< out 3)     1))
    (set! out (b+ (b<< out 4)    op))
    (set! out (b+ (b<< out 1)     s))
    (set! out (b+ (b<< out 4)    rn))
    (set! out (b+ (b<< out 4)    rd))
    (set! out (b+ (b<< out 12)  imm))
    (emit-instruction t (integer->bytelist out 4))))

(define (encode/data-processing-immediate-shift t c op s rd rn rm shtyp shoff)
  ;; 1 30 9 8   7 6 5   4 3 2 1 20   9 8 7 6 5 4 3 2   1 10 9 8 7   6 5     4   3 2 1 0
  ;; cond     | 0 0 0 | op1      s | rn     | rd     | shoff      | shtyp | 0 | rm
  (let ((out #x00000000))
    (set! out (b+  out            c))
    (set! out (b+ (b<< out 7)    op)) ; including the 0s in [27:25]
    (set! out (b+ (b<< out 1)     s))
    (set! out (b+ (b<< out 4)    rn))
    (set! out (b+ (b<< out 4)    rd))
    (set! out (b+ (b<< out 5) shoff))
    (set! out (b+ (b<< out 2) shtyp))
    (set! out (b+ (b<< out 1)     0))
    (set! out (b+ (b<< out 4)    rm))
    (emit-instruction t (integer->bytelist out 4))))

(define (encode/data-processing-register-shift t c op s rd rn rm shtyp rs)
  ;; 1 30 9 8   7 6 5   4 3 2 1 20   9 8 7 6 5 4 3 2   1 10 9 8 7   6 5     4   3 2 1 0
  ;; cond     | 0 0 0 | op1      s | rn     | rd     | rs       0 | shtyp | 1 | rm
  (let ((out #x00000000))
    (set! out (b+  out            c))
    (set! out (b+ (b<< out 7)    op)) ;; including the 0s in [27:25]
    (set! out (b+ (b<< out 1)     s))
    (set! out (b+ (b<< out 4)    rn))
    (set! out (b+ (b<< out 4)    rd))
    (set! out (b+ (b<< out 4)    rs))
    (set! out (b+ (b<< out 1)     0))
    (set! out (b+ (b<< out 2) shtyp))
    (set! out (b+ (b<< out 1)     1))
    (set! out (b+ (b<< out 4)    rm))
    (emit-instruction t (integer->bytelist out 4))))

(define (encode/load-store-immediate-offset t c p? u? b? w? l? rd rn imm)
  ;; 1 30 9 8   7 6 5   4 3 2 1 20   9 8 7 6   5 4 3 2   1 10 9 8 7 6 5 4 3 2 1 0
  ;; cond     | 0 1 0 | P U B W  L | rn      | rd      | imm12
  (let ((out 0))
    (set! out (b+  out              c))
    (set! out (b+ (b<< out 3)   #b010))
    (set! out (b+ (b<< out 1)      p?))
    (set! out (b+ (b<< out 1)      u?))
    (set! out (b+ (b<< out 1)      b?))
    (set! out (b+ (b<< out 1)      w?))
    (set! out (b+ (b<< out 1)      l?))
    (set! out (b+ (b<< out 4)      rn))
    (set! out (b+ (b<< out 4)      rd))
    (set! out (b+ (b<< out 12)    imm))
    (emit-instruction t (integer->bytelist out 4))))

(define (encode/load-store-register-offset t c p? u? b? w? l? rd rn rm shtyp shoff)
  ;; 1 30 9 8   7 6 5   4 3 2 1 20   9 8 7 6   5 4 3 2   1 10 9 8 7   6 5 4   3   2 1 0
  ;; cond     | 0 1 1 | P U B W  L | rn      | rd      | shoff      | shtyp | 0 | rm
  (let ((out 0))
    (set! out (b+  out              c))
    (set! out (b+ (b<< out 3)   #b011))
    (set! out (b+ (b<< out 1)      p?))
    (set! out (b+ (b<< out 1)      u?))
    (set! out (b+ (b<< out 1)      b?))
    (set! out (b+ (b<< out 1)      w?))
    (set! out (b+ (b<< out 1)      l?))
    (set! out (b+ (b<< out 4)      rn))
    (set! out (b+ (b<< out 4)      rd))
    (set! out (b+ (b<< out 5)   shoff))
    (set! out (b+ (b<< out 3)   shtyp))
    (set! out (b+ (b<< out 4)      rm))
    (emit-instruction t (integer->bytelist out 4))))

(define (encode/load-store-multiple t c p? u? b? w? l? rn rlist)
  ;; 1 30 9 8   7 6 5   4 3 2 1 20   9 8 7 6   5 4 3 2   1 10 9 8 7   6 5 4   3   2 1 0
  ;; cond     | 0 1 1 | P U B W  L | rn      | register-list
  (let ((out 0))
    (set! out (b+  out              c))
    (set! out (b+ (b<< out 3)   #b011))
    (set! out (b+ (b<< out 1)      p?))
    (set! out (b+ (b<< out 1)      u?))
    (set! out (b+ (b<< out 1)      b?))
    (set! out (b+ (b<< out 1)      w?))
    (set! out (b+ (b<< out 1)      l?))
    (set! out (b+ (b<< out 4)      rn))
    (set! out (b+ (b<< out 16)  rlist))
    (emit-instruction t (integer->bytelist out 4))))

(define (encode/branch-with-link t c l? imm)
  ;; 1 30 9 8   7 6 5 4   3 2 1 20 9 8 7 6 5 4 3 2 1 10 9 8 7 6 5 4 3 2 1 0
  ;; cond     | 1 0 1 L | imm24
  (let ((out 0))
    (set! out (b+  out              c))
    (set! out (b+ (b<< out 3)   #b101))
    (set! out (b+ (b<< out 1)      l?))
    (set! out (b+ (b<< out 24)     imm))
    (emit-instruction t (integer->bytelist out 4))))

(define (encode/copro-load-store t c p? u? s? w? l? rn crd cnum off)
  ;; 1 30 9 8   7 6 5   4 3 2 1 20   9 8 7 6   5 4 3 2   1 10 9 8   7 6 5 4 3 2 1 0
  ;; cond     | 1 1 0 | P U S W  L | rn      | crd     | cnum     | imm8
  (let ((out 0))
    (set! out (b+  out              c))
    (set! out (b+ (b<< out 3)   #b110))
    (set! out (b+ (b<< out 1)      p?))
    (set! out (b+ (b<< out 1)      u?))
    (set! out (b+ (b<< out 1)      s?))
    (set! out (b+ (b<< out 1)      w?))
    (set! out (b+ (b<< out 1)      l?))
    (set! out (b+ (b<< out 4)      rn))
    (set! out (b+ (b<< out 4)     crd))
    (set! out (b+ (b<< out 4)    cnum))
    (set! out (b+ (b<< out 8)     off))
    (emit-instruction t (integer->bytelist out 4))))

(define (encode/copro-data-processing t c op1 crn crd cnum op2 crm)
  ;; 1 30 9 8   7 6 5 4   3 2 1 20   9 8 7 6   5 4 3 2   1 10 9 8   7 6 5 4   3 2 1 0
  ;; cond     | 1 1 1 0 | op1      | crn     | crd     | cnum     | op2   0 | crm
  (let ((out 0))
    (set! out (b+  out              c))
    (set! out (b+ (b<< out 4)  #b1110))
    (set! out (b+ (b<< out 4)     op1))
    (set! out (b+ (b<< out 4)     crn))
    (set! out (b+ (b<< out 4)     crd))
    (set! out (b+ (b<< out 4)    cnum))
    (set! out (b+ (b<< out 3)     op2))
    (set! out (b+ (b<< out 5)     crm))
    (emit-instruction t (integer->bytelist out 4))))

(define (encode/copro-register-transfer t c op1 crn rt cnum op2 crm)
  ;; 1 30 9 8   7 6 5 4   3 2 1 20   9 8 7 6   5 4 3 2   1 10 9 8   7 6 5 4   3 2 1 0
  ;; cond     | 1 1 1 0 | op1    0 | crn     | rt      | cnum     | op2   1 | crm
  (let ((out 0))
    (set! out (b+  out              c))
    (set! out (b+ (b<< out 4)  #b1110))
    (set! out (b+ (b<< out 3)     op1))
    (set! out (b+ (b<< out 5)     crn))
    (set! out (b+ (b<< out 4)      rt))
    (set! out (b+ (b<< out 4)    cnum))
    (set! out (b+ (b<< out 3)     op2))
    (set! out (b+ (b<< out 1)     #b1))
    (set! out (b+ (b<< out 4)     crm))
    (emit-instruction t (integer->bytelist out 4))))

(define (encode/software-interrupt t c intno)
  ;; 1 30 9 8   7 6 5 4   3 2 1 20 9 8 7 6 5 4 3 2 1 10 9 8 7 6 5 4 3 2 1 0
  ;; cond     | 1 1 1 1 | intno
  (let ((out 0))
    (set! out (b+  out              c))
    (set! out (b+ (b<< out 4)  #b1111))
    (set! out (b+ (b<< out 24)  intno))
    (emit-instruction t (integer->bytelist out 4))))

(define (encode/multiply-accumulate t c a s rd rn rs rm)
  ;; 1 30 9 8   7 6 5 4   3 2 1 20   9 8 7 6   5 4 3 2   1 10 9 8   7 6 5 4   3 2 1 0
  ;; cond     | 0 0 0 0 | 0 0 a  s | rd      | rn      | rs       | 1 0 0 1 | rm
  (let ((out 0))
    (set! out (b+  out             c))
    (set! out (b+ (b<< out 7)      a))
    (set! out (b+ (b<< out 1)      s))
    (set! out (b+ (b<< out 4)     rd))
    (set! out (b+ (b<< out 4)     rn))
    (set! out (b+ (b<< out 4)     rs))
    (set! out (b+ (b<< out 4) #b1001))
    (set! out (b+ (b<< out 4)     rm))
    (emit-instruction t (integer->bytelist out 4))))

(define (encode/multiply-accumulate-long t c u a s rdhi rdlo rs rm)
  ;; 1 30 9 8   7 6 5 4   3 2 1 20   9 8 7 6   5 4 3 2   1 10 9 8   7 6 5 4   3 2 1 0
  ;; cond     | 0 0 0 0 | 1 u a  s | rdhi    | rdlo    | rs       | 1 0 0 1 | rm
  (let ((out 0))
    (set! out (b+  out             c))
    (set! out (b+ (b<< out 5)      1))
    (set! out (b+ (b<< out 1)      u))
    (set! out (b+ (b<< out 1)      a))
    (set! out (b+ (b<< out 1)      s))
    (set! out (b+ (b<< out 4)   rdhi))
    (set! out (b+ (b<< out 4)   rdlo))
    (set! out (b+ (b<< out 4)     rs))
    (set! out (b+ (b<< out 4) #b1001))
    (set! out (b+ (b<< out 4)     rm))
    (emit-instruction t (integer->bytelist out 4))))

(define (encode/swap t c b rd rn rm)
  ;; 1 30 9 8   7 6 5 4   3 2 1 20   9 8 7 6   5 4 3 2   1 10 9 8   7 6 5 4   3 2 1 0
  ;; cond     | 0 0 0 1 | 0 b 0  0 | rn      | rd      | 0  0 0 0 | 1 0 0 1 | rm
  (let ((out 0))
    (set! out (b+  out             c))
    (set! out (b+ (b<< out 5)   #b10))
    (set! out (b+ (b<< out 1)      b))
    (set! out (b+ (b<< out 6)     rn))
    (set! out (b+ (b<< out 4)     rd))
    (set! out (b+ (b<< out 8) #b1001))
    (set! out (b+ (b<< out 4)     rm))
    (emit-instruction t (integer->bytelist out 4))))

(define (encode/load-store-halfword-register-offset t c p? u? w? l? rd rn rm)
  ;; 1 30 9 8   7 6 5 4   3 2 1 20   9 8 7 6   5 4 3 2   1 10 9 8   7 6 5 4   3 2 1 0
  ;; cond     | 0 0 0 p | u 0 w  l | rn      | rd      | 0  0 0 0 | 1 0 0 1 | rm
  (let ((out 0))
    (set! out (b+  out             c))
    (set! out (b+ (b<< out 4)     p?))
    (set! out (b+ (b<< out 1)     u?))
    (set! out (b+ (b<< out 2)     w?))
    (set! out (b+ (b<< out 1)     l?))
    (set! out (b+ (b<< out 4)     rn))
    (set! out (b+ (b<< out 4)     rd))
    (set! out (b+ (b<< out 8) #b1001))
    (set! out (b+ (b<< out 4)     rm))
    (emit-instruction t (integer->bytelist out 4))))

(define (encode/load-store-halfword-immediate-offset t c p? u? w? l? rd rn imm)
  ;; 1 30 9 8   7 6 5 4   3 2 1 20   9 8 7 6   5 4 3 2   1 10 9 8   7 6 5 4   3 2 1 0
  ;; cond     | 0 0 0 p | u 1 w  l | rn      | rd      | imm4hi   | 1 0 1 1 | imm4lo
  (let ((out 0)
        (immhi (b>> (b& imm #xFF) 4))
        (immlo (b&  (b& imm #x0F))))
    (set! out (b+  out             c))
    (set! out (b+ (b<< out 4)     p?))
    (set! out (b+ (b<< out 1)     u?))
    (set! out (b+ (b<< out 1)      1))
    (set! out (b+ (b<< out 1)     w?))
    (set! out (b+ (b<< out 1)     l?))
    (set! out (b+ (b<< out 4)     rn))
    (set! out (b+ (b<< out 4)     rd))
    (set! out (b+ (b<< out 4)  immhi))
    (set! out (b+ (b<< out 4) #b1011))
    (set! out (b+ (b<< out 4)  immlo))
    (emit-instruction t (integer->bytelist out 4))))

(define (encode/load-store-doubleword-register-offset t c p? u? w? rd rn s? rm)
  ;; 1 30 9 8   7 6 5 4   3 2 1 20   9 8 7 6   5 4 3 2   1 10 9 8   7 6 5 4   3 2 1 0
  ;; cond     | 0 0 0 p | u 0 w  0 | rn      | rd      | 0  0 0 0 | 1 1 s 1 | rm
  (let ((out 0))
    (set! out (b+  out             c))
    (set! out (b+ (b<< out 4)     p?))
    (set! out (b+ (b<< out 1)     u?))
    (set! out (b+ (b<< out 1)      0))
    (set! out (b+ (b<< out 1)     w?))
    (set! out (b+ (b<< out 1)      0))
    (set! out (b+ (b<< out 4)     rn))
    (set! out (b+ (b<< out 4)     rd))
    (set! out (b+ (b<< out 6)   #b11))
    (set! out (b+ (b<< out 1)     s?))
    (set! out (b+ (b<< out 1)      1))
    (set! out (b+ (b<< out 4)     rm))
    (emit-instruction t (integer->bytelist out 4))))

(define (encode/load-store-doubleword-immediate-offset t c p? u? w? rd rn s? imm)
  ;; 1 30 9 8   7 6 5 4   3 2 1 20   9 8 7 6   5 4 3 2   1 10 9 8   7 6 5 4   3 2 1 0
  ;; cond     | 0 0 0 p | u 1 w  0 | rn      | rd      | imm4hi   | 1 1 s 1 | imm4lo
  (let ((out 0)
        (immhi (b>> (b& imm #xFF) 4))
        (immlo (b&  (b& imm #x0F))))
    (set! out (b+  out             c))
    (set! out (b+ (b<< out 4)     p?))
    (set! out (b+ (b<< out 1)     u?))
    (set! out (b+ (b<< out 1)      1))
    (set! out (b+ (b<< out 1)     w?))
    (set! out (b+ (b<< out 1)      0))
    (set! out (b+ (b<< out 4)     rn))
    (set! out (b+ (b<< out 4)     rd))
    (set! out (b+ (b<< out 4)  immhi))
    (set! out (b+ (b<< out 2)   #b11))
    (set! out (b+ (b<< out 1)     s?))
    (set! out (b+ (b<< out 1)      1))
    (set! out (b+ (b<< out 4)  immlo))
    (emit-instruction t (integer->bytelist out 4))))

(define (encode/load-store-signed-halfword-register-offset t c p? u? w? h? rd rn h rm)
  ;; 1 30 9 8   7 6 5 4   3 2 1 20   9 8 7 6   5 4 3 2   1 10 9 8   7 6 5 4   3 2 1 0
  ;; cond     | 0 0 0 p | u 0 w  1 | rn      | rd      | 0  0 0 0 | 1 1 h 1 | rm
  (let ((out 0))
    (set! out (b+  out             c))
    (set! out (b+ (b<< out 4)     p?))
    (set! out (b+ (b<< out 1)     u?))
    (set! out (b+ (b<< out 1)      0))
    (set! out (b+ (b<< out 1)     w?))
    (set! out (b+ (b<< out 1)      1))
    (set! out (b+ (b<< out 4)     rn))
    (set! out (b+ (b<< out 4)     rd))
    (set! out (b+ (b<< out 6)   #b11))
    (set! out (b+ (b<< out 1)     h?))
    (set! out (b+ (b<< out 1)      1))
    (set! out (b+ (b<< out 4)     rm))
    (emit-instruction t (integer->bytelist out 4))))

(define (encode/load-store-signed-halfword-immediate-offset t c p? u? w? h? rd rn h imm)
  ;; 1 30 9 8   7 6 5 4   3 2 1 20   9 8 7 6   5 4 3 2   1 10 9 8   7 6 5 4   3 2 1 0
  ;; cond     | 0 0 0 p | u 1 w  1 | rn      | rd      | imm4hi   | 1 1 h 1 | imm4lo
  (let ((out 0)
        (immhi (b>> (b& imm #xFF) 4))
        (immlo (b&  (b& imm #x0F))))
    (set! out (b+  out             c))
    (set! out (b+ (b<< out 4)     p?))
    (set! out (b+ (b<< out 1)     u?))
    (set! out (b+ (b<< out 1)      1))
    (set! out (b+ (b<< out 1)     w?))
    (set! out (b+ (b<< out 1)      1))
    (set! out (b+ (b<< out 4)     rn))
    (set! out (b+ (b<< out 4)     rd))
    (set! out (b+ (b<< out 4)  immhi))
    (set! out (b+ (b<< out 2)   #b11))
    (set! out (b+ (b<< out 1)     h?))
    (set! out (b+ (b<< out 1)      1))
    (set! out (b+ (b<< out 4)  immlo))
    (emit-instruction t (integer->bytelist out 4))))

(define (encode/move-from-status-register t c r rd)
  ;; 1 30 9 8   7 6 5 4   3 2 1 20   9 8 7 6   5 4 3 2   1 10 9 8   7 6 5 4   3 2 1 0
  ;; cond     | 0 0 0 1 | 0 r 0  0 | 1 1 1 1 | rd      | 0  0 0 0 | 0 0 0 0 | 0 0 0 0
  (let ((out 0))
    (set! out (b+  out             c))
    (set! out (b+ (b<< out 5)   #b10))
    (set! out (b+ (b<< out 1)      r))
    (set! out (b+ (b<< out 6) #b1111))
    (set! out (b+ (b<< out 4)     rd))
    (set! out     (b<< out 12))
    (emit-instruction t (integer->bytelist out 4))))

(define (encode/move-to-status-register t c r mask rm)
  ;; 1 30 9 8   7 6 5 4   3 2 1 20   9 8 7 6   5 4 3 2   1 10 9 8   7 6 5 4   3 2 1 0
  ;; cond     | 0 0 0 1 | 0 r 1  0 | mask    | 1 1 1 1 | 0  0 0 0 | 0 0 0 0 | rm
  (let ((out 0))
    (set! out (b+  out              c))
    (set! out (b+ (b<< out 5)    #b10))
    (set! out (b+ (b<< out 1)       r))
    (set! out (b+ (b<< out 2)    #b10))
    (set! out (b+ (b<< out 4)    mask))
    (set! out (b+ (b<< out 4)  #b1111))
    (set! out (b+ (b<< out 12)     rm))
    (emit-instruction t (integer->bytelist out 4))))

(define (encode/move-immediate-to-status-register t c r mask imm)
  ;; 1 30 9 8   7 6 5   4 3 2 1 20   9 8 7 6   5 4 3 2   1 10 9 8 7 6 5 4 3 2 1 0
  ;; cond     | 0 0 1 | 1 0 r 1  0 | mask    | 1 1 1 1 | imm12
  (let ((out 0))
    (set! out (b+  out              c))
    (set! out (b+ (b<< out 5) #b00110))
    (set! out (b+ (b<< out 1)       r))
    (set! out (b+ (b<< out 2)    #b10))
    (set! out (b+ (b<< out 4)    mask))
    (set! out (b+ (b<< out 4)  #b1111))
    (set! out (b+ (b<< out 12)    imm))
    (emit-instruction t (integer->bytelist out 4))))


(define (encode/clz t c rd rm)
  ;; 1 30 9 8   7 6 5 4   3 2 1 20   9 8 7 6   5 4 3 2   1 10 9 8   7 6 5 4   3 2 1 0
  ;; cond     | 0 0 0 1 | 0 1 1  0 | 1 1 1 1 | rd      | 1  1 1 1 | 0 0 0 1 | rm
  (let ((out 0))
    (set! out (b+  out                       c))
    (set! out (b+ (b<< out 12)  #b000101101111))
    (set! out (b+ (b<< out 4)               rd))
    (set! out (b+ (b<< out 8)       #b11110001))
    (set! out (b+ (b<< out 4)               rm))
    (emit-instruction t (integer->bytelist out 4))))

(define (encode/branch-link-exchange t c rm)
  ;; 1 30 9 8   7 6 5 4   3 2 1 20   9 8 7 6   5 4 3 2   1 10 9 8   7 6 5 4   3 2 1 0
  ;; cond     | 0 0 0 1 | 0 0 1  0 | 1 1 1 1 | 1 1 1 1 | 1  1 1 1 | 0 0 1 1 | rm
  (let ((out 0))
    (set! out (b+  out                                   c))
    (set! out (b+ (b<< out 24)  #b000100101111111111110011))
    (set! out (b+ (b<< out 4)                           rm))
    (emit-instruction t (integer->bytelist out 4))))
