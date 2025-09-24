;; token-fee-redistribute.clar
;; SIP-010-like token with fee-on-transfer and redistribution (dividend) to token holders.
;; - Fee-on-transfer (fee-bps out of BPS) is taken from the transfer amount.
;; - Fee tokens are added to the contract's balance and redistributed via magnifiedDividendPerShare.
;; - Holders call withdraw-dividend to claim their proportional share (paid from contract's balance).
;; - Owner can mint and burn; mint/burn adjust dividend-tracking so holders' entitlements remain correct.

(define-constant BPS u10000)
(define-constant MAGNITUDE u1000000000000000000) ;; 1e18 magnification for precision

;; ------------------------
;; Errors
;; ------------------------
(define-constant ERR_NOT_OWNER u100)
(define-constant ERR_INSUFFICIENT_BALANCE u101)
(define-constant ERR_BAD_AMOUNT u102)
(define-constant ERR_TRANSFER_FAIL u103)
(define-constant ERR_NO_WITHDRAW u104)
(define-constant ERR_MATH u105)

;; ------------------------
;; Metadata
;; ------------------------
(define-constant token-name "FeeRedistributeToken")
(define-constant token-symbol "FRT")
(define-constant token-decimals u6)

;; ------------------------
;; Admin / config
;; ------------------------
(define-data-var owner principal tx-sender)
(define-data-var fee-bps uint u50) ;; default 0.50% (50 / 10000)
(define-data-var fee-recipient (optional principal) none) ;; optional fallback recipient if no holders to distribute to

;; ------------------------
;; Supply & balances
;; ------------------------
(define-data-var total-supply uint u0)
(define-map balances { account: principal } { balance: uint })

;; ------------------------
;; Dividend bookkeeping (magnified-per-share)
;; ------------------------
;; magnifiedDividendPerShare is uint scaled by MAGNITUDE
(define-data-var magnifiedDividendPerShare uint u0)
;; per-account correction stored as integer
(define-map magnifiedDividendCorrections { who: principal } { corr: int })
;; withdrawn dividends stored as uint
(define-map withdrawnDividends { who: principal } { amount: uint })

;; ------------------------
;; Helpers: balances & corrections
;; ------------------------
(define-private (get-balance (who principal))
  (default-to u0 (get balance (map-get? balances { account: who }))))

(define-private (set-balance (who principal) (amt uint))
  (map-set balances { account: who } { balance: amt }))

(define-private (get-correction (who principal))
  (default-to 0 (get corr (map-get? magnifiedDividendCorrections { who: who }))))

(define-private (set-correction (who principal) (v int))
  (map-set magnifiedDividendCorrections { who: who } { corr: v }))

(define-private (get-withdrawn (who principal))
  (default-to u0 (get amount (map-get? withdrawnDividends { who: who }))))

(define-private (set-withdrawn (who principal) (v uint))
  (map-set withdrawnDividends { who: who } { amount: v }))

;; ------------------------
;; Read-only: metadata & supply
;; ------------------------
(define-read-only (get-name) (ok token-name))
(define-read-only (get-symbol) (ok token-symbol))
(define-read-only (get-decimals) (ok token-decimals))
(define-read-only (get-total-supply) (ok (var-get total-supply)))

;; ------------------------
;; Dividend accounting view helpers
;; ------------------------
;; calculate base dividend for account
(define-private (calculate-dividend (who principal))
  (let ((bal (get-balance who))
        (mdps (var-get magnifiedDividendPerShare))
        (corr (get-correction who)))
    (let ((total-div (/ (* bal mdps) MAGNITUDE)))
      (if (< corr 0)
          u0
          (+ total-div (to-uint corr))))))

;; public view for accumulated dividend
(define-read-only (accumulative-dividend (who principal))
  (ok (calculate-dividend who)))

;; dividend available to withdraw = accumulative - withdrawn
(define-read-only (dividend-of (who principal))
  (let ((acc (calculate-dividend who))
        (wd (get-withdrawn who)))
    (ok (if (>= acc wd) (- acc wd) u0))))

;; ------------------------
;; Admin functions
;; ------------------------
(define-public (set-owner (p principal))
  (begin
    (asserts! (is-eq tx-sender (var-get owner)) (err ERR_NOT_OWNER))
    (var-set owner p)
    (ok true)))

(define-public (set-fee (bps uint) (fallback (optional principal)))
  (begin
    (asserts! (is-eq tx-sender (var-get owner)) (err ERR_NOT_OWNER))
    (asserts! (<= bps BPS) (err ERR_MATH))
    (var-set fee-bps bps)
    (var-set fee-recipient fallback)
    (ok true)))

;; ------------------------
;; Internal: distribute fee to holders by updating magnifiedDividendPerShare
;; fee-amt is in token units and already credited to contract balance
;; ------------------------
(define-private (distribute-fee (fee-amt uint))
  (let ((ts (var-get total-supply))
        (contract-bal (get-balance (as-contract tx-sender))))
    (let ((eligible (if (> ts contract-bal) (- ts contract-bal) u0)))
      (if (<= eligible u0)
          ;; no eligible holders; send fee to fallback if set, otherwise keep in contract balance
          (match (var-get fee-recipient) 
            recipient (begin
                       (asserts! (>= contract-bal fee-amt) (err ERR_TRANSFER_FAIL))
                       (set-balance (as-contract tx-sender) (- contract-bal fee-amt))
                       (set-balance recipient (+ (get-balance recipient) fee-amt))
                       (ok true))
            (ok true))
          ;; normal distribution: increment magnifiedDividendPerShare
          (begin
            (var-set magnifiedDividendPerShare
                     (+ (var-get magnifiedDividendPerShare) (/ (* fee-amt MAGNITUDE) eligible)))
            (ok true))))))

;; ------------------------
;; Mint & Burn (owner only). Adjust corrections to keep dividend accounting consistent.
;; ------------------------
(define-public (mint (to principal) (amount uint))
  (begin
    (asserts! (is-eq tx-sender (var-get owner)) (err ERR_NOT_OWNER))
    (asserts! (> amount u0) (err ERR_BAD_AMOUNT))
    ;; increase supply and balance
    (var-set total-supply (+ (var-get total-supply) amount))
    (set-balance to (+ (get-balance to) amount))
    ;; adjust correction: correction[to] -= magnifiedDividendPerShare * amount
    (let ((mdps (var-get magnifiedDividendPerShare))
          (corr (get-correction to)))
      (set-correction to (- corr (* (to-int mdps) (to-int amount)))))
    (ok true)))

(define-public (burn (from principal) (amount uint))
  (begin
    (asserts! (is-eq tx-sender (var-get owner)) (err ERR_NOT_OWNER))
    (asserts! (>= (get-balance from) amount) (err ERR_INSUFFICIENT_BALANCE))
    ;; decrease balance and supply
    (set-balance from (- (get-balance from) amount))
    (var-set total-supply (- (var-get total-supply) amount))
    ;; adjust correction: correction[from] += magnifiedDividendPerShare * amount
    (let ((mdps (var-get magnifiedDividendPerShare))
          (corr (get-correction from)))
      (set-correction from (+ corr (* (to-int mdps) (to-int amount)))))
    (ok true)))

;; ------------------------
;; Transfer (SIP-010 style)
;; transfer(amount, sender, recipient, memo)
;; - sender must be tx-sender in UX; enforce that
;; - fee is applied and redistributed
;; ------------------------
(define-public (transfer (amount uint) (sender principal) (recipient principal) (memo (optional (buff 34))))
  (begin
    (asserts! (is-eq tx-sender sender) (err ERR_TRANSFER_FAIL))
    (asserts! (> amount u0) (err ERR_BAD_AMOUNT))
    (asserts! (>= (get-balance sender) amount) (err ERR_INSUFFICIENT_BALANCE))
    ;; compute fee and net
    (let ((bps (var-get fee-bps)))
      (let ((fee (if (> bps u0) (/ (* amount bps) BPS) u0))
            (net (if (> bps u0) (- amount (/ (* amount bps) BPS)) amount)))
        ;; 1) deduct full amount from sender
        (set-balance sender (- (get-balance sender) amount))
        ;; update correction for sender: corr += mdps * amount
        (let ((mdps (var-get magnifiedDividendPerShare))
              (sc (get-correction sender)))
          (set-correction sender (+ sc (* (to-int mdps) (to-int amount)))))
        ;; 2) credit net to recipient
        (set-balance recipient (+ (get-balance recipient) net))
        ;; update correction for recipient: corr -= mdps * net
        (let ((rc (get-correction recipient))
              (mdpsr (var-get magnifiedDividendPerShare)))
          (set-correction recipient (- rc (* (to-int mdpsr) (to-int net)))))
        ;; 3) handle fee: credit to contract balance and distribute
        (if (> fee u0)
            (begin
                (set-balance (as-contract tx-sender) (+ (get-balance (as-contract tx-sender)) fee))
                ;; contract should not receive dividends on its own balance -> adjust its correction
                (let ((cc (get-correction (as-contract tx-sender)))
                      (mdpsc (var-get magnifiedDividendPerShare)))
                    (set-correction (as-contract tx-sender) (- cc (* (to-int mdpsc) (to-int fee)))))
                ;; distribute fee to holders
                (try! (distribute-fee fee))
                (ok true))
            (ok true))))))

;; ------------------------
;; Withdraw dividend (claim)
;; Pays from contract balance
;; ------------------------
(define-public (withdraw-dividend)
  (let ((owed (unwrap! (dividend-of tx-sender) (err ERR_NO_WITHDRAW))))
    (asserts! (> owed u0) (err ERR_NO_WITHDRAW))
    (let ((contract-bal (get-balance (as-contract tx-sender))))
      (asserts! (>= contract-bal owed) (err ERR_TRANSFER_FAIL))
      ;; update withdrawn map
      (set-withdrawn tx-sender (+ (get-withdrawn tx-sender) owed))
      ;; transfer from contract balance to sender
      (set-balance (as-contract tx-sender) (- contract-bal owed))
      (set-balance tx-sender (+ (get-balance tx-sender) owed))
      (ok owed))))

;; ------------------------
;; Views: balances, fee, withdrawn, dividend
;; ------------------------
(define-read-only (get-balance-of (who principal))
  (ok (get-balance who)))

(define-read-only (get-fee-bps) 
  (ok (var-get fee-bps)))

(define-read-only (get-magnified-dividend-per-share) 
  (ok (var-get magnifiedDividendPerShare)))

(define-read-only (get-withdrawn-amount (who principal)) 
  (ok (get-withdrawn who)))

(define-read-only (dividend-available (who principal)) 
  (dividend-of who))