;; LensChain: Photography Portfolio Management System
;; Version: 1.0.0

;; Constants
(define-constant STUDIO_CAPACITY u2900000)
(define-constant BASE_PHOTOGRAPHY_REWARD u44)
(define-constant COMPOSITION_BONUS u24)
(define-constant MAX_PHOTOGRAPHER_LEVEL u26)
(define-constant ERR_INVALID_PHOTO_SESSION u1)
(define-constant ERR_NO_LENS_TOKENS u2)
(define-constant ERR_STUDIO_CAPACITY_EXCEEDED u3)
(define-constant BLOCKS_PER_PHOTOGRAPHY_CYCLE u2592)
(define-constant GEAR_INVESTMENT_MULTIPLIER u16)
(define-constant MIN_INVESTMENT_PERIOD u1296)
(define-constant EARLY_LIQUIDATION_PENALTY u32)

;; Data Variables
(define-data-var total-lens-tokens-distributed uint u0)
(define-data-var total-photo-sessions uint u0)
(define-data-var studio-manager principal tx-sender)

;; Data Maps
(define-map photographer-sessions principal uint)
(define-map photographer-lens-tokens principal uint)
(define-map photo-session-start-time principal uint)
(define-map photographer-skill-level principal uint)
(define-map photographer-last-session principal uint)
(define-map photographer-gear-investments principal uint)
(define-map photographer-investment-start-block principal uint)
(define-map photography-genre-focus principal uint)
(define-map photographer-portfolio-pieces principal uint)
(define-map technical-mastery-level principal uint)

;; Public Functions
(define-public (start-photo-session (genre-type uint) (technical-complexity uint))
  (let
    (
      (photographer tx-sender)
    )
    (asserts! (and (> genre-type u0) (> technical-complexity u0) (<= technical-complexity u100)) (err ERR_INVALID_PHOTO_SESSION))
    (map-set photo-session-start-time photographer burn-block-height)
    (map-set photography-genre-focus photographer genre-type)
    (ok true)
  ))

(define-public (complete-photo-session (technical-complexity uint) (artistic-score uint))
  (let
    (
      (photographer tx-sender)
      (start-block (default-to u0 (map-get? photo-session-start-time photographer)))
      (blocks-shooting (- burn-block-height start-block))
      (last-session-block (default-to u0 (map-get? photographer-last-session photographer)))
      (skill-level (default-to u0 (map-get? photographer-skill-level photographer)))
      (capped-skill (if (<= skill-level MAX_PHOTOGRAPHER_LEVEL) skill-level MAX_PHOTOGRAPHER_LEVEL))
      (technical-bonus (default-to u0 (map-get? technical-mastery-level photographer)))
      (artistic-bonus (/ (* artistic-score u22) u100))
      (complexity-bonus (/ technical-complexity u3))
      (photography-reward (+ BASE_PHOTOGRAPHY_REWARD (* capped-skill COMPOSITION_BONUS) technical-bonus artistic-bonus complexity-bonus))
    )
    (asserts! (and (> start-block u0) (>= blocks-shooting (/ technical-complexity u30)) (<= artistic-score u100)) (err ERR_INVALID_PHOTO_SESSION))
    
    (map-set photographer-sessions photographer (+ (default-to u0 (map-get? photographer-sessions photographer)) u1))
    (map-set photographer-lens-tokens photographer (+ (default-to u0 (map-get? photographer-lens-tokens photographer)) photography-reward))
    
    (if (< (- burn-block-height last-session-block) BLOCKS_PER_PHOTOGRAPHY_CYCLE)
      (map-set photographer-skill-level photographer (+ skill-level u1))
      (map-set photographer-skill-level photographer u1)
    )
    
    (if (>= artistic-score u95)
      (begin
        (map-set photographer-portfolio-pieces photographer (+ (default-to u0 (map-get? photographer-portfolio-pieces photographer)) u1))
        (map-set technical-mastery-level photographer (+ technical-bonus u16))
      )
      true
    )
    
    (map-set photographer-last-session photographer burn-block-height)
    (var-set total-photo-sessions (+ (var-get total-photo-sessions) u1))
    (var-set total-lens-tokens-distributed (+ (var-get total-lens-tokens-distributed) photography-reward))
    
    (asserts! (<= (var-get total-lens-tokens-distributed) STUDIO_CAPACITY) (err ERR_STUDIO_CAPACITY_EXCEEDED))
    (ok photography-reward)
  ))

(define-public (claim-lens-rewards)
  (let
    (
      (photographer tx-sender)
      (token-balance (default-to u0 (map-get? photographer-lens-tokens photographer)))
    )
    (asserts! (> token-balance u0) (err ERR_NO_LENS_TOKENS))
    (map-set photographer-lens-tokens photographer u0)
    (ok token-balance)
  ))

;; Camera Gear Investment Features
(define-public (invest-in-camera-gear (amount uint))
  (let
    (
      (photographer tx-sender)
    )
    (asserts! (> amount u0) (err ERR_INVALID_PHOTO_SESSION))
    (asserts! (>= (var-get total-lens-tokens-distributed) amount) (err ERR_STUDIO_CAPACITY_EXCEEDED))
    
    (map-set photographer-gear-investments photographer amount)
    (map-set photographer-investment-start-block photographer burn-block-height)
    (var-set total-lens-tokens-distributed (- (var-get total-lens-tokens-distributed) amount))
    (ok amount)
  ))

(define-public (liquidate-gear-investment)
  (let
    (
      (photographer tx-sender)
      (invested-amount (default-to u0 (map-get? photographer-gear-investments photographer)))
      (investment-start-block (default-to u0 (map-get? photographer-investment-start-block photographer)))
      (blocks-invested (- burn-block-height investment-start-block))
      (penalty (if (< blocks-invested MIN_INVESTMENT_PERIOD) (/ (* invested-amount EARLY_LIQUIDATION_PENALTY) u100) u0))
      (investment-bonus (if (>= blocks-invested MIN_INVESTMENT_PERIOD) (/ (* invested-amount GEAR_INVESTMENT_MULTIPLIER) u100) u0))
      (final-amount (+ (- invested-amount penalty) investment-bonus))
    )
    (asserts! (> invested-amount u0) (err ERR_NO_LENS_TOKENS))
    
    (map-set photographer-gear-investments photographer u0)
    (map-set photographer-investment-start-block photographer u0)
    (var-set total-lens-tokens-distributed (+ (var-get total-lens-tokens-distributed) final-amount))
    (ok final-amount)
  ))

(define-public (launch-photo-exhibition (exhibition-title (string-utf8 128)) (photo-count uint))
  (let
    (
      (photographer tx-sender)
      (skill-level (default-to u0 (map-get? photographer-skill-level photographer)))
      (portfolio-pieces (default-to u0 (map-get? photographer-portfolio-pieces photographer)))
      (exhibition-bonus (+ (* photo-count u75) (* portfolio-pieces u40) (* skill-level u32)))
    )
    (asserts! (and (> (len exhibition-title) u0) (>= skill-level u20) (> photo-count u0)) (err ERR_INVALID_PHOTO_SESSION))
    
    (map-set photographer-lens-tokens photographer (+ (default-to u0 (map-get? photographer-lens-tokens photographer)) exhibition-bonus))
    (var-set total-lens-tokens-distributed (+ (var-get total-lens-tokens-distributed) exhibition-bonus))
    
    (ok exhibition-bonus)
  ))

(define-public (host-photography-workshop (student-count uint) (workshop-hours uint))
  (let
    (
      (photographer tx-sender)
      (skill-level (default-to u0 (map-get? photographer-skill-level photographer)))
      (technical-mastery (default-to u0 (map-get? technical-mastery-level photographer)))
      (workshop-bonus (+ (* student-count u55) (* workshop-hours u30) (* technical-mastery u12)))
    )
    (asserts! (and (> student-count u0) (> workshop-hours u0) (>= skill-level u24)) (err ERR_INVALID_PHOTO_SESSION))
    
    (map-set photographer-lens-tokens photographer (+ (default-to u0 (map-get? photographer-lens-tokens photographer)) workshop-bonus))
    (var-set total-lens-tokens-distributed (+ (var-get total-lens-tokens-distributed) workshop-bonus))
    
    (ok workshop-bonus)
  ))

(define-public (enter-photo-contest (contest-level uint) (entry-fee uint))
  (let
    (
      (photographer tx-sender)
      (skill-level (default-to u0 (map-get? photographer-skill-level photographer)))
      (portfolio-pieces (default-to u0 (map-get? photographer-portfolio-pieces photographer)))
      (contest-bonus (+ (* contest-level u65) (* portfolio-pieces u25)))
    )
    (asserts! (and (> contest-level u0) (>= skill-level u16) (> entry-fee u0)) (err ERR_INVALID_PHOTO_SESSION))
    (asserts! (>= (var-get total-lens-tokens-distributed) entry-fee) (err ERR_STUDIO_CAPACITY_EXCEEDED))
    
    (map-set photographer-lens-tokens photographer (+ (default-to u0 (map-get? photographer-lens-tokens photographer)) contest-bonus))
    (var-set total-lens-tokens-distributed (+ (- (var-get total-lens-tokens-distributed) entry-fee) contest-bonus))
    
    (ok contest-bonus)
  ))

;; Read-Only Functions
(define-read-only (get-photo-session-count (user principal))
  (default-to u0 (map-get? photographer-sessions user)))

(define-read-only (get-lens-token-balance (user principal))
  (default-to u0 (map-get? photographer-lens-tokens user)))

(define-read-only (get-photographer-skill-level (user principal))
  (default-to u0 (map-get? photographer-skill-level user)))

(define-read-only (get-portfolio-pieces (user principal))
  (default-to u0 (map-get? photographer-portfolio-pieces user)))

(define-read-only (get-gear-investments (user principal))
  (default-to u0 (map-get? photographer-gear-investments user)))

(define-read-only (get-technical-mastery (user principal))
  (default-to u0 (map-get? technical-mastery-level user)))

(define-read-only (get-studio-stats)
  {
    total-photo-sessions: (var-get total-photo-sessions),
    total-lens-tokens-distributed: (var-get total-lens-tokens-distributed),
    studio-capacity: STUDIO_CAPACITY
  })

(define-read-only (calculate-photography-reward (skill-level uint) (artistic-score uint) (technical-bonus uint) (complexity uint))
  (let
    (
      (capped-skill (if (<= skill-level MAX_PHOTOGRAPHER_LEVEL) skill-level MAX_PHOTOGRAPHER_LEVEL))
      (artistic-bonus (/ (* artistic-score u22) u100))
      (complexity-bonus (/ complexity u3))
    )
    (+ BASE_PHOTOGRAPHY_REWARD (* capped-skill COMPOSITION_BONUS) technical-bonus artistic-bonus complexity-bonus)
  ))

;; Private Functions
(define-private (is-studio-manager)
  (is-eq tx-sender (var-get studio-manager)))

(define-private (validate-photography-parameters (technical-complexity uint) (artistic-score uint))
  (and (> technical-complexity u0) (<= artistic-score u100)))