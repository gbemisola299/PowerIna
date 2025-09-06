;;Powerina- Tokenized Renewable Energy Credits for Solar Microgrids
;; Empowering African neighborhoods to crowdfund solar infrastructure and track energy distribution

;; =============================================================================
;; CONSTANTS AND ERROR CODES
;; =============================================================================

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INSUFFICIENT-BALANCE (err u101))
(define-constant ERR-INVALID-AMOUNT (err u102))
(define-constant ERR-MICROGRID-NOT-FOUND (err u103))
(define-constant ERR-ALREADY-EXISTS (err u104))
(define-constant ERR-INVALID-RECIPIENT (err u105))

;; Contract constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant TOKEN-NAME "lorePower Energy Credits")
(define-constant TOKEN-SYMBOL "LPEC")
(define-constant TOKEN-DECIMALS u6)
(define-constant TOKEN-URI u"https://lorepower.africa/token-metadata")

;; =============================================================================
;; DATA VARIABLES AND MAPS
;; =============================================================================

;; SIP-010 compliant token variables
(define-fungible-token energy-credits)
(define-data-var token-total-supply uint u0)

;; Microgrid tracking
(define-map microgrids
  { microgrid-id: uint }
  {
    name: (string-ascii 50),
    location: (string-ascii 100),
    capacity-kwh: uint,
    installed-capacity: uint,
    status: (string-ascii 20),
    created-at: uint,
    owner: principal
  }
)

;; Energy production tracking
(define-map energy-production
  { microgrid-id: uint, period: uint }
  {
    kwh-produced: uint,
    kwh-consumed: uint,
    kwh-surplus: uint,
    credits-minted: uint,
    timestamp: uint
  }
)

;; User balances and participation
(define-map user-profiles
  { user: principal }
  {
    total-contributed: uint,
    total-credits-earned: uint,
    microgrids-supported: (list 10 uint),
    reputation-score: uint
  }
)

;; Global counters
(define-data-var next-microgrid-id uint u1)
(define-data-var total-energy-produced uint u0)
(define-data-var total-credits-minted uint u0)


;; SIP-010 FUNGIBLE TOKEN IMPLEMENTATION
;; =============================================================================

(define-read-only (get-name)
  (ok TOKEN-NAME)
)

(define-read-only (get-symbol)
  (ok TOKEN-SYMBOL)
)

(define-read-only (get-decimals)
  (ok TOKEN-DECIMALS)
)

(define-read-only (get-balance (who principal))
  (ok (ft-get-balance energy-credits who))
)

(define-read-only (get-total-supply)
  (ok (var-get token-total-supply))
)

(define-read-only (get-token-uri)
  (ok (some TOKEN-URI))
)


;; MICROGRID MANAGEMENT FUNCTIONS
;; =============================================================================

;; Register a new microgrid for solar installation
(define-public (register-microgrid (name (string-ascii 50)) (location (string-ascii 100)) (capacity-kwh uint))
  (let ((microgrid-id (var-get next-microgrid-id)))
    (asserts! (> capacity-kwh u0) ERR-INVALID-AMOUNT)
    (asserts! (is-none (map-get? microgrids { microgrid-id: microgrid-id })) ERR-ALREADY-EXISTS)

    (map-set microgrids
      { microgrid-id: microgrid-id }
      {
        name: name,
        location: location,
        capacity-kwh: capacity-kwh,
        installed-capacity: u0,
        status: "pending",
        created-at: block-height,
        owner: tx-sender
      }
    )

    (var-set next-microgrid-id (+ microgrid-id u1))
    (ok microgrid-id)
  )
)

;; Get microgrid information
(define-read-only (get-microgrid (microgrid-id uint))
  (map-get? microgrids { microgrid-id: microgrid-id })
)

;; =============================================================================
;; CROWDFUNDING SYSTEM
;; =============================================================================

;; Crowdfunding campaigns for microgrid installations
(define-map crowdfunding-campaigns
  { microgrid-id: uint }
  {
    target-amount: uint,
    raised-amount: uint,
    deadline: uint,
    status: (string-ascii 20),
    contributors-count: uint
  }
)

;; Individual contributions tracking
(define-map contributions
  { microgrid-id: uint, contributor: principal }
  {
    amount: uint,
    timestamp: uint,
    credits-allocated: uint
  }
)

;; Start a crowdfunding campaign for a microgrid
(define-public (start-crowdfunding (microgrid-id uint) (target-amount uint) (duration-blocks uint))
  (let ((microgrid (unwrap! (map-get? microgrids { microgrid-id: microgrid-id }) ERR-MICROGRID-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get owner microgrid)) ERR-NOT-AUTHORIZED)
    (asserts! (> target-amount u0) ERR-INVALID-AMOUNT)
    (asserts! (> duration-blocks u0) ERR-INVALID-AMOUNT)

    (map-set crowdfunding-campaigns
      { microgrid-id: microgrid-id }
      {
        target-amount: target-amount,
        raised-amount: u0,
        deadline: (+ block-height duration-blocks),
        status: "active",
        contributors-count: u0
      }
    )

    (ok true)
  )
)

;; Contribute STX to a microgrid crowdfunding campaign
(define-public (contribute-to-microgrid (microgrid-id uint) (amount uint))
  (let (
    (campaign (unwrap! (map-get? crowdfunding-campaigns { microgrid-id: microgrid-id }) ERR-MICROGRID-NOT-FOUND))
    (existing-contribution (default-to { amount: u0, timestamp: u0, credits-allocated: u0 }
                           (map-get? contributions { microgrid-id: microgrid-id, contributor: tx-sender })))
  )
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (is-eq (get status campaign) "active") ERR-NOT-AUTHORIZED)
    (asserts! (<= block-height (get deadline campaign)) ERR-NOT-AUTHORIZED)

    ;; Transfer STX to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))

    ;; Update contribution record
    (map-set contributions
      { microgrid-id: microgrid-id, contributor: tx-sender }
      {
        amount: (+ (get amount existing-contribution) amount),
        timestamp: block-height,
        credits-allocated: (get credits-allocated existing-contribution)
      }
    )

    ;; Update campaign totals
    (map-set crowdfunding-campaigns
      { microgrid-id: microgrid-id }
      (merge campaign {
        raised-amount: (+ (get raised-amount campaign) amount),
        contributors-count: (if (is-eq (get amount existing-contribution) u0)
                             (+ (get contributors-count campaign) u1)
                             (get contributors-count campaign))
      })
    )

    ;; Update user profile
    (update-user-contribution tx-sender amount microgrid-id)

    (ok true)
  )
)


;; HELPER FUNCTIONS
;; =============================================================================

;; Update user contribution profile
(define-private (update-user-contribution (user principal) (amount uint) (microgrid-id uint))
  (let ((profile (default-to
                   { total-contributed: u0, total-credits-earned: u0, microgrids-supported: (list), reputation-score: u0 }
                   (map-get? user-profiles { user: user }))))
    (map-set user-profiles
      { user: user }
      (merge profile {
        total-contributed: (+ (get total-contributed profile) amount),
        microgrids-supported: (unwrap-panic (as-max-len?
                                (append (get microgrids-supported profile) microgrid-id) u10))
      })
    )
    true
  )
)

;; Get user profile information
(define-read-only (get-user-profile (user principal))
  (map-get? user-profiles { user: user })
)

;; Get crowdfunding campaign information
(define-read-only (get-crowdfunding-campaign (microgrid-id uint))
  (map-get? crowdfunding-campaigns { microgrid-id: microgrid-id })
)

;; Get user's contribution to a specific microgrid
(define-read-only (get-user-contribution (microgrid-id uint) (user principal))
  (map-get? contributions { microgrid-id: microgrid-id, contributor: user })
)

;; Check if crowdfunding campaign is successful
(define-read-only (is-campaign-successful (microgrid-id uint))
  (match (map-get? crowdfunding-campaigns { microgrid-id: microgrid-id })
    campaign (>= (get raised-amount campaign) (get target-amount campaign))
    false
  )
)

;; =============================================================================
;; ENERGY PRODUCTION AND DISTRIBUTION
;; =============================================================================

;; Record energy production for a microgrid
(define-public (record-energy-production (microgrid-id uint) (kwh-produced uint) (kwh-consumed uint))
  (let (
    (microgrid (unwrap! (map-get? microgrids { microgrid-id: microgrid-id }) ERR-MICROGRID-NOT-FOUND))
    (period block-height)
    (kwh-surplus (if (> kwh-produced kwh-consumed) (- kwh-produced kwh-consumed) u0))
    (credits-to-mint (/ kwh-surplus u10)) ;; 1 credit per 10 kWh surplus
  )
    (asserts! (is-eq tx-sender (get owner microgrid)) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status microgrid) "active") ERR-NOT-AUTHORIZED)

    ;; Record production data
    (map-set energy-production
      { microgrid-id: microgrid-id, period: period }
      {
        kwh-produced: kwh-produced,
        kwh-consumed: kwh-consumed,
        kwh-surplus: kwh-surplus,
        credits-minted: credits-to-mint,
        timestamp: block-height
      }
    )

    ;; Mint energy credits for surplus production
    (if (> credits-to-mint u0)
      (begin
        (try! (ft-mint? energy-credits credits-to-mint (get owner microgrid)))
        (var-set token-total-supply (+ (var-get token-total-supply) credits-to-mint))
        (var-set total-credits-minted (+ (var-get total-credits-minted) credits-to-mint))
      )
      true
    )

    ;; Update global energy tracking
    (var-set total-energy-produced (+ (var-get total-energy-produced) kwh-produced))

    (ok credits-to-mint)
  )
)

;; Energy trading between users
(define-map energy-trades
  { trade-id: uint }
  {
    seller: principal,
    buyer: principal,
    credits-amount: uint,
    price-per-credit: uint,
    total-price: uint,
    status: (string-ascii 20),
    created-at: uint,
    settled-at: (optional uint)
  }
)

(define-data-var next-trade-id uint u1)

;; Create an energy trade offer
(define-public (create-trade-offer (credits-amount uint) (price-per-credit uint))
  (let ((trade-id (var-get next-trade-id)))
    (asserts! (> credits-amount u0) ERR-INVALID-AMOUNT)
    (asserts! (> price-per-credit u0) ERR-INVALID-AMOUNT)
    (asserts! (>= (ft-get-balance energy-credits tx-sender) credits-amount) ERR-INSUFFICIENT-BALANCE)

    (map-set energy-trades
      { trade-id: trade-id }
      {
        seller: tx-sender,
        buyer: tx-sender, ;; Will be updated when accepted
        credits-amount: credits-amount,
        price-per-credit: price-per-credit,
        total-price: (* credits-amount price-per-credit),
        status: "open",
        created-at: block-height,
        settled-at: none
      }
    )

    (var-set next-trade-id (+ trade-id u1))
    (ok trade-id)
  )
)

;; Accept and execute an energy trade
(define-public (accept-trade (trade-id uint))
  (let ((trade (unwrap! (map-get? energy-trades { trade-id: trade-id }) ERR-MICROGRID-NOT-FOUND)))
    (asserts! (is-eq (get status trade) "open") ERR-NOT-AUTHORIZED)
    (asserts! (not (is-eq tx-sender (get seller trade))) ERR-NOT-AUTHORIZED)

    ;; Transfer STX from buyer to seller
    (try! (stx-transfer? (get total-price trade) tx-sender (get seller trade)))

    ;; Transfer energy credits from seller to buyer
    (try! (ft-transfer? energy-credits (get credits-amount trade) (get seller trade) tx-sender))

    ;; Update trade status
    (map-set energy-trades
      { trade-id: trade-id }
      (merge trade {
        buyer: tx-sender,
        status: "completed",
        settled-at: (some block-height)
      })
    )

    (ok true)
  )
)
