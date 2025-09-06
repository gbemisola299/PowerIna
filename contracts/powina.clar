;; PowerIna- Tokenized Renewable Energy Credits for Solar Microgrids
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
 FUNGIBLE TOKEN IMPLEMENTATION
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

;; =============================================================================
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


