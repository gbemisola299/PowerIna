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

