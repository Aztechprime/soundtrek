;; sound-trek-core.clar
;; This contract manages the creation and storage of audio diary entries with location data
;; for the SoundTrek platform, allowing users to create location-based audio content.

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ENTRY-NOT-FOUND (err u101))
(define-constant ERR-INVALID-COORDINATES (err u102))
(define-constant ERR-INVALID-INPUT (err u103))
(define-constant ERR-UNAUTHORIZED-ACCESS (err u104))
(define-constant ERR-ALREADY-EXISTS (err u105))

;; Data space definitions

;; Map to store entry details - maps entry-id to entry data
(define-map entries
  { entry-id: uint }
  {
    creator: principal,
    title: (string-utf8 100),
    description: (string-utf8 500),
    audio-url: (string-utf8 256),
    latitude: int,  ;; Stored as integer with 6 decimal precision (e.g., 40.689247 stored as 40689247)
    longitude: int, ;; Stored as integer with 6 decimal precision
    timestamp: uint,
    is-public: bool
  }
)

;; Maps user to their list of created entry IDs
(define-map user-entries
  { creator: principal }
  { entry-ids: (list 100 uint) }
)

;; Maps entry-id to list of principals who have access
(define-map entry-access
  { entry-id: uint }
  { allowed-users: (list 50 principal) }
)

;; Counter for generating unique entry IDs
(define-data-var next-entry-id uint u1)

;; Private functions

;; Initialize an empty list for new users
(define-private (get-or-create-user-entries (user principal))
  (match (map-get? user-entries { creator: user })
    existing-entries existing-entries
    { entry-ids: (list) }
  )
)




;; Read-only functions


;; Get all entries for a user
(define-read-only (get-user-entries (user principal))
  (match (map-get? user-entries { creator: user })
    user-data (ok (get entry-ids user-data))
    (ok (list))
  )
)


;; Public functions





