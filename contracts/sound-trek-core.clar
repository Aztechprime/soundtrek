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

;; Add entry ID to user's list
(define-private (add-entry-to-user (user principal) (entry-id uint))
  (let
    (
      (current-entries (get entry-ids (get-or-create-user-entries user)))
      (updated-entries (append current-entries entry-id))
    )
    (map-set user-entries
      { creator: user }
      { entry-ids: updated-entries }
    )
  )
)

;; Check if coordinates are valid
(define-private (valid-coordinates? (lat int) (lng int))
  (and
    (>= lat (* (- 90) u1000000))
    (<= lat (* 90 u1000000))
    (>= lng (* (- 180) u1000000))
    (<= lng (* 180 u1000000))
  )
)

;; Check if a user has access to an entry
(define-private (has-access? (entry-id uint) (user principal))
  (let
    (
      (entry (map-get? entries { entry-id: entry-id }))
    )
    (if (is-none entry)
      false
      (let
        (
          (entry-data (unwrap-panic entry))
          (creator (get creator entry-data))
          (is-public (get is-public entry-data))
          (access-list (match (map-get? entry-access { entry-id: entry-id })
                          allowed-data (get allowed-users allowed-data)
                          (list)))
        )
        (or
          (is-eq creator user)
          is-public
          (is-some (index-of access-list user))
        )
      )
    )
  )
)

;; Read-only functions

;; Get entry details
(define-read-only (get-entry (entry-id uint))
  (match (map-get? entries { entry-id: entry-id })
    entry-data
      (let
        (
          (can-access (has-access? entry-id tx-sender))
        )
        (if can-access
          (ok entry-data)
          ERR-UNAUTHORIZED-ACCESS
        )
      )
    ERR-ENTRY-NOT-FOUND
  )
)

;; Get all entries for a user
(define-read-only (get-user-entries (user principal))
  (match (map-get? user-entries { creator: user })
    user-data (ok (get entry-ids user-data))
    (ok (list))
  )
)

;; Check if user has access to an entry (public interface)
(define-read-only (check-access (entry-id uint) (user principal))
  (ok (has-access? entry-id user))
)

;; Public functions

;; Create new audio diary entry
(define-public (create-entry
  (title (string-utf8 100))
  (description (string-utf8 500))
  (audio-url (string-utf8 256))
  (latitude int)
  (longitude int)
  (is-public bool))
  
  ;; Check valid inputs
  (if (not (valid-coordinates? latitude longitude))
    ERR-INVALID-COORDINATES
    (let
      (
        (entry-id (var-get next-entry-id))
        (creator tx-sender)
        (timestamp (unwrap-panic (get-block-info? time (- block-height u1))))
      )
      ;; Update entry ID counter
      (var-set next-entry-id (+ entry-id u1))
      
      ;; Store the entry data
      (map-set entries
        { entry-id: entry-id }
        {
          creator: creator,
          title: title,
          description: description,
          audio-url: audio-url,
          latitude: latitude,
          longitude: longitude,
          timestamp: timestamp,
          is-public: is-public
        }
      )
      
      ;; Initialize access list if not public
      (if (not is-public)
        (map-set entry-access
          { entry-id: entry-id }
          { allowed-users: (list) }
        )
        true
      )
      
      ;; Add entry to user's list
      (add-entry-to-user creator entry-id)
      
      ;; Return success with entry ID
      (ok entry-id)
    )
  )
)

;; Update existing entry
(define-public (update-entry
  (entry-id uint)
  (title (string-utf8 100))
  (description (string-utf8 500))
  (audio-url (string-utf8 256))
  (latitude int)
  (longitude int)
  (is-public bool))
  
  (match (map-get? entries { entry-id: entry-id })
    entry-data
      (let
        (
          (creator (get creator entry-data))
        )
        ;; Check if sender is the creator
        (if (not (is-eq tx-sender creator))
          ERR-NOT-AUTHORIZED
          (if (not (valid-coordinates? latitude longitude))
            ERR-INVALID-COORDINATES
            (begin
              ;; Update the entry
              (map-set entries
                { entry-id: entry-id }
                {
                  creator: creator,
                  title: title,
                  description: description,
                  audio-url: audio-url,
                  latitude: latitude,
                  longitude: longitude,
                  timestamp: (get timestamp entry-data),
                  is-public: is-public
                }
              )
              (ok true)
            )
          )
        )
      )
    ERR-ENTRY-NOT-FOUND
  )
)

;; Grant access to an entry for a specific user
(define-public (grant-access (entry-id uint) (user principal))
  (match (map-get? entries { entry-id: entry-id })
    entry-data
      (let
        (
          (creator (get creator entry-data))
        )
        ;; Check if sender is the creator
        (if (not (is-eq tx-sender creator))
          ERR-NOT-AUTHORIZED
          (let
            (
              (current-access (match (map-get? entry-access { entry-id: entry-id })
                                access-data (get allowed-users access-data)
                                (list)))
            )
            ;; Check if user already has access
            (if (is-some (index-of current-access user))
              (ok true) ;; User already has access
              ;; Add user to access list
              (map-set entry-access
                { entry-id: entry-id }
                { allowed-users: (append current-access user) }
              )
              (ok true)
            )
          )
        )
      )
    ERR-ENTRY-NOT-FOUND
  )
)

;; Revoke access to an entry for a specific user
(define-public (revoke-access (entry-id uint) (user principal))
  (match (map-get? entries { entry-id: entry-id })
    entry-data
      (let
        (
          (creator (get creator entry-data))
        )
        ;; Check if sender is the creator
        (if (not (is-eq tx-sender creator))
          ERR-NOT-AUTHORIZED
          (let
            (
              (current-access (match (map-get? entry-access { entry-id: entry-id })
                                access-data (get allowed-users access-data)
                                (list)))
              (user-index (index-of current-access user))
            )
            (if (is-none user-index)
              (ok true) ;; User doesn't have access anyway
              (let
                (
                  (index (unwrap-panic user-index))
                  (updated-list (filter
                                  (lambda (principal) (not (is-eq principal user)))
                                  current-access))
                )
                (map-set entry-access
                  { entry-id: entry-id }
                  { allowed-users: updated-list }
                )
                (ok true)
              )
            )
          )
        )
      )
    ERR-ENTRY-NOT-FOUND
  )
)

;; Delete an entry
(define-public (delete-entry (entry-id uint))
  (match (map-get? entries { entry-id: entry-id })
    entry-data
      (let
        (
          (creator (get creator entry-data))
        )
        ;; Check if sender is the creator
        (if (not (is-eq tx-sender creator))
          ERR-NOT-AUTHORIZED
          (begin
            ;; Delete the entry data
            (map-delete entries { entry-id: entry-id })
            
            ;; Remove from access list
            (map-delete entry-access { entry-id: entry-id })
            
            ;; Remove from user's entry list
            (let
              (
                (user-data (get-or-create-user-entries creator))
                (user-entries-list (get entry-ids user-data))
                (updated-entries (filter 
                                  (lambda (id) (not (is-eq id entry-id))) 
                                  user-entries-list))
              )
              (map-set user-entries
                { creator: creator }
                { entry-ids: updated-entries }
              )
              (ok true)
            )
          )
        )
      )
    ERR-ENTRY-NOT-FOUND
  )
)