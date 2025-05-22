;; sound-trek-social
;; A social discovery contract for the SoundTrek platform that enables users to find
;; audio content based on location, popularity, and social connections. Users can follow
;; creators, rate content, create playlists, and share recommendations.

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-USER-NOT-FOUND (err u404))
(define-constant ERR-CONTENT-NOT-FOUND (err u405))
(define-constant ERR-ALREADY-FOLLOWED (err u406))
(define-constant ERR-NOT-FOLLOWED (err u407))
(define-constant ERR-ALREADY-RATED (err u408))
(define-constant ERR-INVALID-RATING (err u409))
(define-constant ERR-PLAYLIST-NOT-FOUND (err u410))
(define-constant ERR-CONTENT-ALREADY-IN-PLAYLIST (err u411))
(define-constant ERR-CONTENT-NOT-IN-PLAYLIST (err u412))
(define-constant ERR-INVALID-LOCATION (err u413))

;; Data maps and variables

;; Map storing user profile data
(define-map user-profiles
  { user: principal }
  {
    username: (optional (string-utf8 50)),
    bio: (optional (string-utf8 500)),
    location: (optional {latitude: int, longitude: int}),
    followers-count: uint,
    following-count: uint,
    total-content-count: uint
  }
)

;; Map tracking followers for each user (who follows whom)
(define-map followers-map
  { creator: principal, follower: principal }
  { timestamp: uint }
)

;; Map for content metadata
(define-map content-metadata
  { content-id: uint }
  {
    creator: principal,
    title: (string-utf8 100),
    description: (optional (string-utf8 500)),
    location: {latitude: int, longitude: int},
    timestamp: uint,
    rating-sum: uint,
    rating-count: uint,
    listen-count: uint
  }
)

;; Map tracking user ratings for content
(define-map user-ratings
  { user: principal, content-id: uint }
  { rating: uint }  ;; Rating from 1-5
)

;; Map for user playlists
(define-map playlists
  { playlist-id: uint, owner: principal }
  {
    name: (string-utf8 100),
    description: (optional (string-utf8 500)),
    public: bool,
    created-at: uint,
    updated-at: uint,
    content-count: uint
  }
)

;; Map tracking content in playlists
(define-map playlist-contents
  { playlist-id: uint, content-id: uint }
  { added-at: uint }
)

;; Counter for playlist IDs
(define-data-var next-playlist-id uint u1)

;; Counter for location-based recommendations cache expiry (in blocks)
(define-data-var location-cache-expiry uint u144) ;; ~24 hours in blocks

;; Private functions

;; Calculate distance between two geographic coordinates (using Manhattan distance for simplicity)
;; A more sophisticated implementation would use Haversine formula
(define-private (calculate-distance 
                  (loc1 {latitude: int, longitude: int}) 
                  (loc2 {latitude: int, longitude: int}))
  (let
    (
      (lat-diff (if (> (get latitude loc1) (get latitude loc2))
                    (- (get latitude loc1) (get latitude loc2))
                    (- (get latitude loc2) (get latitude loc1))))
      (long-diff (if (> (get longitude loc1) (get longitude loc2))
                     (- (get longitude loc1) (get longitude loc2))
                     (- (get longitude loc2) (get longitude loc1))))
    )
    (+ lat-diff long-diff)
  )
)

;; Check if user exists
(define-private (user-exists (user principal))
  (is-some (map-get? user-profiles {user: user}))
)

;; Check if content exists
(define-private (content-exists (content-id uint))
  (is-some (map-get? content-metadata {content-id: content-id}))
)

;; Check if playlist exists
(define-private (playlist-exists (playlist-id uint) (owner principal))
  (is-some (map-get? playlists {playlist-id: playlist-id, owner: owner}))
)

;; Check if user is following creator
(define-private (is-following (creator principal) (follower principal))
  (is-some (map-get? followers-map {creator: creator, follower: follower}))
)

;; Get average rating for content
(define-private (get-avg-rating (content-id uint))
  (let
    (
      (metadata (unwrap-panic (map-get? content-metadata {content-id: content-id})))
      (sum (get rating-sum metadata))
      (count (get rating-count metadata))
    )
    (if (is-eq count u0)
      u0
      (/ sum count)
    )
  )
)

;; Validate rating (must be between 1-5)
(define-private (is-valid-rating (rating uint))
  (and (>= rating u1) (<= rating u5))
)

;; Read-only functions

;; Get user profile
(define-read-only (get-user-profile (user principal))
  (if (user-exists user)
    (ok (unwrap-panic (map-get? user-profiles {user: user})))
    ERR-USER-NOT-FOUND
  )
)

;; Check if user is following another user
(define-read-only (check-following (creator principal) (follower principal))
  (ok (is-following creator follower))
)

;; Get content info including average rating
(define-read-only (get-content-info (content-id uint))
  (if (content-exists content-id)
    (let
      (
        (metadata (unwrap-panic (map-get? content-metadata {content-id: content-id})))
        (avg-rating (get-avg-rating content-id))
      )
      (ok (merge metadata {avg-rating: avg-rating}))
    )
    ERR-CONTENT-NOT-FOUND
  )
)

;; Get user's rating for specific content
(define-read-only (get-user-rating (user principal) (content-id uint))
  (match (map-get? user-ratings {user: user, content-id: content-id})
    rating (ok (get rating rating))
    (ok u0)  ;; No rating yet
  )
)

;; Get playlist info
(define-read-only (get-playlist (playlist-id uint) (owner principal))
  (if (playlist-exists playlist-id owner)
    (ok (unwrap-panic (map-get? playlists {playlist-id: playlist-id, owner: owner})))
    ERR-PLAYLIST-NOT-FOUND
  )
)

;; Find nearby content based on user location
;; This function would typically be called via an indexer or off-chain service
;; that would process the results more efficiently
(define-read-only (find-nearby-content 
                    (location {latitude: int, longitude: int}) 
                    (max-distance int))
  (ok location)  ;; Placeholder - in a real implementation this would return content IDs
  ;; Note: A practical implementation would require an indexer to efficiently search
  ;; through content based on location proximity
)

;; Public functions

;; Create or update user profile
(define-public (create-or-update-profile
                 (username (optional (string-utf8 50)))
                 (bio (optional (string-utf8 500)))
                 (location (optional {latitude: int, longitude: int})))
  (let
    (
      (user tx-sender)
      (existing-profile (map-get? user-profiles {user: user}))
    )
    (if (is-some existing-profile)
      ;; Update existing profile
      (map-set user-profiles
        {user: user}
        (merge (unwrap-panic existing-profile)
               {
                 username: username,
                 bio: bio,
                 location: location
               })
      )
      ;; Create new profile
      (map-set user-profiles
        {user: user}
        {
          username: username,
          bio: bio,
          location: location,
          followers-count: u0,
          following-count: u0,
          total-content-count: u0
        }
      )
    )
    (ok true)
  )
)

;; Follow a creator
(define-public (follow-creator (creator principal))
  (let
    (
      (follower tx-sender)
    )
    (asserts! (not (is-eq creator follower)) ERR-NOT-AUTHORIZED)
    (asserts! (user-exists creator) ERR-USER-NOT-FOUND)
    
    (if (is-following creator follower)
      ERR-ALREADY-FOLLOWED
      (begin
        ;; Update followers map
        (map-set followers-map 
          {creator: creator, follower: follower}
          {timestamp: block-height}
        )
        
        ;; Update follower counts
        (let
          (
            (creator-profile (unwrap-panic (map-get? user-profiles {user: creator})))
            (follower-profile (default-to 
              {
                username: none, 
                bio: none, 
                location: none,
                followers-count: u0,
                following-count: u0,
                total-content-count: u0
              }
              (map-get? user-profiles {user: follower})))
          )
          (map-set user-profiles
            {user: creator}
            (merge creator-profile {followers-count: (+ (get followers-count creator-profile) u1)})
          )
          
          (map-set user-profiles
            {user: follower}
            (merge follower-profile {following-count: (+ (get following-count follower-profile) u1)})
          )
          
          (ok true)
        )
      )
    )
  )
)

;; Unfollow a creator
(define-public (unfollow-creator (creator principal))
  (let
    (
      (follower tx-sender)
    )
    (asserts! (not (is-eq creator follower)) ERR-NOT-AUTHORIZED)
    
    (if (is-following creator follower)
      (begin
        ;; Remove from followers map
        (map-delete followers-map {creator: creator, follower: follower})
        
        ;; Update follower counts
        (let
          (
            (creator-profile (unwrap-panic (map-get? user-profiles {user: creator})))
            (follower-profile (unwrap-panic (map-get? user-profiles {user: follower})))
          )
          (map-set user-profiles
            {user: creator}
            (merge creator-profile {followers-count: (- (get followers-count creator-profile) u1)})
          )
          
          (map-set user-profiles
            {user: follower}
            (merge follower-profile {following-count: (- (get following-count follower-profile) u1)})
          )
          
          (ok true)
        )
      )
      ERR-NOT-FOLLOWED
    )
  )
)

;; Rate content
(define-public (rate-content (content-id uint) (rating uint))
  (let
    (
      (user tx-sender)
      (existing-rating (map-get? user-ratings {user: user, content-id: content-id}))
    )
    ;; Check that content exists and rating is valid
    (asserts! (content-exists content-id) ERR-CONTENT-NOT-FOUND)
    (asserts! (is-valid-rating rating) ERR-INVALID-RATING)
    
    ;; Check if user already rated this content
    (if (is-some existing-rating)
      ERR-ALREADY-RATED
      (begin
        ;; Add user rating
        (map-set user-ratings
          {user: user, content-id: content-id}
          {rating: rating}
        )
        
        ;; Update content metadata
        (let
          (
            (metadata (unwrap-panic (map-get? content-metadata {content-id: content-id})))
          )
          (map-set content-metadata
            {content-id: content-id}
            (merge metadata 
              {
                rating-sum: (+ (get rating-sum metadata) rating),
                rating-count: (+ (get rating-count metadata) u1)
              }
            )
          )
          
          (ok true)
        )
      )
    )
  )
)

;; Log a listen (increments listen count for content)
(define-public (log-listen (content-id uint))
  (let
    (
      (user tx-sender)
    )
    (asserts! (content-exists content-id) ERR-CONTENT-NOT-FOUND)
    
    (let
      (
        (metadata (unwrap-panic (map-get? content-metadata {content-id: content-id})))
      )
      (map-set content-metadata
        {content-id: content-id}
        (merge metadata {listen-count: (+ (get listen-count metadata) u1)})
      )
      
      (ok true)
    )
  )
)

;; Create a new playlist
(define-public (create-playlist 
                 (name (string-utf8 100))
                 (description (optional (string-utf8 500)))
                 (public bool))
  (let
    (
      (owner tx-sender)
      (playlist-id (var-get next-playlist-id))
      (current-time block-height)
    )
    ;; Create playlist
    (map-set playlists
      {playlist-id: playlist-id, owner: owner}
      {
        name: name,
        description: description,
        public: public,
        created-at: current-time,
        updated-at: current-time,
        content-count: u0
      }
    )
    
    ;; Increment playlist ID counter
    (var-set next-playlist-id (+ playlist-id u1))
    
    (ok playlist-id)
  )
)

;; Add content to playlist
(define-public (add-to-playlist (playlist-id uint) (content-id uint))
  (let
    (
      (owner tx-sender)
    )
    (asserts! (playlist-exists playlist-id owner) ERR-PLAYLIST-NOT-FOUND)
    (asserts! (content-exists content-id) ERR-CONTENT-NOT-FOUND)
    
    ;; Check if content already in playlist
    (if (is-some (map-get? playlist-contents {playlist-id: playlist-id, content-id: content-id}))
      ERR-CONTENT-ALREADY-IN-PLAYLIST
      (begin
        ;; Add content to playlist
        (map-set playlist-contents
          {playlist-id: playlist-id, content-id: content-id}
          {added-at: block-height}
        )
        
        ;; Update playlist metadata
        (let
          (
            (playlist (unwrap-panic (map-get? playlists {playlist-id: playlist-id, owner: owner})))
          )
          (map-set playlists
            {playlist-id: playlist-id, owner: owner}
            (merge playlist 
              {
                updated-at: block-height,
                content-count: (+ (get content-count playlist) u1)
              }
            )
          )
          
          (ok true)
        )
      )
    )
  )
)

;; Remove content from playlist
(define-public (remove-from-playlist (playlist-id uint) (content-id uint))
  (let
    (
      (owner tx-sender)
    )
    (asserts! (playlist-exists playlist-id owner) ERR-PLAYLIST-NOT-FOUND)
    
    ;; Check if content in playlist
    (if (is-some (map-get? playlist-contents {playlist-id: playlist-id, content-id: content-id}))
      (begin
        ;; Remove content from playlist
        (map-delete playlist-contents {playlist-id: playlist-id, content-id: content-id})
        
        ;; Update playlist metadata
        (let
          (
            (playlist (unwrap-panic (map-get? playlists {playlist-id: playlist-id, owner: owner})))
          )
          (map-set playlists
            {playlist-id: playlist-id, owner: owner}
            (merge playlist 
              {
                updated-at: block-height,
                content-count: (- (get content-count playlist) u1)
              }
            )
          )
          
          (ok true)
        )
      )
      ERR-CONTENT-NOT-IN-PLAYLIST
    )
  )
)



;; Update playlist visibility
(define-public (update-playlist-visibility (playlist-id uint) (public bool))
  (let
    (
      (owner tx-sender)
    )
    (asserts! (playlist-exists playlist-id owner) ERR-PLAYLIST-NOT-FOUND)
    
    (let
      (
        (playlist (unwrap-panic (map-get? playlists {playlist-id: playlist-id, owner: owner})))
      )
      (map-set playlists
        {playlist-id: playlist-id, owner: owner}
        (merge playlist 
          {
            public: public,
            updated-at: block-height
          }
        )
      )
      
      (ok true)
    )
  )
)