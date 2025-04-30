;; sound-trek-marketplace
;; A marketplace contract enabling monetization of premium audio content through 
;; subscriptions and one-time purchases on the SoundTrek platform.

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-AUDIO-NOT-FOUND (err u101))
(define-constant ERR-ALREADY-PURCHASED (err u102))
(define-constant ERR-INSUFFICIENT-FUNDS (err u103))
(define-constant ERR-SUBSCRIPTION-EXPIRED (err u104))
(define-constant ERR-NO-ACTIVE-SUBSCRIPTION (err u105))
(define-constant ERR-INVALID-PRICING-MODEL (err u106))
(define-constant ERR-INVALID-REFUND-PERIOD (err u107))
(define-constant ERR-REFUND-PERIOD-EXPIRED (err u108))
(define-constant ERR-ALREADY-REFUNDED (err u109))
(define-constant ERR-INVALID-PRICE (err u110))
(define-constant ERR-INVALID-SUBSCRIPTION-PERIOD (err u111))

;; Constants
(define-constant FREE-MODEL u1)
(define-constant ONE-TIME-PURCHASE-MODEL u2)
(define-constant SUBSCRIPTION-MODEL u3)

;; Data structures

;; AudioContent stores information about an audio diary
(define-map audio-content
  { content-id: uint }
  {
    creator: principal,
    title: (string-ascii 100),
    pricing-model: uint,      ;; 1=free, 2=one-time purchase, 3=subscription
    price: uint,              ;; in microSTX
    subscription-period: uint,;; days (only used for subscription model)
    refund-period: uint,      ;; hours after purchase when refund is allowed (0 for no refunds)
    created-at: uint          ;; block height when created
  }
)

;; Purchases map tracks one-time purchases
(define-map purchases
  { content-id: uint, buyer: principal }
  {
    purchase-price: uint,     ;; price paid (may differ from current content price if changed)
    purchased-at: uint,       ;; block height when purchased
    refunded: bool            ;; whether the purchase has been refunded
  }
)

;; Subscriptions map tracks active subscriptions
(define-map subscriptions
  { content-id: uint, subscriber: principal }
  {
    subscription-price: uint, ;; price paid per period
    start-block: uint,        ;; block height when subscription started
    end-block: uint,          ;; block height when subscription ends
    auto-renew: bool,         ;; whether to automatically renew subscription
    last-renewed: uint        ;; block height of last renewal
  }
)

;; Platform fee percentage (in basis points, e.g., 250 = 2.5%)
(define-data-var platform-fee-bps uint u250)

;; Admin principal who can modify platform fees
(define-data-var admin-principal principal tx-sender)

;; Private functions

;; Check if the caller is the admin
(define-private (is-admin)
  (is-eq tx-sender (var-get admin-principal))
)

;; Check if the caller is the creator of the specified content
(define-private (is-content-creator (content-id uint))
  (match (map-get? audio-content { content-id: content-id })
    content (is-eq tx-sender (get creator content))
    false
  )
)

;; Calculate platform fee amount based on payment
(define-private (calculate-platform-fee (payment uint))
  (/ (* payment (var-get platform-fee-bps)) u10000)
)

;; Calculate creator payment (total payment minus platform fee)
(define-private (calculate-creator-payment (payment uint))
  (- payment (calculate-platform-fee payment))
)

;; Check if a subscription is currently active
(define-private (is-subscription-active (content-id uint) (user principal))
  (match (map-get? subscriptions { content-id: content-id, subscriber: user })
    subscription (< block-height (get end-block subscription))
    false
  )
)

;; Check if a user has purchased one-time content
(define-private (has-purchased (content-id uint) (user principal))
  (match (map-get? purchases { content-id: content-id, buyer: user })
    purchase (not (get refunded purchase))
    false
  )
)

;; Check if user has access to content (free, purchased, or subscribed)
(define-private (has-access (content-id uint) (user principal))
  (match (map-get? audio-content { content-id: content-id })
    content 
      (or 
        (is-eq (get pricing-model content) FREE-MODEL)
        (and 
          (is-eq (get pricing-model content) ONE-TIME-PURCHASE-MODEL)
          (has-purchased content-id user)
        )
        (and 
          (is-eq (get pricing-model content) SUBSCRIPTION-MODEL)
          (is-subscription-active content-id user)
        )
      )
    false
  )
)

;; Read-only functions

;; Get detailed information about audio content
(define-read-only (get-audio-content (content-id uint))
  (map-get? audio-content { content-id: content-id })
)

;; Check if a user can access specific content
(define-read-only (check-access (content-id uint) (user principal))
  (if (has-access content-id user)
    (ok true)
    (err false)
  )
)

;; Get purchase details for a user
(define-read-only (get-purchase-details (content-id uint) (user principal))
  (map-get? purchases { content-id: content-id, buyer: user })
)

;; Get subscription details for a user
(define-read-only (get-subscription-details (content-id uint) (user principal))
  (map-get? subscriptions { content-id: content-id, subscriber: user })
)

;; Get current platform fee percentage
(define-read-only (get-platform-fee)
  (var-get platform-fee-bps)
)

;; Public functions

;; Create new audio content with specified pricing model
(define-public (create-audio-content 
  (content-id uint)
  (title (string-ascii 100))
  (pricing-model uint)
  (price uint)
  (subscription-period uint)
  (refund-period uint))
  
  (let ((valid-pricing-model (or 
          (is-eq pricing-model FREE-MODEL)
          (is-eq pricing-model ONE-TIME-PURCHASE-MODEL)
          (is-eq pricing-model SUBSCRIPTION-MODEL))))
    
    ;; Validate inputs
    (asserts! valid-pricing-model ERR-INVALID-PRICING-MODEL)
    (asserts! (or (is-eq pricing-model FREE-MODEL) (> price u0)) ERR-INVALID-PRICE)
    (asserts! (or (not (is-eq pricing-model SUBSCRIPTION-MODEL)) (> subscription-period u0)) ERR-INVALID-SUBSCRIPTION-PERIOD)
    
    ;; Insert content into map
    (map-set audio-content
      { content-id: content-id }
      {
        creator: tx-sender,
        title: title,
        pricing-model: pricing-model,
        price: price,
        subscription-period: subscription-period,
        refund-period: refund-period,
        created-at: block-height
      }
    )
    
    (ok true)
  )
)

;; Update existing audio content details
(define-public (update-audio-content
  (content-id uint)
  (title (string-ascii 100))
  (pricing-model uint)
  (price uint)
  (subscription-period uint)
  (refund-period uint))
  
  (let ((content (unwrap! (map-get? audio-content { content-id: content-id }) ERR-AUDIO-NOT-FOUND)))
    
    ;; Ensure caller is the content creator
    (asserts! (is-eq tx-sender (get creator content)) ERR-NOT-AUTHORIZED)
    
    ;; Validate pricing model
    (asserts! (or 
                (is-eq pricing-model FREE-MODEL)
                (is-eq pricing-model ONE-TIME-PURCHASE-MODEL)
                (is-eq pricing-model SUBSCRIPTION-MODEL)) 
              ERR-INVALID-PRICING-MODEL)
    
    ;; Validate other parameters
    (asserts! (or (is-eq pricing-model FREE-MODEL) (> price u0)) ERR-INVALID-PRICE)
    (asserts! (or (not (is-eq pricing-model SUBSCRIPTION-MODEL)) (> subscription-period u0)) ERR-INVALID-SUBSCRIPTION-PERIOD)
    
    ;; Update content
    (map-set audio-content
      { content-id: content-id }
      {
        creator: (get creator content),
        title: title,
        pricing-model: pricing-model,
        price: price,
        subscription-period: subscription-period,
        refund-period: refund-period,
        created-at: (get created-at content)
      }
    )
    
    (ok true)
  )
)

;; Purchase one-time access to content
(define-public (purchase-content (content-id uint))
  (let (
    (content (unwrap! (map-get? audio-content { content-id: content-id }) ERR-AUDIO-NOT-FOUND))
    (existing-purchase (map-get? purchases { content-id: content-id, buyer: tx-sender }))
  )
    
    ;; Verify content is purchasable
    (asserts! (is-eq (get pricing-model content) ONE-TIME-PURCHASE-MODEL) ERR-INVALID-PRICING-MODEL)
    
    ;; Check if already purchased and not refunded
    (asserts! (or 
                (is-none existing-purchase) 
                (get refunded (default-to { refunded: false } existing-purchase))
              ) 
              ERR-ALREADY-PURCHASED)
    
    ;; Process payment
    (let (
      (price (get price content))
      (creator (get creator content))
      (platform-fee (calculate-platform-fee price))
      (creator-payment (calculate-creator-payment price))
    )
      ;; Transfer STX: buyer -> platform fee + creator payment
      (try! (stx-transfer? price tx-sender (as-contract tx-sender)))
      (try! (as-contract (stx-transfer? platform-fee tx-sender (var-get admin-principal))))
      (try! (as-contract (stx-transfer? creator-payment tx-sender creator)))
      
      ;; Record purchase
      (map-set purchases
        { content-id: content-id, buyer: tx-sender }
        {
          purchase-price: price,
          purchased-at: block-height,
          refunded: false
        }
      )
      
      (ok true)
    )
  )
)

;; Subscribe to content
(define-public (subscribe-to-content (content-id uint) (auto-renew bool))
  (let (
    (content (unwrap! (map-get? audio-content { content-id: content-id }) ERR-AUDIO-NOT-FOUND))
    (existing-subscription (map-get? subscriptions { content-id: content-id, subscriber: tx-sender }))
  )
    
    ;; Verify content has subscription model
    (asserts! (is-eq (get pricing-model content) SUBSCRIPTION-MODEL) ERR-INVALID-PRICING-MODEL)
    
    ;; Calculate subscription details
    (let (
      (price (get price content))
      (creator (get creator content))
      (platform-fee (calculate-platform-fee price))
      (creator-payment (calculate-creator-payment price))
      (period-blocks (* (get subscription-period content) u144)) ;; ~144 blocks per day
      (end-block (+ block-height period-blocks))
    )
      
      ;; Process payment
      (try! (stx-transfer? price tx-sender (as-contract tx-sender)))
      (try! (as-contract (stx-transfer? platform-fee tx-sender (var-get admin-principal))))
      (try! (as-contract (stx-transfer? creator-payment tx-sender creator)))
      
      ;; Record subscription
      (map-set subscriptions
        { content-id: content-id, subscriber: tx-sender }
        {
          subscription-price: price,
          start-block: block-height,
          end-block: end-block,
          auto-renew: auto-renew,
          last-renewed: block-height
        }
      )
      
      (ok true)
    )
  )
)

;; Renew an existing subscription
(define-public (renew-subscription (content-id uint))
  (let (
    (content (unwrap! (map-get? audio-content { content-id: content-id }) ERR-AUDIO-NOT-FOUND))
    (subscription (unwrap! (map-get? subscriptions { content-id: content-id, subscriber: tx-sender }) ERR-NO-ACTIVE-SUBSCRIPTION))
  )
    
    ;; Verify content has subscription model
    (asserts! (is-eq (get pricing-model content) SUBSCRIPTION-MODEL) ERR-INVALID-PRICING-MODEL)
    
    ;; Calculate renewal details
    (let (
      (price (get price content))
      (creator (get creator content))
      (platform-fee (calculate-platform-fee price))
      (creator-payment (calculate-creator-payment price))
      (period-blocks (* (get subscription-period content) u144))
      (new-end-block (+ block-height period-blocks))
    )
      
      ;; Process payment
      (try! (stx-transfer? price tx-sender (as-contract tx-sender)))
      (try! (as-contract (stx-transfer? platform-fee tx-sender (var-get admin-principal))))
      (try! (as-contract (stx-transfer? creator-payment tx-sender creator)))
      
      ;; Update subscription
      (map-set subscriptions
        { content-id: content-id, subscriber: tx-sender }
        {
          subscription-price: price,
          start-block: (get start-block subscription),
          end-block: new-end-block,
          auto-renew: (get auto-renew subscription),
          last-renewed: block-height
        }
      )
      
      (ok true)
    )
  )
)

;; Cancel subscription auto-renewal
(define-public (cancel-subscription-renewal (content-id uint))
  (let (
    (subscription (unwrap! (map-get? subscriptions { content-id: content-id, subscriber: tx-sender }) ERR-NO-ACTIVE-SUBSCRIPTION))
  )
    
    ;; Update subscription to disable auto-renewal
    (map-set subscriptions
      { content-id: content-id, subscriber: tx-sender }
      {
        subscription-price: (get subscription-price subscription),
        start-block: (get start-block subscription),
        end-block: (get end-block subscription),
        auto-renew: false,
        last-renewed: (get last-renewed subscription)
      }
    )
    
    (ok true)
  )
)

;; Request refund for a purchase
(define-public (request-refund (content-id uint))
  (let (
    (content (unwrap! (map-get? audio-content { content-id: content-id }) ERR-AUDIO-NOT-FOUND))
    (purchase (unwrap! (map-get? purchases { content-id: content-id, buyer: tx-sender }) ERR-AUDIO-NOT-FOUND))
  )
    
    ;; Verify refund eligibility
    (asserts! (not (get refunded purchase)) ERR-ALREADY-REFUNDED)
    (asserts! (> (get refund-period content) u0) ERR-INVALID-REFUND-PERIOD)
    
    ;; Check if within refund period (refund-period is in hours)
    (let (
      (purchase-block (get purchased-at purchase))
      (refund-blocks (* (get refund-period content) u6)) ;; ~6 blocks per hour
      (refund-deadline (+ purchase-block refund-blocks))
    )
      (asserts! (<= block-height refund-deadline) ERR-REFUND-PERIOD-EXPIRED)
      
      ;; Process refund
      (let (
        (price (get purchase-price purchase))
        (creator (get creator content))
        (platform-fee (calculate-platform-fee price))
        (creator-payment (calculate-creator-payment price))
      )
        ;; Return funds from contract to buyer
        (try! (as-contract (stx-transfer? price (as-contract tx-sender) tx-sender)))
        
        ;; Mark purchase as refunded
        (map-set purchases
          { content-id: content-id, buyer: tx-sender }
          {
            purchase-price: price,
            purchased-at: purchase-block,
            refunded: true
          }
        )
        
        (ok true)
      )
    )
  )
)

;; Update platform fee percentage (admin only)
(define-public (set-platform-fee (new-fee-bps uint))
  (begin
    (asserts! (is-admin) ERR-NOT-AUTHORIZED)
    (asserts! (<= new-fee-bps u1000) ERR-INVALID-PRICE) ;; Max 10%
    (var-set platform-fee-bps new-fee-bps)
    (ok true)
  )
)

;; Change admin principal (current admin only)
(define-public (set-admin (new-admin principal))
  (begin
    (asserts! (is-admin) ERR-NOT-AUTHORIZED)
    (var-set admin-principal new-admin)
    (ok true)
  )
)