;; Decentralized Coverage Platform 
;;
;; 
;; ================================================================
;; SECTION 1: ERROR DEFINITIONS & PLATFORM ADMINISTRATOR SETTINGS
;; ================================================================

;; Platform administrator - initial deployer of contract
(define-constant admin-address tx-sender)

;; Error codes for operation validation
(define-constant error-admin-restricted (err u100))
(define-constant error-insufficient-funds (err u101))
(define-constant error-transaction-rejected (err u102))
(define-constant error-invalid-parameter (err u103))
(define-constant error-protection-cost-invalid (err u104))
(define-constant error-vault-capacity-reached (err u105))
(define-constant error-no-active-protection (err u106))
(define-constant error-rate-outside-bounds (err u107))
(define-constant error-compensation-failed (err u108))

;; ================================================================
;; SECTION 2: PLATFORM CONFIGURATION PARAMETERS
;; ================================================================

;; Base protection rate in basis points (5.00%)
(define-data-var protection-rate uint u500)

;; Maximum capacity of protection vault (in microSTX)
(define-data-var vault-capacity uint u1000000)

;; Current total deposits in vault
(define-data-var vault-balance uint u0)

;; Individual contribution limit (in microSTX)
(define-data-var max-individual-contribution uint u10000)

;; ================================================================
;; SECTION 3: PARTICIPANT DATA STORAGE
;; ================================================================

;; Track participant contributions to the vault
(define-map participant-contributions principal uint)

;; Track participant's active protection amount
(define-map participant-protection-balance principal uint)

;; Store protection plan details for each participant
(define-map participant-plans 
  {participant: principal} 
  {coverage: uint, rate: uint, status: bool})

;; ================================================================
;; SECTION 4: INTERNAL CALCULATION FUNCTIONS
;; ================================================================

;; Calculate compensation amount based on coverage and protection rate
(define-private (determine-compensation-amount (coverage-amount uint))
  (/ (* coverage-amount (var-get protection-rate)) u100))

;; Safely adjust the vault balance (add or subtract)
;; Negative adjustments will be validated for sufficient funds
(define-private (adjust-vault-balance (amount int))
  (let (
    (existing-balance (var-get vault-balance))
    (updated-balance (if (< amount 0)
                     (if (>= existing-balance (to-uint (- 0 amount)))
                         (- existing-balance (to-uint (- 0 amount)))
                         u0)
                     (+ existing-balance (to-uint amount))))
  )
    (asserts! (<= updated-balance (var-get vault-capacity)) error-vault-capacity-reached)
    (var-set vault-balance updated-balance)
    (ok true)))

;; ================================================================
;; SECTION 5: PARTICIPANT INTERACTION FUNCTIONS
;; ================================================================

;; Contribute funds to protection vault
(define-public (contribute-to-vault (amount uint))
  (let (
    (current-contribution (default-to u0 (map-get? participant-contributions tx-sender)))
    (updated-contribution (+ current-contribution amount))
  )
    ;; Verify contribution within limits
    (asserts! (<= updated-contribution (var-get max-individual-contribution)) error-vault-capacity-reached)

    ;; Update participant's contribution record
    (map-set participant-contributions tx-sender updated-contribution)

    ;; Update vault total balance
    (try! (adjust-vault-balance (to-int amount)))

    ;; Return success
    (ok true)))

;; Acquire a protection plan with specified parameters
(define-public (acquire-protection-plan (coverage-amount uint) (plan-rate uint))
  (let (
    (available-balance (default-to u0 (map-get? participant-contributions tx-sender)))
    (new-protection-amount (+ (default-to u0 (map-get? participant-protection-balance tx-sender)) coverage-amount))
  )
    ;; Input validation
    (asserts! (> coverage-amount u0) error-invalid-parameter)
    (asserts! (>= available-balance coverage-amount) error-insufficient-funds)
    (asserts! (<= plan-rate (var-get protection-rate)) error-rate-outside-bounds)

    ;; Adjust participant balances
    (map-set participant-contributions tx-sender (- available-balance coverage-amount))
    (map-set participant-protection-balance tx-sender new-protection-amount)

    ;; Create protection plan record
    (map-set participant-plans {participant: tx-sender} 
             {coverage: coverage-amount, rate: plan-rate, status: true})

    (ok true)))

;; Process compensation request for valid protection plan
(define-public (process-compensation (beneficiary principal) (amount uint))
  (let (
    (protection-plan (default-to {coverage: u0, rate: u0, status: false} 
                               (map-get? participant-plans {participant: beneficiary})))
    (compensation-amount (determine-compensation-amount amount))
    (current-vault-balance (var-get vault-balance))
  )
    ;; Verify plan is active and vault has sufficient funds
    (asserts! (get status protection-plan) error-no-active-protection)
    (asserts! (>= current-vault-balance compensation-amount) error-compensation-failed)

    ;; Update beneficiary's protection balance
    (let (
      (current-protection (default-to u0 (map-get? participant-protection-balance beneficiary)))
      (remaining-protection (- current-protection compensation-amount))
    )
      (asserts! (>= current-protection compensation-amount) error-compensation-failed)
      (map-set participant-protection-balance beneficiary remaining-protection)
    )

    ;; Reduce vault balance by compensation amount
    (var-set vault-balance (- current-vault-balance compensation-amount))
    (ok true)))

;; Suspend participant's active protection plan
(define-public (suspend-protection-plan)
  (begin
    (let ((plan (default-to {coverage: u0, rate: u0, status: false} 
                          (map-get? participant-plans {participant: tx-sender}))))
      ;; Verify plan exists and is active
      (asserts! (get status plan) error-no-active-protection)

      ;; Update plan status to inactive
      (map-set participant-plans {participant: tx-sender} 
               {coverage: (get coverage plan), 
                rate: (get rate plan), 
                status: false})

      (ok true))))

;; Terminate protection plan and return allocated funds
(define-public (terminate-protection-plan)
  (begin
    (let ((plan (default-to {coverage: u0, rate: u0, status: false} 
                          (map-get? participant-plans {participant: tx-sender}))))
      ;; Verify plan exists and is active
      (asserts! (get status plan) error-no-active-protection)

      ;; Return funds to participant's contribution balance
      (map-set participant-contributions tx-sender 
               (+ (default-to u0 (map-get? participant-contributions tx-sender)) (get coverage plan)))

      ;; Deactivate the plan
      (map-set participant-plans {participant: tx-sender} 
               {coverage: (get coverage plan), rate: (get rate plan), status: false})

      (ok true))))


