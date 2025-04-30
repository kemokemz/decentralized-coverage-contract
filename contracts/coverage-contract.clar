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