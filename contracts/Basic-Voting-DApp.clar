(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_PROPOSAL_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_VOTED (err u102))
(define-constant ERR_VOTING_ENDED (err u103))
(define-constant ERR_VOTING_NOT_ENDED (err u104))
(define-constant ERR_INVALID_DURATION (err u105))
(define-constant ERR_PROPOSAL_EXISTS (err u106))
(define-constant ERR_INVALID_OPTION (err u107))

(define-constant MAX_WEIGHT u10)
(define-constant BASE_WEIGHT u1)
(define-constant PARTICIPATION_THRESHOLD u5)
(define-constant DECAY_BLOCKS u144)

(define-data-var next-proposal-id uint u1)

(define-map proposals
  uint
  {
    title: (string-ascii 100),
    description: (string-ascii 500),
    creator: principal,
    start-block: uint,
    end-block: uint,
    option-a: (string-ascii 50),
    option-b: (string-ascii 50),
    votes-a: uint,
    votes-b: uint,
    total-votes: uint,
    finalized: bool
  }
)

(define-map votes
  {proposal-id: uint, voter: principal}
  {option: (string-ascii 1), block-height: uint}
)

(define-map voter-history
  principal
  (list 100 uint)
)

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals proposal-id)
)

(define-read-only (get-vote (proposal-id uint) (voter principal))
  (map-get? votes {proposal-id: proposal-id, voter: voter})
)

(define-read-only (get-voter-history (voter principal))
  (default-to (list) (map-get? voter-history voter))
)

(define-read-only (get-current-proposal-id)
  (var-get next-proposal-id)
)

(define-read-only (has-voted (proposal-id uint) (voter principal))
  (is-some (map-get? votes {proposal-id: proposal-id, voter: voter}))
)

(define-read-only (is-voting-active (proposal-id uint))
  (match (map-get? proposals proposal-id)
    proposal
    (let 
      (
        (current-block stacks-block-height)
        (start-block (get start-block proposal))
        (end-block (get end-block proposal))
      )
      (and 
        (>= current-block start-block)
        (<= current-block end-block)
      )
    )
    false
  )
)

(define-read-only (get-proposal-results (proposal-id uint))
  (match (map-get? proposals proposal-id)
    proposal
    (let
      (
        (votes-a (get votes-a proposal))
        (votes-b (get votes-b proposal))
        (total (get total-votes proposal))
      )
      (ok {
        option-a: (get option-a proposal),
        option-b: (get option-b proposal),
        votes-a: votes-a,
        votes-b: votes-b,
        total-votes: total,
        winner: (if (> votes-a votes-b) "A" 
                 (if (> votes-b votes-a) "B" "TIE"))
      })
    )
    (err ERR_PROPOSAL_NOT_FOUND)
  )
)

(define-read-only (get-proposal-info (proposal-id uint))
  (match (map-get? proposals proposal-id)
    proposal
    (let
      (
        (current-block stacks-block-height)
        (is-active (is-voting-active proposal-id))
      )
      {
        id: proposal-id,
        title: (get title proposal),
        description: (get description proposal),
        creator: (get creator proposal),
        start-block: (get start-block proposal),
        end-block: (get end-block proposal),
        option-a: (get option-a proposal),
        option-b: (get option-b proposal),
        votes-a: (get votes-a proposal),
        votes-b: (get votes-b proposal),
        total-votes: (get total-votes proposal),
        is-active: is-active,
        finalized: (get finalized proposal)
      }
    )
    {
      id: proposal-id,
      title: "",
      description: "",
      creator: tx-sender,
      start-block: u0,
      end-block: u0,
      option-a: "",
      option-b: "",
      votes-a: u0,
      votes-b: u0,
      total-votes: u0,
      is-active: false,
      finalized: false
    }
  )
)

(define-private (update-voter-history (voter principal) (proposal-id uint))
  (let
    (
      (current-history (default-to (list) (map-get? voter-history voter)))
      (new-history (unwrap-panic (as-max-len? (append current-history proposal-id) u100)))
    )
    (map-set voter-history voter new-history)
  )
)

(define-public (create-proposal 
  (title (string-ascii 100))
  (description (string-ascii 500))
  (option-a (string-ascii 50))
  (option-b (string-ascii 50))
  (duration uint))
  
  (let
    (
      (proposal-id (var-get next-proposal-id))
      (current-block stacks-block-height)
      (end-block (+ current-block duration))
    )
    (asserts! (> duration u0) ERR_INVALID_DURATION)
    
    (map-set proposals proposal-id
      {
        title: title,
        description: description,
        creator: tx-sender,
        start-block: current-block,
        end-block: end-block,
        option-a: option-a,
        option-b: option-b,
        votes-a: u0,
        votes-b: u0,
        total-votes: u0,
        finalized: false
      }
    )
    
    (var-set next-proposal-id (+ proposal-id u1))
    (ok proposal-id)
  )
)

(define-public (vote (proposal-id uint) (option (string-ascii 1)))
  (let
    (
      (proposal (unwrap! (map-get? proposals proposal-id) ERR_PROPOSAL_NOT_FOUND))
      (current-block stacks-block-height)
      (voter tx-sender)
    )
    (asserts! (not (has-voted proposal-id voter)) ERR_ALREADY_VOTED)
    (asserts! (is-voting-active proposal-id) ERR_VOTING_ENDED)
    (asserts! (or (is-eq option "A") (is-eq option "B")) ERR_INVALID_OPTION)
    
    (map-set votes 
      {proposal-id: proposal-id, voter: voter}
      {option: option, block-height: current-block}
    )
    
    (let
      (
        (updated-proposal
          (if (is-eq option "A")
            (merge proposal {
              votes-a: (+ (get votes-a proposal) u1),
              total-votes: (+ (get total-votes proposal) u1)
            })
            (merge proposal {
              votes-b: (+ (get votes-b proposal) u1),
              total-votes: (+ (get total-votes proposal) u1)
            })
          )
        )
      )
      (map-set proposals proposal-id updated-proposal)
      (update-voter-history voter proposal-id)
      (ok true)
    )
  )
)

(define-public (finalize-proposal (proposal-id uint))
  (let
    (
      (proposal (unwrap! (map-get? proposals proposal-id) ERR_PROPOSAL_NOT_FOUND))
      (current-block stacks-block-height)
    )
    (asserts! (>= current-block (get end-block proposal)) ERR_VOTING_NOT_ENDED)
    (asserts! (not (get finalized proposal)) ERR_VOTING_NOT_ENDED)
    
    (map-set proposals proposal-id
      (merge proposal {finalized: true})
    )
    (ok true)
  )
)

(define-public (extend-voting (proposal-id uint) (additional-blocks uint))
  (let
    (
      (proposal (unwrap! (map-get? proposals proposal-id) ERR_PROPOSAL_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get creator proposal)) ERR_NOT_AUTHORIZED)
    (asserts! (is-voting-active proposal-id) ERR_VOTING_ENDED)
    (asserts! (> additional-blocks u0) ERR_INVALID_DURATION)
    
    (map-set proposals proposal-id
      (merge proposal {
        end-block: (+ (get end-block proposal) additional-blocks)
      })
    )
    (ok true)
  )
)

(define-read-only (is-proposal-active (proposal-id uint))
  (is-voting-active proposal-id)
)

(define-read-only (is-proposal-finished (proposal-id uint))
  (match (map-get? proposals proposal-id)
    proposal
    (>= stacks-block-height (get end-block proposal))
    false
  )
)

(define-map delegations
  principal
  principal
)

(define-map delegation-weights
  {proposal-id: uint, delegate: principal}
  uint
)

(define-read-only (get-delegate (delegator principal))
  (map-get? delegations delegator)
)

(define-read-only (get-delegation-weight (proposal-id uint) (delegate principal))
  (default-to u0 (map-get? delegation-weights {proposal-id: proposal-id, delegate: delegate}))
)

(define-read-only (has-delegate (delegator principal))
  (is-some (map-get? delegations delegator))
)

(define-public (set-delegate (delegate principal))
  (begin
    (asserts! (not (is-eq tx-sender delegate)) (err u108))
    (map-set delegations tx-sender delegate)
    (ok true)
  )
)

(define-public (revoke-delegate)
  (begin
    (map-delete delegations tx-sender)
    (ok true)
  )
)

(define-public (vote-as-delegate (proposal-id uint) (option (string-ascii 1)) (delegators (list 50 principal)))
  (let
    (
      (proposal (unwrap! (map-get? proposals proposal-id) ERR_PROPOSAL_NOT_FOUND))
      (delegate tx-sender)
    )
    (asserts! (is-voting-active proposal-id) ERR_VOTING_ENDED)
    (asserts! (or (is-eq option "A") (is-eq option "B")) ERR_INVALID_OPTION)
    
    (let
      (
        (valid-delegators (filter validate-delegation delegators))
        (vote-count (len valid-delegators))
      )
      (asserts! (> vote-count u0) (err u109))
      
      (map-set delegation-weights 
        {proposal-id: proposal-id, delegate: delegate}
        vote-count
      )
      
      (let
        (
          (updated-proposal
            (if (is-eq option "A")
              (merge proposal {
                votes-a: (+ (get votes-a proposal) vote-count),
                total-votes: (+ (get total-votes proposal) vote-count)
              })
              (merge proposal {
                votes-b: (+ (get votes-b proposal) vote-count),
                total-votes: (+ (get total-votes proposal) vote-count)
              })
            )
          )
        )
        (map-set proposals proposal-id updated-proposal)
        (ok vote-count)
      )
    )
  )
)

(define-private (validate-delegation (delegator principal))
  (and
    (is-some (map-get? delegations delegator))
    (is-eq tx-sender (unwrap-panic (map-get? delegations delegator)))
  )
)

(define-map user-weights
  principal
  {
    base-weight: uint,
    participation-bonus: uint,
    last-vote-block: uint,
    consecutive-votes: uint
  }
)

(define-read-only (min (a uint) (b uint))
  (if (< a b) a b)
)

(define-read-only (get-user-weight (user principal))
  (let
    (
      (weight-data (default-to 
        {base-weight: BASE_WEIGHT, participation-bonus: u0, last-vote-block: u0, consecutive-votes: u0}
        (map-get? user-weights user)))
      (current-block stacks-block-height)
      (last-vote (get last-vote-block weight-data))
      (is-decayed (> (- current-block last-vote) DECAY_BLOCKS))
    )
    (if is-decayed
      BASE_WEIGHT
      (min (+ (get base-weight weight-data) (get participation-bonus weight-data)) MAX_WEIGHT)
    )
  )
)

(define-read-only (get-weight-details (user principal))
  (default-to 
    {base-weight: BASE_WEIGHT, participation-bonus: u0, last-vote-block: u0, consecutive-votes: u0}
    (map-get? user-weights user))
)

(define-private (update-user-weight (user principal))
  (let
    (
      (current-weight (get-weight-details user))
      (current-block stacks-block-height)
      (last-vote (get last-vote-block current-weight))
      (consecutive (get consecutive-votes current-weight))
      (new-consecutive (if (< (- current-block last-vote) DECAY_BLOCKS) (+ consecutive u1) u1))
      (new-bonus (if (>= new-consecutive PARTICIPATION_THRESHOLD) 
                   (min (/ new-consecutive PARTICIPATION_THRESHOLD) (- MAX_WEIGHT BASE_WEIGHT))
                   u0))
    )
    (map-set user-weights user
      {
        base-weight: BASE_WEIGHT,
        participation-bonus: new-bonus,
        last-vote-block: current-block,
        consecutive-votes: new-consecutive
      }
    )
  )
)

(define-public (weighted-vote (proposal-id uint) (option (string-ascii 1)))
  (let
    (
      (proposal (unwrap! (map-get? proposals proposal-id) ERR_PROPOSAL_NOT_FOUND))
      (voter tx-sender)
      (vote-weight (get-user-weight voter))
    )
    (asserts! (not (has-voted proposal-id voter)) ERR_ALREADY_VOTED)
    (asserts! (is-voting-active proposal-id) ERR_VOTING_ENDED)
    (asserts! (or (is-eq option "A") (is-eq option "B")) ERR_INVALID_OPTION)
    
    (map-set votes 
      {proposal-id: proposal-id, voter: voter}
      {option: option, block-height: stacks-block-height}
    )
    
    (update-user-weight voter)
    
    (let
      (
        (updated-proposal
          (if (is-eq option "A")
            (merge proposal {
              votes-a: (+ (get votes-a proposal) vote-weight),
              total-votes: (+ (get total-votes proposal) vote-weight)
            })
            (merge proposal {
              votes-b: (+ (get votes-b proposal) vote-weight),
              total-votes: (+ (get total-votes proposal) vote-weight)
            })
          )
        )
      )
      (map-set proposals proposal-id updated-proposal)
      (update-voter-history voter proposal-id)
      (ok vote-weight)
    )
  )
)