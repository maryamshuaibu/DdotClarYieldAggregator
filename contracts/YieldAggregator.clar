;; Cross-Chain Yield Aggregator with Automated Arbitrage
;; A protocol for maximizing yields across multiple blockchain networks

;; Constants
(define-constant contract-owner tx-sender)
(define-constant min-deposit-amount u1000000) ;; 1 STX minimum
(define-constant max-single-strategy-allocation u3000) ;; 30% max per strategy (3000/10000)
(define-constant rebalance-threshold u500) ;; 5% yield difference triggers rebalance (500/10000)
(define-constant performance-fee-rate u2000) ;; 20% performance fee (2000/10000)
(define-constant management-fee-rate u200) ;; 2% annual management fee (200/10000)
(define-constant arbitrage-profit-share u5000) ;; 50% of arbitrage profits to users (5000/10000)
(define-constant min-arbitrage-profit u100000) ;; 0.1 STX minimum profit to execute
(define-constant cross-chain-delay u12) ;; ~2 hours for cross-chain confirmations

;; Supported chains and protocols
(define-constant ethereum-chain-id u1)
(define-constant bitcoin-chain-id u0)
(define-constant stacks-chain-id u1000)
(define-constant solana-chain-id u101)

;; Error codes
(define-constant err-not-authorized (err u100))
(define-constant err-insufficient-balance (err u101))
(define-constant err-strategy-not-found (err u102))
(define-constant err-invalid-allocation (err u103))
(define-constant err-rebalance-not-needed (err u104))
(define-constant err-arbitrage-not-profitable (err u105))
(define-constant err-cross-chain-pending (err u106))
(define-constant err-strategy-inactive (err u107))
(define-constant err-insufficient-liquidity (err u108))
(define-constant err-slippage-too-high (err u109))

;; Yield strategies across different chains
(define-map yield-strategies
  { strategy-id: uint }
  {
    strategy-name: (string-ascii 64),
    target-chain: uint,
    protocol-name: (string-ascii 32), ;; "Uniswap", "Aave", "Compound", etc.
    asset-pair: (string-ascii 24), ;; "STX-USDC", "ETH-USDC", etc.
    current-apy: uint, ;; Annual percentage yield (basis points)
    tvl-deployed: uint,
    risk-score: uint, ;; 1-10 risk rating
    is-active: bool,
    last-yield-update: uint,
    strategy-type: (string-ascii 20) ;; "lending", "lp", "staking", "arbitrage"
  }
)

;; User vault positions
(define-map user-vaults
  { user: principal }
  {
    total-deposited: uint,
    total-shares: uint,
    last-deposit: uint,
    accumulated-yield: uint,
    performance-fees-paid: uint,
    is-active: bool
  }
)

;; Strategy allocations for the vault
(define-map strategy-allocations
  { strategy-id: uint }
  {
    allocated-amount: uint,
    target-allocation: uint, ;; Percentage of total TVL (basis points)
    actual-allocation: uint,
    last-rebalance: uint,
    pending-rebalance: uint
  }
)

;; Cross-chain bridge state
(define-map bridge-transactions
  { bridge-id: uint }
  {
    source-chain: uint,
    target-chain: uint,
    asset: (string-ascii 12),
    amount: uint,
    user: principal,
    status: (string-ascii 20), ;; "pending", "confirmed", "failed"
    initiated-at: uint,
    confirmed-at: (optional uint),
    bridge-fee: uint
  }
)

;; Arbitrage opportunities tracking
(define-map arbitrage-opportunities
  { opportunity-id: uint }
  {
    asset-pair: (string-ascii 24),
    source-chain: uint,
    target-chain: uint,
    source-price: uint,
    target-price: uint,
    profit-potential: uint,
    max-trade-size: uint,
    last-updated: uint,
    is-active: bool
  }
)

;; Protocol performance tracking
(define-map daily-performance
  { day: uint }
  {
    total-tvl: uint,
    total-yield-generated: uint,
    arbitrage-profits: uint,
    performance-fees: uint,
    active-strategies: uint,
    unique-users: uint
  }
)

;; Real-time yield data from external chains (simulated oracle data)
(define-map external-yield-data
  { target-chain-id: uint, protocol: (string-ascii 32) }
  {
    current-apy: uint,
    tvl: uint,
    last-update: uint,
    is-reliable: bool
  }
)

;; Global state variables
(define-data-var total-vault-tvl uint u0)
(define-data-var total-vault-shares uint u0)
(define-data-var next-strategy-id uint u1)
(define-data-var next-bridge-id uint u1)
(define-data-var next-opportunity-id uint u1)
(define-data-var total-performance-fees uint u0)
(define-data-var total-arbitrage-profits uint u0)
(define-data-var emergency-pause bool false)
(define-data-var auto-rebalance-enabled bool true)

;; Helper functions
(define-private (calculate-share-price)
  (if (> (var-get total-vault-shares) u0)
    (/ (var-get total-vault-tvl) (var-get total-vault-shares))
    u1000000)) ;; 1.0 initial share price

(define-private (calculate-shares-to-mint (deposit-amount uint))
  (let ((share-price (calculate-share-price)))
    (/ deposit-amount share-price)))

(define-private (calculate-withdrawal-amount (shares uint))
  (* shares (calculate-share-price)))

(define-private (calculate-performance-fee (profit uint))
  (/ (* profit performance-fee-rate) u10000))

(define-private (should-rebalance (current-apy uint) (target-apy uint))
  (let ((apy-diff (if (> target-apy current-apy) 
                    (- target-apy current-apy) 
                    (- current-apy target-apy))))
    (>= apy-diff rebalance-threshold)))

;; Core Vault Functions

;; 1. Deposit into yield vault
(define-public (deposit-into-vault (amount uint))
  (begin
    (asserts! (not (var-get emergency-pause)) err-not-authorized)
    (asserts! (>= amount min-deposit-amount) err-insufficient-balance)
    
    ;; Transfer STX from user to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    ;; Calculate shares to mint
    (let ((shares-to-mint (calculate-shares-to-mint amount))
          (current-vault (default-to 
                           { total-deposited: u0, total-shares: u0, last-deposit: u0, 
                             accumulated-yield: u0, performance-fees-paid: u0, is-active: true }
                           (map-get? user-vaults { user: tx-sender }))))
      
      ;; Update user vault
      (map-set user-vaults { user: tx-sender }
        {
          total-deposited: (+ (get total-deposited current-vault) amount),
          total-shares: (+ (get total-shares current-vault) shares-to-mint),
          last-deposit: stacks-block-height,
          accumulated-yield: (get accumulated-yield current-vault),
          performance-fees-paid: (get performance-fees-paid current-vault),
          is-active: true
        })
      
      ;; Update global vault state
      (var-set total-vault-tvl (+ (var-get total-vault-tvl) amount))
      (var-set total-vault-shares (+ (var-get total-vault-shares) shares-to-mint))
      
      (print {
        event: "vault-deposit",
        user: tx-sender,
        amount: amount,
        shares-minted: shares-to-mint,
        new-share-price: (calculate-share-price)
      })
      
      (ok shares-to-mint)
    )
  )
)

;; 2. Withdraw from vault
(define-public (withdraw-from-vault (shares uint))
  (let ((user-vault (unwrap! (map-get? user-vaults { user: tx-sender }) err-insufficient-balance)))
    
    (asserts! (>= (get total-shares user-vault) shares) err-insufficient-balance)
    (asserts! (get is-active user-vault) err-not-authorized)
    
    ;; Calculate withdrawal amount
    (let ((withdrawal-amount (calculate-withdrawal-amount shares)))
      
      ;; Update user vault
      (map-set user-vaults { user: tx-sender }
        (merge user-vault {
          total-shares: (- (get total-shares user-vault) shares)
        }))
      
      ;; Update global state
      (var-set total-vault-shares (- (var-get total-vault-shares) shares))
      (var-set total-vault-tvl (- (var-get total-vault-tvl) withdrawal-amount))
      
      ;; Transfer withdrawal amount to user
      (try! (as-contract (stx-transfer? withdrawal-amount tx-sender tx-sender)))
      
      (print {
        event: "vault-withdrawal",
        user: tx-sender,
        shares-burned: shares,
        withdrawal-amount: withdrawal-amount
      })
      
      (ok withdrawal-amount)
    )
  )
)

;; 3. Add new yield strategy
(define-public (add-yield-strategy 
                (strategy-name (string-ascii 64))
                (target-chain uint)
                (protocol-name (string-ascii 32))
                (asset-pair (string-ascii 24))
                (initial-apy uint)
                (risk-score uint))
  (let ((strategy-id (var-get next-strategy-id)))
    
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (asserts! (<= risk-score u10) err-invalid-allocation)
    
    ;; Create strategy
    (map-set yield-strategies { strategy-id: strategy-id }
      {
        strategy-name: strategy-name,
        target-chain: target-chain,
        protocol-name: protocol-name,
        asset-pair: asset-pair,
        current-apy: initial-apy,
        tvl-deployed: u0,
        risk-score: risk-score,
        is-active: true,
        last-yield-update: stacks-block-height,
        strategy-type: "lending"
      })
    
    ;; Initialize allocation
    (map-set strategy-allocations { strategy-id: strategy-id }
      {
        allocated-amount: u0,
        target-allocation: u0,
        actual-allocation: u0,
        last-rebalance: stacks-block-height,
        pending-rebalance: u0
      })
    
    (var-set next-strategy-id (+ strategy-id u1))
    
    (print {
      event: "strategy-added",
      strategy-id: strategy-id,
      strategy-name: strategy-name,
      target-chain: target-chain,
      initial-apy: initial-apy
    })
    
    (ok strategy-id)
  )
)

;; 4. Allocate capital to strategy
(define-public (allocate-to-strategy (strategy-id uint) (amount uint))
  (let ((strategy (unwrap! (map-get? yield-strategies { strategy-id: strategy-id }) err-strategy-not-found))
        (allocation (unwrap! (map-get? strategy-allocations { strategy-id: strategy-id }) err-strategy-not-found)))
    
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (asserts! (get is-active strategy) err-strategy-inactive)
    (asserts! (<= amount (var-get total-vault-tvl)) err-insufficient-liquidity)
    
    ;; Check allocation doesn't exceed maximum per strategy
    (let ((new-allocation-pct (/ (* (+ (get allocated-amount allocation) amount) u10000) (var-get total-vault-tvl))))
      (asserts! (<= new-allocation-pct max-single-strategy-allocation) err-invalid-allocation)
      
      ;; Update strategy allocation
      (map-set strategy-allocations { strategy-id: strategy-id }
        (merge allocation {
          allocated-amount: (+ (get allocated-amount allocation) amount),
          actual-allocation: new-allocation-pct,
          last-rebalance: stacks-block-height
        }))
      
      ;; Update strategy TVL
      (map-set yield-strategies { strategy-id: strategy-id }
        (merge strategy {
          tvl-deployed: (+ (get tvl-deployed strategy) amount)
        }))
      
      (print {
        event: "capital-allocated",
        strategy-id: strategy-id,
        amount: amount,
        new-allocation-pct: new-allocation-pct
      })
      
      (ok amount)
    )
  )
)

;; 5. Execute cross-chain arbitrage
(define-public (execute-arbitrage (opportunity-id uint) (trade-size uint))
  (let ((opportunity (unwrap! (map-get? arbitrage-opportunities { opportunity-id: opportunity-id }) err-arbitrage-not-profitable)))
    
    (asserts! (get is-active opportunity) err-arbitrage-not-profitable)
    (asserts! (<= trade-size (get max-trade-size opportunity)) err-insufficient-liquidity)
    (asserts! (>= (get profit-potential opportunity) min-arbitrage-profit) err-arbitrage-not-profitable)
    
    ;; Calculate expected profit
    (let ((price-diff (- (get target-price opportunity) (get source-price opportunity)))
          (gross-profit (/ (* trade-size price-diff) (get source-price opportunity)))
          (net-profit (- gross-profit (/ gross-profit u100)))) ;; Subtract 1% for fees
      
      (asserts! (> net-profit min-arbitrage-profit) err-arbitrage-not-profitable)
      
      ;; Execute the arbitrage (simplified - in reality would involve cross-chain transactions)
      (let ((user-share (/ (* net-profit arbitrage-profit-share) u10000))
            (protocol-share (- net-profit user-share)))
        
        ;; Update global arbitrage profits
        (var-set total-arbitrage-profits (+ (var-get total-arbitrage-profits) net-profit))
        
        ;; Distribute profits to vault (simplified)
        (var-set total-vault-tvl (+ (var-get total-vault-tvl) user-share))
        
        ;; Mark opportunity as executed
        (map-set arbitrage-opportunities { opportunity-id: opportunity-id }
          (merge opportunity { is-active: false }))
        
        (print {
          event: "arbitrage-executed",
          opportunity-id: opportunity-id,
          trade-size: trade-size,
          net-profit: net-profit,
          user-share: user-share
        })
        
        (ok net-profit)
      )
    )
  )
)

;; 6. Auto-rebalance based on yield opportunities
(define-public (auto-rebalance-portfolio)
  (begin
    (asserts! (not (var-get emergency-pause)) err-not-authorized)
    (asserts! (var-get auto-rebalance-enabled) err-not-authorized)
    
    ;; Find best yielding strategy (simplified logic)
    (let ((best-apy u2500)) ;; Placeholder for dynamic best APY discovery
      
      ;; Execute rebalancing logic (simplified)
      ;; In reality, this would analyze all strategies and optimize allocations
      
      (print {
        event: "portfolio-rebalanced",
        new-best-apy: best-apy,
        rebalance-time: stacks-block-height
      })
      
      (ok true)
    )
  )
)

;; Read-only functions

(define-read-only (get-vault-info (user principal))
  (map-get? user-vaults { user: user })
)

(define-read-only (get-strategy-info (strategy-id uint))
  (map-get? yield-strategies { strategy-id: strategy-id })
)

(define-read-only (get-strategy-allocation (strategy-id uint))
  (map-get? strategy-allocations { strategy-id: strategy-id })
)

(define-read-only (get-arbitrage-opportunity (opportunity-id uint))
  (map-get? arbitrage-opportunities { opportunity-id: opportunity-id })
)

(define-read-only (get-current-share-price)
  (calculate-share-price)
)

(define-read-only (calculate-user-balance (user principal))
  (match (map-get? user-vaults { user: user })
    vault (some (calculate-withdrawal-amount (get total-shares vault)))
    none)
)

(define-read-only (get-protocol-stats)
  {
    total-tvl: (var-get total-vault-tvl),
    total-shares: (var-get total-vault-shares),
    share-price: (calculate-share-price),
    total-strategies: (- (var-get next-strategy-id) u1),
    total-arbitrage-profits: (var-get total-arbitrage-profits),
    auto-rebalance-enabled: (var-get auto-rebalance-enabled)
  }
)

(define-read-only (get-best-yield-opportunities)
  ;; Simplified - would return top yielding strategies across all chains
  (list 
    { strategy-id: u1, apy: u2500, chain: ethereum-chain-id }
    { strategy-id: u2, apy: u2200, chain: stacks-chain-id }
    { strategy-id: u3, apy: u1800, chain: solana-chain-id }
  )
)

;; Oracle functions (simulated - in reality would use Chainlink, etc.)

(define-public (update-external-yield-data (target-chain-id uint) (protocol (string-ascii 32)) (new-apy uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    
    (map-set external-yield-data { target-chain-id: target-chain-id, protocol: protocol }
      {
        current-apy: new-apy,
        tvl: u0, ;; Would be populated by oracle
        last-update: stacks-block-height,
        is-reliable: true
      })
    
    (ok true)
  )
)

(define-public (create-arbitrage-opportunity 
                (asset-pair (string-ascii 24))
                (source-chain uint)
                (target-chain uint)
                (source-price uint)
                (target-price uint))
  (let ((opportunity-id (var-get next-opportunity-id)))
    
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    
    (let ((profit-potential (- target-price source-price)))
      (asserts! (> profit-potential min-arbitrage-profit) err-arbitrage-not-profitable)
      
      (map-set arbitrage-opportunities { opportunity-id: opportunity-id }
        {
          asset-pair: asset-pair,
          source-chain: source-chain,
          target-chain: target-chain,
          source-price: source-price,
          target-price: target-price,
          profit-potential: profit-potential,
          max-trade-size: u1000000000, ;; 1000 STX max
          last-updated: stacks-block-height,
          is-active: true
        })
      
      (var-set next-opportunity-id (+ opportunity-id u1))
      
      (print {
        event: "arbitrage-opportunity-created",
        opportunity-id: opportunity-id,
        asset-pair: asset-pair,
        profit-potential: profit-potential
      })
      
      (ok opportunity-id)
    )
  )
)

;; Admin functions

(define-public (withdraw-performance-fees)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (let ((fees (var-get total-performance-fees)))
      (var-set total-performance-fees u0)
      (try! (as-contract (stx-transfer? fees tx-sender contract-owner)))
      (ok fees)
    )
  )
)

(define-public (toggle-auto-rebalance)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (var-set auto-rebalance-enabled (not (var-get auto-rebalance-enabled)))
    (ok (var-get auto-rebalance-enabled))
  )
)

(define-public (emergency-pause-toggle)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (var-set emergency-pause (not (var-get emergency-pause)))
    (ok (var-get emergency-pause))
  )
)