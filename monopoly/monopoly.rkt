#lang racket
(require rackunit
         "monopoly-plot-posns.rkt")

;; a gamestate is (make-state turn-cycle player-map owner-map card-decks)
(struct gamestate (id-vec turn player-map owner-map cards) #:prefab)

;; an owner-id is (id any)
;; newtyping this to help avoid bugs in transition....
(struct id (v) #:transparent)

;; an owner-map is a map from index -> ownerid, representing a map
;; from numbered spaces to owners.

;; a card-decks is (list (listof string) (listof string)), representing
;; a list of "chance" cards and a list of "community-chest" cards

;; a player is (player byte integer)
(struct player (posn cash in-jail?) #:prefab)

;; functional update of pmap in gamestate
(define (update-gamestate-pmap new-pmap state)
  (match-define (struct gamestate (tvec turn pmap omap cards)) state)
  (gamestate tvec turn new-pmap omap cards))

;; roll the dice
;; -> byte
(define (roll)
  (list (random 6) (random 6)))

(check-not-exn (lambda () (roll)))

;; LOGGING ... the only thing that uses mutation, so far.
(define posn-log empty)
(define (maybe-log-posn posn)
  (set! posn-log (cons posn posn-log)))


;; number of spaces on the board. 
;; INVARIANT: assumes this number is larger than the greatest possible roll
(define BOARD-SPACES 40)

;; bonus for passing 'go'
(define GO-BONUS 200)
(define UTILITY-LIST-PRICE 150)
(define RAILROAD-LIST-PRICE 200)
(define LUXURY-TAX-COST 75)

;; a space map is a map from space indexes to spaces

;; a space is one of
;; - a property, 
;; - a railroad,
;; - "chance", 
;; - "community chest",
;; - ...


;; a property is (make-property nat nat nat nat nat nat nat nat nat symbol)
(struct property (name list-price
                       rent
                       rent-1h
                       rent-2h
                       rent-3h
                       rent-4h
                       rent-hotel
                       house-cost
                       color))

(define mediterranean
  (property "Mediterranean Avenue"
            60 2 10 30 90 160 250 50 'purple))
(define baltic
  (property "Baltic Avenue"
            60 4 20 60 180 320 450 50 'purple))

(define oriental
  (property "Oriental Avenue"
            100 6 30 90 270 400 550 50 'lightblue))
(define vermont
  (property "Vermont Avenue"
            100 6 30 90 270 400 550 50 'lightblue))
(define connecticut
  (property "Connecticut Avenue"
            120 8 40 100 300 450 600 50 'lightblue))

(define st-charles
  (property "St. Charles Place"
            140 10 50 150 450 625 750 100 'violet))
(define states 
  (property "States Avenue"
            140 10 50 150 450 625 750 100 'violet))
(define virginia
  (property "Virginia Avenue"
            160 12 60 180 500 700 900 100 'violet))

(define st-james
  (property "St. James Place"
            180 14 70 200 550 750 950 100 'orange))
(define tennessee
  (property "Tennessee Avenue"
            180 14 70 200 550 750 950 100 'orange))
(define new-york
  (property "New York Avenue"
            200 16 80 220 600 800 1000 100 'orange))

(define indiana
  (property "Indiana Avenue"
            220 18 90 250 700 875 1050 150 'red))
(define kentucky
  (property "Kentucky Avenue"
            220 18 90 250 700 875 1050 150 'red))
(define illinois 
  (property "Illinois Avenue"
            240 20 100 300 750 925 1100 150 'red))

(define atlantic
  (property "Atlantic Avenue"
            260 22 110 330 800 975 1150 150 'yellow))
(define ventnor
  (property "Ventnor Avenue"
            260 22 110 330 800 975 1150 150 'yellow))
(define marvin
  (property "Marvin Gardens"
            280 24 120 360 850 1025 1200 150 'yellow))
(define pacific
  (property "Pacific Avenue"
            300 26 130 390 900 1100 1275 200 'green))
(define no-carolina
  (property "North Carolina Avenue"
            300 26 130 390 900 1100 1275 200 'green))
(define pennsylvania
  (property "Pennsylvania Avenue"
            300 28 150 450 1000 1200 1400 200 'green))

(define park
  (property "Park Place"
            350 35 175 500 1100 1300 1500 200 'blue))
(define boardwalk
  (property "Boardwalk" 
            400 50 200 600 1400 1700 2000 200 'blue))


(define community-chest-cards
  '((go-to-go "Advance to Go (Collect $200)")
    (get-200 "Bank error in your favor - Collect $200")
    (pay-50 "Doctor's fees - Pay $50")
    (get-50 "From sale of stock you get $50")
    (goojf "Get Out of Jail Free")
    (go-to-jail "Go to Jail - Go directly to jail - Do not pass Go - Do not collect $200")
    (get-50-from-each "Grand Opera Night - Collect $50 from every player for opening night seats")
    (get-100 "Holiday Fund matures - Receive $100")
    (get-20 "Income tax refund - Collect $20")
    (get-10-from-each "It is your birthday - Collect $10 from each player")
    (get-100 "Life insurance matures - Collect $100")
    (pay-100 "Pay hospital fees of $100")
    (pay-150 "Pay school fees of $150")
    (get-25 "Receive $25 consultancy fee")
    (street-repairs "You are assessed for street repairs - $40 per house - $115 per hotel")
    (get-10 "You have won second prize in a beauty contest - Collect $10")
    (get-100 "You inherit $100")))

(define chance-cards
  '((go-to-go "Advance to Go (Collect $200)")
    (go-to-illinois "Advance to Illinois Ave. - If you pass Go, collect $200")
    (go-to-st-charles "Advance to St. Charles Place - If you pass Go, collect $200")
    (go-to-utility "Advance token to nearest Utility. If unowned, you may buy it from the Bank. If owned, throw dice and pay owner a total ten times the amount thrown.")
    (go-to-rr "Advance token to the nearest Railroad and pay owner twice the rental to which he/she is otherwise entitled. If Railroad is unowned, you may buy it from the Bank.")
    (go-to-rr "Advance token to the nearest Railroad and pay owner twice the rental to which he/she is otherwise entitled. If Railroad is unowned, you may buy it from the Bank.")
    (get-50 "Bank pays you dividend of $50")
    (goojf "Get out of Jail Free - This card may be kept until needed, or traded/sold")
    (go-back-3 "Go Back 3 Spaces")
    (go-to-jail "Go to Jail - Go directly to Jail - Do not pass Go, do not collect $200")
    (general-repairs "Make general repairs on all your property - For each house pay $25 - For each hotel $100")
    (pay-15 "Pay poor tax of $15")
    (go-to-reading "Take a trip to Reading Railroad - If you pass Go, collect $200")
    (go-to-boardwalk "Take a walk on the Boardwalk - Advance token to Boardwalk")
    (pay-50-to-each "You have been elected Chairman of the Board - Pay each player $50")
    (get-150 "Your building loan matures - Collect $150")
    (get-100 "You have won a crossword competition - Collect $100")))

;; a travel-info is (make-travel-info roll maybe-card)
;; representing how the player got here. Needed for utilities
;; and some cards
(struct travel-info (roll card))

;; a railroad is (make-railroad string)
(struct railroad (name) #:transparent)

;; a utility is (utility string)
(struct utility (name) #:transparent)


(define init-pmap
  (hash (id 0) (player 0 1500 #f)
        (id 1) (player 0 1500 #f)
        (id 2) (player 0 1500 #f)
        (id 3) (player 0 1500 #f)))

(define init-tvec (vector (id 0) (id 1) (id 2) (id 3)))

(define init-card-decks
  (list (shuffle (map first chance-cards))
        (shuffle (map first community-chest-cards))))

(define init-state
  (gamestate init-tvec
             0
             init-pmap
             (hash)
             init-card-decks))

;; return the name of a space for printing as a string
;; posn -> string
(define (space-name posn)
  (define the-space (hash-ref SPACEMAP posn))
  (cond [(railroad? the-space)
         (~a "the "(railroad-name the-space) " railroad")]
        [(utility? the-space)
         (~a "the "(utility-name the-space))]
        [(property? the-space)
         (property-name the-space)]
        [else (error 'space-name "unnamed space for posn ~s" posn)]))

;; the map from board indexes to spaces:
(define SPACEMAP
  (hash 0 'go
        1 mediterranean
        2 'community-chest
        3 baltic
        4 'income-tax
        5 (railroad "Reading")
        6 oriental
        7 'chance
        8 vermont
        9 connecticut
        10 'jail
        11 st-charles
        12 (utility "Electric Company")
        13 states
        14 virginia
        15 (railroad "Pennsylvania")
        16 st-james
        17 'community-chest
        18 tennessee
        19 new-york
        20 'free-parking
        21 kentucky
        22 'chance
        23 indiana
        24 illinois
        25 (railroad "B&O")
        26 atlantic
        27 ventnor
        28 (utility "Water Works")
        29 marvin
        30 'go-to-jail
        31 pacific
        32 no-carolina
        33 'community-chest
        34 pennsylvania
        35 (railroad "Short Line")
        36 'chance
        37 park
        38 'luxury-tax
        39 boardwalk))

;; return the first element of the list, blow up if it's not
;; of length 1
(define (uniq l)
  (unless (and (list? l) (pair? l) (empty? (rest l)))
    (error 'uniq "expected just one element, got: ~s" l))
  (first l))

(define (find-posn space)
  (first 
   (uniq (filter (lambda (kvpair) 
                   (equal? (second kvpair) space))
                 (hash-map SPACEMAP (lambda (k v) (list k v)))))))

(define GO-POSN (find-posn 'go))
(define READING-RR-POSN (find-posn (railroad "Reading")))
(define ILLINOIS-POSN (find-posn illinois))
(define ST-CHARLES-POSN (find-posn st-charles))
(define JAIL-POSN (find-posn 'jail))

;; is this space a railroad?
;; index -> boolean
(define (rr-space? posn)
  (railroad? (hash-ref SPACEMAP posn #f)))

;; a list of the posns that are railroad spaces
(define RR-SPACES
  (for/list ([posn BOARD-SPACES]
             #:when (rr-space? posn))
    posn))

;; the next railroad reached by going forward at least 0 spaces
;; posn -> posn
(define (next-railroad posn)
  (define rotated-railroad-posns
    (for/list ([p RR-SPACES]) (modulo (- p posn) BOARD-SPACES)))
  (modulo (+ posn (first (sort rotated-railroad-posns <)))
          BOARD-SPACES))

(check-equal? (next-railroad 5) 5)
(check-equal? (next-railroad 6) 15)
(check-equal? (next-railroad 38) 5)

;; is this space a utility?
;; index -> boolean
(define (utility-space? posn)
  (utility? (hash-ref SPACEMAP posn #f)))

;; a list of the posns that are railroad spaces
(define UTILITY-SPACES
  (for/list ([posn BOARD-SPACES]
             #:when (utility-space? posn))
    posn))

;; the next railroad reached by going forward at least 0 spaces
;; posn -> posn
(define (next-utility posn)
  (define rotated-utility-posns
    (for/list ([p UTILITY-SPACES]) (modulo (- p posn) BOARD-SPACES)))
  (modulo (+ posn (first (sort rotated-utility-posns <)))
          BOARD-SPACES))

(check-equal? (next-utility 12) 12)
(check-equal? (next-utility 13) 28)
(check-equal? (next-utility 35) 12)

;; a map from colors to the spaces of that color
(define COLORMAP
  (let ()
    (define colors
      (remove-duplicates
       (for/list ([p (in-hash-values SPACEMAP)]
                  #:when (property? p))
         (property-color p))))
    (for/hash ([color colors])
    (define properties
      (for/list ([i BOARD-SPACES]
                 #:when (property? (hash-ref SPACEMAP i #f))
                 #:when (equal? color (property-color (hash-ref SPACEMAP i #f))))
        i))
    (values color properties))))

;; take a turn.
(define (take-turn gs roll-generator)
  (match-define (struct gamestate (id-vec turn pmap owner-map cards)) gs)
  (define player-id (vector-ref id-vec turn))
  (define player (hash-ref pmap player-id))
  (cond [(player-in-jail? player)
         ;; for now, just pay the $50 and roll:
         (cond [(<= 50 (player-cash player))
                ;; note that doubles here don't allow the player
                ;; to move again:
                (make-move (transfer-to-bank 50 player-id gs)
                           (apply + (roll-generator)))]
               [else
                ;; this will kill the player:
                (maybe-transfer-to-bank 50 player-id gs)])]
        [else
         (let loop ([gs gs] [doubles-rolled 0])
           ;; roll the dice
           (define roll (roll-generator))
           ;; move the token, perform actions:
           (define gs2 (make-move gs (apply + roll)))
           ;; was it doubles?
           (cond [(equal? (first roll) (second roll))
                  (cond [(<= 2 doubles-rolled)
                         ;; too many doubles, go to jail
                         (send-player-to-jail player-id gs2)]
                        [else
                         ;; roll again
                         (loop gs2 (add1 doubles-rolled))])]
                 [else gs2]))]))

;; make a move. This corresponds to a single roll of the dice
;; gamestate index -> gamestate
(define (make-move gs roll)
  (match-define (struct gamestate (id-vec turn pmap owner-map cards)) gs)
  (define playerid (vector-ref id-vec turn))
  (define the-player-posn (player-posn (hash-ref pmap playerid)))
  (move-and-handle playerid 
                   (modulo (+ the-player-posn roll)
                           BOARD-SPACES)
                   (travel-info roll #f)
                   gs))

;; move the player from one location to another, crediting them with GO-BONUS
;; if they pass GO. travel-info will contain a symbol if this move is as a result of that
;; card
;; id posn travel-info gamestate -> gamestate
(define (move-and-handle id to travel-info gs)
  (match-define (struct gamestate (id-vec turn pmap owner-map cards)) gs)
  (define new-player
    (move-player (hash-ref pmap id) to))
  (define gs2 (update-gamestate-pmap (hash-set pmap id new-player) gs))
  (handle-landing travel-info gs2))


;; handle the player landing on a space
;; gamestate chance-info -> gamestate
(define (handle-landing travel-info gs)
  (match-define (struct gamestate (id-vec turn pmap owner-map cards)) gs)
  (define playerid (vector-ref id-vec turn))
  (define new-posn (player-posn (hash-ref pmap playerid)))
  (define the-space (hash-ref SPACEMAP new-posn #f))
  (cond [(or (property? the-space)
             (railroad? the-space)
             (utility? the-space))
         (define space-owner (hash-ref owner-map new-posn #f))
         (cond [(and space-owner (not (equal? space-owner playerid)))
                ;; pay rent to the owner or fold
                (maybe-transfer-money (rent-owed new-posn travel-info gs)
                                      playerid
                                      space-owner
                                      gs)]
               [(false? space-owner)
                ;; buy it, if we have enough cash
                (maybe-buy-property gs)]
               [else 
                ;; ignore it
                gs])]
        [(equal? the-space 'go-to-jail)
         (send-player-to-jail playerid gs)]
        [(equal? the-space 'luxury-tax)
         (maybe-transfer-to-bank LUXURY-TAX-COST playerid gs)]
        [(equal? the-space 'chance)
         (handle-chance playerid travel-info gs)]
        [else
         gs]))

;; handle a "chance" card
;; id travel-info gamestate -> gamestate
(define (handle-chance id ti gs)
  (match-define (struct gamestate (id-vec turn pmap omap (list chance-cards cc-cards))) gs)
  (define playerid (vector-ref id-vec turn))
  (define this-card (first chance-cards))
  (define new-card-decks (list (append (rest chance-cards) (list this-card)) cc-cards))
  (define gs2 (gamestate id-vec turn pmap omap new-card-decks))
  (define ti2 (travel-info (travel-info-roll ti) this-card))
  (define player-current-posn (player-posn (hash-ref pmap playerid)))
  (define (to-posn posn) (move-and-handle playerid posn ti2 gs2))
  (match this-card
    ['go-to-go (to-posn GO-POSN)]
    ['go-to-illinois (to-posn ILLINOIS-POSN)]
    ['go-to-st-charles (to-posn ST-CHARLES-POSN)]
    ['go-to-reading (to-posn READING-RR-POSN)]
    ['go-to-utility (to-posn (next-utility player-current-posn))]
    ['go-to-rr (to-posn (next-railroad player-current-posn))]
    ['get-50 (transfer-from-bank 50 playerid gs2)]
    ;; unimplemented: 'goojf
    ;; untested:
    ['go-back-3 (to-posn (modulo (- player-current-posn 3) BOARD-SPACES))]
    ;; untested
    ['go-to-jail (send-player-to-jail playerid gs2)]
    ;; unimplemented: 'general-repairs
    ;; untested
    ['pay-15 (maybe-transfer-to-bank 15 playerid gs2)]
    ;
    [other gs2]))


#;(
    (goojf "Get out of Jail Free - This card may be kept until needed, or traded/sold")
    (general-repairs "Make general repairs on all your property - For each house pay $25 - For each hotel $100")
    (pay-15 "Pay poor tax of $15")
    (go-to-reading "Take a trip to Reading Railroad - If you pass Go, collect $200")
    (go-to-boardwalk "Take a walk on the Boardwalk - Advance token to Boardwalk")
    (pay-50-to-each "You have been elected Chairman of the Board - Pay each player $50")
    (get-150 "Your building loan matures - Collect $150")
    (get-100 "You have won a crossword competition - Collect $100"))

;; send the player to jail
;; id gamestate -> gamestate
(define (send-player-to-jail id state)
  (match-define (struct gamestate (id-vec turn pmap owner-map cards)) state)
  (update-gamestate-pmap
   (hash-set pmap id (player JAIL-POSN (player-cash 
                                        (hash-ref pmap id)) #t))
   state))

;; transfer money or die
;; nat playerid playerid states -> state
(define (maybe-transfer-money owed payer-id owner-id state)
  (define pcash (player-cash (hash-ref (gamestate-player-map state) payer-id)))
  (cond [(< owed pcash)
         (transfer-money owed payer-id owner-id state)]
        ;; this player has died!
        [else
         (display (~a "player "(id-v payer-id)" is out of the game!\n"))
         (display (~a " his or her properties go to player "owner-id"\n"))
         (remove-player
          payer-id
          (transfer-properties-from-player
           payer-id
           owner-id 
           (transfer-money pcash payer-id owner-id state)))]))

;; buy the property for the player if we have enough money
;; INVARIANT: assumes the player is on a buyable state owned by nobody
;; state -> state
(define (maybe-buy-property state)
  (match-define (struct gamestate (tvec turn pmap omap cards)) state)
  (define player (hash-ref pmap (vector-ref tvec turn)))
  (cond [(< (posn-list-price (player-posn player))
            (player-cash player))
         (buy-property player state)]
        [else
         state]))

;; buy the property for the player, decreasing cash by the property's list price
;; state -> state
(define (buy-property player gs)
  (match-define (struct gamestate (tvec turn pmap omap cards)) gs)
  (define player-id (vector-ref tvec turn))
  (define posn (player-posn (hash-ref pmap player-id)))
  (define pmap2 (change-cash pmap player-id (- (posn-list-price posn))))
  (define omap2 (hash-safe-set omap posn player-id))
  (display (~a "player "(id-v player-id)" purchases "(space-name posn)".\n"))
  (when (and (property? (hash-ref SPACEMAP posn))
             (has-monopoly-on-color player-id
                                    (property-color (hash-ref SPACEMAP posn))
                                    omap2))
    (display (~a " ... and now has a monopoly!\n")))
  (gamestate tvec turn pmap2 omap2 cards))

;; a hash-set where the old val must be #f
(define (hash-safe-set map key val)
  (when (hash-ref map key #f)
    (error 'hash-safe-set "value for key ~s wasn't false in map: ~s"
           key map))
  (hash-set map key val))

;; transfer all properties owned by player a to player b
;; state -> state
(define (transfer-properties-from-player a b state)
  (match-define (struct gamestate (turnvec turn pvec owner-map cards)) state)
  (gamestate turnvec turn pvec
             (for/hash ([(k v) (in-hash owner-map)])
               (cond [(equal? v a)
                      (values k b)]
                     [else
                      (values k v)]))
             cards))

;; remove a player. adjust the turn.
;; if it's this player's turn, set the turn to be the previous player
;; (a hack, so it can be advanced normally)
;; state -> state
(define (remove-player id state)
  (match-define (struct gamestate (id-vec turn player-map owner-map cards)) state)
  (gamestate
   (for/vector ([p id-vec]
                #:when (not (equal? p id)))
     p)
   (cond [(equal? id (vector-ref id-vec turn))
          (modulo (sub1 turn) (sub1 (vector-length id-vec)))]
         [else turn])   
   player-map
   owner-map
   cards))


;; RENT AND MONOPOLIES

;; how much rent is owed on this space?
;; index card-info gamestate -> integer
(define (rent-owed posn travel-info gs)
  (define owner-map (gamestate-owner-map gs))
  (define the-owner (hash-ref owner-map posn))
  (define rr-mult (cond [(equal? (travel-info-card travel-info) 'go-to-rr) 2]
                        [else 1]))
  (cond [(rr-space? posn)
         (* rr-mult
            (match (rrs-owned-by the-owner owner-map)
              [1 25]
              [2 50]
              [3 100]
              [4 200]
              [other (error 'rent-owed
                            "internal error: unexpected # of rrs owned by one player: ~v"
                            other)]))]
        [(utility-space? posn)
         (define rent-mult
           (cond [(eq? (travel-info-card travel-info) 'go-to-utility) 10]
                 [else 4]))
         (* rent-mult (travel-info-roll travel-info))]
        [else 
         (define the-property (hash-ref SPACEMAP posn))
         (cond [(has-monopoly-on-color the-owner 
                                       (property-color the-property)
                                       owner-map)
                (* 2 (property-rent the-property))]
               [else (property-rent the-property)])]))



;; how many railroads does this player own?
;; index owner-map -> index
(define (rrs-owned-by player-num owner-map)
  (for/sum ([posn RR-SPACES]
            #:when (equal? player-num (hash-ref owner-map posn #f)))
    1))

;; how many properties of this color does this player own?
(define (has-monopoly-on-color player-id color owner-map)
  (for/and ([posn (in-list (hash-ref COLORMAP color))])
    (equal? player-id (hash-ref owner-map posn #f))))

(check-equal? (has-monopoly-on-color (id 4)
                                     'yellow
                                     (hash 29 (id 4) 27 (id 4)
                                           26 (id 4)))
              true)
(check-equal? (has-monopoly-on-color (id 4)
                                     'yellow
                                     (hash 29 (id 4) 27 (id 3)
                                           26 (id 4)))
              false)

;; transfer money from one player to another
;; integer id id gamestate -> gamestate
(define (transfer-money cash from to state)
  (match-define (struct gamestate (tvec turn playermap owner-map cards)) state)
  (define playermap2 
    (change-cash playermap from (- cash)))
  (define playermap3 (change-cash playermap2 to cash))
  (gamestate tvec
             turn
             playermap3
             owner-map
             cards))

;; transfer money to the bank
;; integer id gamestate -> gamestate
(define (maybe-transfer-to-bank cash from state)
  (match-define (struct gamestate (tvec turn pmap omap cards)) state)
  (cond [(< cash (player-cash (hash-ref pmap from)))
         (transfer-to-bank cash from state)]
        ;; this player has died!
        [else
         (display (~a "player "(id-v from)" is out of the game!\n"))
         (display (~a " his or her properties should go up for auction,\n"
                      " but that's not implemented yet.\n"))
         (remove-player
          from
          (un-own-properties
           from
           (transfer-to-bank (player-cash (hash-ref pmap from)) from state)))]))

;; remove ownership from these properties
;; player-id state -> state
(define (un-own-properties owner state)
  (match-define (struct gamestate (tvec turn pmap omap cards)) state)
  (define omap2 (for/hash ([(k v) omap]
                           #:when (not (equal? v owner)))
                  (values k v)))
  (gamestate tvec turn pmap omap2 cards))

;; transfer cash from a player to the bank
(define (transfer-to-bank cash from state)
  (match-define (struct gamestate (tvec turn pmap omap cards)) state)
  (update-gamestate-pmap (change-cash pmap from (- cash)) state))

;; transfer cash from the bank to a player
(define (transfer-from-bank cash from state)
  (transfer-to-bank (- cash) from state))

;; change the amount of cash by a given amount.
;; playermap id integer -> playermap
(define (change-cash pmap id cash)
  (match-define (struct player (pposn pcash in-jail?)) (hash-ref pmap id))
  (hash-set pmap id (player pposn (+ cash pcash) in-jail?)))

;; the price of a property or railroad
;; posn -> nat
(define (posn-list-price posn)
  (define the-space (hash-ref SPACEMAP posn))
  (cond 
    [(railroad? the-space) RAILROAD-LIST-PRICE]
    [(utility? the-space) UTILITY-LIST-PRICE]
    [(property? the-space) (property-list-price the-space)]
    [else (error 'buy-property
                 "can't buy space ~s" posn)]))


;; change to next player's turn. 
;; gamestate -> gamestate
(define (next-turn gs)
  (match-define (struct gamestate (tvec turn pmap omap cards)) gs)
  (gamestate tvec
             (modulo (add1 turn) 
                     (vector-length tvec))
             pmap
             omap
             cards))

;; move the player the given number of spaces, increase cash for
;; passing GO if necessary. Assumes player is not in jail.
;; player index -> player
(define (move-player pl to)
  (match-define (struct player (posn cash in-jail?)) pl)
  (maybe-log-posn to)
  (cond [(< to posn)
         (player to (+ cash GO-BONUS) #f)]
        [else
         (player to cash #f)]))


;;
;; TEST CASES
;;


(check-equal? (move-player (player 38 1500 #f) 2)
              (player 2 (+ 1500 GO-BONUS) #f))
(check-equal? (move-player (player 24 1500 #f) 31)
              (player 31 1500 #f))
;; a convenience function to preserve old test cases
(define (gamestate1 id-vec turn player-map owner-map)
  (gamestate id-vec turn player-map owner-map init-card-decks))

(check-equal? (remove-player (id 0) init-state)
              (gamestate1 (vector (id 1) (id 2) (id 3))
                         2
                         init-pmap
                         (hash)))
(check-equal? (remove-player (id 2) init-state)
              (gamestate1 (vector (id 0) (id 1) (id 3))
                         0
                         init-pmap
                         (hash)))

(check-equal? (transfer-properties-from-player (id 1)
                                               (id 3)
                                               (gamestate1
                                                (vector (id 0) (id 1) (id 2) (id 3))
                                                1
                                                (hash (id 0) (vector 0 1500)
                                                      (id 1) (vector 0 1500)
                                                      (id 2) (vector 0 1500)
                                                      (id 3) (vector 0 1500))
                                                (hash 5 (id 1)
                                                      15 (id 0)
                                                      24 (id 3)
                                                      25 (id 1))))
              (gamestate1
               (vector (id 0) (id 1) (id 2) (id 3))
               1
               (hash (id 0) (vector 0 1500)
                     (id 1) (vector 0 1500)
                     (id 2) (vector 0 1500)
                     (id 3) (vector 0 1500))
               (hash 5 (id 3)
                     15 (id 0)
                     24 (id 3)
                     25 (id 3))))

;; TRANSFER-TO-BANK
(check-equal? (maybe-transfer-to-bank 100 (id 3) init-state)
              (gamestate1 (vector (id 0) (id 1) (id 2) (id 3))
                         0
                         (hash (id 0) (player 0 1500 #f)
                               (id 1) (player 0 1500 #f)
                               (id 2) (player 0 1500 #f)
                               (id 3) (player 0 1400 #f))
                         (hash)))

;; NEXT-TURN
(check-equal? (next-turn init-state)
              (gamestate1
               (vector (id 0) (id 1) (id 2) (id 3))
               1
               init-pmap
               (hash)))
(check-equal? (next-turn (gamestate1
                          (vector (id 0) (id 1) (id 3) (id 2))
                          3 
                          init-pmap
                          (hash)))
              (gamestate1
               (vector (id 0) (id 1) (id 3) (id 2))
               0
               init-pmap
               (hash)))

(check-equal? (transfer-money 234 (id 1) (id 2)
                              init-state)
              (gamestate1
               (vector (id 0) (id 1) (id 2) (id 3))
               0
               (hash (id 0) (player 0 1500 #f)
                     (id 1) (player 0 (- 1500 234) #f)
                     (id 2) (player 0 (+ 1500 234) #f)
                     (id 3) (player 0 1500 #f))
               (hash)))

(check-equal? (maybe-transfer-money 234 (id 1) (id 2)
                                    init-state)
              (gamestate1
               (vector (id 0) (id 1) (id 2) (id 3))
               0
               (hash (id 0) (player 0 1500 #f)
                     (id 1) (player 0 (- 1500 234) #f)
                     (id 2) (player 0 (+ 1500 234) #f)
                     (id 3) (player 0 1500 #f))
               (hash)))

;; RENT-OWED 

(define test-travel-info (travel-info 3 #f))

(check-equal? (rrs-owned-by (id 3) (hash 5 (id 1) 15 (id 3) 34 (id 0) 35 (id 3)))
              2)

(check-equal? (rent-owed 28 (travel-info 9 #f)
                         (gamestate1 (vector (id 3) (id 4))
                                    0
                                    (hash)
                                    (hash 28 (id 4))))
              (* 9 4))

(check-equal? (rent-owed 15
                         test-travel-info
                         (gamestate1 (vector (id 3))
                                    0
                                    (hash)
                                    (hash 15 (id 3))))
              25)
(check-equal? (rent-owed 15 test-travel-info
                         (gamestate1 (vector (id 3))
                                     0
                                     (hash)
                                     (hash 15 (id 3) 25 (id 3))))
              50)
(check-equal? (rent-owed 15
                         (travel-info 3 'go-to-rr)
                         (gamestate1 (vector (id 3))
                                     0
                                     (hash)
                                     (hash 15 (id 3) 25 (id 3))))
              100)
(check-equal? (rent-owed 15 test-travel-info
                         (gamestate1 (vector (id 3))
                                     0
                                     (hash)
                                     (hash 15 (id 3) 5 (id 3) 25 (id 3))))
              100)
(check-equal? (rent-owed 15 test-travel-info
                         (gamestate1 (vector (id 3))
                                       0
                                       (hash)
                                       (hash 15 (id 3) 25 (id 3) 35 (id 3) 5 (id 3))))
              200)

(check-equal? (rent-owed 24 test-travel-info
                         (gamestate1 (vector (id 3))
                                     0 
                                     (hash)
                                     (hash 24 (id 3))))
              20)

;; BUY-PROPERTY

(check-equal? (maybe-buy-property (gamestate1 init-tvec
                                             3
                                             (hash (id 0) (player 0 1500 #f)
                                                   (id 1) (player 0 1500 #f)
                                                   (id 2) (player 0 1500 #f)
                                                   (id 3) (player 24 1500 #f))
                                             (hash)))
              (gamestate1 init-tvec
                         3
                         (hash (id 0) (player 0 1500 #f)
                               (id 1) (player 0 1500 #f)
                               (id 2) (player 0 1500 #f)
                               (id 3) (player 24 (- 1500 240) #f))
                         (hash 24 (id 3))))

;; can't afford it:
(check-equal? (maybe-buy-property (gamestate1 init-tvec
                                             3
                                             (hash (id 0) (player 0 1500 #f)
                                                   (id 1) (player 0 1500 #f)
                                                   (id 2) (player 0 1500 #f)
                                                   (id 3) (player 24 100 #f))
                                             (hash)))
              (gamestate1 init-tvec
                         3
                         (hash (id 0) (player 0 1500 #f)
                               (id 1) (player 0 1500 #f)
                               (id 2) (player 0 1500 #f)
                               (id 3) (player 24 100 #f))
                         (hash)))

(check-equal? (buy-property (id 3)
                            (gamestate1 init-tvec
                                       3
                                       (hash (id 0) (player 0 1500 #f)
                                             (id 1) (player 0 1500 #f)
                                             (id 2) (player 0 1500 #f)
                                             (id 3) (player 24 1500 #f))
                                       (hash)))
              (gamestate1 init-tvec
                         3
                         (hash (id 0) (player 0 1500 #f)
                               (id 1) (player 0 1500 #f)
                               (id 2) (player 0 1500 #f)
                               (id 3) (player 24 (- 1500 240) #f))
                         (hash 24 (id 3))))

(check-equal? (buy-property (id 3)
                            (gamestate1 init-tvec
                                       3
                                       (hash (id 0) (player 0 1500 #f)
                                             (id 1) (player 0 1500 #f)
                                             (id 2) (player 0 1500 #f)
                                             (id 3) (player 5 1500 #f))
                                       (hash)))
              (gamestate1 init-tvec
                         3
                         (hash (id 0) (player 0 1500 #f)
                               (id 1) (player 0 1500 #f)
                               (id 2) (player 0 1500 #f)
                               (id 3) (player 5 (- 1500 200) #f))
                         (hash 5 (id 3))))

(check-equal? (make-move
               (gamestate1 (vector (id 0))
                          0
                          (hash (id 0) (player 38 1500 #f))
                          (hash))
               6)
              (gamestate1 (vector (id 0))
                          0
                          (hash (id 0) (player 4 (+ 1500 GO-BONUS) #f))
                          (hash)))

;; player 9 lands on luxury tax, pays $75
(check-equal? (make-move 
               (gamestate1 (vector (id 1) (id 9) (id 25))
                          1
                          (hash (id 1) (player 12 1234 #f)
                                (id 2) (player 23 0 #f)
                                (id 9) (player 32 128 #f)
                                (id 25) (player 0 4423 #f))
                          (hash 25 (id 1) 5 (id 1) 15 (id 9)))
               6)
              (gamestate1 (vector (id 1) (id 9) (id 25))
                          1
                          (hash (id 1) (player 12 1234 #f)
                                (id 2) (player 23 0 #f)
                                (id 9) (player 38 (- 128 75) #f)
                                (id 25) (player 0 4423 #f))
                          (hash 25 (id 1) 5 (id 1) 15 (id 9))))
;; player 9 lands on luxury tax, goes bankrupt
(check-equal? (make-move 
               (gamestate1 (vector (id 1) (id 9) (id 25))
                          1
                          (hash (id 1) (player 12 1234 #f)
                                (id 2) (player 23 0 #f)
                                (id 9) (player 32 28 #f)
                                (id 25) (player 0 4423 #f))
                          (hash 25 (id 1) 5 (id 1) 15 (id 9)))
               6)
              (gamestate1 (vector (id 1) (id 25))
                          0
                          (hash (id 1) (player 12 1234 #f)
                                (id 2) (player 23 0 #f)
                                (id 9) (player 38 0 #f)
                                (id 25) (player 0 4423 #f))
                          (hash 25 (id 1) 5 (id 1))))

;; player 9 goes bankrupt, gives everything to player 1:
(check-equal? (make-move 
               (gamestate1 (vector (id 1) (id 9) (id 25))
                          1
                          (hash (id 1) (player 12 1234 #f)
                                (id 2) (player 23 0 #f)
                                (id 9) (player 19 28 #f)
                                (id 25) (player 0 4423 #f))
                          (hash 25 (id 1) 5 (id 1) 15 (id 9)))
               6)
              (gamestate1 (vector (id 1) (id 25))
                         0
                         (hash (id 1) (player 12 (+ 1234 28) #f)
                               (id 2) (player 23 0 #f)
                               (id 9) (player 25 0 #f)
                               (id 25) (player 0 4423 #f))
                         (hash 25 (id 1) 5 (id 1) 15 (id 1))))

;; player 9 pays monopoly price to player 1
(check-equal? (make-move
               (gamestate1 (vector (id 1) (id 9) (id 25))
                          1
                          (hash (id 1) (player 12 1234 #f)
                                (id 2) (player 23 0 #f)
                                (id 9) (player 19 428 #f)
                                (id 25) (player 0 4423 #f))
                          (hash 25 (id 1) 5 (id 1) 15 (id 9)
                                26 (id 1) 27 (id 1) 29 (id 1)))
               7)
              (gamestate1 (vector (id 1) (id 9) (id 25))
                         1
                         (hash (id 1) (player 12 (+ 1234 44) #f)
                               (id 2) (player 23 0 #f)
                               (id 9) (player 26 (- 428 44) #f)
                               (id 25) (player 0 4423 #f))
                         (hash 25 (id 1) 5 (id 1) 15 (id 9)
                                26 (id 1) 27 (id 1) 29 (id 1))))

;; player 9 lands in jail
(check-equal? (make-move
               (gamestate1 (vector (id 1) (id 9) (id 25))
                          1
                          (hash (id 1) (player 12 1234 #f)
                                (id 2) (player 23 0 #f)
                                (id 9) (player 19 428 #f)
                                (id 25) (player 0 4423 #f))
                          (hash 25 (id 1) 5 (id 1) 15 (id 9)
                                26 (id 1) 27 (id 1) 29 (id 1)))
               11)
              (gamestate1 (vector (id 1) (id 9) (id 25))
                          1
                          (hash (id 1) (player 12 1234 #f)
                                (id 2) (player 23 0 #f)
                                (id 9) (player 10 428 #t)
                                (id 25) (player 0 4423 #f))
                          (hash 25 (id 1) 5 (id 1) 15 (id 9)
                                26 (id 1) 27 (id 1) 29 (id 1))))

;; player 9 is in jail, pays $50 then moves
(check-equal? (take-turn
               (gamestate1 (vector (id 1) (id 9) (id 25))
                          1
                          (hash (id 1) (player 12 1234 #f)
                                (id 2) (player 23 0 #f)
                                (id 9) (player 10 428 #t)
                                (id 25) (player 0 4423 #f))
                          (hash 25 (id 1) 5 (id 1) 15 (id 9)
                                26 (id 1) 27 (id 1) 29 (id 1)))
               (lambda () '(6 4)))
              (gamestate1 (vector (id 1) (id 9) (id 25))
                          1
                          (hash (id 1) (player 12 1234 #f)
                                (id 2) (player 23 0 #f)
                                (id 9) (player 20 (- 428 50) #f)
                                (id 25) (player 0 4423 #f))
                          (hash 25 (id 1) 5 (id 1) 15 (id 9)
                                26 (id 1) 27 (id 1) 29 (id 1))))

;; player 9 rolls double 4s and then a 6&4
(check-equal? (take-turn
               (gamestate1 (vector (id 1) (id 9) (id 25))
                          1
                          (hash (id 1) (player 12 1234 #f)
                                (id 2) (player 23 0 #f)
                                (id 9) (player 10 428 #f)
                                (id 25) (player 0 4423 #f))
                          (hash 25 (id 1) 5 (id 1) 15 (id 9)
                                26 (id 1) 27 (id 1) 29 (id 1)))
               (call-with-values (lambda () (sequence-generate '((5 5) (2 3))))
                                 (lambda (a b) b)))
              (gamestate1 (vector (id 1) (id 9) (id 25))
                          1
                          (hash (id 1) (player 12 (+ 1234 50) #f)
                                (id 2) (player 23 0 #f)
                                (id 9) (player 25 (- 428 50) #f)
                                (id 25) (player 0 4423 #f))
                          (hash 25 (id 1) 5 (id 1) 15 (id 9)
                                26 (id 1) 27 (id 1) 29 (id 1))))


;; CHANCE CARDS

;; player 9 draws the "take a ride on the reading" chance card, moves
;; there and collects $200, pays owner $50:
(check-equal? (handle-landing
               (travel-info 3 #f)
               (gamestate (vector (id 1) (id 9) (id 25))
                          1
                          (hash (id 1) (player 12 1234 #f)
                                (id 2) (player 23 0 #f)
                                (id 9) (player 36 428 #f)
                                (id 25) (player 0 4423 #f))
                          (hash 25 (id 1) 5 (id 1) 15 (id 9)
                                26 (id 1) 27 (id 1) 29 (id 1))
                          (list (list
                                 'go-to-reading
                                 'go-to-illinois)
                                (list))))
              (gamestate (vector (id 1) (id 9) (id 25))
                          1
                          (hash (id 1) (player 12 (+ 1234 50) #f)
                                (id 2) (player 23 0 #f)
                                (id 9) (player 5 (+ 200 (- 428 50)) #f)
                                (id 25) (player 0 4423 #f))
                          (hash 25 (id 1) 5 (id 1) 15 (id 9)
                                26 (id 1) 27 (id 1) 29 (id 1))
                          (list (list 
                                 'go-to-illinois
                                 'go-to-reading)
                                (list))))

;; player 9 lands on the water works, buys it
(check-equal? (handle-landing
               (travel-info 3 #f)
               (gamestate (vector (id 1) (id 9) (id 25))
                          1
                          (hash (id 1) (player 12 1234 #f)
                                (id 2) (player 23 0 #f)
                                (id 9) (player 28 428 #f)
                                (id 25) (player 0 4423 #f))
                          (hash 25 (id 1) 5 (id 1) 15 (id 9)
                                26 (id 1) 27 (id 1) 29 (id 1))
                          (list empty empty)))
              (gamestate (vector (id 1) (id 9) (id 25))
                          1
                          (hash (id 1) (player 12 1234 #f)
                                (id 2) (player 23 0 #f)
                                (id 9) (player 28 (- 428 150) #f)
                                (id 25) (player 0 4423 #f))
                          (hash 25 (id 1) 5 (id 1) 15 (id 9)
                                26 (id 1) 27 (id 1) 29 (id 1)
                                28 (id 9))
                          (list empty empty)))

;; player 9 lands on the water works, pays player 1 4x
(check-equal? (handle-landing
               (travel-info 3 #f)
               (gamestate (vector (id 1) (id 9) (id 25))
                          1
                          (hash (id 1) (player 12 1234 #f)
                                (id 2) (player 23 0 #f)
                                (id 9) (player 28 428 #f)
                                (id 25) (player 0 4423 #f))
                          (hash 25 (id 1) 5 (id 1) 15 (id 9)
                                26 (id 1) 27 (id 1) 29 (id 1)
                                28 (id 1))
                          (list empty empty)))
              (gamestate (vector (id 1) (id 9) (id 25))
                          1
                          (hash (id 1) (player 12 (+ 1234 12) #f)
                                (id 2) (player 23 0 #f)
                                (id 9) (player 28 (- 428 12) #f)
                                (id 25) (player 0 4423 #f))
                          (hash 25 (id 1) 5 (id 1) 15 (id 9)
                                26 (id 1) 27 (id 1) 29 (id 1)
                                28 (id 1))
                          (list empty empty)))

;; player 9 draws go-to-utility, pays player 1 10x
(check-equal? (handle-landing
               (travel-info 3 #f)
               (gamestate (vector (id 1) (id 9) (id 25))
                          1
                          (hash (id 1) (player 12 1234 #f)
                                (id 2) (player 23 0 #f)
                                (id 9) (player 22 428 #f)
                                (id 25) (player 0 4423 #f))
                          (hash 25 (id 1) 5 (id 1) 15 (id 9)
                                26 (id 1) 27 (id 1) 29 (id 1)
                                28 (id 1))
                          (list (list 'go-to-utility 'bogus) empty)))
              (gamestate (vector (id 1) (id 9) (id 25))
                          1
                          (hash (id 1) (player 12 (+ 1234 30) #f)
                                (id 2) (player 23 0 #f)
                                (id 9) (player 28 (- 428 30) #f)
                                (id 25) (player 0 4423 #f))
                          (hash 25 (id 1) 5 (id 1) 15 (id 9)
                                26 (id 1) 27 (id 1) 29 (id 1)
                                28 (id 1))
                          (list (list 'bogus 'go-to-utility) empty)))

;; player 9 draws go-to-rr, pays player 1 2x
(check-equal? (handle-landing
               (travel-info 3 #f)
               (gamestate (vector (id 1) (id 9) (id 25))
                          1
                          (hash (id 1) (player 12 1234 #f)
                                (id 2) (player 23 0 #f)
                                (id 9) (player 22 428 #f)
                                (id 25) (player 0 4423 #f))
                          (hash 25 (id 1) 5 (id 1) 15 (id 9)
                                26 (id 1) 27 (id 1) 29 (id 1)
                                28 (id 1))
                          (list (list 'go-to-rr 'bogus) empty)))
              (gamestate (vector (id 1) (id 9) (id 25))
                          1
                          (hash (id 1) (player 12 (+ 1234 100) #f)
                                (id 2) (player 23 0 #f)
                                (id 9) (player 25 (- 428 100) #f)
                                (id 25) (player 0 4423 #f))
                          (hash 25 (id 1) 5 (id 1) 15 (id 9)
                                26 (id 1) 27 (id 1) 29 (id 1)
                                28 (id 1))
                          (list (list 'bogus 'go-to-rr) empty)))

;; transfer-from-bank
(check-equal? (transfer-from-bank 
               50 
               (id 9)
               (gamestate (vector (id 1) (id 9) (id 25))
                          1
                          (hash (id 1) (player 12 1234 #f)
                                (id 2) (player 23 0 #f)
                                (id 9) (player 22 428 #f)
                                (id 25) (player 0 4423 #f))
                          (hash 25 (id 1) 5 (id 1) 15 (id 9)
                                26 (id 1) 27 (id 1) 29 (id 1)
                                28 (id 1))
                          (list (list 'go-to-rr 'bogus) empty)))
              (gamestate (vector (id 1) (id 9) (id 25))
                          1
                          (hash (id 1) (player 12 1234 #f)
                                (id 2) (player 23 0 #f)
                                (id 9) (player 22 (+ 50 428) #f)
                                (id 25) (player 0 4423 #f))
                          (hash 25 (id 1) 5 (id 1) 15 (id 9)
                                26 (id 1) 27 (id 1) 29 (id 1)
                                28 (id 1))
                          (list (list 'go-to-rr 'bogus) empty)))
;; 


(printf "\nstarting game!\n\n")

(define ABORT-TURN 50000)

(set! posn-log empty)

(time
 (let loop ([turn 0]
            [gs init-state])
   (define next-state (take-turn gs roll))
   (cond [(< ABORT-TURN turn)
          (display (~a "game aborted after "ABORT-TURN" turns.\n"))
          next-state]
         [(= 1 (vector-length (gamestate-id-vec next-state)))
          (display (~a "game ends after "turn" turns\n"))
          next-state]
         [else (loop (add1 turn) (next-turn next-state))])))

(plot-posn-distr posn-log)

;; computed from a game with 4M moves. No chance or community chest cards, though.
#;(define computed-landing-distribution
 (list
  (vector 0 (ivl 0.021992305789842986 0.022340795363969983))
  (vector 1 (ivl 0.022287882895731696 0.02263864385997687))
  (vector 2 (ivl 0.022026055673652203 0.022374805424774376))
  (vector 3 (ivl 0.021863386408044957 0.022210880174728442))
  (vector 4 (ivl 0.021774087070796044 0.022120889223745072))
  (vector 5 (ivl 0.02192543521662568 0.02227340865562368))
  (vector 6 (ivl 0.02179756475775102 0.022144548889564783))
  (vector 7 (ivl 0.021917050221344193 0.02226495888205706))
  (vector 8 (ivl 0.02171161990845023 0.02205793735817253))
  (vector 9 (ivl 0.02183886045867973 0.022186164425212974))
  (vector 10 (ivl 0.0226815788516006 0.02303534030152635))
  (vector 11 (ivl 0.02351828071263816 0.023878327502308172))
  (vector 12 (ivl 0.024488601904442298 0.024855789066229428))
  (vector 13 (ivl 0.025723206869101175 0.026099262076011164))
  (vector 14 (ivl 0.02694345521131159 0.027328051731978387))
  (vector 15 (ivl 0.028216868071102057 0.028610156783329798))
  (vector 16 (ivl 0.028170098861138564 0.028563072156965127))
  (vector 17 (ivl 0.02809753346309943 0.028490016804468176))
  (vector 18 (ivl 0.02801762803272911 0.028409571061560324))
  (vector 19 (ivl 0.02793562578257017 0.02832701344622907))
  (vector 20 (ivl 0.02780979198994289 0.02820032570613482))
  (vector 21 (ivl 0.02732890691624005 0.027716157655620216))
  (vector 22 (ivl 0.027515554749606132 0.0279040834291244))
  (vector 23 (ivl 0.02772569365697957 0.02811565548139592))
  (vector 24 (ivl 0.027760087930837993 0.02815028375981471))
  (vector 25 (ivl 0.027718982588338503 0.0281088987349585))
  (vector 26 (ivl 0.027664035826831964 0.028053577760509974))
  (vector 27 (ivl 0.027527508679820577 0.0279161190445185))
  (vector 28 (ivl 0.027600281058801766 0.027989388285294597))
  (vector 29 (ivl 0.027509682647061284 0.02789817119347558))
  (vector 30 (ivl 0.02691472499975678 0.027299122860228446))
  (vector 31 (ivl 0.02590165542985118 0.02627897322250453))
  (vector 32 (ivl 0.025024533505334567 0.025395599194730875))
  (vector 33 (ivl 0.023966950286352724 0.024330318061967064))
  (vector 34 (ivl 0.022994155644675423 0.023350279517264535))
  (vector 35 (ivl 0.021624208429474094 0.0219698463719072))
  (vector 36 (ivl 0.02174075722998504 0.02208730085838487))
  (vector 37 (ivl 0.02178624515216449 0.022133141557206377))
  (vector 38 (ivl 0.02177806988901168 0.022124902920732284))
  (vector 39 (ivl 0.02180112833979826 0.022148140084277988))))
