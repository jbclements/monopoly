#lang racket
(require rackunit)

(provide (all-defined-out))

;; a gamestate is (make-state turn-cycle player-map property-map card-decks)
(struct gamestate (id-vec turn player-map property-map cards) #:prefab)

;; an owner-id is (id any)
;; newtyping this to help avoid bugs in transition....
(struct id (v) #:transparent)

;; a property-state is (property-state id nat-or-mortgaged)
;; representing the owner and the number of houses on the property
;; when the property is mortgaged, the number of houses is 'mortgaged
(struct property-state (owner houses) #:transparent)

;; an property-map is a map from index -> property-state, representing a map
;; from numbered spaces to owners.

;; a card-decks is (list (listof string) (listof string)), representing
;; a list of "chance" cards and a list of "community-chest" cards

;; a player is (player byte integer)
(struct player (posn cash in-jail?) #:prefab)

;; free actions may be taken at "any time" (whatever that turns out to mean)
;; a free action is one of:
;; - (buy-house/a posn)
;; - (sell-house/a posn)
;; - (mortgage/a posn)
;; - (unmortgage/a posn
;; - (try-exchange/a bundle bundle)
(struct buy-house/a (posn))
(struct sell-house/a (posn))
(struct mortgage/a (posn))
(struct unmortgage/a (posn))
(struct try-exchange/a (offered requested))

;; a bundle is (bundle number list-of-posns)
(struct bundle (cash properties))

;; functional update of pmap in gamestate
(define (update-gamestate-pmap new-pmap state)
  (match-define (struct gamestate (tvec turn pmap prmap cards)) state)
  (gamestate tvec turn new-pmap prmap cards))

;; functional update of player
;; id (player->player) state -> state
(define (update-gamestate-player id player-thunk state)
  (define pmap (gamestate-player-map state))
  (update-gamestate-pmap (hash-set pmap id (player-thunk (hash-ref pmap id))) state))

;; functional-update of prmap
;; id (prmap -> prmap) state -> state
(define (update-gamestate-prmap prmap-thunk state)
  (match-define (struct gamestate (tvec turn pmap prmap cards)) state)
  (gamestate tvec turn pmap (prmap-thunk prmap) cards))

;; functional-update of cards
;; id card-decks state -> state
(define (update-gamestate-cards new-decks state)
  (match-define (struct gamestate (tvec turn pmap prmap cards)) state)
  (gamestate tvec turn pmap prmap new-decks))

;; whose turn is it?
;; state -> id
(define (gamestate-pturn gs)
  (vector-ref (gamestate-id-vec gs) (gamestate-turn gs)))

;; roll the dice
;; -> byte
(define (roll)
  (list (random 6) (random 6)))

(check-not-exn (lambda () (roll)))

;; LOGGING ... the only thing that uses mutation, so far.
(define posn-log empty)
(define (maybe-log-posn posn)
  (set! posn-log (cons posn posn-log)))
(define (reset-posn-log)
  (set! posn-log empty))


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


;; a property is (make-property string nat (vector nat nat nat nat nat nat) nat symbol)
(struct property (name list-price
                       rent-vec
                       house-cost
                       color))

;; convenience function... changed the data definition.
(define (make-property name list-price rent0 rent1 rent2 rent3 rent4 rent5 house-cost color)
  (property name list-price (vector rent0 rent1 rent2 rent3 rent4 rent5) house-cost color))

(define mediterranean
  (make-property "Mediterranean Avenue"
            60 2 10 30 90 160 250 50 'purple))
(define baltic
  (make-property "Baltic Avenue"
            60 4 20 60 180 320 450 50 'purple))

(define oriental
  (make-property "Oriental Avenue"
            100 6 30 90 270 400 550 50 'lightblue))
(define vermont
  (make-property "Vermont Avenue"
            100 6 30 90 270 400 550 50 'lightblue))
(define connecticut
  (make-property "Connecticut Avenue"
            120 8 40 100 300 450 600 50 'lightblue))

(define st-charles
  (make-property "St. Charles Place"
            140 10 50 150 450 625 750 100 'violet))
(define states 
  (make-property "States Avenue"
            140 10 50 150 450 625 750 100 'violet))
(define virginia
  (make-property "Virginia Avenue"
            160 12 60 180 500 700 900 100 'violet))

(define st-james
  (make-property "St. James Place"
            180 14 70 200 550 750 950 100 'orange))
(define tennessee
  (make-property "Tennessee Avenue"
            180 14 70 200 550 750 950 100 'orange))
(define new-york
  (make-property "New York Avenue"
            200 16 80 220 600 800 1000 100 'orange))

(define indiana
  (make-property "Indiana Avenue"
            220 18 90 250 700 875 1050 150 'red))
(define kentucky
  (make-property "Kentucky Avenue"
            220 18 90 250 700 875 1050 150 'red))
(define illinois 
  (make-property "Illinois Avenue"
            240 20 100 300 750 925 1100 150 'red))

(define atlantic
  (make-property "Atlantic Avenue"
            260 22 110 330 800 975 1150 150 'yellow))
(define ventnor
  (make-property "Ventnor Avenue"
            260 22 110 330 800 975 1150 150 'yellow))
(define marvin
  (make-property "Marvin Gardens"
            280 24 120 360 850 1025 1200 150 'yellow))

(define pacific
  (make-property "Pacific Avenue"
            300 26 130 390 900 1100 1275 200 'green))
(define no-carolina
  (make-property "North Carolina Avenue"
            300 26 130 390 900 1100 1275 200 'green))
(define pennsylvania
  (make-property "Pennsylvania Avenue"
            300 28 150 450 1000 1200 1400 200 'green))

(define park
  (make-property "Park Place"
            350 35 175 500 1100 1300 1500 200 'blue))
(define boardwalk
  (make-property "Boardwalk" 
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
(define BOARDWALK-POSN (find-posn boardwalk))
(define COLORS (remove-duplicates
                (for/list ([p (in-hash-values SPACEMAP)]
                           #:when (property? p))
                  (property-color p))))

;; is this space a railroad?
;; index -> boolean
(define (rr-space? posn)
  (railroad? (hash-ref SPACEMAP posn)))

;; a list of the posns that are railroad spaces
(define RR-SPACES
  (for/list ([posn BOARD-SPACES]
             #:when (rr-space? posn))
    posn))

(define OWNABLE-SPACES
  (for/list ([posn BOARD-SPACES]
             #:when (or (rr-space? posn) 
                        (property? (hash-ref SPACEMAP posn))))
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

;; MOVING MECHANICS

;; take a turn.
(define (take-turn gs roll-generator)
  (define player-id (gamestate-pturn gs))
  (define player (hash-ref (gamestate-player-map gs) player-id))
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
  (define playerid (gamestate-pturn gs))
  (define the-player-posn (player-posn (hash-ref (gamestate-player-map gs) playerid)))
  (move-and-handle playerid 
                   (modulo (+ the-player-posn roll)
                           BOARD-SPACES)
                   (travel-info roll #f)
                   gs))

;; move the player from one location to another, crediting them with GO-BONUS
;; if they pass GO. travel-info will contain a symbol if this move is as a result of that
;; card
;; id posn travel-info gamestate -> gamestate
(define (move-and-handle id to ti gs)
  (define can-collect-go-bonus?
    (match ti
      [(struct travel-info (r 'go-back-3)) #f]
      [other #t]))
  (define gs2
    (update-gamestate-player id 
                             (lambda (p) (move-player p to can-collect-go-bonus?))
                             gs))
  (handle-landing ti gs2))


;; handle the player landing on a space
;; gamestate chance-info -> gamestate
(define (handle-landing travel-info gs)
  (match-define (struct gamestate (_1 _2 pmap property-map _3)) gs)
  (define playerid (gamestate-pturn gs))
  (define new-posn (player-posn (hash-ref pmap playerid)))
  (define the-space (hash-ref SPACEMAP new-posn #f))
  (cond [(or (property? the-space)
             (railroad? the-space)
             (utility? the-space))
         (define pstate (hash-ref property-map new-posn #f))
         (cond [(not pstate)
                ;; buy it, if we have enough cash
                (maybe-buy-property gs)]
               [(not (equal? (property-state-owner pstate) playerid))
                ;; pay rent to the owner or fold
                (maybe-transfer-money (rent-owed new-posn travel-info gs)
                                      playerid
                                      (property-state-owner pstate)
                                      gs)]
               [else 
                ;; we own it, ignore it
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
  (match-define (list chance-cards cc-cards) (gamestate-cards gs))
  ;(match-define (struct gamestate (id-vec turn pmap prmap (list chance-cards cc-cards))) gs)
  (define this-card (first chance-cards))
  ;; THIS WON'T WORK FOR GOOJF:
  (define new-card-decks (list (append (rest chance-cards) (list this-card)) cc-cards))
  (define gs2 (update-gamestate-cards new-card-decks gs))
  (handle-card id ti this-card gs2))

;; handle a "community chest" card
;; id travel-info gamestate -> gamestate
(define (handle-community-chest id ti gs)
  (match-define (list chance-cards cc-cards) (gamestate-cards gs))
  ;(match-define (struct gamestate (id-vec turn pmap prmap (list chance-cards cc-cards))) gs)
  (define this-card (first cc-cards))
  ;; THIS WON'T WORK FOR GOOJF:
  (define new-card-decks (list chance-cards (append (rest cc-cards) (list this-card))))
  (define gs2 (update-gamestate-cards new-card-decks gs))
  (handle-card id ti this-card gs2))

;; handle any card
;; id travel-info card state -> state
(define (handle-card id ti card gs)
  (define ti2 (travel-info (travel-info-roll ti) card))
  (define player-current-posn (player-posn (hash-ref (gamestate-player-map gs) id)))
  (define (to-posn posn) (move-and-handle id posn ti2 gs))
  (match card
    ['go-to-go (to-posn GO-POSN)]
    ['go-to-illinois (to-posn ILLINOIS-POSN)]
    ['go-to-st-charles (to-posn ST-CHARLES-POSN)]
    ['go-to-reading (to-posn READING-RR-POSN)]
    ['go-to-utility (to-posn (next-utility player-current-posn))]
    ['go-to-rr (to-posn (next-railroad player-current-posn))]
    ;; unimplemented: 'goojf
    ['go-back-3 (to-posn (modulo (- player-current-posn 3) BOARD-SPACES))]
    ['go-to-jail (send-player-to-jail id gs)]
    ;; unimplemented: 'general-repairs
    ;; untested
    ['pay-15 (maybe-transfer-to-bank 15 id gs)]
    ['pay-50 (maybe-transfer-to-bank 50 id gs)]
    ['pay-100 (maybe-transfer-to-bank 100 id gs)]
    ['pay-150 (maybe-transfer-to-bank 150 id gs)]
    ['go-to-boardwalk (to-posn BOARDWALK-POSN)]
    ['go-to-go (to-posn GO-POSN)]
    ['get-10 (transfer-from-bank 10 id gs)]
    ['get-20 (transfer-from-bank 20 id gs)]
    ['get-25 (transfer-from-bank 25 id gs)]
    ['get-50 (transfer-from-bank 50 id gs)]
    ['get-100 (transfer-from-bank 100 id gs)]
    ['get-150 (transfer-from-bank 150 id gs)]
    ['get-200 (transfer-from-bank 150 id gs)]
    [other gs]))

#;((goojf "Get Out of Jail Free")
    (get-50-from-each "Grand Opera Night - Collect $50 from every player for opening night seats")
    (get-10-from-each "It is your birthday - Collect $10 from each player")
    (street-repairs "You are assessed for street repairs - $40 per house - $115 per hotel"))
#;(
    (goojf "Get out of Jail Free - This card may be kept until needed, or traded/sold")
    (general-repairs "Make general repairs on all your property - For each house pay $25 - For each hotel $100")
    (pay-50-to-each "You have been elected Chairman of the Board - Pay each player $50")
    )


;; send the player to jail
;; id gamestate -> gamestate
(define (send-player-to-jail id state)
  (define (player-to-jail p)
    (player JAIL-POSN (player-cash p) #t))
  (update-gamestate-player id player-to-jail state))

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
  (match-define (struct gamestate (tvec turn pmap _1 _2)) state)
  (define player (hash-ref pmap (vector-ref tvec turn)))
  (cond [(< (posn-list-price (player-posn player))
            (player-cash player))
         (buy-property state)]
        [else
         state]))

;; buy the property for the player, decreasing cash by the property's list price
;; state -> state
(define (buy-property gs)
  (match-define (struct gamestate (tvec turn pmap prmap cards)) gs)
  (define player-id (gamestate-pturn gs))
  (define posn (player-posn (hash-ref pmap player-id)))
  (define gs2
    (update-gamestate-player
     player-id (player-change-cash (- (posn-list-price posn)))
     (update-gamestate-prmap
      (lambda (prmap)
        (hash-safe-set prmap posn 
                       (property-state player-id 0)))
      gs)))
  (display (~a "player "(id-v player-id)" purchases "(space-name posn)".\n"))
  (when (and (property? (hash-ref SPACEMAP posn))
             (has-monopoly-on-color player-id
                                    (property-color (hash-ref SPACEMAP posn))
                                    (gamestate-property-map gs2)))
    (display (~a " ... and now has a monopoly!\n")))
  gs2)

;; buy a house. assume it's possible
;; id posn gamestate -> gamestate
(define (buy-house owner posn state)
  (display (~a "player "(id-v owner)" purchases a house on property "posn".\n"))
  (update-gamestate-player
   owner
   (player-change-cash (- (property-house-cost (hash-ref SPACEMAP posn))))
   (update-gamestate-prmap
    (lambda (prmap)
      (match-define (struct property-state (pr-owner houses))
        (hash-ref prmap posn))
      (unless (equal? owner pr-owner)
        (raise-argument-error 'buy-house (~a "owner of property "posn)
                              0 owner posn state))
      (hash-set prmap posn (property-state owner (add1 houses))))
    state)))

;; a hash-set where the old val must be #f
(define (hash-safe-set map key val)
  (when (hash-ref map key #f)
    (error 'hash-safe-set "value for key ~s wasn't false in map: ~s"
           key map))
  (hash-set map key val))

;; transfer all properties owned by player a to player b
;; state -> state
(define (transfer-properties-from-player a b state)
  (update-gamestate-prmap 
   (lambda (prmap)
     (for/hash ([(k v) (in-hash prmap)])
       (cond [(equal? (property-state-owner v) a)
              (values k (property-state b 0))]
             [else
              (values k v)])))
   state))

;; remove a player. adjust the turn.
;; if it's this player's turn, set the turn to be the previous player
;; (a hack, so it can be advanced normally)
;; state -> state
(define (remove-player id state)
  (match-define (struct gamestate (id-vec turn player-map property-map cards)) state)
  (gamestate
   (for/vector ([p id-vec]
                #:when (not (equal? p id)))
     p)
   (cond [(equal? id (vector-ref id-vec turn))
          (modulo (sub1 turn) (sub1 (vector-length id-vec)))]
         [else turn])   
   player-map
   property-map
   cards))


;; RENT AND MONOPOLIES

;; how much rent is owed on this space? 
;; assumes the lander is not the owner
;; assumes the space is owned
;; index card-info gamestate -> integer
(define (rent-owed posn travel-info gs)
  (define property-map (gamestate-property-map gs))
  (match-define (struct property-state (the-owner houses)) (hash-ref property-map posn))
  (cond [(eq? houses 'mortgaged) 0]
        [else
         (define rr-mult (cond [(equal? (travel-info-card travel-info) 'go-to-rr) 2]
                               [else 1]))
         (cond [(rr-space? posn)
                (* rr-mult
                   (match (rrs-owned-by the-owner property-map)
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
                (define rent-vec (property-rent-vec the-property))
                (define multiplier 
                  (cond [(and (= houses 0)
                              (has-monopoly-on-color the-owner 
                                                     (property-color the-property)
                                                     property-map)) 2]
                        [else 1]))
                (* multiplier (vector-ref (property-rent-vec the-property) houses))])]))





;; how many railroads does this player own?
;; index property-map -> index
(define (rrs-owned-by player-num property-map)
  (for/sum ([posn RR-SPACES]
            #:when (equal? player-num
                           (property-state-owner
                            (hash-ref property-map posn
                                      (property-state #f #f)))))
    1))

;; how many properties of this color does this player own?
(define (has-monopoly-on-color player-id color property-map)
  (for/and ([posn (in-list (hash-ref COLORMAP color))])
    (equal? player-id 
            (property-state-owner
             (hash-ref property-map posn (property-state #f #f))))))

;; does some player have a monopoly?
(define (some-player-has-a-monopoly? gs)
  (define prmap (gamestate-property-map gs))
  (for/or ([color COLORS])
    (define owners-of-color
      (for/list ([posn (properties-of-color color)])
        (property-state-owner 
         (hash-ref prmap posn (property-state #f #f)))))
    (and (not (false? (first owners-of-color)))
         (all-the-same owners-of-color))))

;; all the properties of a given color
;; color -> list-of-posns
(define (properties-of-color color)
  (for/list ([(posn p) (in-hash SPACEMAP)]
             #:when (and (property? p)
                         (equal? color (property-color p))))
    posn))

;; are all the values in the (non-empty) list the same?
(define (all-the-same l)
  (for/and ([elt (rest l)]) 
    (equal? elt (first l))))

;; are all of the ownable properties owned?
;; state -> boolean
(define (all-properties-owned? state)
  (define prmap (gamestate-property-map state))
  (for/and ([posn (in-list OWNABLE-SPACES)])
    (hash-ref prmap posn #f)))

;; transfer money from one player to another
;; integer id id gamestate -> gamestate
(define (transfer-money cash from to state)
  (update-gamestate-player 
   from (player-change-cash (- cash))
   (update-gamestate-player
    to (player-change-cash cash)
    state)))

;; transfer money to the bank
;; integer id gamestate -> gamestate
(define (maybe-transfer-to-bank cash from state)
  (match-define (struct gamestate (tvec turn pmap prmap cards)) state)
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
  (update-gamestate-prmap
   (lambda (prmap)
     (for/hash ([(k v) prmap]
                #:when (not (equal? (property-state-owner v) owner)))
       (values k v)))
   state))

;; transfer cash from a player to the bank
(define (transfer-to-bank cash from state)
  (update-gamestate-player from (player-change-cash (- cash)) state))

;; transfer cash from the bank to a player
(define (transfer-from-bank cash from state)
  (transfer-to-bank (- cash) from state))

;; ACTIONS

#;(define (mortgage-property id posn state))


;; change the player's cash
;; nat -> player -> player 
(define ((player-change-cash change) p)
  (player (player-posn p) (+ change (player-cash p)) (player-in-jail? p)))

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
  (match-define (struct gamestate (tvec turn pmap prmap cards)) gs)
  (gamestate tvec
             (modulo (add1 turn) 
                     (vector-length tvec))
             pmap
             prmap
             cards))

;; move the player the given number of spaces, increase cash for
;; passing GO if necessary. Assumes player is not in jail.
;; player index -> player
(define (move-player pl to bonus-possible?)
  (match-define (struct player (posn cash in-jail?)) pl)
  (maybe-log-posn to)
  (define maybe-bonus (cond [bonus-possible? GO-BONUS]
                            [else 0]))
  (cond [(< to posn)
         (player to (+ cash maybe-bonus) #f)]
        [else
         (player to cash #f)]))



;; for testing:
(define (make-prmap old-style-hash)
  (for/hash ([(k v) (in-hash old-style-hash)])
    (values k (property-state v 0))))


;; a convenience function to preserve old test cases
(define (gamestate1 id-vec turn player-map prmap)
  (gamestate id-vec turn player-map (make-prmap prmap) init-card-decks))


