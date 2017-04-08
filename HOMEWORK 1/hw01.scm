#lang scheme






;our grid is a list which has included 4 sub-list and each sub-lists have 5 elements 
(define initialize-grid
  (lambda (row column)
    (define per-row
      (lambda (column)
        (if (= 0 column)
            null
            (cons 1 (per-row (- column 1))))))
    (define a-row (per-row column))
    (define (build-grid row)
      (if (= 1 row)
          (list a-row)
          (cons a-row (build-grid (- row 1)))))
    (let ([grid (build-grid row)])
      (cons (cons "*" (cdr (car grid)))
            (sub-list grid 2 (length grid))))))

;To create each sub_list we use this procedure
(define sub-list
  (lambda (l from to)
    (define sub-list-from
      (lambda (l from-elt)
        (if (= 1 from-elt)
            l
            (sub-list-from (cdr l)
                           (- from-elt 1)))))
    (define sub-list-to
      (lambda (l untill-elt)
        (if (= 0 untill-elt)
            null
            (append (list (car l))
                    (sub-list-to (cdr l) (- untill-elt 1))))))
    (sub-list-to (sub-list-from l from)
                 (- to (- from 1)))))

;then we display our grid with this procedure
(define display-grid
  (lambda (grid)
    (let loop ([n (length grid)]
               [grid grid])
      (display "\n")
      (display (car grid))
      (if (= n 1)
          (void)
          (loop (- n 1) (cdr grid))))))

;To check whether input is correct or not
(define check-input
  (lambda (row column max-row max-column)
    (cond [(> row max-row) false]
          [(< row 1) false]
          [(> column max-column) false]
          [(< column 1) false]
          [else true] )))

;If input is correct then eat corresponding cookies
(define eat
  (lambda (row column grid)
    (let ([search-row (get-elt grid row)])
      (let ([search-column (get-elt search-row column)])
        (cond [(equal? search-column 0) false]
              [else true] )))))

;To eat corresponding cookies we use get-elt procedure
(define get-elt
  (lambda (list n)
    (if (= n 1)
        (car list)
        (get-elt (cdr list) (- n 1)))))

;To check, who is selected poison cookie
(define lose?
  (lambda (row column)
    (and (= row 1) (= column 1))))


;After each operation we uptade our list with this procedure
(define current-update-grid
  (lambda (grid row column)
    (define update-rows
      (lambda (rows nth-elt)
        (let loop ([zero-list (all-zero-list (- (length (car rows)) (- nth-elt 1)))]
                   [rows rows])
          (if (null? rows)
              null
              (append (list (append (sub-list (car rows) 1 (- nth-elt 1))
                                    zero-list))
                      (loop zero-list (cdr rows)))))))
    (define all-zero-list
      (lambda (n)
        (if (= n 0)
            null
            (cons 0 (all-zero-list (- n 1))))))

    (append (sub-list grid 1 (- row 1))
            (update-rows (sub-list grid row (length grid))
                         column))))

;Four procedures belongs to human move

;take row input as a string
(define user-move-row
  (lambda (input)
    (display input)
    (read-line)))

;then convert to integer
(define convert-string-to-number-row
  (lambda ()
    (string->number (user-move-row "\nEnter row:"))))

;take column input as a string
(define user-move-column
  (lambda (input)
    (display input)
    (read-line)))

;then convert to integer
(define convert-string-to-number-column
  (lambda ()
    (string->number (user-move-column "\nEnter column:"))))


;Three procedures belongs to computer move

;choose any arbitrary number for row
(define computer-move-row1
  (lambda ()
    (+ 1 (random 5))))

;choose any arbitrary number for column
(define computer-move-column1
  (lambda ()
    (+ 1 (random 5))))

;We assumed that computer always moves correctly
;And we hold input as a list
(define computer-input-is-always-valid
  (lambda (grid)
    (define computer-row (computer-move-row1))
    (define computer-column (computer-move-column1))
    (cond
      [(check-input computer-row computer-column 4 5)
       (cond
         [(eat computer-row computer-column grid)
          (cons computer-row computer-column)]
         [else (computer-input-is-always-valid grid)])]
      [else (computer-input-is-always-valid grid)])))
    
  


 
                  
(define intro
  (lambda()
    (define first-grid (initialize-grid 4 5))
    (display "Welcome Chomp\n") (display "Here is the game table:\n")
    (display-grid first-grid)
    ))
    
            


(define play-interface
  (lambda (whoIsPlaying row column update-grid)
  (define play-grid update-grid)
  (define listOfComputerMoves (computer-input-is-always-valid play-grid))
  (define computer-move-row (car listOfComputerMoves))
  (define computer-move-column (cdr listOfComputerMoves))

  (cond
    [(= 0 whoIsPlaying);if it is computer turn
     (cond;begin poison cookie operation
       [(lose? computer-move-row computer-move-column);if computer choosed poison cookie
        (display "\nComputer wanted to eat first row and first column cookie that's why ")
        (display "Computer is lost\n")]
       [else (display "\nComputer have eaten ")
             (display computer-move-row) (display ". row and ") (display computer-move-column) (display ". column\n")
             (display "Computer played\n..\nAfter computer turn table is :: \n")
             (display-grid (current-update-grid play-grid computer-move-row computer-move-column));otherwise update grid with computer moves
             (play-interface 1 4 5 (current-update-grid play-grid computer-move-row computer-move-column))]
       )];end of computer turn

    [(= 1 whoIsPlaying);if it is human turn
     (define human-row (convert-string-to-number-row));take input from user
     (define human-column (convert-string-to-number-column));take input from user
     (cond ;begin valid input process
       [(check-input human-row human-column 4 5) ;if human inputs are correct
        (cond
          [(eat human-row human-column play-grid) ;if cookie has not been eaten before
           (cond
             [(lose? human-row human-column) ;if human choosed poison cookie
              (display "Human lost")] ;then he lost
             [else (display "\nHuman played\n..\nAfter human turn table is ::\n");otherwise update grid
                   (display-grid (current-update-grid play-grid human-row human-column))
                   (play-interface 0 4 5 (current-update-grid play-grid human-row human-column))] ;and return function
             )]
          [else (display "\nThat cookie have eaten before, play again\n");if cookie has been eaten already
                (play-interface 1 4 5 play-grid)]
          )]
        [else (display "\nYour input is incorrect, play again\n")
              (play-interface 1 4 5 play-grid)]
        )]
    )))


(intro)
(play-interface 1 4 5 (initialize-grid 4 5))

  
                 
                                      
                 
  



    
        
                  
          
 

            
            
            
        
 
    

            
        
  