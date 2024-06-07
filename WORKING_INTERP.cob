       IDENTIFICATION DIVISION.
       PROGRAM-ID. AST_BUILD.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01  HEAD-PTR       POINTER VALUE NULL.
       01  TEMP-NODE-PTR  POINTER VALUE NULL.
       01  CURR-NODE-PTR  POINTER VALUE NULL. 
       01  STACK-STATUS   PIC X VALUE 'Y'.
           88  STACK-EMPTY VALUE 'Y'.
           88  STACK-NOT-EMPTY VALUE 'N'.
       01  ALP-ITEM       PIC X(9) VALUE SPACE.
       01  NUM-ITEM       PIC S9(9) VALUE -1.
       01  PTR-ITEM       POINTER VALUE NULL.
       01  CHOICE         PIC 9 VALUE 0.
           88  PUSH-CHOICE VALUE 1.
           88  POP-CHOICE VALUE 2.
           88  DISPLAY-CHOICE VALUE 3.
           88  TEST-CHOICE VALUE 4.

           88  EXIT-CHOICE VALUE 5.

       *> FOR TESTING
       01  TESTING            PIC X VALUE 'N'.
           88 NO-TESTING      VALUE 'N'.
           88 YES-TESTING     VALUE 'Y'.
       01  TEST-STATUS        PIC X VALUE 'N'.
           88  TEST-PASSED    VALUE 'Y'.
           88  TEST-FAILED    VALUE 'N'.
       01  TEST-ITEM-1        PIC X VALUE 'A'.
       01  TEST-ITEM-2        PIC X VALUE 'B'.
       01  TEST-ITEM-3        PIC X VALUE 'C'.
       01  STACK-OUTPUT       PIC X(10).




       
       01 TOP-EXPR-PTR POINTER VALUE NULL.
      * POINTER 0 is the one ALL calls return to
       01 WORKING-EXPR-PTR-0 POINTER VALUE NULL.
       01 WORKING-EXPR-PTR-1 POINTER VALUE NULL.
       01 WORKING-EXPR-PTR-2 POINTER VALUE NULL.
       01 WORKING-EXPR-PTR-3 POINTER VALUE NULL.
       
      * WORKING-VALUE-0 is set in ALL language terminal calls
       01 WORKING-VALUE-0 PIC S9(9) VALUE -1.
       01 WORKING-VALUE-1 PIC S9(9) VALUE -1.
       01 WORKING-VALUE-2 PIC S9(9) VALUE -1.

       01  INTERP-CHOICE         PIC 9 VALUE 0.
           88  NUMC-CHOICE VALUE 0.
           88  PLUSC-CHOICE VALUE 1.


       LINKAGE SECTION. 

       01 CURR-NODE BASED.
           02  NXT POINTER VALUE NULL.
           02  NUM-VAL PIC S9(9) VALUE -1.
           02  ALP-VAL PIC X(9) VALUE SPACE.
           02  PTR-VAL POINTER VALUE NULL.

       01 TEMP-NODE BASED.
           02  NXT POINTER VALUE NULL.
           02  NUM-VAL PIC S9(9) VALUE -1.
           02  ALP-VAL PIC X(9) VALUE SPACE.
           02  PTR-VAL POINTER VALUE NULL.
       
      
      * JANKY TYPE CASTING

      * ZODE_ID = 0 = NUMC
      * ZODE_ID = 1 = PLUSC

       01 GEN_ZODE BASED.
           02 ZODE_ID PIC X VALUE SPACE.


       01 NUMC BASED.
           02 ZODE_ID PIC X VALUE SPACE.
           02 VAL PIC X VALUE SPACE.

       01 PLUSC BASED.
           02 ZODE_ID PIC X VALUE SPACE.
           02 LT POINTER VALUE NULL.
           02 RT POINTER VALUE NULL.

       PROCEDURE DIVISION.
       PERFORM MAIN.


       MAIN.
           SET YES-TESTING TO TRUE
          
      *    
           PERFORM BUILD-AST.

           MOVE TOP-EXPR-PTR to WORKING-EXPR-PTR-0
           PERFORM INTERP.
           
           PERFORM POP-STACK

           DISPLAY "RESULT: "
           DISPLAY NUM-ITEM
           
       STOP RUN.

       BUILD-AST.
      *    BUILD THE AST (PlusC (PlusC (NumC 1) (NumC 2)) (NumC 10))
           PERFORM ALLOCATE-NUMC.
           MOVE WORKING-EXPR-PTR-0 TO WORKING-EXPR-PTR-1
           SET ADDRESS OF NUMC TO WORKING-EXPR-PTR-1
           MOVE 0 TO ZODE_ID OF NUMC.
           MOVE 1 TO VAL OF NUMC.
           

           PERFORM ALLOCATE-NUMC.
           MOVE WORKING-EXPR-PTR-0 TO WORKING-EXPR-PTR-2
           SET ADDRESS OF NUMC TO WORKING-EXPR-PTR-2
           MOVE 0 TO ZODE_ID OF NUMC.
           MOVE 2 TO VAL OF NUMC.

           PERFORM ALLOCATE-PLUSC.
           MOVE WORKING-EXPR-PTR-0 TO WORKING-EXPR-PTR-3
           SET ADDRESS OF PLUSC TO WORKING-EXPR-PTR-3
           MOVE 1 TO ZODE_ID OF PLUSC.
           MOVE WORKING-EXPR-PTR-1 TO LT OF PLUSC
           MOVE WORKING-EXPR-PTR-2 TO RT OF PLUSC
       
      *    PTR-1 is now PLUSC, PTR-2 & PTR-3 are free
           MOVE WORKING-EXPR-PTR-3 TO WORKING-EXPR-PTR-1

           PERFORM ALLOCATE-NUMC.
           MOVE WORKING-EXPR-PTR-0 TO WORKING-EXPR-PTR-2
           SET ADDRESS OF NUMC TO WORKING-EXPR-PTR-2
           MOVE 0 TO ZODE_ID OF NUMC.
           MOVE 8 TO VAL OF NUMC.


           PERFORM ALLOCATE-PLUSC.
           MOVE WORKING-EXPR-PTR-0 TO WORKING-EXPR-PTR-3
           SET ADDRESS OF PLUSC TO WORKING-EXPR-PTR-3
           MOVE 1 TO ZODE_ID OF PLUSC.
           MOVE WORKING-EXPR-PTR-1 TO LT OF PLUSC
           MOVE WORKING-EXPR-PTR-2 TO RT OF PLUSC


           MOVE WORKING-EXPR-PTR-3 TO TOP-EXPR-PTR.
          
       INTERP.
           SET ADDRESS OF GEN_ZODE TO WORKING-EXPR-PTR-0.
           MOVE ZODE_ID OF GEN_ZODE TO INTERP-CHOICE
           EVALUATE TRUE
               WHEN NUMC-CHOICE
                   SET ADDRESS OF NUMC TO WORKING-EXPR-PTR-0
                   MOVE VAL OF NUMC TO NUM-ITEM
                   PERFORM PUSH-STACK

               WHEN PLUSC-CHOICE
                   SET ADDRESS OF PLUSC TO WORKING-EXPR-PTR-0
                   MOVE RT TO WORKING-EXPR-PTR-0
                   PERFORM INTERP

                   MOVE LT TO WORKING-EXPR-PTR-0
                   PERFORM INTERP

                   PERFORM POP-STACK
                   MOVE NUM-ITEM TO WORKING-VALUE-1
                   PERFORM POP-STACK
                   MOVE NUM-ITEM TO WORKING-VALUE-2

                   ADD WORKING-VALUE-1 TO WORKING-VALUE-2
                    GIVING NUM-ITEM

                   PERFORM PUSH-STACK

           END-EVALUATE.

       ALLOCATE-PLUSC.
           ALLOCATE PLUSC
               RETURNING WORKING-EXPR-PTR-0.

       ALLOCATE-NUMC.
           ALLOCATE NUMC
               RETURNING WORKING-EXPR-PTR-0.


       ALLOCATE-NODE.
           ALLOCATE CURR-NODE
               RETURNING CURR-NODE-PTR. 
       
       PUSH-STACK.
           IF NO-TESTING
                  DISPLAY "Enter value to push: "
                  ACCEPT ALP-ITEM
           END-IF
           PERFORM ALLOCATE-NODE
           SET ADDRESS OF CURR-NODE TO CURR-NODE-PTR
           MOVE NUM-ITEM TO NUM-VAL OF CURR-NODE
           MOVE ALP-ITEM TO ALP-VAL OF CURR-NODE
           MOVE PTR-ITEM TO PTR-VAL OF CURR-NODE
           IF STACK-EMPTY
               SET HEAD-PTR TO CURR-NODE-PTR
               SET STACK-NOT-EMPTY TO TRUE
           ELSE    
               SET ADDRESS OF CURR-NODE TO CURR-NODE-PTR
               SET NXT OF CURR-NODE TO HEAD-PTR
               SET HEAD-PTR TO CURR-NODE-PTR
           END-IF.

       POP-STACK.
           IF STACK-EMPTY
               DISPLAY "Stack is empty."
           ELSE
               SET CURR-NODE-PTR TO HEAD-PTR
               SET ADDRESS OF CURR-NODE TO CURR-NODE-PTR
               SET HEAD-PTR TO NXT OF CURR-NODE
               MOVE NUM-VAL OF CURR-NODE TO NUM-ITEM
               MOVE ALP-VAL OF CURR-NODE TO ALP-ITEM
               MOVE PTR-VAL OF CURR-NODE TO PTR-ITEM

               IF NO-TESTING
                  DISPLAY "Popped ALP value: " ALP-ITEM
                  DISPLAY "Popped NUM value: " NUM-ITEM
                  DISPLAY "Popped PTR value: " PTR-ITEM
               END-IF
        
               FREE CURR-NODE
               IF HEAD-PTR = NULL
                   SET STACK-EMPTY TO TRUE
               END-IF
           END-IF.
       
       DISPLAY-STACK.
           IF STACK-EMPTY
               DISPLAY "Stack is empty."
           ELSE
               DISPLAY "STACK: "
               SET TEMP-NODE-PTR TO HEAD-PTR
               PERFORM UNTIL TEMP-NODE-PTR = NULL
                   SET CURR-NODE-PTR TO TEMP-NODE-PTR
                   SET ADDRESS OF CURR-NODE TO CURR-NODE-PTR
                   DISPLAY NUM-VAL OF CURR-NODE
                   DISPLAY ALP-VAL OF CURR-NODE
                   DISPLAY PTR-VAL OF CURR-NODE
                   SET TEMP-NODE-PTR TO NXT OF CURR-NODE
               END-PERFORM
           END-IF.

       TEST-STACK.
           SET YES-TESTING TO TRUE
           DISPLAY "Running Stack Tests..."

           PERFORM TEST-PUSH-POP
           IF TEST-PASSED
               DISPLAY "TEST-PUSH-POP PASSED"
           ELSE
               DISPLAY "TEST-PUSH-POP FAILED"
           END-IF
           
           SET TEST-FAILED TO TRUE
           
           PERFORM TEST-MULTIPLE-VALUES
           IF TEST-PASSED
               DISPLAY "TEST-MULTIPLE-VALUES PASSED"
           ELSE
               DISPLAY "TEST-MULTIPLE-VALUES FAILED"
           END-IF.

       TEST-PUSH-POP.
           PERFORM CLEAR-STACK
           MOVE 'A' TO ALP-ITEM
           PERFORM PUSH-STACK
           PERFORM POP-STACK
           IF ALP-ITEM = 'A'
               SET TEST-PASSED TO TRUE
           ELSE
               SET TEST-FAILED TO TRUE
           END-IF.

       TEST-MULTIPLE-VALUES.
           PERFORM CLEAR-STACK
           MOVE TEST-ITEM-1 TO ALP-ITEM
           PERFORM PUSH-STACK
           MOVE TEST-ITEM-2 TO ALP-ITEM
           PERFORM PUSH-STACK
           MOVE TEST-ITEM-3 TO ALP-ITEM
           PERFORM PUSH-STACK

           PERFORM POP-STACK
           IF ALP-ITEM NOT = TEST-ITEM-3
               DISPLAY "3 FAILED"
               SET TEST-FAILED TO TRUE
           END-IF
           PERFORM POP-STACK
           IF ALP-ITEM NOT = TEST-ITEM-2
               DISPLAY ALP-ITEM 'AND' TEST-ITEM-2
               SET TEST-FAILED TO TRUE
           END-IF
           PERFORM POP-STACK
           IF ALP-ITEM = TEST-ITEM-1
               SET TEST-PASSED TO TRUE
           ELSE
               DISPLAY ALP-ITEM 'AND' TEST-ITEM-1
               SET TEST-FAILED TO TRUE
           END-IF.

       CLEAR-STACK.
           SET TEMP-NODE-PTR TO HEAD-PTR
           PERFORM UNTIL TEMP-NODE-PTR = NULL
               SET CURR-NODE-PTR TO TEMP-NODE-PTR
               SET ADDRESS OF CURR-NODE TO CURR-NODE-PTR
               SET TEMP-NODE-PTR TO NXT OF CURR-NODE
               FREE CURR-NODE
           END-PERFORM
           SET STACK-EMPTY TO TRUE.










