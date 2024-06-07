       IDENTIFICATION DIVISION.
       PROGRAM-ID. MyStack.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  HEAD-PTR       POINTER VALUE NULL.
       01  TEMP-NODE-PTR  POINTER VALUE NULL.
       01  CURR-NODE-PTR  POINTER VALUE NULL. 
       01  STACK-STATUS   PIC X VALUE 'Y'.
           88  STACK-EMPTY VALUE 'Y'.
           88  STACK-NOT-EMPTY VALUE 'N'.
       01  ITEM           PIC X VALUE SPACE.
       01  CHOICE         PIC 9 VALUE 0.
           88  PUSH-CHOICE VALUE 1.
           88  POP-CHOICE VALUE 2.
           88  DISPLAY-CHOICE VALUE 3.
           88  TEST-CHOICE VALUE 4.

           88  EXIT-CHOICE VALUE 5.

       *> TESTING
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

       LINKAGE SECTION. 
       01 CURR-NODE BASED.
           02  NXT POINTER VALUE NULL.
           02  VAL PIC X VALUE SPACE.

       01 TEMP-NODE BASED.
           02  NXT POINTER VALUE NULL.
           02  VAL PIC X VALUE SPACE. 

       PROCEDURE DIVISION.
       
       PERFORM MAIN.

       MAIN.
           DISPLAY "1. Push to Stack"
           DISPLAY "2. Pop from Stack"
           DISPLAY "3. Display Stack"
           DISPLAY "4. Run Tests"
           DISPLAY "5. Exit"

           PERFORM UNTIL EXIT-CHOICE
               DISPLAY "What would you like to do: "
               ACCEPT CHOICE
               EVALUATE TRUE
                   WHEN PUSH-CHOICE
                       PERFORM PUSH-STACK
                   WHEN POP-CHOICE
                       PERFORM POP-STACK
                   WHEN DISPLAY-CHOICE
                       PERFORM DISPLAY-STACK
                   WHEN TEST-CHOICE
                       PERFORM TEST-STACK
                   WHEN EXIT-CHOICE
                       DISPLAY "Exiting program."
                   WHEN OTHER
                       DISPLAY "Invalid choice, try again."
               END-EVALUATE
           END-PERFORM.
       STOP RUN.

       ALLOCATE-NODE.
           ALLOCATE CURR-NODE
               RETURNING CURR-NODE-PTR. 
       
       PUSH-STACK.
           IF NO-TESTING
                  DISPLAY "Enter value to push: "
                  ACCEPT ITEM
           END-IF
           PERFORM ALLOCATE-NODE
           SET ADDRESS OF CURR-NODE TO CURR-NODE-PTR
           MOVE ITEM TO VAL OF CURR-NODE
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
               MOVE VAL OF CURR-NODE TO ITEM
               DISPLAY "Popped value: " ITEM
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
                   DISPLAY VAL OF CURR-NODE
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
           MOVE 'A' TO ITEM
           PERFORM PUSH-STACK
           PERFORM POP-STACK
           IF ITEM = 'A'
               SET TEST-PASSED TO TRUE
           ELSE
               SET TEST-FAILED TO TRUE
           END-IF.

       TEST-MULTIPLE-VALUES.
           PERFORM CLEAR-STACK
           MOVE TEST-ITEM-1 TO ITEM
           PERFORM PUSH-STACK
           MOVE TEST-ITEM-2 TO ITEM
           PERFORM PUSH-STACK
           MOVE TEST-ITEM-3 TO ITEM
           PERFORM PUSH-STACK

           PERFORM POP-STACK
           IF ITEM NOT = TEST-ITEM-3
               DISPLAY "3 FAILED"
               SET TEST-FAILED TO TRUE
           END-IF
           PERFORM POP-STACK
           IF ITEM NOT = TEST-ITEM-2
               DISPLAY ITEM 'AND' TEST-ITEM-2
               SET TEST-FAILED TO TRUE
           END-IF
           PERFORM POP-STACK
           IF ITEM = TEST-ITEM-1
               SET TEST-PASSED TO TRUE
           ELSE
               DISPLAY ITEM 'AND' TEST-ITEM-1
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

       END PROGRAM MyStack.
