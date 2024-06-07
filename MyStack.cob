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
       01  ITEM           PIC X.
       01  CHOICE         PIC 9 VALUE 0.
           88  PUSH-CHOICE VALUE 1.
           88  POP-CHOICE VALUE 2.
           88  DISPLAY-CHOICE VALUE 3.
           88  EXIT-CHOICE VALUE 4.

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
           DISPLAY "4. Exit"
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
           DISPLAY "Enter value to push: "
           ACCEPT ITEM
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
               DISPLAY "Popped value: " VAL OF CURR-NODE
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
