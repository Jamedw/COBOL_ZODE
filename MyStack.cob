       *> Stack implementation made with linked list
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MyStack.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  HEAD-PTR    POINTER VALUE NULL. *> Top of stack
       01  TEMP-NODE-PTR   POINTER VALUE NULL.
       01  CURR-NODE-PTR   POINTER VALUE NULL. 
       01  STACK-STATUS    PIC X VALUE 'Y'.
           88  STACK-EMPTY VALUE 'Y'.
           88  STACK-NOT-EMPTY VALUE 'N'.
       01  ITEM    PIC X VALUE SPACE. *> Item to put into stack
       01  CHOICE  PIC 9 VALUE 0.
           88  PUSH-CHOICE VALUE 1.
           88  POP-CHOICE VALUE 2.
           88  DISPLAY-CHOICE VALUE 3.
           88  TEST-CHOICE VALUE 4.

           88  EXIT-CHOICE VALUE 5.

       *> For testing
       01  READING PIC X VALUE 'N'.
           88 NO-READING   VALUE 'N'.
           88 YES-READING  VALUE 'Y'.
       01  TEST-STATUS PIC X VALUE 'N'.
           88  TEST-PASSED VALUE 'Y'.
           88  TEST-FAILED VALUE 'N'.
       01  TEST-ITEM-1 PIC X VALUE 'A'.
       01  TEST-ITEM-2 PIC X VALUE 'B'.
       01  TEST-ITEM-3 PIC X VALUE 'C'.
       01  STACK-OUTPUT    PIC X(10).

       *> For dynamic allocation 
       LINKAGE SECTION. 
       01 CURR-NODE BASED.
           02  NXT POINTER VALUE NULL.
           02  VAL PIC X VALUE SPACE.

       01 TEMP-NODE BASED.
           02  NXT POINTER VALUE NULL.
           02  VAL PIC X VALUE SPACE. 

       PROCEDURE DIVISION.
       
       PERFORM MAIN.

       *> Main function to ask user what they want to do. 
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
       
       *> Function to push "ITEM" onto top of stack
       PUSH-STACK.
           IF YES-READING
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

       *> Remove top value of stack. It will be accessible in "ITEM" 
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
       
       *> Show contents of stack
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

       *> Free the stack
       CLEAR-STACK.
           SET TEMP-NODE-PTR TO HEAD-PTR
           PERFORM UNTIL TEMP-NODE-PTR = NULL
               SET CURR-NODE-PTR TO TEMP-NODE-PTR
               SET ADDRESS OF CURR-NODE TO CURR-NODE-PTR
               SET TEMP-NODE-PTR TO NXT OF CURR-NODE
               FREE CURR-NODE
           END-PERFORM
           SET STACK-EMPTY TO TRUE.

       *> Testing
       TEST-STACK.
           SET NO-READING TO TRUE
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
           END-IF
           
           SET TEST-FAILED TO TRUE

           SET YES-READING TO TRUE.
       
       *> Check single push and pop works as expected. 
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

       *> Check multiple pushes and pops work as expected. 
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
               SET TEST-FAILED TO TRUE
           END-IF
           PERFORM POP-STACK
           IF ITEM NOT = TEST-ITEM-2
               SET TEST-FAILED TO TRUE
           END-IF
           PERFORM POP-STACK
           IF ITEM = TEST-ITEM-1
               SET TEST-PASSED TO TRUE
           ELSE
               SET TEST-FAILED TO TRUE
           END-IF.

       END PROGRAM MyStack.
