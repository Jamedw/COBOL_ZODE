000000 IDENTIFICATION DIVISION.   
000000 PROGRAM-ID. test.
000000
000000 DATA DIVISION.
000000     WORKING-STORAGE SECTION.
000000     01 WS-TABLE.
000000         05 WS-A PIC A(5) VALUE 'test' OCCURS 5 TIMES.

000000*    comment

000000 PROCEDURE DIVISION.
000000     MOVE "something else" TO WS-A(2)
000000     DISPLAY "another thing " WS-TABLE.
000000     STOP RUN.
