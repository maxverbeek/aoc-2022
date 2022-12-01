      * Sample COBOL program
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT STUDENT ASSIGN TO KEYBOARD
           ORGANIZATION IS LINE SEQUENTIAL.            

       DATA DIVISION.
       FILE SECTION.
       FD STUDENT.
       01 STUDENT-FILE.
           05 STUDENT-ID PIC X(10).

       WORKING-STORAGE SECTION.
       01 WS-CURR PIC 9(20).
       01 WS-HIGHEST PIC 9(20) VALUE ZEROES.
       01 WS-HIGHEST2 PIC 9(20) VALUE ZEROES.
       01 WS-HIGHEST3 PIC 9(20) VALUE ZEROES.
       01 WS-TOTAL PIC 9(20) VALUE ZEROES.
       01 WS-N PIC 9(20).
       01 WS-LINE PIC X(20).
       01 WS-EOF PIC A(1). 

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT STUDENT.
           PERFORM UNTIL WS-EOF='Y'
               READ STUDENT INTO WS-LINE
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END PERFORM CheckBlock
               END-READ
           END-PERFORM.
           PERFORM FinishBlock.
           DISPLAY WS-HIGHEST.
           DISPLAY WS-HIGHEST2.
           DISPLAY WS-HIGHEST3.
           COMPUTE WS-TOTAL = WS-HIGHEST + WS-HIGHEST2 + WS-HIGHEST3.
           DISPLAY WS-TOTAL.
           CLOSE STUDENT.
           STOP RUN.

       CheckBlock.
           IF WS-LINE NOT EQUAL SPACES
               MOVE WS-LINE TO WS-N
               COMPUTE WS-CURR = WS-CURR + WS-N
               MOVE 0 TO WS-N
           ELSE
               PERFORM FinishBlock
           END-IF.

        FinishBlock.
           IF WS-CURR > WS-HIGHEST
               MOVE WS-HIGHEST2 TO WS-HIGHEST3
               MOVE WS-HIGHEST TO WS-HIGHEST2
               MOVE WS-CURR TO WS-HIGHEST
           ELSE
               IF WS-CURR > WS-HIGHEST2
                   MOVE WS-HIGHEST2 TO WS-HIGHEST3
                   MOVE WS-CURR TO WS-HIGHEST2
               ELSE
                   IF WS-CURR > WS-HIGHEST3
                       MOVE WS-CURR TO WS-HIGHEST3
                   END-IF
               END-IF
           END-IF.
           MOVE 0 TO WS-CURR.
