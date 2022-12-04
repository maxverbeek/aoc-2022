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
           05 FILLER-1 PIC 9(3).
           05 FILLER-2 PIC X.
           05 FILLER-3 PIC 9(3).
           05 FILLER-4 PIC X.
           05 FILLER-5 PIC 9(3).
           05 FILLER-6 PIC X.
           05 FILLER-7 PIC 9(3).

       WORKING-STORAGE SECTION.
       01 WS-COUNT PIC 9(20) VALUE 0.
       01 WS-LINE.
           05 NUM-1 PIC 9(3).
           05 FILLER-2 PIC X.
           05 NUM-2 PIC 9(3).
           05 FILLER-4 PIC X.
           05 NUM-3 PIC 9(3).
           05 FILLER-6 PIC X.
           05 NUM-4 PIC 9(3).
       01 WS-EOF PIC A(1). 
       01 WS-SURFACETOTAL PIC 9(6). 

       PROCEDURE DIVISION.
      * All the numbers of the input for this program are left-padded
      * with zeroes so that they all have a length of 3 digits. See the
      * leftpad.py python file for this.
       MAIN-PROCEDURE.
           OPEN INPUT STUDENT.
           PERFORM UNTIL WS-EOF='Y'
               READ STUDENT INTO WS-LINE
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END PERFORM CountOverlap
               END-READ
           END-PERFORM.
           DISPLAY WS-COUNT.
           CLOSE STUDENT.
           STOP RUN.

       CountPairs.
           IF NUM-1 >= NUM-3 AND NUM-2 <= NUM-4
               COMPUTE WS-COUNT = WS-COUNT + 1
           ELSE
               IF NUM-3 >= NUM-1 AND NUM-4 <= NUM-2
                   COMPUTE WS-COUNT = WS-COUNT + 1
               END-IF
           END-IF.

      * Count the overlap by computing the total surface area
      * and counting the surface area of the the patches done by the
      * elves. If these patches added up do not fit in the total surface
      * area, then by the pigeon hole principle there must be overlap.
       CountOverlap.
           SET WS-SURFACETOTAL TO 0.
           COMPUTE WS-SURFACETOTAL = FUNCTION MAX(NUM-2, NUM-4) -
           FUNCTION MIN(NUM-1, NUM-3) + 1.

           IF NUM-2 - NUM-1 + NUM-4 - NUM-3 + 2 > WS-SURFACETOTAL
               COMPUTE WS-COUNT = WS-COUNT + 1
           END-IF.
