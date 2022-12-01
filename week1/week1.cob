      * Sample COBOL program
       IDENTIFICATION DIVISION.
       PROGRAM-ID. hello.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SYSIN ASSIGN TO KEYBOARD ORGANIZATION LINE SEQUENTIAL.
       DATA DIVISION.
       
       FILE SECTION.
       FD SYSIN.
       01 ln PIC X(255).
           88 EOF VALUE HIGH-VALUES.
       WORKING-STORAGE SECTION.
           01 n PIC 9(10).
           01 highest PIC 9(10).
           01 curr PIC 9(10).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY 'reading blocks'.
           OPEN INPUT SYSIN.
           PERFORM ReadBlocks UNTIL EOF.
           DISPLAY highest.
           CLOSE SYSIN.
           STOP RUN.
        ReadBlocks.
           DISPLAY 'single block'.
           PERFORM ReadBlock UNTIL ln EQUAL SPACES.
           IF curr > highest
               MOVE curr TO highest
           END-IF.
           MOVE 0 to CURR.
        ReadBlock.
           READ SYSIN
               AT END SET EOF TO TRUE
           END-READ.
           IF ln NOT EQUAL SPACES OR NOT EOF
               MOVE ln TO n
               COMPUTE curr = curr + n
           END-IF.
