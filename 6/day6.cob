      * Sample COBOL program
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-COUNT PIC 9(20).
       01 WS-LINE.
           05 WS-CHARS PIC X OCCURS 9999 TIMES INDEXED BY WS-IDX1.
       01 WS-LEN PIC 9(10).
       01 WS-CHAR PIC X.
       01 WS-I PIC 9(10).
       01 WS-START-I PIC 9(10).
       01 WS-START-J PIC 9(10).
       01 WS-J PIC 9(10).

      * For part 1:
      * 01 WS-RANGE PIC 9(10) VALUE 4.
      
      * For part 2:
       01 WS-RANGE PIC 9(10) VALUE 14.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           ACCEPT WS-LINE.
           INSPECT WS-LINE TALLYING WS-LEN FOR CHARACTERS BEFORE INITIAL
               SPACES.
           PERFORM CountCharBody VARYING WS-START-I FROM 1 BY 1 UNTIL
           WS-START-I > WS-LEN - WS-RANGE
           STOP RUN.

       CountCharBody.
           PERFORM VARYING WS-I FROM WS-START-I BY 1 UNTIL
               WS-I EQUAL WS-START-I + WS-RANGE
               COMPUTE WS-START-J = WS-I + 1
               PERFORM VARYING WS-J FROM WS-START-J BY 1 UNTIL
                   WS-J = WS-START-I + WS-RANGE
                   IF WS-CHARS(WS-I) EQUAL WS-CHARS(WS-J)
                       EXIT PARAGRAPH
                   END-IF
               END-PERFORM
           END-PERFORM.

           COMPUTE WS-J = WS-J - 1.

           DISPLAY WS-J.
           STOP RUN.
