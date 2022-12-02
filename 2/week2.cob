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
           05 FILLER-1 PIC X.
           05 FILLER-2 PIC X.
           05 FILLER-3 PIC X.

       WORKING-STORAGE SECTION.
       01 WS-SCORE PIC 9(20).
       01 WS-LINE.
            05 WS-OPP PIC X.
            05 WS-BLANK PIC X.
            05 WS-OUR PIC X.
       01 WS-EOF PIC A(1). 

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT STUDENT.
           PERFORM UNTIL WS-EOF='Y'
               READ STUDENT INTO WS-LINE
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END PERFORM PredictShape
               END-READ
           END-PERFORM.
           DISPLAY WS-SCORE.
           CLOSE STUDENT.
           STOP RUN.

       PredictShape.
           EVALUATE WS-OUR
               WHEN 'X'
                   EVALUATE WS-OPP
                       WHEN 'A'
                           MOVE 'Z' TO WS-OUR
                       WHEN 'B'
                           MOVE 'X' TO WS-OUR
                       WHEN 'C'
                           MOVE 'Y' TO WS-OUR
                   END-EVALUATE
                WHEN 'Y'
                    EVALUATE WS-OPP
                        WHEN 'A'
                            MOVE 'X' TO WS-OUR
                        WHEN 'B'
                            MOVE 'Y' TO WS-OUR
                        WHEN 'C'
                            MOVE 'Z' TO WS-OUR
                    END-EVALUATE
                WHEN 'Z'
                    EVALUATE WS-OPP
                        WHEN 'A'
                            MOVE 'Y' TO WS-OUR
                        WHEN 'B'
                            MOVE 'Z' TO WS-OUR
                        WHEN 'C'
                            MOVE 'X' TO WS-OUR
                    END-EVALUATE
           END-EVALUATE.
           PERFORM AddGameScore.

       AddGameScore.
           EVALUATE WS-OUR
               WHEN 'X'
                   EVALUATE WS-OPP
                       WHEN 'A'
                           COMPUTE WS-SCORE = WS-SCORE + 3
                       WHEN 'B'
                           COMPUTE WS-SCORE = WS-SCORE + 0
                       WHEN 'C'
                           COMPUTE WS-SCORE = WS-SCORE + 6
                   END-EVALUATE
                WHEN 'Y'
                    EVALUATE WS-OPP
                        WHEN 'A'
                            COMPUTE WS-SCORE = WS-SCORE + 6
                        WHEN 'B'
                            COMPUTE WS-SCORE = WS-SCORE + 3
                        WHEN 'C'
                            COMPUTE WS-SCORE = WS-SCORE + 0
                    END-EVALUATE
                WHEN 'Z'
                    EVALUATE WS-OPP
                        WHEN 'A'
                            COMPUTE WS-SCORE = WS-SCORE + 0
                        WHEN 'B'
                            COMPUTE WS-SCORE = WS-SCORE + 6
                        WHEN 'C'
                            COMPUTE WS-SCORE = WS-SCORE + 3
                    END-EVALUATE
           END-EVALUATE.
           PERFORM AddShapeScore.

       AddShapeScore.
           EVALUATE WS-OUR
               WHEN 'X'
                   COMPUTE WS-SCORE = WS-SCORE + 1

               WHEN 'Y'
                   COMPUTE WS-SCORE = WS-SCORE + 2

               WHEN 'Z'
                   COMPUTE WS-SCORE = WS-SCORE + 3
           END-EVALUATE.
