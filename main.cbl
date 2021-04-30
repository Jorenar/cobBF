       IDENTIFICATION DIVISION.
       PROGRAM-ID. Brainfuck.
       AUTHOR.     Jorengarenar.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CLASS bf-symbols IS '>', '<', '+', '-', '.', ',', '[', ']' .

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ifile ASSIGN TO DYNAMIC ws-filename
               ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD ifile.
       01 ifile-chars PIC X(30000).
           88 eof-flag VALUE HIGH-VALUE.

       WORKING-STORAGE SECTION.
      * Don't forget to also change `ifile` and in `interpret.cbl`!
       78 ws-array-size VALUE 30000.

       01 ws-bf-full-code.
           02 ws-bf   PIC X(1) OCCURS ws-array-size TIMES INDEXED BY i.
       01 ws-temp-full-code.
           02 ws-temp PIC X(1) OCCURS ws-array-size TIMES INDEXED BY j.

       01 ws-filename    PIC X(300).
       01 ws-braces-flag PIC 9999 VALUE ZERO.
       01 ws-s-idx       PIC 99999 VALUE 1.
       01 ws-e-idx       PIC 99999.

       PROCEDURE DIVISION.
       000-MAIN-PARA.
           ACCEPT ws-filename FROM COMMAND-LINE

           OPEN INPUT ifile.
           PERFORM UNTIL eof-flag
               READ ifile INTO ws-temp-full-code
                   AT END
                       SET eof-flag TO TRUE
                   NOT AT END
                       PERFORM
                           PREPROCESS-PARA
                           VARYING j FROM 1 BY 1
                           UNTIL j > ws-array-size
               END-READ
           END-PERFORM.
           CLOSE ifile.

           PERFORM CHECK-BRACES-PARA.

           MOVE i TO ws-e-idx
           CALL 'interpret' USING ws-bf-full-code, ws-s-idx, ws-e-idx
           STOP RUN.

       PREPROCESS-PARA.
           IF ws-temp(j) IS bf-symbols THEN
               MOVE ws-temp(j) TO ws-bf(i)
               SET i UP BY 1
           END-IF.

       CHECK-BRACES-PARA.
           PERFORM
               CHECK-IF-BRACE-PARA
               VARYING i FROM 1 BY 1
               UNTIL ws-bf(i) = SPACE OR ws-braces-flag < 0

           IF ws-braces-flag NOT EQUAL 0 THEN
               DISPLAY "Unbalanced braces!"
               STOP RUN
           END-IF.

       CHECK-IF-BRACE-PARA.
           EVALUATE ws-bf(i)
               WHEN '['
                   ADD 1 TO ws-braces-flag
               WHEN ']'
                   SUBTRACT 1 FROM ws-braces-flag
           END-EVALUATE.
