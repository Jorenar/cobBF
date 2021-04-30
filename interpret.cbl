       IDENTIFICATION DIVISION.
       PROGRAM-ID. interpret RECURSIVE.
       AUTHOR.     Jorengarenar.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT f-sysin ASSIGN TO KEYBOARD
               ORGANIZATION LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD f-sysin.
       01 f-sysin-char PIC X.

       WORKING-STORAGE SECTION.
       78 ws-array-size VALUE 30000.

       01 ws-mem-array.
           02 ws-tape PIC 999
               VALUE 0 OCCURS ws-array-size
               TIMES INDEXED BY idx.

       LOCAL-STORAGE SECTION.
       01 lss-char-tmp PIC 999.
       01 lss-braces PIC 999 VALUE ZERO.
       01 lss-s PIC 99999.
       01 lss-i PIC 99999 VALUE 1.

       LINKAGE SECTION.
       01 ls-s PIC 99999.
       01 ls-e PIC 99999.
       01 ls-bf-full-code.
           02 ls-bf PIC X(1) OCCURS ws-array-size TIMES.

       PROCEDURE DIVISION USING ls-bf-full-code, ls-s, ls-e.
       000-BEGIN-PARA.
           PERFORM
               EVAL-SYMBOL-PARA
               VARYING lss-i FROM ls-s BY 1
               UNTIL lss-i = ls-e

           EXIT PROGRAM.

       EVAL-SYMBOL-PARA.
      D    DISPLAY lss-i
           EVALUATE ls-bf(lss-i)
               WHEN '<'
                   IF idx > 1 THEN
                       SET idx DOWN BY 1
                   ELSE
                       SET idx TO ws-array-size
                   END-IF
               WHEN '>'
                   IF idx < ws-array-size THEN
                       SET idx UP BY 1
                   ELSE
                       SET idx TO 1
                   END-IF
               WHEN '+'
                   IF ws-tape(idx) < 255
                       ADD 1 TO ws-tape(idx)
                   ELSE
                       MOVE 0 TO ws-tape(idx)
                   END-IF
               WHEN '-'
                   IF ws-tape(idx) > 0
                       SUBTRACT 1 FROM ws-tape(idx)
                   ELSE
                       MOVE 255 TO ws-tape(idx)
                   END-IF
               WHEN '.'
                   COMPUTE lss-char-tmp = ws-tape(idx) + 1
                   DISPLAY FUNCTION CHAR(lss-char-tmp) WITH NO ADVANCING
               WHEN ','
                   OPEN INPUT f-sysin
                   READ f-sysin
                       AT END DISPLAY LOW-VALUE WITH NO ADVANCING
                       NOT AT END COMPUTE
                           ws-tape(idx) = FUNCTION ORD(f-sysin-char) - 1
                   END-READ
                   CLOSE f-sysin
               WHEN '['
                   COMPUTE lss-s = lss-i + 1

                   MOVE 1 TO lss-braces
                   PERFORM
                       CHECK-IF-BRACE-PARA
                       VARYING lss-i FROM lss-s BY 1
                       UNTIL lss-braces = 0

                   SUBTRACT 1 FROM lss-i

                   PERFORM UNTIL ws-tape(idx) EQUAL 0
                       CALL 'interpret'
                       USING ls-bf-full-code, lss-s, BY CONTENT lss-i
                   END-PERFORM
           END-EVALUATE.

       CHECK-IF-BRACE-PARA.
           EVALUATE ls-bf(lss-i)
               WHEN '['
                   ADD 1 TO lss-braces
               WHEN ']'
                   SUBTRACT 1 FROM lss-braces

           END-EVALUATE.
