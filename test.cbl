       IDENTIFICATION DIVISION.
       PROGRAM-ID.  TEST.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  CALL-STATUS  PIC S9(3).

       PROCEDURE DIVISION.
           CALL "C$COPY"
               USING "test.cbl" "copy.cbl" "S"
               GIVING CALL-STATUS
               ON EXCEPTION
                   DISPLAY "Argh!"
           END-CALL
           GOBACK
           .
