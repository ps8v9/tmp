       IDENTIFICATION DIVISION.
       PROGRAM-ID.  TEST.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  CALL-STATUS  PIC S9(3).

       PROCEDURE DIVISION.
           CALL "C$XML"
               USING 14, "foo", "bar"
               GIVING CALL-STATUS
               ON EXCEPTION
                   DISPLAY "Argh!"
           END-CALL

           DISPLAY "call status: " CALL-STATUS
           GOBACK
           .
