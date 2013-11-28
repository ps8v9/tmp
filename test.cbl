       IDENTIFICATION DIVISION.
       PROGRAM-ID.  TEST.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  CALL-STATUS  PIC S9(3).

       PROCEDURE DIVISION.
           CALL "C$NetInit"
               GIVING CALL-STATUS
               ON EXCEPTION
                   DISPLAY "Argh! (C$NetInit)"
           END-CALL
           DISPLAY "call status after NetInit: " CALL-STATUS

           CALL "C$NetCleanup"
               ON EXCEPTION
                   DISPLAY "Argh! (C$NetCleanup)"
           END-CALL
           DISPLAY "NetCleanup complete"

           GOBACK
           .
