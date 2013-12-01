       IDENTIFICATION DIVISION.
       PROGRAM-ID.  TEST.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  CALL-STATUS  PIC S9(3)  VALUE 0.
       01  PTR          POINTER    VALUE NULL.

       PROCEDURE DIVISION.
           CALL "C$NetInit"
               GIVING CALL-STATUS
               ON EXCEPTION
                   DISPLAY "Exception when calling C$NetInit."
           END-CALL

           CALL "C$NetCleanup"
               ON EXCEPTION
                   DISPLAY "Exception when calling C$NetCleanup."
           END-CALL

           GOBACK
           .
