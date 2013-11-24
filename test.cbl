       IDENTIFICATION DIVISION.
       PROGRAM-ID. test.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  CALL-STATUS  BINARY-LONG.

       PROCEDURE DIVISION.
           CALL "C$COPY"
               USING "test.cbl", "copy.cbl", "S"
               GIVING CALL-STATUS
           END-CALL
           CALL "C$XML"
               USING 14, 42
               GIVING CALL-STATUS
           END-CALL

           GOBACK
           .

