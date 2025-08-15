      ******************************************************************
      * Author:
      * KRITTANON MUSEESUT 6621650213
      * KASIDIT SANOTRAI   6621654987
      * THANAREE KRAIWAS   6621655053
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-RAW-SALES-DATA.
           05 FILLER PIC 9(5) VALUE 15000. *> JAN
           05 FILLER PIC 9(5) VALUE 18500. *> FEB
           05 FILLER PIC 9(5) VALUE 21000. *> MAR
           05 FILLER PIC 9(5) VALUE 17500. *> APR
           05 FILLER PIC 9(5) VALUE 25000. *> MAY
           05 FILLER PIC 9(5) VALUE 22500. *> JUN

       01 WS-SALES-TABLE REDEFINES WS-RAW-SALES-DATA.
           05 WS-SALES-MONTH PIC 9(5) OCCURS 6 TIMES.

       01 WS-SALES-INDEX PIC 9 VALUE 1.

       01 WS-SALES-TOTAL PIC 9(7).
       01 WS-TOTAL-DISPLAY PIC Z(7).
       01 WS-INDEX-COUNT PIC 9.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           PERFORM VARYING WS-SALES-INDEX FROM 1 BY 1
           UNTIL WS-SALES-INDEX > 6
               DISPLAY "MONTH " WS-SALES-INDEX " SALES : "
               WS-SALES-MONTH (WS-SALES-INDEX)

               ADD WS-SALES-MONTH(WS-SALES-INDEX) TO WS-SALES-TOTAL
               ADD 1 TO WS-INDEX-COUNT

           END-PERFORM.

               MOVE WS-SALES-TOTAL TO WS-TOTAL-DISPLAY.
               DISPLAY "TOTAL SALES FOR " WS-INDEX-COUNT  " MONTHS: "
               WS-TOTAL-DISPLAY.


            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
