      ******************************************************************
      * Author: KRITTANON
      * Date: 06/10/2025
      * Purpose: Simple product report
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRODUCT-REPORT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRODUCT ASSIGN TO "PRODUCT.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT REPORT-REC ASSIGN TO "REPORT.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD PRODUCT.
           01 PRODUCT-REC.
               05 P-ID     PIC X(4).
               05 P-NAME   PIC X(10).
               05 P-PRICE  PIC X(10).

       FD REPORT-REC.
           01 REPORT-LINE PIC X(80).

       WORKING-STORAGE SECTION.
           01 END-FILE PIC X VALUE "N".
              88 EOF VALUE "Y".
              88 NOT-EOF VALUE "N".

       PROCEDURE DIVISION.
       MAIN.
           OPEN INPUT PRODUCT
           OPEN OUTPUT REPORT-REC

           MOVE "=========== PRODUCT REPORT ===========" TO REPORT-LINE
           WRITE REPORT-LINE

           PERFORM UNTIL EOF
               READ PRODUCT
                   AT END
                       SET EOF TO TRUE
                   NOT AT END
                       STRING
                           "id: " DELIMITED BY SIZE
                           P-ID DELIMITED BY SIZE
                           " , name: " DELIMITED BY SIZE
                           P-NAME DELIMITED BY SIZE
                           " , price: " DELIMITED BY SIZE
                           P-PRICE DELIMITED BY SIZE
                           INTO REPORT-LINE
                       END-STRING
                       WRITE REPORT-LINE
               END-READ
           END-PERFORM

           MOVE "----------------------------------------" TO
           REPORT-LINE
           WRITE REPORT-LINE

           CLOSE PRODUCT REPORT-REC
           STOP RUN.

       END PROGRAM PRODUCT-REPORT.
