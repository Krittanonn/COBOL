******************************************************************
      * Author: Krittanon Museesut
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MINI_STOCK_MANAGEMENT.
       DATA DIVISION.

       FILE SECTION.

       WORKING-STORAGE SECTION.
       01 WS-EXIT PIC X VALUE "N".
       01 WS-OPTION PIC 9 VALUE 0.
       01 WS-DUMMY PIC X.
       01 WS-INPUT-PRODUCT-ID PIC X(7).
       01 WS-INPUT-QUANTITY PIC 9(5).
       01 WS-TOTAL-PRICE PIC 9(10)V99.

       01 WS-FOUND PIC X VALUE "N".
       01 WS-INDEX PIC 9(4) VALUE 1.
       01 WS-ITEM-VALUE PIC 9(10)99V.

       01 PRODUCT-TABLE.
           05 PRODUCT-ENTRY OCCURS 100 TIMES INDEXED BY PROD.
           10 PRODUCT-ID       PIC X(7).
           10 PRODUCT-ID-PARTS REDEFINES PRODUCT-ID.
               15 CATEGORY       PIC X(2).
               15 FILLER         PIC X.
               15 SERIAL-NO      PIC X(4).
           10 PRODUCT-NAME     PIC X(30).
           10 PRODUCT-PRICE    PIC 9(5)V99.
           10 PRODUCT-STOCK    PIC 9(5).

       PROCEDURE DIVISION.

           MAIN-PROGRAM.
               MOVE "BK-1000" TO PRODUCT-ID(1)
               MOVE "COBOL Programming " TO PRODUCT-NAME(1)
               MOVE 15 TO PRODUCT-STOCK(1)
               MOVE 1250.50 TO PRODUCT-PRICE(1)

               MOVE "BK-1002" TO PRODUCT-ID(2)
               MOVE "JCL for Mainframes" TO PRODUCT-NAME(2)
               MOVE 8 TO PRODUCT-STOCK(2)
               MOVE 1100.00 TO PRODUCT-PRICE(2)

               MOVE "FD-2001" TO PRODUCT-ID(3)
               MOVE "Instant Noodles  " TO PRODUCT-NAME(3)
               MOVE 150 TO PRODUCT-STOCK(3)
               MOVE 6.00 TO PRODUCT-PRICE(3)

               MOVE "FD-2002" TO PRODUCT-ID(4)
               MOVE "Canned Tuna      " TO PRODUCT-NAME(4)
               MOVE 80 TO PRODUCT-STOCK(4)
               MOVE 35.50 TO PRODUCT-PRICE(4)

               MOVE "EL-3001" TO PRODUCT-ID(5)
               MOVE "USB-C Cable     " TO PRODUCT-NAME(5)
               MOVE 45 TO PRODUCT-STOCK(5)
               MOVE 150.00 TO PRODUCT-PRICE(5)

               MOVE "EL-3002" TO PRODUCT-ID(6)
               MOVE "Wireless Mouse  " TO PRODUCT-NAME(6)
               MOVE 22 TO PRODUCT-STOCK(6)
               MOVE 499.00 TO PRODUCT-PRICE(6)

               MOVE "ST-4001" TO PRODUCT-ID(7)
               MOVE "A4 Paper Ream   " TO PRODUCT-NAME(7)
               MOVE 30 TO PRODUCT-STOCK(7)
               MOVE 120.00 TO PRODUCT-PRICE(7)

               MOVE "FD-2003" TO PRODUCT-ID(8)
               MOVE "Potato Chips    " TO PRODUCT-NAME(8)
               MOVE 120 TO PRODUCT-STOCK(8)
               MOVE 25.00 TO PRODUCT-PRICE(8)

               MOVE "ST-4002" TO PRODUCT-ID(9)
               MOVE "Ballpoint Pen Box" TO PRODUCT-NAME(9)
               MOVE 50 TO PRODUCT-STOCK(9)
               MOVE 85.75 TO PRODUCT-PRICE(9)

               MOVE "BK-1003" TO PRODUCT-ID(10)
               MOVE "Database Design " TO PRODUCT-NAME(10)
               MOVE 12 TO PRODUCT-STOCK(10)
               MOVE 1800.25 TO PRODUCT-PRICE(10)

               PERFORM UNTIL WS-EXIT = "Y"
                   DISPLAY "=== MAIN MENU ==="
                   DISPLAY "1. SELL ITEM"
                   DISPLAY "2. RESTOCK ITEM"
                   DISPLAY "3. PRINT INVENTORY REPORT"
                   DISPLAY "4. EXIT PROGRAM"

                   DISPLAY "CHOSE OPTION (1-4): "
                   ACCEPT WS-OPTION

                   EVALUATE WS-OPTION
                       WHEN 1
                           PERFORM SELL-ITEM
                       WHEN 2
                           PERFORM RESTOCK-ITEM
                       WHEN 3
                           PERFORM PRINT-INVENTORY-REPORT
                       WHEN 4
                           MOVE "Y" TO WS-EXIT
                           DISPLAY "EXITING PROGRAM. GOOD BYE!"
                       WHEN OTHER
                           DISPLAY "INVALID OPTION. TRY AGAIN."
                   END-EVALUATE
               END-PERFORM.

            STOP RUN.

           SELL-ITEM.
               MOVE "N" TO WS-FOUND.

               DISPLAY "=== SELL ITEM MENU ===".
               DISPLAY "ENTER PRODUCT-ID TO SELL: ".
               ACCEPT WS-INPUT-PRODUCT-ID.

               DISPLAY "ENTER QUANTITY TO SELL: ".
               ACCEPT WS-INPUT-QUANTITY.

               PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL
                   WS-INDEX > 100
                   OR WS-FOUND = "Y"
                   IF PRODUCT-ID(WS-INDEX) = WS-INPUT-PRODUCT-ID
                       MOVE "Y" TO WS-FOUND
                       IF PRODUCT-STOCK(WS-INDEX) >= WS-INPUT-QUANTITY
                           SUBTRACT WS-INPUT-QUANTITY
                               FROM PRODUCT-STOCK(WS-INDEX)
                           MULTIPLY PRODUCT-PRICE(WS-INDEX) BY
                               WS-INPUT-QUANTITY GIVING WS-TOTAL-PRICE
                           DISPLAY "SALE SUCCESSFUL."
                           DISPLAY "TOTAL PRICE: " WS-TOTAL-PRICE
                       ELSE
                           DISPLAY "ERROR : NOT ENOUGH STOCK AVAILABLE:"
                           PRODUCT-STOCK(WS-INDEX)
                       END-IF
                   END-IF
               END-PERFORM.

               IF WS-FOUND = "N"
                   DISPLAY "ERROR : PRODUCT ID NOT FOUND."
               END-IF.

               DISPLAY " "
               DISPLAY "PRESS ENTER TO RETURN TO MAIN MENU..."
               ACCEPT WS-DUMMY.

           RESTOCK-ITEM.
               DISPLAY "=== RESTOCK ITEM MENU ===".
               DISPLAY "ENTER PRODUCT-ID TO RESTOCK: ".
               ACCEPT WS-INPUT-PRODUCT-ID.

               DISPLAY "ENTER QUANTITY TO ADD: ".
               ACCEPT WS-INPUT-QUANTITY.

               PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL
                   WS-INDEX > 100
                   OR WS-FOUND = "Y"
                   IF PRODUCT-ID(WS-INDEX) = WS-INPUT-PRODUCT-ID
                       MOVE "Y" TO WS-FOUND
                           ADD WS-INPUT-QUANTITY
                           TO PRODUCT-STOCK(WS-INDEX)

                       DISPLAY "RESTOCK SUCCESFUL. NEW QUANTITY: "
                       PRODUCT-STOCK(WS-INDEX)
                   END-IF
               END-PERFORM.

               IF WS-FOUND = "N"
                   DISPLAY "ERROR : PRODUCT ID NOT FOUND."
               END-IF.

               DISPLAY " "
               DISPLAY "PRESS ENTER TO RETURN TO MAIN MENU..."
               ACCEPT WS-DUMMY.

           PRINT-INVENTORY-REPORT.
               DISPLAY "=== PRINT INVENTORY REPORT MENU ===".
               DISPLAY " ".
               DISPLAY "ID      | PRODUCT NAME                   | CAT "
               "| STOCK | PRICE   | TOTAL VALUE".
               DISPLAY "--------|--------------------------------|-----"
               "|-------|---------|------------".

               PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > 10
                   IF PRODUCT-ID(WS-INDEX) NOT = SPACES
                       MULTIPLY PRODUCT-STOCK(WS-INDEX) BY
                           PRODUCT-PRICE(WS-INDEX) GIVING WS-ITEM-VALUE
                       DISPLAY PRODUCT-ID(WS-INDEX) " | "
                           PRODUCT-NAME(WS-INDEX) " | "
                           CATEGORY(WS-INDEX) "  | "
                           PRODUCT-STOCK(WS-INDEX) "   | "
                           PRODUCT-PRICE(WS-INDEX) " | "
                           WS-ITEM-VALUE
                   END-IF
               END-PERFORM.

               DISPLAY " ".
               DISPLAY "PRESS ENTER TO RETURN TO MAIN MENU..."
               ACCEPT WS-DUMMY.


       END PROGRAM MINI_STOCK_MANAGEMENT.
