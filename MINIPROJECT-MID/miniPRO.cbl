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
       01 WS-COMPLETE PIC X VALUE "N".
       01 LOGIN-COMPONENT.
        05 AUTH-KEY PIC X VALUE "N".
        05 USERNAME-BASE PIC X(20) VALUE "kasidit".
        05 PASSWORD-BASE PIC X(10) VALUE "000111".
        05 USERNAME-INPUT PIC X(20).
        05 PASSWORD-INPUT PIC X(10).

       01 DISPLAY-STAR PIC X VALUE " ".
       01 WS-EXIT PIC X VALUE "N".
       01 WS-OPTION PIC 9 VALUE 0.
       01 WS-DUMMY PIC X.
       01 WS-INPUT-PRODUCT-ID PIC X(7).
       01 WS-INPUT-QUANTITY PIC 9(5).
       01 WS-TOTAL-PRICE PIC 9(10)V99.
       01 WS-DATE.
           05 WS-YEAR     PIC 9(4).
           05 WS-MONTH    PIC 9(2).
           05 WS-DAY      PIC 9(2).


       01 WS-MONTH-ABBR PIC XXX.
       01 WS-FOUND PIC X VALUE "N".
       01 WS-INDEX PIC 9(4) VALUE 1.
       01 WS-ITEM-VALUE PIC 9(10)99V.
       01 WS-ALL-PRICE PIC 9(20)99V.

       01 PRODUCT-TABLE.
           05 PRODUCT-ENTRY OCCURS 100 TIMES.
           10 PRODUCT-ID       PIC X(7).
           10 PRODUCT-ID-PARTS REDEFINES PRODUCT-ID.
               15 CATEGORY       PIC X(2).
               15 FILLER         PIC X.
               15 SERIAL-NO      PIC X(4).
           10 PRODUCT-NAME     PIC X(30).
           10 PRODUCT-PRICE    PIC 9(5)V99.
           10 PRODUCT-STOCK    PIC 9(5).

       01 DISPLAY-GROUP.
           05 DISPLAY-PRODUCT-ID PIC X(7).
           05 DISPLAY-NAME PIC X(30).
           05 DISPLAY-PRICE PIC Z,ZZZ,ZZZ.99.
           05 DISPLAY-STOCK PIC ZZZZZ.
           05 DISPLAY-VALUE PIC Z,ZZZ,ZZZ.99.
           05 DISPLAY-ALL PIC Z,ZZZ,ZZZ.99.

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

               PERFORM UNTIL AUTH-KEY = "Y"
                       DISPLAY "ENTER USERNAME :"
                       ACCEPT USERNAME-INPUT

                       DISPLAY "ENTER PASSWORD"
                       ACCEPT PASSWORD-INPUT

                       IF USERNAME-BASE = USERNAME-BASE AND
                          PASSWORD-INPUT = PASSWORD-BASE
                           MOVE "Y" TO AUTH-KEY
                       ELSE
                           DISPLAY
                           "THIS IS NOT RIGHT USERNAME OR PASSWORD"
                   END-IF

                   END-PERFORM

                   DISPLAY "ENTER DATE (YYYYMMDD): "
                   ACCEPT WS-DATE

               EVALUATE WS-MONTH
                   WHEN 1  MOVE "JAN" TO WS-MONTH-ABBR
                   WHEN 2  MOVE "FEB" TO WS-MONTH-ABBR
                   WHEN 3  MOVE "MAR" TO WS-MONTH-ABBR
                   WHEN 4  MOVE "APR" TO WS-MONTH-ABBR
                   WHEN 5  MOVE "MAY" TO WS-MONTH-ABBR
                   WHEN 6  MOVE "JUN" TO WS-MONTH-ABBR
                   WHEN 7  MOVE "JUL" TO WS-MONTH-ABBR
                   WHEN 8  MOVE "AUG" TO WS-MONTH-ABBR
                   WHEN 9  MOVE "SEP" TO WS-MONTH-ABBR
                   WHEN 10 MOVE "OCT" TO WS-MONTH-ABBR
                   WHEN 11 MOVE "NOV" TO WS-MONTH-ABBR
                   WHEN 12 MOVE "DEC" TO WS-MONTH-ABBR
               END-EVALUATE.



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


           PERFORM UNTIL WS-COMPLETE = "Y"

           DISPLAY "ENTER PRODUCT-ID TO SELL: "
           ACCEPT WS-INPUT-PRODUCT-ID

           DISPLAY "ENTER QUANTITY TO SELL: "
           ACCEPT WS-INPUT-QUANTITY

           MOVE "N" TO WS-FOUND

                     PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL
               WS-INDEX > 100 OR WS-FOUND = "Y"
               IF PRODUCT-ID(WS-INDEX) = WS-INPUT-PRODUCT-ID
                   MOVE "Y" TO WS-FOUND
               END-IF
           END-PERFORM

                    IF WS-FOUND = "N"
               DISPLAY "ERROR : PRODUCT ID NOT FOUND. PLEASE TRY AGAIN."
           ELSE
               SUBTRACT 1 FROM WS-INDEX
               IF PRODUCT-STOCK(WS-INDEX) >= WS-INPUT-QUANTITY
                SUBTRACT WS-INPUT-QUANTITY FROM PRODUCT-STOCK(WS-INDEX)

                MULTIPLY PRODUCT-PRICE(WS-INDEX)BY WS-INPUT-QUANTITY
                     GIVING WS-TOTAL-PRICE

                 MOVE WS-TOTAL-PRICE TO DISPLAY-PRICE

                 DISPLAY "SALE SUCCESSFUL."
                 DISPLAY "TOTAL PRICE: " DISPLAY-PRICE

               MOVE "Y" TO WS-COMPLETE

               ELSE
                   DISPLAY "ERROR : NOT ENOUGH STOCK AVAILABLE: "
                       PRODUCT-STOCK(WS-INDEX)
                   DISPLAY "PLEASE TRY AGAIN."
               END-IF
           END-IF

       END-PERFORM



               DISPLAY " "
               DISPLAY "PRESS ENTER TO RETURN TO MAIN MENU..."
               ACCEPT WS-DUMMY.

                               RESTOCK-ITEM.
        MOVE "N" TO WS-FOUND.

         PERFORM UNTIL WS-COMPLETE = "Y"
        DISPLAY "ENTER PRODUCT-ID TO RESTOCK: "
        ACCEPT WS-INPUT-PRODUCT-ID

        DISPLAY "ENTER QUANTITY TO RESTOCK: "
        ACCEPT WS-INPUT-QUANTITY

        MOVE "N" TO WS-FOUND

        PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL
            WS-INDEX > 100 OR WS-FOUND = "Y"
            IF PRODUCT-ID(WS-INDEX) = WS-INPUT-PRODUCT-ID
                MOVE "Y" TO WS-FOUND
            END-IF
        END-PERFORM

        IF WS-FOUND = "N"
            DISPLAY "ERROR : PRODUCT ID NOT FOUND. PLEASE TRY AGAIN."
        ELSE
            SUBTRACT 1 FROM WS-INDEX
            ADD WS-INPUT-QUANTITY TO PRODUCT-STOCK(WS-INDEX)
            MULTIPLY PRODUCT-PRICE(WS-INDEX) BY WS-INPUT-QUANTITY
            GIVING WS-TOTAL-PRICE
            MOVE WS-TOTAL-PRICE TO DISPLAY-PRICE

            DISPLAY "RESTOCK SUCCESSFUL."
            DISPLAY "TOTAL PRICE: " DISPLAY-PRICE

            MOVE "Y" TO WS-COMPLETE
        END-IF
       END-PERFORM

       DISPLAY " "
       DISPLAY "PRESS ENTER TO RETURN TO MAIN MENU..."
       ACCEPT WS-DUMMY.


           PRINT-INVENTORY-REPORT.
               MOVE "N" TO WS-FOUND.

               DISPLAY "DATE ENTERED: " WS-DAY "/" WS-MONTH-ABBR "/"
               WS-YEAR.

               DISPLAY "=== PRINT INVENTORY REPORT MENU ===".
               DISPLAY " ".
               DISPLAY "ID      | PRODUCT NAME                   | CAT "
               "| STOCK     | PRICE      | TOTAL VALUE".
               DISPLAY "--------|--------------------------------|-----"
               "|-----------|------------|-------------".



               PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > 10
                   IF PRODUCT-ID(WS-INDEX) NOT = SPACES
                       MULTIPLY PRODUCT-STOCK(WS-INDEX)
                           BY PRODUCT-PRICE(WS-INDEX)
                           GIVING WS-ITEM-VALUE

                       ADD WS-ITEM-VALUE TO WS-ALL-PRICE

                       IF PRODUCT-STOCK(WS-INDEX) < 10
                           MOVE "*" TO DISPLAY-STAR
                       END-IF


                       MOVE PRODUCT-ID(WS-INDEX) TO DISPLAY-PRODUCT-ID
                       MOVE PRODUCT-NAME(WS-INDEX) TO DISPLAY-NAME
                       MOVE PRODUCT-STOCK(WS-INDEX) TO DISPLAY-STOCK
                       MOVE PRODUCT-PRICE(WS-INDEX) TO DISPLAY-PRICE
                       MOVE WS-ITEM-VALUE TO DISPLAY-VALUE
                       MOVE WS-ALL-PRICE TO DISPLAY-ALL

                       DISPLAY DISPLAY-PRODUCT-ID " | "
                               DISPLAY-NAME " | "
                               CATEGORY(WS-INDEX) "  | "
                               DISPLAY-STOCK "     |"
                               DISPLAY-PRICE "| "
                               DISPLAY-VALUE DISPLAY-STAR

                       MOVE " " TO DISPLAY-STAR
                   END-IF


               END-PERFORM.


               DISPLAY " ".
               DISPLAY "PRESS ENTER TO RETURN TO MAIN MENU...".
               ACCEPT WS-DUMMY.



       END PROGRAM MINI_STOCK_MANAGEMENT.
