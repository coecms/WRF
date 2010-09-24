      SUBROUTINE UFBQCD(LUNIT,NEMO,QCD)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    UFBQCD
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
C
C ABSTRACT: THIS SUBROUTINE READS IN A MNEMONIC KNOWN TO BE IN THE BUFR
C   TABLE ASSOCIATED WITH THE BUFR FILE IN LOGICAL UNIT LUNIT, AND
C   RETURNS THE DESCRIPTOR ENTRY (Y) ASSOCIATED WITH IT WHEN THE FXY
C   DESCRIPTOR IS A SEQUENCE DESCRIPTOR (F=3) WITH TABLE D CATEGORY 63
C   (X=63).  THIS ROUTINE WILL NOT WORK FOR ANY OTHER TYPE OF
C   DESCRIPTOR OR ANY OTHER SEQUENCE DESCRIPTOR TABLE D CATEGORY.
C   LUNIT MUST ALREADY BE OPENED FOR INPUT OR OUTPUT VIA A CALL TO
C   OPENBF.  THIS ROUTINE IS ESPECIALLY USEFUL WHEN THE CALLING PROGRAM
C   IS WRITING "EVENTS" TO AN OUTPUT BUFR FILE (USUALLY THE "PREPBUFR"
C   FILE) USING THE SAME BUFR TABLE SINCE THE DESCRIPTOR ENTRY (Y) HERE
C   DEFINES THE EVENT PROGRAM CODE.  THUS, THE CALLING PROGRAM CAN PASS
C   THE PROGRAM CODE INTO VARIOUS EVENTS WITHOUT ACTUALLY KNOWING ITS
C   VALUE AS LONG AS IT KNOWS THE MNEMONIC NAME ASSOCIATED WITH IT.
C
C PROGRAM HISTORY LOG:
C 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
C 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE
C                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB
C                           ROUTINE "BORT"
C 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C                           INTERDEPENDENCIES
C 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED
C                           DOCUMENTATION (INCLUDING HISTORY); OUTPUTS
C                           MORE COMPLETE DIAGNOSTIC INFO WHEN ROUTINE
C                           TERMINATES ABNORMALLY
C
C USAGE:    CALL UFBQCD (LUNIT, NEMO, QCD)
C   INPUT ARGUMENT LIST:
C     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE
C                (ASSOCIATED BUFR TABLE MAY BE INTERNAL OR EXTERNAL)
C     NEMO     - CHARACTER*(*): MNEMONIC
C
C   OUTPUT ARGUMENT LIST:
C     QCD      - REAL: SEQUENCE DESCRIPTOR ENTRY (I.E., EVENT PROGRAM
C                CODE) IN BUFR TABLE ASSOCIATED WITH NEMO (Y IN FXY
C                DESCRIPTOR, WHERE F=3 AND X=63)
C
C REMARKS:
C    THIS SUBROUTINE IS THE INVERSE OF BUFR ARCHIVE LIBRARY ROUTINE
C    UFBQCP.
C
C    THIS ROUTINE CALLS:        ADN30    BORT     NEMTAB   STATUS
C    THIS ROUTINE IS CALLED BY: None
C                               Normally called only by application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      CHARACTER*(*) NEMO
      CHARACTER*128 BORT_STR
      CHARACTER*6   FXY,ADN30
      CHARACTER*1   TAB

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900

      CALL NEMTAB(LUN,NEMO,IDN,TAB,IRET)
      IF(TAB.NE.'D') GOTO 901

      FXY = ADN30(IDN,6)
      IF(FXY(2:3).NE.'63') GOTO 902
      READ(FXY(4:6),'(F3.0)',ERR=903) QCD

C  EXITS
C  -----

      RETURN
900   CALL BORT('BUFRLIB: UFBQCD - BUFR FILE IS CLOSED, IT MUST BE'//
     . ' OPEN')
901   WRITE(BORT_STR,'("BUFRLIB: UFBQCD - INPUT MNEMONIC ",A," NOT '//
     . 'DEFINED AS A SEQUENCE DESCRIPTOR IN BUFR TABLE")') NEMO
      CALL BORT(BORT_STR)
902   WRITE(BORT_STR,'("BUFRLIB: UFBQCD - BUFR TABLE SEQ. DESCRIPTOR '//
     . 'ASSOC. WITH INPUT MNEMONIC ",A," HAS INVALID CATEGORY ",A," -'//
     . ' CATEGORY MUST BE 63")') NEMO,FXY(2:3)
      CALL BORT(BORT_STR)
903   WRITE(BORT_STR,'("BUFRLIB: UFBQCD - ERROR READING ENTRY '//
     . '(PROGRAM CODE) FROM BUFR TBL SEQ. DESCRIPTOR ",A," ASSOC. '//
     . 'WITH INPUT MNEM. ",A)') FXY,NEMO
      CALL BORT(BORT_STR)
      END