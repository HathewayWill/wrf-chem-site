      PROGRAM TOTAL
C-----------------------------------------------------------------------
C Number of release points
      PARAMETER(IPOINT=224096)
C Number of Primary (IPRIM), VOC (IVOC), and PM2.5 (IPM25) species
      PARAMETER(IPRIM=7,IVOC=50,IPM25=19)
      COMMON/RELPDAT/EM(IPOINT)
      CHARACTER*80 INFIL,FIL
      CHARACTER*9 NAM
      CHARACTER*10 INARG,NAMINF(IPRIM),NAMVOC(IVOC),NAMPM2(IPM25)
      CHARACTER*4 HRNAM
      INTEGER LLET,LLET2
      DIMENSION STKHGT(IPOINT),STKDIAM(IPOINT),STKTMP(IPOINT)
      DIMENSION STKVEL(IPOINT),STKFLOW(IPOINT),XLON(IPOINT),XLAT(IPOINT)
      INTEGER ISTATE(IPOINT),ICOUN(IPOINT),IRTYP(IPOINT)
      CHARACTER*15 SITEID(IPOINT)
      CHARACTER*8 ERPTID(IPOINT),UNITID(IPOINT),PROCID(IPOINT)
      INTEGER STATUS,SYSTEM
      CHARACTER*4 SPNAM
      CHARACTER*6 COSIRB  ! COSIRB is Boilder ID for OSIR coding
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
      DATA NAMINF/'VOC       ','NOX       ','CO        ','SO2       ',
     -'PM10-PRI  ','PM25-PRI  ','NH3       '/
      DATA (NAMVOC(I),I=1,IVOC)/
     -' METHANE  ',' ALKANE1  ',' ALKANE2  ',' ALKANE3  ',' ALKANE4  ',
     -' ALKANE5  ',' ETHYLENE ',' OLEFIN1  ',' OLEFIN2  ',' ISOPRENE ',
     -' TERPENES ','AROMATIC1 ','AROMATIC2 ','   CH2O   ',' CH3CHO   ',
     -'HI_ALDEHY ','BENZALDHY ',' ACETONE  ','   MEK    ','  PRD2    ',
     -'   MEOH   ',' GLYOXAL  ','METHGLYOX ','   BACL   ',' PHENOLS  ',
     -' CRESOLS  ','  MACR    ','  MVK     ','   IPRD   ',' HCOOH    ',
     -' CH3COOH  ',' RCOOH    ',' XYLENEOLS',' CATECHOLS',' NONVOLATL',
     -' PROPYLENE',' ACETYLENE',' BENZENE  ',' BUTANES  ',' PENTANES ',
     -' TOLUENE  ',' m-XYLENE ',' o-XYLENE ',' p-XYLENE ',' PROPANE  ',
     -'  DIENES  ',' STYRENES ',' ETHANOL  ','ETHE_GLYC ',' UNKNOWN  '/
      DATA (NAMPM2(I),I=1,IPM25)/'PMFINE','PSO4','PNO3','POC','PEC',
     -'PNCOM','PNH4','PAL','PCA','PFE','PH2O','PMG','PMOTHR','PK','PMN'
     -,'PCL','PNA','PTI','PSI'/
C Open output file
      OPEN(7,FILE='point_read.output')
C
C Read point realease info file, print information for maximum stack height in inventory
C
      WRITE(7,'(/30A1,A,30A1)')('-',I=1,30),' Release point info file ',
     -('-',I=1,30)
      OPEN(8,FILE='../point/Relpnt_info.txt')
      SHGTMX=-1.E20
      DO I=1,IPOINT
C23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
      READ(8,55)ISTATE(I),ICOUN(I),SITEID(I),ERPTID(I),UNITID(I),
     -PROCID(I),IRTYP(I),STKHGT(I),STKDIAM(I),STKTMP(I),STKVEL(I),
     -STKFLOW(I),XLON(I),XLAT(I),FUGHT,IOSIR,COSIRB
      IF(STKHGT(I).GT.SHGTMX)THEN
      IMAX=I
      SHGTMX=STKHGT(I)
      ENDIF
      ENDDO
 55   FORMAT(I2.2,I3.3,A15,3A8,1X,I2,1X,F10.3,F10.3,F10.2,F10.3,F10.4
     -,F11.5,F10.5,F10.3,I7,1X,A)
      CLOSE(8)
      WRITE(7,*)'Max stack ht, Record number of max= ',IMAX
      WRITE(7,*)'Max stack ht, FIPS State= ',ISTATE(IMAX)
      WRITE(7,*)'Max stack ht, FIPS County= ',ICOUN(IMAX)
      WRITE(7,*)'Max stack ht, Site ID= ',SITEID(IMAX)
      WRITE(7,*)'Max stack ht, Report ID= ',ERPTID(IMAX)
      WRITE(7,*)'Max stack ht, Unit ID= ',UNITID(IMAX)
      WRITE(7,*)'Max stack ht, Process ID= ',PROCID(IMAX)
      WRITE(7,*)'Max stack ht, Release Type= ',IRTYP(IMAX)
      WRITE(7,*)'Max stack ht, Height= ',STKHGT(IMAX),' meter'
      WRITE(7,*)'Max stack ht, Stack Diameter= ',STKDIAM(IMAX),' meter'
      WRITE(7,*)'Max stack ht, Exit Temperature= ',STKTMP(IMAX),' deg K'
      WRITE(7,*)'Max stack ht, Flow velocity= ',STKVEL(IMAX),' m/s'
      WRITE(7,*)'Max stack ht, Flow rate= ',STKFLOW(IMAX),' m(3)/s'
      WRITE(7,*)'Max stack ht, Longitude= ',XLON(IMAX)
      WRITE(7,*)'Max stack ht, Latitude= ',XLAT(IMAX)
C
C Read Daily Average (short ton/dy) primary emissions,
C print sums, maximums, location of maximums in lat,lon
C
      WRITE(7,'(/30A1,A,30A1)')('-',I=1,30),' Daily av. emis. ton/dy  ',
     -('-',I=1,30)
      WRITE(7,'(A,4X,A,9X,A,5X,A,3X,A,4X,A)')'Species','Total',
     -'Max','I-Max','Lat-Max','Lon-Max'
      DO ISP=1,IPRIM
C Unzip point daily average emission file
      STATUS=SYSTEM('gunzip ../point/dayav/'//NAMINF(ISP))
      OPEN(8,FILE='../point/dayav/'//NAMINF(ISP))
      READ(8,'(12E9.2)')EM
      CLOSE(8)
C Zip back up the daily average point emission file
      STATUS=SYSTEM('gzip ../point/dayav/'//NAMINF(ISP))
C Get total emission and max emission information
      CALL EMSUM(ETOT,EMAX,IEMAX)
C Output Total and Max info
      WRITE(7,'(A9,1P2E11.3,I7,0P2F11.5)')NAMINF(ISP),ETOT,EMAX,
     -IEMAX,XLAT(IEMAX),XLON(IEMAX)
      ENDDO   ! End of ISP loop (primary species)
C
C Read Daily Average (mole/dy) speciated VOC emissions,
C print sums, maximums, location of maximums in lat,lon
C
      WRITE(7,'(/30A1,A,30A1)')('-',I=1,30),' Day avrg. VOC em mole/dy '
     -,('-',I=1,30)
      WRITE(7,'(A,4X,A,9X,A,5X,A,3X,A,4X,A)')'Species','Total',
     -'Max','I-Max','Lat-Max','Lon-Max'
      DO ISP=1,IVOC
C Unzip point emission file
      WRITE(SPNAM,'(A2,I2.2)')'HC',ISP
      STATUS=SYSTEM('gunzip ../point/dayav/VOCspec/'//SPNAM)
      OPEN(8,FILE='../point/dayav/VOCspec/'//SPNAM)
      READ(8,'(12E9.2)')EM
      CLOSE(8)
C Zip back up the daily average point emission file
      STATUS=SYSTEM('gzip ../point/dayav/VOCspec/'//SPNAM)
C Get total emission and max emission information
      CALL EMSUM(ETOT,EMAX,IEMAX)
C Output Total and Max info
      WRITE(7,'(A9,1P2E11.3,I7,0P2F11.5)')NAMVOC(ISP),ETOT,EMAX,
     -IEMAX,XLAT(IEMAX),XLON(IEMAX)
      ENDDO   ! End of ISP loop (VOC species)
C
C Read Daily Average (ton/dy) speciated PM2.5 emissions,
C print sums, maximums, location of maximums in and lat,lon
C
      WRITE(7,'(/30A1,A,30A1)')('-',I=1,30),' Day avrg. PM2.5em ton/dy '
     -,('-',I=1,30)
      WRITE(7,'(A,4X,A,9X,A,5X,A,3X,A,4X,A)')'Species','Total',
     -'Max','I-Max','Lat-Max','Lon-Max'
      DO ISP=1,IPM25
C Unzip point emission file
      WRITE(SPNAM,'(A2,I2.2)')'PM',ISP
      STATUS=SYSTEM('gunzip ../point/dayav/PM25spec/'//SPNAM)
      OPEN(8,FILE='../point/dayav/PM25spec/'//SPNAM)
      READ(8,'(12E9.2)')EM
      CLOSE(8)
C Zip back up the daily average point emission file
      STATUS=SYSTEM('gzip ../point/dayav/PM25spec/'//SPNAM)
C Get total emission and max emission information
      CALL EMSUM(ETOT,EMAX,IEMAX)
C Output Total and Max info
      WRITE(7,'(A9,1P2E11.3,I7,0P2F11.5)')NAMPM2(ISP),ETOT,EMAX,
     -IEMAX,XLAT(IEMAX),XLON(IEMAX)
      ENDDO   ! End of ISP loop (PM2.5 species)
C
C Read UTC Hour 20, (short ton/hr) primary emissions,
C print sums, maximums, location of maximums in lat,lon
C
      WRITE(7,'(/30A1,A,30A1)')('-',I=1,30),' Hr_20 emis. ton/hr  ',
     -('-',I=1,30)
      WRITE(7,'(A,4X,A,9X,A,5X,A,3X,A,4X,A)')'Species','Total',
     -'Max','I-Max','Lat-Max','Lon-Max'
      DO ISP=1,IPRIM
C Unzip point emission file
      STATUS=SYSTEM('gunzip ../point/HR20/'//NAMINF(ISP))
      OPEN(8,FILE='../point/HR20/'//NAMINF(ISP))
      READ(8,'(12E9.2)')EM
      CLOSE(8)
C Zip back up the daily average point emission file
      STATUS=SYSTEM('gzip ../point/HR20/'//NAMINF(ISP))
C Get total emission and max emission information
      CALL EMSUM(ETOT,EMAX,IEMAX)
C Output Total and Max info
      WRITE(7,'(A9,1P2E11.3,I7,0P2F11.5)')NAMINF(ISP),ETOT,EMAX,
     -IEMAX,XLAT(IEMAX),XLON(IEMAX)
      ENDDO   ! End of ISP loop (primary species)
C
C Read UTC Hour 20, Day Average (mole/hr) speciated VOC emissions,
C print sums, maximums, location of maximums in lat,lon
C
      WRITE(7,'(/30A1,A,30A1)')('-',I=1,30),' Hr_20 VOC em mole/hr '
     -,('-',I=1,30)
      WRITE(7,'(A,4X,A,9X,A,5X,A,3X,A,4X,A)')'Species','Total',
     -'Max','I-Max','Lat-Max','Lon-Max'
      DO ISP=1,IVOC
C Unzip point emission file
      WRITE(SPNAM,'(A2,I2.2)')'HC',ISP
      STATUS=SYSTEM('gunzip ../point/HR20/'//SPNAM)
      OPEN(8,FILE='../point/HR20/'//SPNAM)
      READ(8,'(12E9.2)')EM
      CLOSE(8)
C Zip back up the daily average point emission file
      STATUS=SYSTEM('gzip ../point/HR20/'//SPNAM)
C Get total emission and max emission information
      CALL EMSUM(ETOT,EMAX,IEMAX)
C Output Total and Max info
      WRITE(7,'(A9,1P2E11.3,I7,0P2F11.5)')NAMVOC(ISP),ETOT,EMAX,
     -IEMAX,XLAT(IEMAX),XLON(IEMAX)
      ENDDO   ! End of ISP loop (VOC species)
C
C Read UTC Hour 20, Day Average (ton/hr) speciated PM2.5 emissions,
C print sums, maximums, location of maximums in lat,lon
C
      WRITE(7,'(/30A1,A,30A1)')('-',I=1,30),' Hr_20 PM2.5em ton/hr '
     -,('-',I=1,30)
      WRITE(7,'(A,4X,A,9X,A,5X,A,3X,A,4X,A)')'Species','Total',
     -'Max','I-Max','Lat-Max','Lon-Max'
      DO ISP=1,IPM25
C Unzip point emission file
      WRITE(SPNAM,'(A2,I2.2)')'PM',ISP
      STATUS=SYSTEM('gunzip ../point/HR20/'//SPNAM)
      OPEN(8,FILE='../point/HR20/'//SPNAM)
      READ(8,'(12E9.2)')EM
      CLOSE(8)
C Zip back up the hourly average point emission file
      STATUS=SYSTEM('gzip ../point/HR20/'//SPNAM)
C Get total emission and max emission information
      CALL EMSUM(ETOT,EMAX,IEMAX)
C Output Total and Max info
      WRITE(7,'(A9,1P2E11.3,I7,0P2F11.5)')NAMPM2(ISP),ETOT,EMAX,
     -IEMAX,XLAT(IEMAX),XLON(IEMAX)
      ENDDO   ! End of ISP loop (PM2.5 species)
      CLOSE(7)
      STOP9999
      END
      SUBROUTINE EMSUM(ETOT,EMAX,IEMAX)
      PARAMETER(IPOINT=151140)
      COMMON/RELPDAT/EM(IPOINT)
C Calculate total and max
      EMAX=-1.E25
      ETOT=0.
      DO I=1,IPOINT
      ETOT=ETOT+EM(I)
      IF(EM(I).GT.EMAX)THEN
      EMAX=EM(I)
      IEMAX=I
      ENDIF
      ENDDO
      RETURN
      END
