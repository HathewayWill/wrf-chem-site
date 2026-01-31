      PROGRAM TOTAL
C-----------------------------------------------------------------------
      PARAMETER(IX=1332,JX=1008,IP=IX+1,JP=JX+1,IM=IX-1,JM=JX-1)
C Number of Primary (IPRIM), VOC (IVOC), and PM2.5 (IPM25) species
      PARAMETER(IPRIM=7,IVOC=50,IPM25=19)
C-----------------------------------------------------------------------
      COMMON/EM2D/EMT(IX,JX)
      DIMENSION XLAT(IX,JX),XLON(IX,JX),XLATD(IP,JP),XLOND(IP,JP)
      CHARACTER*80 INFIL,FIL,FIL24
      CHARACTER*9 NAM
      CHARACTER*10 INARG,NAMINF(IPRIM),NAMVOC(IVOC),NAMPM2(IPM25)
      CHARACTER*4 HRNAM,SPNAM
      INTEGER STATUS,SYSTEM
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
      OPEN(7,FILE='area_read.output')
C
C Read Cross point and Dot point Latitude and Longitudes, print corners
C
      WRITE(7,'(/30A1,A,30A1)')('-',I=1,30),' Lat, Lon position files ',
     -('-',I=1,30)
      OPEN(8,FILE='../grid_loc/LAT_xrs.txt')
      READ(8,'(12F10.5)')XLAT
      CLOSE(8)
      OPEN(8,FILE='../grid_loc/LON_xrs.txt')
      READ(8,'(12F10.5)')XLON
      CLOSE(8)
      OPEN(8,FILE='../grid_loc/LAT_dot.txt')
      READ(8,'(12F10.5)')XLATD
      CLOSE(8)
      OPEN(8,FILE='../grid_loc/LON_dot.txt')
      READ(8,'(12F10.5)')XLOND
      CLOSE(8)
      WRITE(7,'(A,2F11.5)')'(1,1) Cross Point Lat,Lon= ',
     -XLAT(1,1),XLON(1,1)
      WRITE(7,'(2(A,I4),A,2F11.5)')'(',IX,',',JX,
     -') Cross Point Lat,Lon= ',XLAT(IX,JX),XLON(IX,JX)
      WRITE(7,'(A,2F11.5)')'(1,1) Dot Point Lat,Lon= ',
     -XLATD(1,1),XLOND(1,1)
      WRITE(7,'(2(A,I4),A,2F11.5)')'(',IP,',',JP,') Dot Point Lat,Lon= '
     -,XLATD(IP,JP),XLOND(IP,JP)
C
C Read Daily Ozone Season Day Average (short ton/dy) primary emissions,
C print sums, maximums, location of maximums in grid space and lat,lon
C
      WRITE(7,'(/30A1,A,30A1)')('-',I=1,30),' Daily OSD emis. ton/dy  ',
     -('-',I=1,30)
      WRITE(7,'(A,4X,A,7X,A,6X,A,2X,A,2X,A,4X,A)')'Species','Total',
     -'Max','I-Max','J-Max','Lat-Max','Lon-Max'
      DO ISP=1,IPRIM
C Unzip area emission file
      STATUS=SYSTEM('gunzip ../area4k/dayav/'//NAMINF(ISP))
      OPEN(8,FILE='../area4k/dayav/'//NAMINF(ISP))
      READ(8,'(12E9.2)')EMT
      CLOSE(8)
C Zip back up the daily average area emission file
      STATUS=SYSTEM('gzip ../area4k/dayav/'//NAMINF(ISP))
C Get total emission and max emission information
      CALL EMSUM(ETOT,EMAX,IEMAX,JEMAX)
C Get approximate lat,lon of center of grid point
      CALL MAPCF(FLOAT(IEMAX)+.5,FLOAT(JEMAX)+.5,XLTMX,XLNMX)
C Output Total and Max info
      WRITE(7,'(A9,1P2E11.3,2I6,0P2F11.5)')NAMINF(ISP),ETOT,EMAX,
     -IEMAX,JEMAX,XLTMX,XLNMX
      ENDDO   ! End of ISP loop (primary species)
C
C Read Daily Ozone Season Day Average (mole/dy) speciated VOC emissions,
C print sums, maximums, location of maximums in grid space and lat,lon
C 12/14/08 - new units for individual VOC daily average, used to be mole/hr, now mole/dy
      WRITE(7,'(/30A1,A,30A1)')('-',I=1,30),' Day avrg. VOC em mole/dy '
     -,('-',I=1,30)
      WRITE(7,'(A,4X,A,7X,A,6X,A,2X,A,2X,A,4X,A)')'Species','Total',
     -'Max','I-Max','J-Max','Lat-Max','Lon-Max'
      DO ISP=1,IVOC
C Unzip area emission file
      WRITE(SPNAM,'(A2,I2.2)')'HC',ISP
      STATUS=SYSTEM('gunzip ../area4k/dayav/VOCspec/'//SPNAM)
      OPEN(8,FILE='../area4k/dayav/VOCspec/'//SPNAM)
      READ(8,'(12E9.2)')EMT
      CLOSE(8)
C Zip back up the daily average area emission file
      STATUS=SYSTEM('gzip ../area4k/dayav/VOCspec/'//SPNAM)
C Get total emission and max emission information
      CALL EMSUM(ETOT,EMAX,IEMAX,JEMAX)
C Get approximate lat,lon of center of grid point
      CALL MAPCF(FLOAT(IEMAX)+.5,FLOAT(JEMAX)+.5,XLTMX,XLNMX)
C Output Total and Max info
      WRITE(7,'(A9,1P2E11.3,2I6,0P2F11.5)')NAMVOC(ISP),ETOT,EMAX,
     -IEMAX,JEMAX,XLTMX,XLNMX
      ENDDO   ! End of ISP loop (VOC species)
C
C Read Daily Ozone Season Day Average (ton/dy) speciated PM2.5 emissions,
C print sums, maximums, location of maximums in grid space and lat,lon
C
      WRITE(7,'(/30A1,A,30A1)')('-',I=1,30),' Day avrg. PM2.5em ton/dy '
     -,('-',I=1,30)
      WRITE(7,'(A,4X,A,7X,A,6X,A,2X,A,2X,A,4X,A)')'Species','Total',
     -'Max','I-Max','J-Max','Lat-Max','Lon-Max'
      DO ISP=1,IPM25
C Unzip area emission file
      WRITE(SPNAM,'(A2,I2.2)')'PM',ISP
      STATUS=SYSTEM('gunzip ../area4k/dayav/PM25spec/'//SPNAM)
      OPEN(8,FILE='../area4k/dayav/PM25spec/'//SPNAM)
      READ(8,'(12E9.2)')EMT
      CLOSE(8)
C Zip back up the daily average area emission file
      STATUS=SYSTEM('gzip ../area4k/dayav/PM25spec/'//SPNAM)
C Get total emission and max emission information
      CALL EMSUM(ETOT,EMAX,IEMAX,JEMAX)
C Get approximate lat,lon of center of grid point
      CALL MAPCF(FLOAT(IEMAX)+.5,FLOAT(JEMAX)+.5,XLTMX,XLNMX)
C Output Total and Max info
      WRITE(7,'(A9,1P2E11.3,2I6,0P2F11.5)')NAMPM2(ISP),ETOT,EMAX,
     -IEMAX,JEMAX,XLTMX,XLNMX
      ENDDO   ! End of ISP loop (VOC species)
C
C Read UTC Hour 20, Ozone Season Day (short ton/hr) primary emissions,
C print sums, maximums, location of maximums in grid space and lat,lon
C
      WRITE(7,'(/30A1,A,30A1)')('-',I=1,30),' Hr_20 OSD emis. ton/hr  ',
     -('-',I=1,30)
      WRITE(7,'(A,4X,A,7X,A,6X,A,2X,A,2X,A,4X,A)')'Species','Total',
     -'Max','I-Max','J-Max','Lat-Max','Lon-Max'
      DO ISP=1,IPRIM
C Unzip area emission file
      STATUS=SYSTEM('gunzip ../area4k/HR20/'//NAMINF(ISP))
      OPEN(8,FILE='../area4k/HR20/'//NAMINF(ISP))
      READ(8,'(12E9.2)')EMT
      CLOSE(8)
C Zip back up the daily average area emission file
      STATUS=SYSTEM('gzip ../area4k/HR20/'//NAMINF(ISP))
C Get total emission and max emission information
      CALL EMSUM(ETOT,EMAX,IEMAX,JEMAX)
C Get approximate lat,lon of center of grid point
      CALL MAPCF(FLOAT(IEMAX)+.5,FLOAT(JEMAX)+.5,XLTMX,XLNMX)
C Output Total and Max info
      WRITE(7,'(A9,1P2E11.3,2I6,0P2F11.5)')NAMINF(ISP),ETOT,EMAX,
     -IEMAX,JEMAX,XLTMX,XLNMX
      ENDDO   ! End of ISP loop (primary species)
C
C Read UTC Hour 20, Ozone Season Day Average (mole/hr) speciated VOC emissions,
C print sums, maximums, location of maximums in grid space and lat,lon
C
      WRITE(7,'(/30A1,A,30A1)')('-',I=1,30),' Hr_20 OSD VOC em mole/hr '
     -,('-',I=1,30)
      WRITE(7,'(A,4X,A,7X,A,6X,A,2X,A,2X,A,4X,A)')'Species','Total',
     -'Max','I-Max','J-Max','Lat-Max','Lon-Max'
      DO ISP=1,IVOC
C Unzip area emission file
      WRITE(SPNAM,'(A2,I2.2)')'HC',ISP
      STATUS=SYSTEM('gunzip ../area4k/HR20/'//SPNAM)
      OPEN(8,FILE='../area4k/HR20/'//SPNAM)
      READ(8,'(12E9.2)')EMT
      CLOSE(8)
C Zip back up the daily average area emission file
      STATUS=SYSTEM('gzip ../area4k/HR20/'//SPNAM)
C Get total emission and max emission information
      CALL EMSUM(ETOT,EMAX,IEMAX,JEMAX)
C Get approximate lat,lon of center of grid point
      CALL MAPCF(FLOAT(IEMAX)+.5,FLOAT(JEMAX)+.5,XLTMX,XLNMX)
C Output Total and Max info
      WRITE(7,'(A9,1P2E11.3,2I6,0P2F11.5)')NAMVOC(ISP),ETOT,EMAX,
     -IEMAX,JEMAX,XLTMX,XLNMX
      ENDDO   ! End of ISP loop (VOC species)
C
C Read UTC Hour 20, Ozone Season Day Average (ton/hr) speciated PM2.5 emissions,
C print sums, maximums, location of maximums in grid space and lat,lon
C
      WRITE(7,'(/30A1,A,30A1)')('-',I=1,30),' Hr_20 OSD PM2.5em ton/hr '
     -,('-',I=1,30)
      WRITE(7,'(A,4X,A,7X,A,6X,A,2X,A,2X,A,4X,A)')'Species','Total',
     -'Max','I-Max','J-Max','Lat-Max','Lon-Max'
      DO ISP=1,IPM25
C Unzip area emission file
      WRITE(SPNAM,'(A2,I2.2)')'PM',ISP
      STATUS=SYSTEM('gunzip ../area4k/HR20/'//SPNAM)
      OPEN(8,FILE='../area4k/HR20/'//SPNAM)
      READ(8,'(12E9.2)')EMT
      CLOSE(8)
C Zip back up the daily average area emission file
      STATUS=SYSTEM('gzip ../area4k/HR20/'//SPNAM)
C Get total emission and max emission information
      CALL EMSUM(ETOT,EMAX,IEMAX,JEMAX)
C Get approximate lat,lon of center of grid point
      CALL MAPCF(FLOAT(IEMAX)+.5,FLOAT(JEMAX)+.5,XLTMX,XLNMX)
C Output Total and Max info
      WRITE(7,'(A9,1P2E11.3,2I6,0P2F11.5)')NAMPM2(ISP),ETOT,EMAX,
     -IEMAX,JEMAX,XLTMX,XLNMX
      ENDDO   ! End of ISP loop (PM2.5 species)
      CLOSE(7)
      STOP9999
      END
      SUBROUTINE EMSUM(ETOT,EMAX,IEMAX,JEMAX)
C-----------------------------------------------------------------------
      PARAMETER(IX=1332,JX=1008,IP=IX+1,JP=JX+1,IM=IX-1,JM=JX-1)
C-----------------------------------------------------------------------
      COMMON/EM2D/EMT(IX,JX)
C Calculate total and max
      EMAX=-1.E25
      ETOT=0.
      DO I=1,IX
      DO J=1,JX
      ETOT=ETOT+EMT(I,J)
      IF(EMT(I,J).GT.EMAX)THEN
      EMAX=EMT(I,J)
      IEMAX=I
      JEMAX=J
      ENDIF
      ENDDO
      ENDDO
      RETURN
      END
      SUBROUTINE MAPCF (XI,YJ,XLAT,XLON)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                     C
C                                                                     C
C     THIS SUBROUTINE COMPUTES THE LATITUDE AND LONGITUDE FROM MODEL  C
C     INDEXES OF A POINT.                                             C
C                                                                     C
C     INPUT :                                                         C
C                                                                     C
C        XI : X COORDINATE OF THE POINT IN MODEL INDEX.               C
C                                                                     C
C        YJ : Y COORDINATE OF THE POINT IN MODEL INDEX.               C
C                                                                     C
C     OUTPUT :                                                        C
C                                                                     C
C        XLAT : LATITUDE OF THE POINT.                                C
C                                                                     C
C        XLON : LONGITUDE OF THE POINT.                               C
C                                                                     C
C     INFORMATION NEEDED:                                             C
C                                                                     C
C        XLATC : LATITUDE OF THE CENTER POINT OF THE MODEL DOMAIN.    C
C                                                                     C
C        XLONC : LONGITUDE OF THE CENTER POINT OF THE MODEL DOMAIN.   C
C                                                                     C
C        DXKM  : GRID SIZE IN KILOMETERS.                             C
C                                                                     C
C        IL    : X DIMENSION FOR THE MODEL DOT-POINT DOMAIN.          C
C                                                                     C
C        JL    : Y DIMENSION FOR THE MODEL DOT-POINT DOMAIN.          C
C                                                                     C
C                                                                     C
C     NOTE ***                                                        C
C                                                                     C
C     A WEST LONGITUDE IS GIVEN BY A NEGATIVE NUMBER; POSITIVE        C
C     ANGLES DENOTE EAST LONGITUDE.                                   C
C                                                                     C
C     A NORTH LATITUDE IS GIVEN BY A POSITIVE NUMBER, AND A NEGATIVE  C
C     NUMBER FOR A SOUTH LATITUDE.                                    C
C                                                                     C
C                                                                     C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C-----------------------------------------------------------------------
C 4km EPA-surrogate file domain - nested within following centered grid
      DATA DXKM,XLATC,XLONC,IL,JL/4.,40.,-97.,1369,1045/
      DATA CLAT1,CLAT2/45.,33./
      DATA A,POLE / 6370.997,90./
      DATA KOUNT / 0 /
      SAVE
      KOUNT = KOUNT+1
      IF(KOUNT.EQ.1)THEN
        FRTY5D=ATAN(1.)
        CONV=45./FRTY5D
        RLAT1=CLAT1/CONV
        RLAT2=CLAT2/CONV
        XN=ALOG(COS(RLAT1)/COS(RLAT2))/ALOG(TAN(FRTY5D+.5*RLAT2)/
     -TAN(FRTY5D+.5*RLAT1))
C
C-----A IS RADIUS OF EARTH IN KM
C     SUBTRACT XROT DEGREES TO ROTATE LAMBERT CONFORMAL PROJECTION
C     CALCULATE R
C
        PSX = (POLE-XLATC)/CONV
        CELL = A*SIN(RLAT1)/XN
        CELL2 = (TAN(PSX/2.))/(TAN(RLAT1/2.))
        R = CELL*(CELL2)**XN
        CNTRJ = (JL+1)/2.
        CNTRI = (IL+1)/2.
        RXN = 1.0/XN
      ENDIF   ! END of KOUNT=1 test
      X = (XI-CNTRI)*DXKM
      Y = (YJ-CNTRJ)*DXKM-R
C
C-----NOW CALC LAT AND LONG OF THIS POINT
C
      FLP = ATAN2(X,-Y)
      FLPP = (FLP/XN)*CONV+XLONC
      IF (FLPP.LT.-180.) FLPP = FLPP+360.
      IF (FLPP.GT.180.) FLPP = FLPP-360.
      XLON = FLPP
C
C-----NOW SOLVE FOR LATITUDE
C
      ZZZZ=X*X+Y*Y
      RS = SQRT(X*X+Y*Y)
      CELL = (RS*XN)/(A*SIN(RLAT1))
      CEL1 = TAN(RLAT1/2.)*(CELL)**RXN
      CEL2 = ATAN(CEL1)
      PSX = 2.*CEL2*CONV
      XLAT = POLE-PSX
      RETURN
      END
