SUBROUTINE GSTATS_PRINT(KULOUT,PAVEAVE,KLEN)

!**** *GSTATS_PRINT* - print timing statistics

!     PURPOSE.
!     --------
!       To print out timings gathered by GSTATS


!**   INTERFACE.
!     ----------
!       *CALL* *GSTATS_PRINT*

!        EXPLICIT ARGUMENTS     None
!        --------------------


!        IMPLICIT ARGUMENTS
!        --------------------
!        Module YOMSTATS

!     METHOD.
!     -------


!     EXTERNALS.   
!     ----------   

!     REFERENCE.
!     ----------
!        ECMWF Research Department documentation of the IFS

!     AUTHOR.
!     -------
!        Mats Hamrud ECMWF

!     MODIFICATIONS.
!     --------------
!        ORIGINAL : 98-11-15
!        D.Salmond: 99-09-21 : Timer for SLCOMM2
!        G.Mozdzynski 05-09-25 : fix master ncalls overwrite for nproc>1
!        C.Larsson    8-May-2006 : Added xml file output
!     ------------------------------------------------------------------
USE PARKIND1  ,ONLY : JPIM     ,JPRB

USE YOMGSTATS
USE YOMMPI   , ONLY : MREALT
USE MPL_MODULE

IMPLICIT NONE

INTEGER(KIND=JPIM) :: KULOUT,KLEN
REAL(KIND=JPRB) :: PAVEAVE(0:KLEN)
CHARACTER*7  CLACTION(0:3)
INTEGER(KIND=JPIM),PARAMETER :: JPARRAYS=8
REAL(KIND=JPRB) :: ZREABUF(JPARRAYS*(JPMAXSTAT+1))
REAL(KIND=JPRB) :: ZAVEAVE(0:JPMAXSTAT),ZAVEMAX(0:JPMAXSTAT),ZTIMELCALL(0:JPMAXSTAT),&
         &ZTHISTIME(0:JPMAXSTAT),ZFRACMAX(0:JPMAXSTAT),&
         &ZSUMMAX(0:JPMAXSTAT),ZSUMTOT(0:JPMAXSTAT)
REAL(KIND=JPRB) :: ZT_SUM,ZT_SUM2,ZT_SUM3,ZT_SUMIO,ZT_SUM4

INTEGER(KIND=JPIM) :: ICALLSX(0:JPMAXSTAT)

!     LOCAL INTEGER SCALARS
INTEGER(KIND=JPIM) :: ICALLS, IERR, ILBUF, ILSEND, &
             &ISEND, ITAG, JJ, JNUM, JROC, JCALL, ICALLER,IACTION
INTEGER(KIND=JPIM) :: IMEM, INUM, JMEM

!     LOCAL REAL SCALARS
REAL(KIND=JPRB) :: ZAVE, ZAVETCPU, ZAVEVCPU, ZCOMTIM, ZDETAIL,&
          &ZFRAC, ZMAX, ZMEAN, ZSTDDEV, ZSUM, ZSUMB, &
          &ZTOTAL, ZTOTCPU, ZTOTMEAN, ZTOTUNBAL, ZTOTVCPU, &
          &ZUNBAL, ZMEANT, ZMAXT

INTEGER(KIND=JPIM) :: IXMLLUN  

!     ------------------------------------------------------------------

ILBUF = JPARRAYS*(JPMAXSTAT+1)
ZAVEAVE(:) = 0.0_JPRB
ZAVEMAX(:) = 0.0_JPRB
ZFRACMAX(:)= 0.0_JPRB
ZSUMMAX(:)= 0.0_JPRB
ZSUMTOT(:)= 0.0_JPRB

! OPEN GSTATS.XML for xml statistics
IXMLLUN=40
OPEN (UNIT=IXMLLUN, FILE='gstats.xml',ACTION='write')
WRITE(IXMLLUN,'(A)')'<?xml version="1.0" encoding="UTF-8"?>'
WRITE(IXMLLUN,'(A)')'<gstats '
WRITE(IXMLLUN,'(A)')' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.ecmwf.int/services/prepifs/gstats   ./gstats.xsd">'

WRITE(KULOUT,'(A)')'===-=== START OF TIMING STATISTICS ===-==='
IF(LSYNCSTATS.AND.NPROC_STATS>1) THEN
  WRITE(KULOUT,'(A)')'START OF TIMINGS SYNCRONIZED'
ENDIF
IF (LSTATS .AND. MYPROC_STATS /= 1) THEN
  JJ = 1
  DO JNUM=0,JPMAXSTAT
    ZREABUF(JJ  ) = TIMESUM(JNUM)
    ZREABUF(JJ+1) = TIMESQSUM(JNUM)
    ZREABUF(JJ+2) = TIMEMAX(JNUM)
    ZREABUF(JJ+3) = TIMESUMB(JNUM)
    ZREABUF(JJ+4) = TIMELCALL(JNUM)
    ZREABUF(JJ+5) = NCALLS(JNUM)
    ZREABUF(JJ+6) = TTCPUSUM(JNUM)
    ZREABUF(JJ+7) = TVCPUSUM(JNUM)
    JJ = JJ+JPARRAYS
  ENDDO
  ILSEND = ILBUF
  ISEND =1
  ITAG = JPTAGSTAT
  CALL MPL_SEND(ZREABUF(1:ILSEND),KDEST=NPRCIDS_STATS(ISEND), &
       & KTAG=ITAG,CDSTRING='GSTATS_PRINT:')
ELSEIF(LSTATS) THEN
  DO JROC=1,NPROC_STATS
    IF (JROC /= 1) THEN
      ITAG = JPTAGSTAT
      CALL MPL_RECV(ZREABUF(1:ILBUF),KSOURCE=NPRCIDS_STATS(JROC), &
       & KTAG=ITAG,CDSTRING='GSTATS_PRINT:')
      JJ = 1
      DO JNUM=0,JPMAXSTAT
        TIMESUM(JNUM)   = ZREABUF(JJ  )
        TIMESQSUM(JNUM) = ZREABUF(JJ+1)
        TIMEMAX(JNUM)   = ZREABUF(JJ+2)
        TIMESUMB(JNUM)  = ZREABUF(JJ+3)
        TIMELCALL(JNUM) = ZREABUF(JJ+4)
        ICALLSX(JNUM)    = NINT(ZREABUF(JJ+5))
        TTCPUSUM(JNUM)  = ZREABUF(JJ+6)
        TVCPUSUM(JNUM)  = ZREABUF(JJ+7)
        JJ = JJ+JPARRAYS
      ENDDO
    ELSE
      ICALLSX(:)=NCALLS(:)
    ENDIF
    IF (JROC == 1) THEN
      ZTOTAL=TIMESUM(0)
      ZTOTCPU = TTCPUSUM(0)
      ZTOTVCPU = TVCPUSUM(0)
    ENDIF
    IF(.NOT. LSTATSCPU) THEN
      TTCPUSUM(1:JPMAXSTAT) = -0.0_JPRB
      TVCPUSUM(1:JPMAXSTAT) = -0.0_JPRB
    ENDIF
    IF( LDETAILED_STATS .AND. JROC < 3 ) THEN
      ZT_SUM=0.0_JPRB
      ZT_SUM2=0.0_JPRB
      ZT_SUM3=0.0_JPRB
      ZT_SUM4=0.0_JPRB
      ZT_SUMIO=0.0_JPRB
      WRITE(KULOUT,'(A,I4)') 'TIMING STATISTICS:PROCESSOR=',JROC
      WRITE(IXMLLUN,'(A,I4,A)')'<timing processor="',JROC,'">'
      IF(NPROC_STATS > 1) THEN
        WRITE(KULOUT,'(A,F6.1,A)')'STARTUP COST ',&
         &TIME_START(JROC),' SECONDS'
      ENDIF
      WRITE(KULOUT,'(A)')&
       &'NUM  ROUTINE                                  '//&
       &'CALLS  SUM(s)   AVE(ms)   CPUAVE(ms) VAVE(ms) '//&
       &'STDDEV(ms)  MAX(ms) '//&
       &'SUMB(s) FRAC(%)'
    ENDIF
    DO JNUM=0,JPMAXSTAT
      IF(ICALLSX(JNUM) > 1) THEN
        ICALLS = ICALLSX(JNUM)/2
        ZSUM = TIMESUM(JNUM)
        ZAVE = TIMESUM(JNUM)/ICALLS*1000._JPRB
        ZMAX = TIMEMAX(JNUM)*1000._JPRB
        ZSUMB = TIMESUMB(JNUM)
        ZFRAC = TIMESUM(JNUM)/ZTOTAL*100.0_JPRB
        ZFRACMAX(JNUM)=MAX(ZFRACMAX(JNUM),ZFRAC)
        ZSUMMAX(JNUM)=MAX(ZSUMMAX(JNUM),TIMESUM(JNUM))
        ZSUMTOT(JNUM)=ZSUMTOT(JNUM)+ZSUM
        ZAVEAVE(JNUM)=ZAVEAVE(JNUM)+ZAVE
        ZAVEMAX(JNUM)=MAX(ZAVEMAX(JNUM),ZAVE)
        ZAVETCPU = TTCPUSUM(JNUM)/ICALLS*1000._JPRB
        ZAVEVCPU = TVCPUSUM(JNUM)/ICALLS*1000._JPRB
        IF(ICALLS > 1 ) THEN
          ZSTDDEV = 1000._JPRB*&
           &SQRT(MAX((TIMESQSUM(JNUM)-TIMESUM(JNUM)**2/ICALLS)&
           &/(ICALLS-1),0.0_JPRB))
        ELSE
          ZSTDDEV = 0.0_JPRB
        ENDIF
        IF((JNUM > 500.AND.JNUM < 700) .OR. (JNUM > 800.AND.JNUM < 1000 )) THEN
          ZT_SUM=ZT_SUM+ZSUM
        ENDIF
        IF(JNUM > 700.AND.JNUM < 800) THEN
          ZT_SUM4=ZT_SUM4+ZSUM
        ENDIF
        IF(JNUM > 1000 .AND. JNUM < 1700) THEN
          ZT_SUM2=ZT_SUM2+ZSUM
        ENDIF
        IF(JNUM >= 1700 .AND. JNUM < 1800) THEN
          ZT_SUMIO=ZT_SUMIO+ZSUM
        ENDIF
        IF(JNUM >= 1800) THEN
          ZT_SUM3=ZT_SUM3+ZSUM
        ENDIF
        IF( LDETAILED_STATS .AND. JROC < 3 ) THEN
          IF(JNUM < 501 .OR. LSTATS_COMMS .OR. LSTATS_OMP) THEN 
            WRITE(KULOUT,'(I4,1X,A40,1X,I5,6(1X,F9.1),1X,F5.1,1X,F8.2))')&
             &JNUM,CCDESC(JNUM),ICALLS,ZSUM,ZAVE,ZAVETCPU,ZAVEVCPU,&
             &ZSTDDEV,ZMAX,ZSUMB,ZFRAC
            WRITE(IXMLLUN,&
             & '(A,I4,A,//,A,A40,A,//,A,I5,A,//,6(A,F9.1,A,//),A,F5.1,A,//,A,F8.2,A),//,A)')&
             & '<num id="',JNUM,'">',&
             & '<description>',CCDESC(JNUM),'</description>',&
             & '<sum unit="seconds">',ICALLS,'</sum>',&
             & '<sumcall unit="ms">',ZSUM,'</sumcall>',&
             & '<average unit="ms">',ZAVE,'</average>',&
             & '<cpuaverage unit="ms">',ZAVETCPU,'</cpuaverage>',&
             & '<vave unit="ms">',ZAVEVCPU,'</vave>',&
             & '<stddev unit="ms">',ZSTDDEV,'</stddev>',&
             & '<max unit="ms">',ZMAX,'</max>',&
             & '<sumb unit="seconds">',ZSUMB,'</sumb>',&
             & '<frac unit="percent">',ZFRAC,'</frac>',&
             & '</num>'
         ENDIF
        ENDIF
      ENDIF
    ENDDO
!    ZCOMTIM = SUM(TIMESUM(51:99))+SUM(TIMESUM(151:199))
!    ZDETAIL = SUM(TIMESUM(:))-TIMESUM(0)-TIMESUM(1)-SUM(TIMESUM(20:23))
    IF( LDETAILED_STATS .AND. JROC < 3 ) THEN
      WRITE(KULOUT,*) ''
      WRITE(KULOUT,'((A,2F8.1))')&
       &'CPU-TIME AND VECTOR CPU-TIME AS PERCENT OF TOTAL ',&
       &TTCPUSUM(0)/TIMESUM(0)*100.0_JPRB,TVCPUSUM(0)/TIMESUM(0)*100.0_JPRB
      WRITE(IXMLLUN,'((A,F8.1,A,//,A,F8.1,A))')&
       &'<cpufraction>',&
       &TTCPUSUM(0)/TIMESUM(0)*100.0_JPRB,&
       &'</cpufraction>',&
       &'<cpuvectorfraction>',&
       &TVCPUSUM(0)/TIMESUM(0)*100.0_JPRB,&
       &'</cpuvectorfraction>'


      IF(ZT_SUM > 0.0_JPRB) THEN
        WRITE(KULOUT,'(A,F10.1,A,F6.2,A)')'SUMMED TIME IN COMMUNICATIONS   = '&
         & ,ZT_SUM, ' SECONDS ',ZT_SUM/TIMESUM(0)*100.0_JPRB,&
         &' PERCENT OF TOTAL'
        WRITE(IXMLLUN,'(A,F10.1,A,//,A,F6.2,A)')'<zcom unit="seconds">',&
         &ZT_SUM,'</zcom>',&
         &'<fraczcom unit="percent">',ZT_SUM/TIMESUM(0)*100.0_JPRB,&
         &'</fraczcom>'
      ENDIF
      IF(ZT_SUM2 > 0.0_JPRB) THEN
        WRITE(KULOUT,'(A,F10.1,A,F6.2,A)')'SUMMED TIME IN PARALLEL REGIONS = '&
         & ,ZT_SUM2, ' SECONDS ',ZT_SUM2/TIMESUM(0)*100.0_JPRB,&
         &' PERCENT OF TOTAL'
        WRITE(IXMLLUN,'(A,F10.1,A,//,A,F6.2,A)') &
         &'<parallelztime unit="seconds">',&
         &ZT_SUM2, '</parallelztime>',&
         &'<fracparallelztime unit="percent">',ZT_SUM2/TIMESUM(0)*100.0_JPRB,&
         &'</fracparallelztime>'
      ENDIF
      IF(ZT_SUMIO > 0.0_JPRB) THEN
        WRITE(KULOUT,'(A,F10.1,A,F6.2,A)')'SUMMED TIME IN I/O SECTIONS     = '&
         & ,ZT_SUMIO, ' SECONDS ',ZT_SUMIO/TIMESUM(0)*100.0_JPRB,&
         &' PERCENT OF TOTAL'
        WRITE(IXMLLUN,'(A,F10.1,A,//,A,F6.2,A)')'<ioztime unit="seconds">',&
         &ZT_SUMIO, '</ioztime>',&
         &'<fracioztime unit="percent">',ZT_SUMIO/TIMESUM(0)*100.0_JPRB,&
         &'</fracioztime>'
      ENDIF
      IF(ZT_SUM3 > 0.0_JPRB) THEN
        WRITE(KULOUT,'(A,F10.1,A,F6.2,A)')'SUMMED TIME IN SERIAL SECTIONS  = '&
        & ,ZT_SUM3, ' SECONDS ',ZT_SUM3/TIMESUM(0)*100.0_JPRB,&
         &' PERCENT OF TOTAL'
        WRITE(IXMLLUN,'(A,F10.1,A,//,A,F6.2,A)')'<serialztime unit="seconds">',&
         & ZT_SUM3,'</serialztime>',&
         &'<fracserialztime unit="percent">',&
         &ZT_SUM3/TIMESUM(0)*100.0_JPRB,&
         &'</fracserialztime>'
      ENDIF
      IF(ZT_SUM4 > 0.0_JPRB) THEN
        WRITE(KULOUT,'(A,F10.1,A,F6.2,A)')'SUMMED TIME IN BARRIERS         = '&
         & ,ZT_SUM4, ' SECONDS ',ZT_SUM4/TIMESUM(0)*100.0_JPRB,&
         &' PERCENT OF TOTAL'
        WRITE(IXMLLUN,'(A,F10.1,A,//,A,F6.2,A)')&
         &'<barrierztime unit="seconds">',&
         &ZT_SUM4,'</barrierztime>',&
         & '<fracbarrierztime unit="percent">',&
         & ZT_SUM4/TIMESUM(0)*100.0_JPRB,'</fracbarrierztime>'
      ENDIF
      IF(LSTATS_COMMS.AND.LSTATS_OMP)THEN
        WRITE(KULOUT,'(A,F8.2)')'FRACTION OF TOTAL TIME ACCOUNTED FOR ',&
         & (ZT_SUM+ZT_SUM2+ZT_SUMIO+ZT_SUM3+ZT_SUM4)/TIMESUM(0)*100.0_JPRB
        WRITE(IXMLLUN,'(A,F8.2,A)')'<fractotal unit="percent">',&
         &(ZT_SUM+ZT_SUM2+ZT_SUMIO+ZT_SUM3+ZT_SUM4)/TIMESUM(0)*100.0_JPRB,&
         &'</fractotal>'
      ENDIF
    ENDIF
    IF( LDETAILED_STATS .AND. JROC < 3 ) THEN
      WRITE(IXMLLUN,'(A)')'</timing>'
    ENDIF
  ENDDO
  WRITE(IXMLLUN,'(A)')'<timing_all_processors>'
  WRITE(KULOUT,*) ''
  WRITE(KULOUT,'(A)') 'STATS FOR ALL PROCESSORS'
  WRITE(KULOUT,'(A)') ' NUM ROUTINE                                '//&
   &'  CALLS  MEAN(ms)   MAX(ms)   FRAC(%)  UNBAL(%)'
  ZTOTUNBAL = 0.0_JPRB
  DO JNUM=0,500
    IF(NCALLS(JNUM) > 1) THEN
      ICALLS = NCALLS(JNUM)/2
      ZMEAN = ZAVEAVE(JNUM)/NPROC_STATS
      ZMAX  = ZAVEMAX(JNUM)
      ZMEANT = ZSUMTOT(JNUM)/NPROC_STATS
      ZMAXT  = ZSUMMAX(JNUM)
      IF(ZMEANT .NE. 0.0)THEN
        ZUNBAL= (ZMAXT-ZMEANT)/ZMEANT*100._JPRB
      ELSE
        ZUNBAL=0.0
      ENDIF
      ZFRAC=ZFRACMAX(JNUM)
      ZTOTUNBAL = ZTOTUNBAL+(ZMAXT-ZMEANT)
      WRITE(KULOUT,'(I4,1X,A40,1X,I5,2(1X,F9.1),2(1X,F9.2))')&
       &JNUM,CCDESC(JNUM),ICALLS,ZMEAN,ZMAX,ZFRAC,ZUNBAL

      WRITE(IXMLLUN,'(A,I4,A,//,A,A40,A,//,A,I5,A,2(A,F9.1,A,//),2(A,F9.2,A,//),A)')&
       &'<item id="',JNUM,'">',&
       &'<description>',CCDESC(JNUM),'</description>',&
       &'<calls>',ICALLS,'</calls>',&
       &'<mean unit="ms">',ZMEAN,'</mean>',&
       &'<max unit="ms">',ZMAX,'</max>',&
       &'<fraction unit="percent">',ZFRAC,'</fraction>',&
       &'<unbalanced unit="percent">',ZUNBAL,'</unbalanced>','</item>'
    ENDIF
  ENDDO
  WRITE(IXMLLUN,'(A)')'</timing_all_processors>'

IF(LSTATS_COMMS)THEN
  WRITE(IXMLLUN,'(A)')'<timing_communications>'
  WRITE(KULOUT,*) ''
  WRITE(KULOUT,*) 'STATS FOR COMMUNICATIONS'
  WRITE(KULOUT,*)  &
 &'NUM ROUTINE                CALLS    MEAN(ms)  MAX(ms)   FRAC(%)  UNBAL(%)'
  ZT_SUM=0.0
  DO JNUM=501,1000
    IF(NCALLS(JNUM) > 1) THEN
      ICALLS = NCALLS(JNUM)/2
      ZMEAN = ZAVEAVE(JNUM)/NPROC_STATS
      ZMAX  = ZAVEMAX(JNUM)
      ZMEANT = ZSUMTOT(JNUM)/NPROC_STATS
      ZMAXT  = ZSUMMAX(JNUM)
      IF(ZMEANT .NE. 0.0)THEN
        ZUNBAL= (ZMAXT-ZMEANT)/ZMEANT*100._JPRB
      ELSE
        ZUNBAL=0.0
      ENDIF
      ZFRAC=ZFRACMAX(JNUM)
      ZTOTUNBAL = ZTOTUNBAL+(ZMAXT-ZMEANT)
      WRITE(KULOUT,'(I4,1X,A22,1X,I5,2(1X,F9.1),2(1X,F9.2))')&
       &JNUM,CCDESC(JNUM),ICALLS,ZMEAN,ZMAX,ZFRAC,ZUNBAL
      WRITE(IXMLLUN,'(A,I4,A,//,A,A22,A,//,A,I5,A,//,2(A,F9.1,A,//),2(A,F9.2,A,//),A)')&
       &'<comitem id="',JNUM,'">',&
       &'<description>',CCDESC(JNUM),'</description>',&
       &'<calls>',ICALLS,'</calls>',&
       &'<mean unit="ms">',ZMEAN,'</mean>',&
       &'<max unit="ms">',ZMAX,'</max>',&
       &'<fraction unit="percent">',ZFRAC,'</fraction>',&
       &'<unbalanced unit="percent">',ZUNBAL,'</unbalanced>','</comitem>'

      ZT_SUM=ZT_SUM+ZMEANT
    ENDIF
  ENDDO

  WRITE(KULOUT,*) ''
  WRITE(KULOUT,'(A,F10.1,A)')'SUMMED TIME IN COMMUNICATIONS   = ',ZT_SUM, ' SECONDS '
  WRITE(IXMLLUN,'(A,F10.1,A)')'<zcom unit="seconds">',ZT_SUM, '</zcom>'
  WRITE(KULOUT,*) ''
  WRITE(IXMLLUN,'(A)')'</timing_communications>'

ENDIF
IF(LSTATS_OMP)THEN
  WRITE(IXMLLUN,'(A)')'<timing_parallel>'
  WRITE(KULOUT,*) ''
  WRITE(KULOUT,*) 'STATS FOR PARALLEL REGIONS'
  WRITE(KULOUT,*)  &
 &'NUM ROUTINE                CALLS    MEAN(ms)  MAX(ms)   FRAC(%)  UNBAL(%)'
  ZT_SUM=0.0
  DO JNUM=1001,1700
    IF(NCALLS(JNUM) > 1) THEN
      ICALLS = NCALLS(JNUM)/2
      ZMEAN = ZAVEAVE(JNUM)/NPROC_STATS
      ZMAX  = ZAVEMAX(JNUM)
      ZMEANT = ZSUMTOT(JNUM)/NPROC_STATS
      ZMAXT  = ZSUMMAX(JNUM)
      IF(ZMEANT .NE. 0.0)THEN
        ZUNBAL= (ZMAXT-ZMEANT)/ZMEANT*100._JPRB
      ELSE
        ZUNBAL=0.0
      ENDIF
      ZFRAC=ZFRACMAX(JNUM)
      ZTOTUNBAL = ZTOTUNBAL+(ZMAXT-ZMEANT)
      WRITE(KULOUT,'(I4,1X,A22,1X,I5,2(1X,F9.1),2(1X,F9.2))')&
       &JNUM,CCDESC(JNUM),ICALLS,ZMEAN,ZMAX,ZFRAC,ZUNBAL
      WRITE(IXMLLUN,'(A,I4,A,//,A,A22,A,//,A,I5,A,//,2(A,F9.1,A,//),2(A,F9.2,A,//),A)')&
       &'<parallelitem id="',JNUM,'">',&
       &'<description>',CCDESC(JNUM),'</description>',&
       &'<calls>',ICALLS,'</calls>',&
       &'<mean unit="seconds">',ZMEAN,'</mean>',&
       &'<max unit="seconds">',ZMAX,'</max>',&
       &'<fraction unit="percent">',ZFRAC,'</fraction>',&
       &'<unbalanced unit="percent">',ZUNBAL,'</unbalanced>',&
       &'</parallelitem>'

      ZT_SUM=ZT_SUM+ZMEANT
    ENDIF
  ENDDO

  WRITE(KULOUT,*) ''
  WRITE(KULOUT,'(A,F10.1,A)')'SUMMED TIME IN PARALLEL REGIONS = ',ZT_SUM, ' SECONDS '
  WRITE(KULOUT,*) ''

  WRITE(IXMLLUN,'(A,F10.1,A)')'<zpar unit="seconds">',ZT_SUM, '</zpar>'

  WRITE(KULOUT,*) ''
  WRITE(KULOUT,*) 'STATS FOR I/O REGIONS'
  WRITE(KULOUT,*)  &
   &'NUM ROUTINE                CALLS    MEAN(ms)  MAX(ms)   FRAC(%)  UNBAL(%)'
  ZT_SUM=0.0
  DO JNUM=1701,1800
    IF(NCALLS(JNUM) > 1) THEN
      ICALLS = NCALLS(JNUM)/2
      ZMEAN = ZAVEAVE(JNUM)/NPROC_STATS
      ZMAX  = ZAVEMAX(JNUM)
      ZMEANT = ZSUMTOT(JNUM)/NPROC_STATS
      ZMAXT  = ZSUMMAX(JNUM)
      IF(ZMEANT .NE. 0.0)THEN
        ZUNBAL= (ZMAXT-ZMEANT)/ZMEANT*100._JPRB
      ELSE
        ZUNBAL=0.0
      ENDIF
      ZFRAC=ZFRACMAX(JNUM)
      ZTOTUNBAL = ZTOTUNBAL+(ZMAXT-ZMEANT)
      WRITE(KULOUT,'(I4,1X,A22,1X,I5,2(1X,F9.1),2(1X,F9.2))')&
       &JNUM,CCDESC(JNUM),ICALLS,ZMEAN,ZMAX,ZFRAC,ZUNBAL
      WRITE(IXMLLUN,'(A,I4,A,//,A,A22,A,//,A,I5,A,//,2(A,F9.1,A,//),2(A,F9.2,A,//),A)')&
       &'<para_io_item id="',JNUM,'">',&
       &'<description>',CCDESC(JNUM),'</description>',&
       &'<calls>',ICALLS,'</calls>',&
       &'<mean unit="seconds">',ZMEAN,'</mean>','<max unit="seconds">',&
       & ZMAX,'</max>',&
       &'<fraction unit="percent">',ZFRAC,'</fraction>',&
       &'<unbalanced unit="percent">',ZUNBAL,'</unbalanced>',&
       &'</para_io_item>'

      ZT_SUM=ZT_SUM+ZMEANT
    ENDIF
  ENDDO

  WRITE(KULOUT,*) ''
  WRITE(KULOUT,'(A,F10.1,A)')'SUMMED TIME IN I/O REGIONS = ',&
       &ZT_SUM, ' SECONDS '
  WRITE(KULOUT,*) ''

  WRITE(IXMLLUN,'(A,F10.1,A)')'<zio unit="seconds">',ZT_SUM,'</zio>'

  WRITE(KULOUT,*) ''
  WRITE(KULOUT,*) 'STATS FOR SERIAL(no OMP) REGIONS'
  WRITE(KULOUT,*)  &
   &'NUM ROUTINE                CALLS    MEAN(ms)  MAX(ms)   FRAC(%)  UNBAL(%)'
  ZT_SUM=0.0
  DO JNUM=1801,JPMAXSTAT
    IF(NCALLS(JNUM) > 1) THEN
      ICALLS = NCALLS(JNUM)/2
      ZMEAN = ZAVEAVE(JNUM)/NPROC_STATS
      ZMAX  = ZAVEMAX(JNUM)
      ZMEANT = ZSUMTOT(JNUM)/NPROC_STATS
      ZMAXT  = ZSUMMAX(JNUM)
      IF(ZMEANT .NE. 0.0)THEN
        ZUNBAL= (ZMAXT-ZMEANT)/ZMEANT*100._JPRB
      ELSE
        ZUNBAL=0.0
      ENDIF
      ZFRAC=ZFRACMAX(JNUM)
      ZTOTUNBAL = ZTOTUNBAL+(ZMAXT-ZMEANT)
      WRITE(KULOUT,'(I4,1X,A22,1X,I5,2(1X,F9.1),2(1X,F9.2))')&
       &JNUM,CCDESC(JNUM),ICALLS,ZMEAN,ZMAX,ZFRAC,ZUNBAL
      WRITE(IXMLLUN,'(A,I4,A,A,A22,A,A,I5,A,2(A,F9.1,A),2(A,F9.2,A,//),A)')&
       &'<serialitem id="',JNUM,'">',&
       &'<description>',CCDESC(JNUM),'</description>',&
       &'<calls>',ICALLS,'</calls>',&
       &'<mean unit="ms">',ZMEAN,'</mean>','<max unit="ms">',ZMAX,'</max>',&
       &'<fraction unit="percent">',ZFRAC,'</fraction>',&
       &'<unbalanced unit="percent">',ZUNBAL,'</unbalanced>','</serialitem>'

      ZT_SUM=ZT_SUM+ZMEANT
    ENDIF
  ENDDO

  WRITE(KULOUT,*) ''
  WRITE(KULOUT,'(A,F10.1,A)')'SUMMED TIME IN SERIAL REGIONS = ',ZT_SUM, ' SECONDS '
  WRITE(KULOUT,*) ''

  WRITE(IXMLLUN,'(A,F10.1,A)')'<zserial unit="seconds">',&
   &ZT_SUM, '</zserial>'

  WRITE(IXMLLUN,'(A)')'</timing_parallel>'

ENDIF

  WRITE(KULOUT,'(A,F10.1,A,F4.1,A)')&
   &'TOTAL MEASURED IMBALANCE =',ZTOTUNBAL,&
   &' SECONDS, ',ZTOTUNBAL/ZTOTAL*100._JPRB,' PERCENT'
ELSE
  ZTOTAL=TIMESUM(0)
  ZTOTCPU = TTCPUSUM(0)
  ZTOTVCPU = TVCPUSUM(0)
ENDIF
IF ( MYPROC_STATS == 1) THEN
  WRITE(KULOUT,'(3(A,F10.1)/)')'TOTAL WALLCLOCK TIME ',ZTOTAL,&
   &' CPU TIME',ZTOTCPU,' VECTOR TIME ',ZTOTVCPU
  WRITE(IXMLLUN,'(3(A,F10.1,A,//)/)')'<totalwallclocktime>',ZTOTAL,&
   &'</totalwallclocktime>',&
   &'<cputime>',ZTOTCPU,'</cputime>',&
   & '<vectortime>',ZTOTVCPU,'</vectortime>'
ENDIF

!   Trace stats

IF (LTRACE_STATS) THEN
  WRITE(KULOUT,'(A)') '=== TRACE OF CALLS TO GSTATS'
  IF (NCALLS_TOTAL > NTRACE_STATS) THEN
    WRITE(KULOUT,'(A,2I10)') ' ONLY PART OF TRACE STORED AS BUFFER TO SMALL ',&
     & NCALLS_TOTAL,NTRACE_STATS
  ENDIF
  WRITE(KULOUT,'(A)') '==='
  CLACTION(0)='ON'
  CLACTION(1)='OFF'
  CLACTION(2)='SUSPEND'
  CLACTION(3)='RESUME'
  DO JCALL=1,MIN(NCALLS_TOTAL,NTRACE_STATS)
    ICALLER = MOD(NCALL_TRACE(JCALL),(JPMAXSTAT+1))
    IACTION   = NCALL_TRACE(JCALL)/(JPMAXSTAT+1)
    IF (IACTION == 0) THEN
      ZTIMELCALL(ICALLER) = TIME_TRACE(JCALL)
      ZTHISTIME(ICALLER) = 0.0_JPRB
    ELSEIF (IACTION == 2) THEN
      ZTHISTIME(ICALLER) = TIME_TRACE(JCALL)-ZTIMELCALL(ICALLER)
    ELSEIF (IACTION == 3) THEN
      ZTIMELCALL(ICALLER) = TIME_TRACE(JCALL)
    ENDIF
    IF (IACTION == 1) THEN
      WRITE(KULOUT,'(1X,F10.3,1X,A,1X,A,1X,F10.3)') &
       &TIME_TRACE(JCALL),CCDESC(ICALLER),CLACTION(IACTION),&
       &ZTHISTIME(ICALLER)+(TIME_TRACE(JCALL)-ZTIMELCALL(ICALLER))
    ELSE
      WRITE(KULOUT,'(1X,F10.3,1X,A,1X,A)') TIME_TRACE(JCALL),CCDESC(ICALLER),&
       & CLACTION(IACTION)
    ENDIF
  ENDDO
ENDIF

IF(LSTATS .AND. MYPROC_STATS == 1) THEN
  PAVEAVE(0:KLEN) = ZAVEAVE(0:KLEN)
ELSE
  PAVEAVE(0:KLEN) = 0.0_JPRB
ENDIF

WRITE(KULOUT,'(/A)')'===-=== END   OF TIMING STATISTICS ===-==='


IF(LSTATS_MEM)THEN
  WRITE(IXMLLUN,'(A)')'<memory>'
  WRITE(KULOUT,*) ''
  WRITE(KULOUT,*) 'STATS FOR MEMORY'
  WRITE(KULOUT,*)  &
 &' NUM ROUTINE               CALLS  CALL    MAXINCR   TOTINCR   MININCR'
  WRITE(KULOUT,*)  &
 &'                                   NO      (KB)      (KB)      (KB)'
! DO JNUM=1001,JPMAXSTAT
  DO JNUM=0,JPMAXSTAT
    IF(NCALLS(JNUM) > 1) THEN
      ICALLS = NCALLS(JNUM)/2
      IMEM=NTMEM(JNUM,1)
      INUM=NTMEM(JNUM,3)/2
      JMEM=NTMEM(JNUM,4)
      WRITE(KULOUT,'(I4,1X,A20,1X,I5,1X,I6,3(1X,I9))')&
       &JNUM,CCDESC(JNUM),ICALLS,INUM,IMEM,JMEM,NTMEM(JNUM,5)

      WRITE(IXMLLUN,'(A,I4,A,//,A,A20,A,//,A,I5,A,//,A,I6,A,//,3(A,I9,A,//))')&
       &'<memitem id="',JNUM,'"/>',&
       &'<description>',CCDESC(JNUM),'</description>',&
       &'<calls>',ICALLS,'</calls>',&
       &'<callnum>',INUM,'</callnum>','<maxincr unit="kb">',IMEM,'</maxincr>',&
       &'<totincr unit="kb">',JMEM,'</totincr>',&
       &'<minincr unit="kb">',NTMEM(JNUM,5),'</minincr>'
    ENDIF
  ENDDO

  WRITE(KULOUT,*) ''
  WRITE(KULOUT,'(/A)')'===-=== END   OF MEMORY STATISTICS ===-==='
  WRITE(KULOUT,*) ''
  WRITE(IXMLLUN,'(A)')'</memory>'
ENDIF
WRITE(IXMLLUN,'(A)')'</gstats>'
CLOSE(IXMLLUN)

RETURN
END SUBROUTINE GSTATS_PRINT
