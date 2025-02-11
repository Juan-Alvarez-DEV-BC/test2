     HOption(*NoDebugIo)
      // PMO15301
     Hdftactgrp(*no) actgrp(*caller)
     H**************************************************************************
     H* PROGRAMA    : CIBRREGOPT
     H* AUTOR       : PEDRO RODRIGUEZ
     H* PROVEEDOR   : GBS INTERNATIONAL LTDA.
     H* FECHA       : AGOSTO 4 DE 2004.
     H* DESCRIPCION : Efectúa la validación y complementa información desde el
     H*               archivo descifrado CIBFFTRNOP hacia el archivo de Ordenes
     H*               de Pago en Tránsito CIBFF102OP.
     H**************************************************************************
     H* Descripción de los posibles estados en el archivo CIBFF102OP:
     H* '00' => OP interpretada y pendiente de validar de acuerdo al convenio.
     H* '01' => OP validada y se encuentra lista para ser aprobada.
     H* '02' => OP pendiente de investigación (no clientes BANCOLOMBIA).
     H* '03' => OP corregida y/o investigada lista para ingresar al maestro.
     H* '11' => Código BIC del banco tenedor de los fondos inválido.
     H* '12' => OP se encuentra en el maestro de ordenes de pago.
     H* '13' => OP con moneda inválida.
     H* '14' => OP con valor inválido.
     H* '15' => OP con sucursal inválida (clientes BANCOLOMBIA).
     H* '16' => OP con nit del beneficiario inválido (clientes BANCOLOMBIA).
     H* '17' => OP sin teléfono de beneficiario.
     H* '20' => OP rechazada por nit del beneficiario en listas de control.
     H* '21' => OP devuelta al exterior.
     H* '22' => OP suspendida investigación por intentos fallidos de localizac.
     H* '23' => OP con tasa inválida.
     H* '30' => OP con CLIENTE AOB NO INSCRITO ABONO O  PAGO AGIL
     H* '31' => OP ClIENTE AOB SIN NRO TELEFONO
     H* '32' => OP CLIENTE AOB CON CODIGO DE BANCO NO VALIDO
     H* '33' => OP CLIENTE AOB NO SUPERA COINCIDENCIA DE NOMBRES
     H* '34' => OP CLIENTE AOB SIN NRO CUENTA
     H* '35' => OP CLIENTE AOB BLOQUEADO POR SISTEMA
     H* '36' => OP CLIENTE AOB EXCEDE CUPO DIVISAS AUTORIZADO
     H* '37' => OP CON CIUDAD NO VÁLIDA
     H**************************************************************************
8545  * PROGRAMA   :  CIBRREGOPT                                           *
8545  * NOMBRE     :  Mphasis.Sachin Bhavsar                               *
8545  * FECHA      :  December 2010.                                       *
8545  * REQ. NUM.  :  PMO08545. CU 7.2.2                                   *
8545  * DESCRIPCION:  Information Synchronization with Legacy Applications *
8545  *               To populate Phone No. from CADDRL0 instead of CNAMEL0*
      **********************************************************************
     H* MODIFICACION: Se incluye validación para que se tome el campo 50F
     H*               INC 1705325
     H* AUTOR       : ANDREA AVILA G - HP
     H* FECHA       : ABRIL 7 DE 2010.
     H* ID CAMBIO   : HP
     H**************************************************************************
     H* MODIFICACION: Se incluyen validaciones a la OP para abonos AOB
     H* AUTOR       : Sonia Sarrazola- HP
     H* FECHA       : Enero  2012.
     H* ID CAMBIO   : PMO11659
     H**************************************************************************
     H* MODIFICACION: Se incluyen monitoreo de datos para evitar cancelaciones
     H* AUTOR       : HP - Álvaro Marín L.
     H* FECHA       : Diciembre 2012
     H* ID CAMBIO   : PMO15301 Mejoras CIB
     H**************************************************************************
     H* MODIFICACION: Se incluyen controles a OP de AOB para devoluciones
     H*               campo ESTADO de CIBFF102OP con valores 42,43,44
     H* AUTOR       : HP - Gustavo Orozco R.
     H* FECHA       : Julio de 2013
     H* ID CAMBIO   : PMO16740 Devoluciones AOB
     H**************************************************************************
     H* MODIFICACION: Se incluye validacion de Pais
     H* AUTOR       : Juan Camilo Montoya Duque.
     H* FECHA       : Marzo de 2016
     H* ID CAMBIO   : PMO17759  SARLAFT
     H**************************************************************************
     H**************************************************************************
     H* MODIFICACION: Se incluye validacion de ORDENANTE
     H*               Se realiza un llamado al CIBOVALORD
     H* AUTOR       : Alex R. Murillo - TCS.
     H* FECHA       : JUNIO de 2016
     H* ID CAMBIO   : IDE08341 Validación Ordenante Remesas Familiares
     H*               TAG. ARM201606-1
     H**************************************************************************
     H*______________ Archivos Fisicos y Logicos _______________________________
     F
     FCIBFFBANOPIF   E           K DISK
     F
     FCIBLFTRNOPUF   E           K DISK
     F                                     INFDS(DsInfdsTrnOp)
     FCIBFFBANCOIF   E           K DISK
     FCIBL25ORPNIF   E           K DISK    PREFIX (O)
     FSTALFDSCCAIF   E           K DISK
     FCIBFFOFCCIIF   E           K DISK
     F*DBALLC    IF   E           K DISK
     FCXREFLC   IF   E           K DISK
     FCNAMEL0   IF   E           K DISK
     FCNAMELZ   IF   E           K DISK    PREFIX (Z)
     F                                     RENAME(@CNAME:@CNAMELZ)
     FCNAMEL3   IF   E           K DISK    PREFIX (X)
     F                                     RENAME(@CNAME:@CNAMEL3)
     FCIBFF102CTIF   E           K DISK
     FCIBLF102OPUF A E           K DISK
     F                                     INFDS(DsInfds102Op)
     FCIBLA102OPIF   E           K DISK    PREFIX (L)
     F                                     RENAME(REG102OP:REG102LA)
8545 FCADDRL0   IF   E           K DISK
     F
     FCIBFFCDGOSIF   E           K Disk
     FSTFFFBANCOIF   E           K DISK    PREFIX (B)
     FCIBFFDCAUTIF   E           K DISK
     FCIBFFUSAOBIF   E           K DISK    PREFIX (A)
     FCIBFFTOPESIF   E           K DISK    PREFIX (T)
     F* PMO16740 Devoluciones AOB
     FCIBFFUSRNCIF   E           K DISK
     FCIBFFBAAOBIF A E           K DISK
     FCIBL1USAOBIF   E           K DISK    RENAME(REGUSAOB:REGL1AOB) PREFIX (L1)
     FCINDVL0   IF   E           K DISK
     FXECITYL0  IF   E           K DISK
     Fcibffmonpro  a e             disk
     FCIBL1OPREMIF   E           K DISK
      *- Prototipos Externos -----------------------------------------
     DCIBOBANGOS       Pr                  ExtPgm('CIBOBANGOS')
     D p_Lista                        3A
     D p_Codigo                       5A
     D p_Nombre                      30A
     D Fn_QuitaPunto   Pr          2000a
     D  p_Valor                    2000a   Const

     D* Prototipo CIBROFICTA que reemplazara a CIBROFCCTA
     DCIBROFICTA       Pr                  ExtPgm('CIBROFICTA')
     D p_Numcta                      11A
     D p_Tipocta                      1A
     D p_Ofccta                       4A
     D p_Inderr                       2A

     D*______________ Variables de trabajo _____________________________________

     DW_Caracter       C                   '0123456789ABCDEFGHIJKLMN#OPQRSTUVWX-
     D                                      XYZabcdefghijklmn¦opqrstuvwxyz'
     DW_otros          C                   '°]Ñ$%&/()=¡~*^!{}¢`*><+´.'
     DW_Digit          C                   '0123456789/'
     DW_Digit1         C                   '0123456789'
     D*****
     DW_FECHASIS       S               D   DATFMT(*ISO)
     DW_FECHAVLR       S              8
     DW_CARACT         S             20
     D*****
     DVSPOS            S              3  0
     DW_POS1           S              3  0
     D***
     DwNombreAOB       S             60A
     DLK_NIT           S             15A
     DLK_TIPOID        S              1A
     DLK_NAME          S             60A
     DLK_FUNC          S              1A
     DLK_STATUS        S              1A
     DLK_MESSAGE       S            300A
     DwFECNBEN         S              8S 0
     DwCITY            S              8S 0
     DwCIUBEN          S             50A
     DW_TIPOID         S              1A
     D***
     DLNG              S              3P 0
     DSPC              S              2S 0
     DW_BANDERA        S              1A
     DW_CAMPO1         S              1A
     DW_POSCAMPO       S              3P 0
     DW_VALIDOS        S            180A
     DNUMERO           S              2P 0
     DWTPONIT          S              1  0
     DW_POS            S              3  0
     DWEXISTE          S              1A
     D
     DWTOPE            S             15S 2
     DW_NIT            S             15S 0
     DWNIT15           S             15  0
     DW_NITBENEF       S             15
     DW_NITBENEF20     S             20
     DW_TIPONIT        S              1
     DW_CUENT          S             20
     DW_CUENTAOB       S             17
     DW_OFICINA        S              3S 0
     D*
     DW_OFCORIG        S              3S 0
     DW_OFCCIO         S              3S 0
     DW_VALORE         S             15S 0
     DW_CAMPOCAR       S             50A
     DW_TIPOCTA        S              1A
     DW_CHEQUEO        S            180A
     D*
     DW_LIMINF         S             12  5
     DW_LIMSUP         S             12  5
     DW_RESTINF        S              7  5
     DW_RESTSUP        S              7  5
     DW_AA             S              4
     DW_M              S              2
     DW_D              S              2
     DW_VALCAM         S             10P 5
     DW_AAAA           S              4S 0
     DW_MM             S              2S 0
     DW_DD             S              2S 0
     DW_CODCAM         S              2S 0
     DW_CAMP           S              2S 0
     DW_CONT           S              2S 0
     DW_LONG           S              3S 0
     DW_REG            S              1
     DW_REG1           S             50
8545 DW_CADTLM         S                   LIKE(CADTLM)
8545 DW_CAPHO1         S                   LIKE(CAPHO1)
8545 DW_PHFL           S              1A   INZ('0')
     DW_CARDEL         C                   '|'
     DW_BANCO          S             15  0
     D**
     DWNITBENEFUS      S             15S 0
     DWNITALFA         S             15
     D***ARM201606-1
     DWNomOrdn         S             60A   INZ(*Blanks)
     D***-----------
     D*
     D* Parámetros para llamar rutina de conversión cuentas Conavi
     D*
     DW_CTACHAR        S             15A
     DW_CTANUME        S             11S 0
     D*
     DVLRDLR           S             16S 2
     DFACTOR           S             16S 0
     DWDIA             S              2A
     DWMES             S              2A
     DWANO             S              4A
     DWCODCAM          S              2A
     DWCODSWF          S              3A
     DWNOMBRE          S             25A
     DWPAIS            S             20A
     DWTRM             S             10P 5
     DWFACTOR          S             10P 5
     DWVLRCAM          S             10P 5
     DWRESP            S              1A
     Dw_Lista          S              3A
     Dw_Codigo         S              5A
     Dw_Nombre1        S             30A
     Dw_IndDup         S              1A   Inz(*Blanks)
     DW_REFERUN        S             70A
     DW_VLRODP         S             20A
     D w_ParTasa1      s             12S 5 INZ(*Zeros)
     D w_ParTasaA1     s             12A   INZ(*Blanks)
     D w_ParTasaA      s             10A   INZ(*Blanks)
     D w_ParValor      s             20A   INZ(*Blanks)
     D w_Len           s             15S 0 INZ(*Zeros)
     D w_Refer         s             30A   INZ(*Blanks)
     D
     D* Marzo de 2016  PMO17759 SARLAFT
     D*Variables para el CIBRVALPAI
     Dw_Refppal        S             70A   Inz(*Blanks)
     Dw_nompais        S             60A   Inz(*Blanks)
     Dw_Func           S              1A   Inz(*Blanks)
     Dw_EstVal         S              1A   Inz(*Blanks)
     Dw_msg            S            300A   Inz(*Blanks)
     Dw_codrec         S              1A   Inz(*Blanks)
     D*
     Dw_Estado         S              2A   Inz(*Blanks)
     D*
     D*Variables para el CIBRDEVORD
     Dw_codmod         S              3A   Inz(*Blanks)
     Dw_Refmod         S             25A   Inz(*Blanks)
     Dw_Refarmod       S             25A   Inz(*Blanks)
     Dw_nitmod         S             15A   Inz(*Blanks)
     Dw_ctamod         S             20A   Inz(*Blanks)
     Dw_nommod         S             30A   Inz(*Blanks)
     Dw_tel1mod        S             15A   Inz(*Blanks)
     Dw_tel2mod        S             15A   Inz(*Blanks)
     Dw_fecingmod      S              8s 0 Inz(*Zeros)
     Dw_valmod         S             16s 2 Inz(*Zeros)
     Dw_dirmod         S             35A   Inz(*Blanks)
     Dw_indmod         S              1A   Inz(*Blanks)
     D*
     D EcNit           S             13S 0
     D EcTipo          S              1S 0
     D EcValor         S             13S 2
     D EcResp          S              1S 0
     D EcTarifa        S              1S 0 Inz(1)
     D EcOperacion     S              1A   Inz('C')

     D*Variables para el CIBROFICTA
     DW_CUENTA         S             11A
     DW_Inderr         S              2A
     DW_Sucursal       S              4A

      *
     D
     DFECSIS           DS                  INZ
     DMESSIS                   1      2  0
     DDIASIS                   3      4  0
     DYEASIS                   5      8  0
     D
      // PMO15301
      // INFDS para CIBFFTRNOP
     d DsInfdsTrnOp    ds                  Qualified
     d/copy *libl/cibsrc,cibrcpffds
      // INFDS para CIBFF102OP
     d DsInfds102Op    ds                  Qualified
     d/copy *libl/cibsrc,cibrcpffds
      // PSDS  Program Status Data Structure
     d/copy *libl/cibsrc,cibrcppsds


      // Definición del prototipo para monitoreo de procesos
     dMonitoreo        pr
      // Definición del prototipo para monitoreo de procesos
     dGeneraLogMon     pr
      // Se redefinen los registros para poder moverlos a la trama del log
     d DsRegTrnOp    e ds                  extname(CibffTrnOp)
     d DsReg102Op    e ds                  extname(Cibff102Op)

     d W_TipoTrama     s              5a   inz
     d W_TramaTrnOp    c                   CONST('TrnOp')
     d W_Trama102Op    c                   CONST('102Op')
     C*______________ Lista de claves __________________________________________
     C* Clave para accesar el CIBFFORPND
     C     KORPND        KLIST
     C                   KFLD                    ONROCRED
     C
     C* Clave para accesar el CIBFFBANCO
     C     KBANCO        KLIST
     C                   KFLD                    CODBCO
     C                   KFLD                    SUBCODBCO
     C
     C* Clave para accesar el CIBFFOFCCI
     C     KOFI          KLIST
     C                   KFLD                    CODOFC
     C
     C** Clave para accesar el DBALLC
     C*     KDBAL         KLIST
     C*                   KFLD                    DMAPCD
     C*                   KFLD                    DMACCT
     C
     C* Clave para accesar el CXREFLC
     C     KXREF         KLIST
     C                   KFLD                    CXCDAP
     C                   KFLD                    CXNOAC
     C                   KFLD                    CXNOPL
     C
     C* Clave para accesar el CNAMEL0
     C     KNAME         KLIST
     C                   KFLD                    CNNAMK
     C
     C* Clave para accesar el CNAMELZ
     C     KNAMELZ       KLIST
     C                   KFLD                    ZCNNOSS
     C                   KFLD                    ZCNCDTI
     C
     C* Clave para accesar el CIBLF102OP
     C     K102OP        KLIST
     C                   KFLD                    ESTADO
     C                   KFLD                    REMITENTE
     C                   KFLD                    REFPALOP
     C
     C* Clave para accesar el CIBLFTRNOP
     C     KTRNOP        KLIST
     C                   KFLD                    T_ESTDESC
     C                   KFLD                    T_CODCONV
     C                   KFLD                    T_REFODEP
     C
     C* Clave para accesar el CIBFF102CT
     C     K102CT        KLIST
     C                   KFLD                    REMITENTE
     C                   KFLD                    ACAMPO20
     C
     C* Clave para accesar el CIBLA102OP
     C     K102LA        KLIST
     C                   KFLD                    LCODCONV
     C                   KFLD                    LREFODEPA
8545  *
8545 C* Clave para accesar el CADDRL0
8545 C     KADDL0        KLIST
8545 C                   KFLD                    CNNAMK
     C* Clave para accesar el CIBFFUSAOB
     C     CLAVEAOB      KLIST
     C                   KFLD                    ANROCUENTA
     C                   KFLD                    ACODBANCO
8545  *
     C* Clave para accesar el CIBFFCDGOS
     C     CLAVECDGOS    KLIST
     C                   KFLD                    LISTA
     C                   KFLD                    CODIGO
8545  *
     C* Clave para accesar el CIBFFTOPES
     C     TCLAVETOPE    KLIST
     C                   KFLD                    TNIT
     C                   KFLD                    TTIPOOPR
8545  *
     C* Clave para accesar el CIBFFBAAOB
     C     KEYAOB        KLIST
     C                   KFLD                    BAREFOP
     C                   KFLD                    BANITBEN
     C* Clave para CIBFFOPREM
     C     KOPREM        KLIST
     C                   KFLD                    RREFPPAL
     C*______________ Programa Principal _______________________________________
     C
     C                   MOVEL     *DATE         W_FECHASIS
     C                   MOVEL     W_FECHASIS    W_FECH            8 0
     C                   MOVEL     W_FECH        W_FEC             8
     C
     C                   MOVEL     *DATE         FECSIS
     C                   Z-ADD     YEASIS        YEAACT            2 0
     C                   Z-ADD     MESSIS        MESACT            2 0
     C                   Z-ADD     DIASIS        DIAACT            2 0
     C
     C                   MOVE      '00'          T_ESTDESC
     C                   EVAL      T_CODCONV =  ' '
     C                   EVAL      T_REFODEP =  ' '
      // PMO15301
      /free
        clear W_TipoTrama;
        w_IndDup = '0';
        w_Lista = 'VDU';
        w_Codigo = 'DUPOP';
        w_Nombre1 = *Blanks;
        CIBOBANGOS(W_Lista:w_Codigo:w_Nombre1);
        If %Trim(w_Nombre1) = '1';
          w_IndDup = '1';
        EndIf;
      /end-free

     C     KTRNOP        SETLL     CIBLFTRNOP                         10
     C                   READ      CIBLFTRNOP                             10
     C
     C                   DOW       *IN10 =*OFF AND
     C                             T_ESTDESC='00'
     C
     C     T_CODCONV     CHAIN     CIBFFBANOP                         25
     C                   IF        NOT *IN25 AND B_TIPFORM='MT103'
     C                   EXSR      EXTR_CAMP
      // PMO15301
      /free
         if  W_TipoTrama <> *blanks;
             read ciblftrnop;
             if %eof;
                *in10 = *on;
             endif;
             iter;
         endif;
      /end-free
     C                   ENDIF
     C
     C                   EXSR      ADDREGIS
      // PMO15301
      /free
         if  W_TipoTrama <> *blanks;
             read ciblftrnop;
             if %eof;
                *in10 = *on;
             endif;
             iter;
         endif;
      /end-free
     C
     C                   IF        B_TASAFIJA='S'
     C                   EXSR      VALIDA_TASA
      // PMO15301
      /free
         if  W_TipoTrama <> *blanks;
             read ciblftrnop;
             if %eof;
                *in10 = *on;
             endif;
             iter;
         endif;
      /end-free
     C                   ENDIF
     C
     C* Se valida que la referencia de la OP no exísta en el CIBFF102OP
     C* para el mismo convenio
     C                   MOVEL     T_REFODEP     LREFODEPA
     C                   MOVE      T_CODCONV     LCODCONV
     C     K102LA        CHAIN     REG102LA                           16
     C                   IF        *IN16=*OFF
     C                   MOVE      '12'          T_ESTDESC
     C                   CLEAR                   REG102OP
     C                   ELSE
     C* Se valida que la referencia de la OP no exísta en el CIBFFORPND
     C                   MOVEL     T_REFODEP     ONROCRED
     C     KORPND        CHAIN     REGCIBPDN                          12
     C                   IF        *IN12=*OFF
     C                   MOVE      '12'          T_ESTDESC
     C                   ELSE
     C                   IF        W_INDDUP='0'
     C                   WRITE     REG102OP
     C
     C***-ARM201606-1---*
     C                   CALL      'CIBOVALORD'
     C                   PARM                    T_REFODEP
     C                   PARM                    T_IDORDEN
     C                   PARM                    WNomOrdn
     C                   PARM                    T_CODCONV
     C***-Fin-ARM201606-1
     C
     C                   CLEAR                   REG102OP
     C                   MOVE      '01'          T_ESTDESC
     C                   ELSE
     C
      /Free
            w_ParTasaA1 = Fn_QuitaPunto(%Char(T_TASACAMB));
            w_Len = %Len(%Trim(w_ParTasaA1));
            If w_Len > 10;
              w_ParTasaA = %Subst(w_PartasaA1:3:10);
            Else;
              w_ParTasaA = w_PartasaA1;
            EndIf;
            w_ParValor = Fn_QuitaPunto(%Char(T_VLRODEP));
            W_Refer =  T_REFODEP;
      /End-Free
     C                   CALL      'CIBRREFERE'
     C                   PARM                    T_CODCONV
     C                   PARM                    w_Refer
     C                   PARM                    w_ParTasaA
     C                   PARM                    w_ParValor
     C                   PARM                    W_ReferUn
     C     W_REFERUN     CHAIN     REGOPREM                           17
     C                   IF        *IN17=*OFF
     C                   MOVE      '12'          T_ESTDESC
     C                   ELSE
     C                   WRITE     REG102OP
     C
     C***-ARM201606-1---*
     C                   CALL      'CIBOVALORD'
     C                   PARM                    T_REFODEP
     C                   PARM                    T_IDORDEN
     C                   PARM                    WNomOrdn
     C                   PARM                    T_CODCONV
     C***-Fin-ARM201606-1
     C
     C                   CLEAR                   REG102OP
     C                   MOVE      '01'          T_ESTDESC
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C
     C
      /free
        monitor;
      /end-free
     C                   UPDATE    REGTRNOP
      // PMO15301
      /free
        on-error;
          W_TipoTrama = W_TramaTrnOp;
          Monitoreo();
        endmon;
      /end-free
     C                   READ      CIBLFTRNOP                             10
     C                   ENDDO
     C
     C* Se validan aquellos campos de las órdenes de pago
     C* que ingresaron al CIBFF102OP
     C
     C                   MOVE      '00'          ESTADO
     C                   EVAL      REMITENTE = ' '
     C                   EVAL      REFODEPA  = ' '
     c                   monitor
     C     K102OP        SETLL     CIBLF102OP
     C                   READ      CIBLF102OP                             13
     C                   DOW       *IN13=*OFF  AND
     C                             (ESTADO = '00')
     C                   EXSR      VALIDA_CAMPOS
      // PMO15301
      /free
         if  W_TipoTrama <> *blanks;
             read ciblf102op;
             if %eof;
                *in13 = *on;
             endif;
             iter;
         endif;
      /end-free
     C                   UPDATE    REG102OP
     C                   READ      CIBLF102OP                             13
     C                   ENDDO
     c                   on-error
     c                   endmon
     C
     C* Se validan las órdenes de pago  en ESTADO = 30
     C* que ingresaron al CIBFF102OP
     C
     C                   MOVE      '30'          ESTADOALLUS       2
     C                   EVAL      REMITENTE = ' '
     C                   EVAL      REFODEPA  = ' '
     C     ESTADOALLUS   SETLL     CIBLF102OP
     C     ESTADOALLUS   READE     CIBLF102OP                             13
     C                   DOW       *IN13=*OFF  AND
     C                             (ESTADO = '30')
     C                   MOVE      '00'          ESTADO
     C                   EXSR      HALLA_SUC
      // PMO15301
      /free
         if  W_TipoTrama <> *blanks;
             read ciblf102op;
             if %eof;
                *in13 = *on;
             endif;
             iter;
         endif;
      /end-free
     C                   EXSR      VALIDA_CAMPOS
      // PMO15301
      /free
         if  W_TipoTrama <> *blanks;
             read ciblf102op;
             if %eof;
                *in13 = *on;
             endif;
             iter;
         endif;
      /end-free
     C                   UPDATE    REG102OP
     C     ESTADOALLUS   READE     CIBLF102OP                             13
     C                   ENDDO
     C
     C* Se validan aquellos campos de las órdenes de pago
     C* que ingresaron al CIBFF102OP para AOB
     C
     C                   MOVE      '45'          ESTADOAOB         2
     C                   EVAL      REMITENTE = ' '
     C                   EVAL      REFODEPA  = ' '
     C     ESTADOAOB     SETLL     CIBLF102OP
     C     ESTADOAOB     READE     CIBLF102OP                             13
     C                   DOW       *IN13=*OFF  AND
     C                             (ESTADO = '45')
     C                   MOVE      '00'          ESTADO
     C                   EXSR      VALIDA_CAMPOS
      // PMO15301
      /free
         if  W_TipoTrama <> *blanks;
             read ciblf102op;
             if %eof;
                *in13 = *on;
             endif;
             iter;
         endif;
      /end-free
     C                   UPDATE    REG102OP
     C     ESTADOAOB     READE     CIBLF102OP                             13
     C                   ENDDO
     C
     C                   SETON                                        LR
     C* Fin Rutina.
     C*_________________________________________________________________________
     C* Esta rutina se encarga de extraer todos los registros del archivo rela-
     C* cionados con dicho codigo de convenio, y se almacena en el archivo
     C* CIBFF102OP.
     C     ADDREGIS      BEGSR
     C
     C                   CLEAR                   REG102OP
      // PMO15301
      /free
         monitor;
      /end-free
     C                   EVAL      CODCONV=T_CODCONV
     C
     C                   IF        T_BANCREMI<>*BLANKS
     C     W_Caracter    CHECK     T_BANCREMI    VSPOS
     C                   IF        VSPOS<>0
     C                   MOVEL     T_BANCREMI    W_CHEQUEO
     C                   EXSR      ELIM_CAR
      // PMO15301
      /free
         if  W_TipoTrama <> *blanks;
             leavesr;
         endif;
      /end-free
     C                   MOVEL     W_VALIDOS     REMITENTE
     C                   EVAL      REMITENTE=%TRIM(REMITENTE)
     C                   ELSE
     C                   EVAL      REMITENTE=%TRIM(T_BANCREMI)
     C                   ENDIF
     C                   ENDIF
     C
     C                   IF        T_REFODEP<>*BLANKS
     C     W_Caracter    CHECK     T_REFODEP     VSPOS
     C                   IF        VSPOS<>0
     C                   EVAL      REFODEPA=%SUBST(T_REFODEP:1:VSPOS-1)
     C                   ELSE
     C                   EVAL      REFODEPA=T_REFODEP
     C                   ENDIF
     C                   ENDIF
     C
     C                   IF        T_REFARCHI<>*BLANKS
     C                   EVAL      REFPALOP=T_REFARCHI
     C                   ENDIF
     C
     C                   IF        T_BANCORDE<>*BLANKS
     C     W_Caracter    CHECK     T_BANCORDE    VSPOS
     C                   IF        VSPOS<>0
     C                   MOVEL     T_BANCORDE    W_CHEQUEO
     C                   EXSR      ELIM_CAR
      // PMO15301
      /free
         if  W_TipoTrama <> *blanks;
             leavesr;
         endif;
      /end-free
     C                   MOVEL     W_VALIDOS     BANCORDEA
     C                   EVAL      BANCORDEA=%TRIM(BANCORDEA)
     C                   ELSE
     C                   EVAL      BANCORDEA=%TRIM(T_BANCORDE)
     C                   ENDIF
     C                   ENDIF
     C
     C                   IF        T_BANCCORR<>*BLANKS
     C     W_Caracter    CHECK     T_BANCCORR    VSPOS
     C                   IF        VSPOS<>0
     C                   MOVEL     T_BANCCORR    W_CHEQUEO
     C                   EXSR      ELIM_CAR
      // PMO15301
      /free
         if  W_TipoTrama <> *blanks;
             leavesr;
         endif;
      /end-free
     C                   MOVEL     W_VALIDOS     BANCCORR
     C                   EVAL      BANCCORR=%TRIM(BANCCORR)
     C                   ELSE
     C                   EVAL      BANCCORR=%TRIM(T_BANCCORR)
     C                   ENDIF
     C                   ENDIF
     C
     C* Se asigna la fecha del sistema a fecha ingreso
     C                   MOVE      *DATE         W_FECHASIS
     C                   MOVE      W_FECHASIS    FECHAING
     C
     C* Se valida el formato de fecha de la Orden de Pago
     C                   SELECT
     C     B_FMTFECHA    WHENEQ    'AAAAMMDD'
     C                   MOVEL     T_FECHAODP    FECHAVLR
     C     B_FMTFECHA    WHENEQ    'AAMMDD'
     C     '20'          CAT       T_FECHAODP    W_FECHAVLR
     C                   MOVEL     W_FECHAVLR    FECHAVLR
     C     B_FMTFECHA    WHENEQ    'MM/DD/AAAA'
     C                   EVAL      W_FECHAVLR=%SUBST(T_FECHAODP:7:4)+
     C                             %SUBST(T_FECHAODP:1:2)+%SUBST(T_FECHAODP:4:2)
     C                   MOVEL     W_FECHAVLR    FECHAVLR
     C                   ENDSL
     C*                  IF        T_FECHAODP<>*BLANKS
     C*                  EVAL      T_FECHAODP=%TRIM(T_FECHAODP)
     C*                  ENDIF
     C
     C* Se valida la moneda de la Orden de Pago
     C                   IF        T_MONODEP<>*BLANKS
     C                   EVAL      MONODEPA=T_MONODEP
     C     MONODEPA      CHAIN     STALFDSCCA                         15
     C                   IF        NOT *IN15
     C                   EVAL      CODMONODP=DCCODCAM
     C                   ELSE
     C                   EVAL      ESTADO = '13'
     C                   ENDIF
     C                   ELSE
     C                   EVAL      ESTADO = '13'
     C                   ENDIF
     C
     C* Se valida el banco tenedor de los fondos
     C                   EVAL      CODBCO = B_CODBCO
     C                   EVAL      SUBCODBCO = B_SBCODBCO
     C     KBANCO        CHAIN     REGCIBBCO                          14
     C                   IF        NOT *IN14
     C                   EVAL      BANCCORR = CODSWIFT
     C                   ELSE
     C                   EVAL      ESTADO = '11'
     C                   ENDIF
     C
     C* Se valida el valor de la Orden de Pago
     C                   IF        T_VLRODEP > 0
     C                   EVAL      VLRODEPA=T_VLRODEP
     C                   ELSE
     C                   EVAL      ESTADO = '14'
     C                   ENDIF
     C
     C                   IF        T_VLRCOMIS<>*ZEROS
     C                   EVAL      VLRCOMIS=T_VLRCOMIS
     C                   ENDIF
     C
     C                   IF        T_TASACAMB<>*ZEROS
     C                   EVAL      TASACAMB=T_TASACAMB
     C                   ENDIF
     C
     C                   IF        T_MSGBENEF<>*BLANKS
     C     'Ñ':'¦'       XLATE     T_MSGBENEF    T_MSGBENEF
     C     W_Caracter    CHECK     T_MSGBENEF    VSPOS
     C                   IF        VSPOS<>0
     C                   MOVEL     T_MSGBENEF    W_CHEQUEO
     C                   EXSR      ELIM_CAR
      // PMO15301
      /free
         if  W_TipoTrama <> *blanks;
             leavesr;
         endif;
      /end-free
     C                   MOVEL     W_VALIDOS     T_MSGBENEF
     C                   EVAL      T_MSGBENEF=%TRIM(T_MSGBENEF)
     C     W_Caracter    CHECK     T_MSGBENEF    VSPOS
     C                   IF        VSPOS<>0
     C                   EVAL      MSGBENEF=%SUBST(T_MSGBENEF:1:VSPOS-1)
     C                   ELSE
     C                   EVAL      MSGBENEF=T_MSGBENEF
     C                   ENDIF
     C                   ELSE
     C     W_Caracter    CHECK     T_MSGBENEF    VSPOS
     C                   IF        VSPOS<>0
     C                   EVAL      MSGBENEF=%SUBST(T_MSGBENEF:1:VSPOS-1)
     C                   ELSE
     C                   EVAL      MSGBENEF=T_MSGBENEF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C
     C                   IF        T_NOMBENEF<>*BLANKS
     C                   EVAL      NOMBENEF=T_NOMBENEF
     C                   ENDIF
     C                   IF        T_PRIAPBEN<>*BLANKS OR T_SEGAPBEN<>*BLANKS
     C     NOMBENEF      CAT       T_PRIAPBEN:1  NOMBENEF
     C     NOMBENEF      CAT       T_SEGAPBEN:1  NOMBENEF
     C                   ENDIF
     C
     C* Se valida que TELEFONO1 no contenga dígitos inválidos
     C                   IF        T_TEL1BEN<>*BLANKS
     C                   CLEAR                   W_CHEQUEO
     C     W_Caracter    CHECK     T_TEL1BEN     VSPOS
     C                   IF        VSPOS<>0
     C                   MOVEL     T_TEL1BEN     W_CHEQUEO
     C                   EXSR      ELIM_CAR
      // PMO15301
      /free
         if  W_TipoTrama <> *blanks;
             leavesr;
         endif;
      /end-free
     C                   MOVEL     W_VALIDOS     T_TEL1BEN
     C                   ELSE
     C     W_Digit1      CHECK     T_TEL1BEN     VSPOS
     C                   IF        VSPOS=0
     C                   MOVE      T_TEL1BEN     TELBEN1
     C                   ENDIF
     C                   IF        VSPOS=1
     C                   EVAL      T_TEL1BEN = '000000000      '
     C                   MOVE      T_TEL1BEN     TELBEN1
     C                   ENDIF
     C                   IF        VSPOS>1
     C                   EVAL      W_CARACT=%SUBST(T_TEL1BEN:1:VSPOS-1)
     C                   EVAL      TELBEN1=%FLOAT(%TRIM(W_CARACT))
     C                   MOVEL     TELBEN1       T_TEL1BEN
     C                   ENDIF
     C
     C*                  IF        VSPOS<>0
     C*                  EVAL      W_CARACT=%SUBST(T_TEL1BEN:1:VSPOS-1)
     C*                  EVAL      TELBEN1=%FLOAT(%TRIM(W_CARACT))
     C*                  MOVEL     TELBEN1       T_TEL1BEN
     C*                  ELSE
     C*                  MOVE      T_TEL1BEN     TELBEN1
     C*                  ENDIF
     C                   ENDIF
     C                   ENDIF
     C
     C* Se valida que TELEFONO2 no contenga dígitos inválidos
     C                   IF        T_TEL2BEN<>*BLANKS
     C                   CLEAR                   W_CHEQUEO
     C     W_Caracter    CHECK     T_TEL2BEN     VSPOS
     C                   IF        VSPOS<>0
     C                   MOVEL     T_TEL2BEN     W_CHEQUEO
     C                   EXSR      ELIM_CAR
      // PMO15301
      /free
         if  W_TipoTrama <> *blanks;
             leavesr;
         endif;
      /end-free
     C                   MOVEL     W_VALIDOS     T_TEL2BEN
     C                   ELSE
     C     W_Digit1      CHECK     T_TEL2BEN     VSPOS
     C                   IF        VSPOS<>0
     C                   EVAL      W_CARACT=%SUBST(T_TEL2BEN:1:VSPOS-1)
     C                   EVAL      TELBEN2=%FLOAT(%TRIM(W_CARACT))
     C                   ELSE
     C                   MOVE      T_TEL2BEN     TELBEN2
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C
     C                   IF        T_DIRBENEF<>*BLANKS
     C                   EVAL      DIRBENEF=T_DIRBENEF
     C                   ENDIF
     C
     C*PMO11659
     C                   IF        T_TASACTXT <>*BLANKS
     C                             AND T_TASACTXT <>'000000000000'
     C                   CLEAR                   W_CUENT
     C                   CLEAR                   W_BANCO
     C                   MOVEL     T_TASACTXT    W_CUENT
     C                   CALL      'CIBRRTNNUM'
     C                   PARM                    W_CUENT
     C                   MOVE      W_CUENT       W_BANCO
     C                   Z-ADD     W_BANCO       DISPONIB3
     C                   ENDIF
     C
     C                   IF        T_CTABENEF<>*BLANKS
     C                             AND T_CTABENEF<>'00000000000000000000'
     C                   MOVE      *BLANKS       W_CUENT
     C                   MOVEL     T_CTABENEF    W_CUENT
     C                   CALL      'CIBRRTNNUM'
     C                   PARM                    W_CUENT
     C
     C                   IF        DISPONIB3=  0
     C                   MOVE      W_CUENT       CTABENEF
     C                   ELSE
     C                   EVAL      W_CUENTAOB  = %TRIM(W_CUENT)
     C                   Z-ADD     0             CTABENEF
     C                   EVAL      MSGBENEF =  W_CUENTAOB
     C                   ENDIF
     C
     C                   EXSR      HALLA_SUC
     C
      // PMO15301
      /free
         if  W_TipoTrama <> *blanks;
             leavesr;
         endif;
      /end-free
     C                   ELSE
     C                   MOVE      025           SUCURBEN
     C                   ENDIF
     C
     C                   IF        T_CIUBENEF<>*BLANKS
     C                   EVAL      CIUBENEF=T_CIUBENEF
     C                   ENDIF
     C
     C                   IF        T_IDORDEN<>*BLANKS
     C                   EVAL      IDORDEN=T_IDORDEN
     C                   ENDIF
     C
     C                   IF        T_IDBENEF<>'000000000000000' AND
     C                             T_IDBENEF<>*BLANKS AND SUCURBEN=0
     C                   MOVE      T_IDBENEF     W_NITBENEF20
     C                   CALL      'CIBRRTNNUM'
     C                   PARM                    W_NITBENEF20
     C                   MOVE      W_NITBENEF20  W_NITBENEF
     C                   MOVE      W_NITBENEF    W_NIT
     C                   MOVE      W_NIT         NITBENEF
     C                   MOVEL     T_CLAVEODP    W_TIPONIT
     C                   IF        W_TIPONIT>'0' AND W_TIPONIT<'9'
     C                   MOVE      W_TIPONIT     TIPONITBEN
     C                   EXSR      VALIDA_NIT
      // PMO15301
      /free
         if  W_TipoTrama <> *blanks;
             leavesr;
         endif;
      /end-free
     C                   ENDIF
     C                   ENDIF
     C
     C***ARM201606-1----*
     C                   CLEAR                   WNomOrdn
     C                   EVAL      WNomOrdn=%TRIM(T_NOMORDEN)
     C***---------------*
     C
     C                   IF        T_NOMORDEN<>*BLANKS
     C                   EVAL      NOMORDEN=T_NOMORDEN
     C                   ENDIF
     C                   IF        T_PRIAPORD<>*BLANKS OR T_SEGAPORD<>*BLANKS
     C     NOMORDEN      CAT       T_PRIAPORD:1  NOMORDEN
     C     NOMORDEN      CAT       T_SEGAPORD:1  NOMORDEN
     C***ARM201606-1----*
     C     WNomOrdn      CAT       T_PRIAPORD:1  WNomOrdn
     C     WNomOrdn      CAT       T_SEGAPORD:1  WNomOrdn
     C***---------------*
     C                   ENDIF
     C
     C                   IF        T_DIRORDEN<>*BLANKS
     C                   EVAL      DIRORDEN=T_DIRORDEN
     C                   ENDIF
     C
     C                   IF        T_PAIORDEN<>*BLANKS
     C                   EVAL      CEDUORDEN=T_PAIORDEN
     C                   ENDIF
     C
     C*PMO11659
     C                   IF        SUCURBEN <> 0 AND DISPONIB3 = 0
     C                   EXSR      HALLA_OFICINA
      // PMO15301
      /free
         if  W_TipoTrama <> *blanks;
             leavesr;
         endif;
      /end-free
     C                   ENDIF
     C
     C                   IF        T_ESTDESC<>*BLANKS
     C                   IF        ESTADO=*BLANKS
     C                   EVAL      ESTADO=T_ESTDESC
     C                   ENDIF
     C                   ENDIF
     C*
      // PMO15301
      /free
        on-error;
          W_TipoTrama = W_TramaTrnOp;
          Monitoreo();
        endmon;
      /end-free
     C                   ENDSR
     C* Fin Rutina
     C*_________________________________________________________________________
     C* Rutina para hallar la sucursal a la que pertenece la cuenta
     C*
     C     HALLA_SUC     BEGSR
     C
      // PMO15301
      /free
         monitor;
      /end-free
     C* Se condicionan validaciones para que se realicen solo para OP no AOB
     C                   IF        DISPONIB3=  0
     C* Se busca primero para cuenta de Ahorro
     C*
     C                   MOVE      CTABENEF      W_CUENTA
       // w_Lista = 'DES';
       // w_Codigo = 'DESOF';
       // w_Nombre1 = *Blanks;
       // CIBOBANGOS(W_Lista:w_Codigo:w_Nombre1);

        // If %Trim(w_Nombre1) = '1';

          Clear W_TIPOCTA;
          W_TIPOCTA = 'S';
          CIBROFICTA (W_CUENTA:W_TIPOCTA:W_Sucursal:W_Inderr);

          If W_Sucursal = *zeros Or W_Sucursal=*Blanks;

             Clear W_TIPOCTA;
             W_TIPOCTA = 'D';
             CIBROFICTA(W_CUENTA:W_TIPOCTA:W_Sucursal:W_Inderr);

             If W_Sucursal = *zeros Or W_Sucursal=*Blanks;
                SUCURBEN = *Zeros;
             Else;
                SUCURBEN = %Int(W_Sucursal);
                TIPOCTABEN = W_TIPOCTA;
             EndIF;
          Else;
            SUCURBEN = %Int(W_Sucursal);
            TIPOCTABEN = W_TIPOCTA;
          Endif ;

       // Else;
     C*
     C*                   Z-ADD     0             SUCURBEN
     C*                   MOVE      'S'           DMAPCD
     C*                   Z-ADD     CTABENEF      DMACCT
     C*     KDBAL         CHAIN     @DBAL                              30
     C*                   IF        *IN30 = *OFF
     C*                   MOVE      DMBRN         SUCURBEN
     C*                   EVAL      W_TIPOCTA =   'S'
     C*                   EVAL      TIPOCTABEN=W_TIPOCTA
     C*                   ENDIF
     C*                   IF        SUCURBEN = 0
     C** Si no se existe en cta ahorro se busca en corriente
     C*
     C*                   MOVE      'D'           DMAPCD
     C*                   Z-ADD     CTABENEF      DMACCT
     C*     KDBAL         CHAIN     @DBAL                              30
     C*                   IF        *IN30 = *OFF
     C*                   MOVE      DMBRN         SUCURBEN
     C*                   EVAL      W_TIPOCTA = 'D'
     C*                   EVAL      TIPOCTABEN=W_TIPOCTA
     C*                   ENDIF
     C*                   ENDIF
     C*                   ENDIF
     C** METER ENDIF de nuevo Flujo
     C**
     C                   IF        TIPOCTABEN = *BLANKS AND CTABENEF <> 0
     C                   EXSR      CONVER_CTA
      // PMO15301
      /free
         if  W_TipoTrama <> *blanks;
             leavesr;
         endif;
      /end-free
     C                   ENDIF
     C*
     C                   IF        W_TIPOCTA<>*BLANKS
     C                   EXSR      HALLA_NIT
      // PMO15301
      /free
         if  W_TipoTrama <> *blanks;
             leavesr;
         endif;
      /end-free
     C                   ENDIF
     C
     C* Validaciones para  OP AOB
     C                   ELSE
     C                   EVAL      SUCURBEN = 25
     C                   EVAL      OFORIGPA = 25
     C                   EVAL      OFCCIOPA = 25
     C* Se valida si cuenta  inscrita a abonos
     C                   IF        MSGBENEF <> *blanks
     C                   MOVE      *ZEROS        W_CUENTAOB
     C                   EVAL      ANROCUENTA =%SUBST(MSGBENEF:1:17)
     C*                  EVAL      ANROCUENTA = %DEC(W_CUENTAOB:17:0)
     C                   EVAL      ACODBANCO = DISPONIB3
     C     CLAVEAOB      CHAIN     CIBFFUSAOB                         35
     C                   IF        NOT *IN35 and
     C                             AEstado = 'A'
     C                   EVAL      TIPOCTABEN = ATIPOCUENTA
     C*                  EVAL      WNITBENEFUS = ANITBENEF
     C                   EVAL      NITBENEF = %EditC(AnitBenef:'X')
     C                   MOVE      ATIPONIT      TIPONITBEN
     C*                  EVAL      LISTA   = 'AOB'
     C*                  EVAL      CODIGO  = %CHAR(ATIPONIT)
     C*    CLAVECDGOS    CHAIN     CIBFFCDGOS                         25
     C*                  IF        NOT *IN25
     C*                  MOVEL     NOMBRE        TIPONITBEN
     C*                  ENDIF
     C                   ENDIF
     C                   ENDIF
     C* Fin validaciones para  OP AOB
     C                   ENDIF
      // PMO15301
      /free
        on-error;
          W_TipoTrama = W_Trama102Op;
          Monitoreo();
        endmon;
      /end-free
     C                   ENDSR
     C* Fin Rutina
     C*_________________________________________________________________________
     C* Rutina para hallar el nit del cliente beneficiario
     C
     C     HALLA_NIT     BEGSR
     C
      // PMO15301
      /free
         monitor;
      /end-free
     C                   MOVE      W_TIPOCTA     CXCDAP
     C                   Z-ADD     CTABENEF      CXNOAC
     C                   CLEAR                   CXNOPL
     C
     C     KXREF         SETLL     CXREFLC
     C                   READ      CXREFLC                                39
     C
     C                   DOW       *IN39 = *OFF
     C                             AND       W_TIPOCTA = CXCDAP
     C                             AND       CTABENEF  = CXNOAC
     C                   IF        CXCDAR = 'T'
     C                   MOVE      CXNAMK        CNNAMK
     C     KNAME         CHAIN     @CNAME                             50
     C                   IF        *IN50 = *OFF
     C                             AND CXNAMK = CNNAMK
     C                   EVAL      W_CAMPOCAR = CNNOSS
     C
     C                   SELECT
     C     CNCDTI        WHENEQ    '1'
     C                   EVAL      TIPONITBEN = '2'
     C     CNCDTI        WHENEQ    '2'
     C                   EVAL      TIPONITBEN = '6'
     C     CNCDTI        WHENEQ    '3'
     C                   EVAL      TIPONITBEN = '1'
     C     CNCDTI        WHENEQ    '4'
     C                   EVAL      TIPONITBEN = '4'
     C     CNCDTI        WHENEQ    '5'
     C                   EVAL      TIPONITBEN = '3'
     C     CNCDTI        WHENEQ    '6'
     C                   EVAL      TIPONITBEN = '6'
     C     CNCDTI        WHENEQ    '7'
     C                   EVAL      TIPONITBEN = '7'
     C                   ENDSL
     C
     C*                  EVAL      NOMBENEF = CNSNAM
     C                   EVAL      NITBENEF = CNNOSS
8545  *
8545  * Process CADDRL0 for CNNAMK. Check For Dispatch Code = '2'
8545  * Populete CAPHO1 from 1.record with greatest value of CADTLM
8545  *                      2. First record if all rcds CADTLM = zeros
8545 C                   EVAL      W_CADTLM = *ZEROS
8545 C                   EVAL      W_CAPHO1  = *ZEROS
8545 C                   EVAL      W_PHFL  = '0'
8545 C                   EVAL      W_CAMPOCAR = *BLANKS
8545 C     KADDL0        SETLL     CADDRL0
8545 C     KADDL0        READE     CADDRL0
8545 C                   DOW       NOT %EOF
8545 C                   IF        CACDAC = '2'
8545 C                   IF        CADTLM  >  W_CADTLM
8545 C                   EVAL      W_CADTLM = CADTLM
8545 C                   EVAL      W_CAPHO1  = CAPHO1
8545 C                   EVAL      W_PHFL  = '1'
8545 C                   ENDIF
8545 C                   IF        CADTLM  = *ZEROS AND
8545 C                             W_PHFL  = '0'
8545 C                   EVAL      W_CADTLM = CADTLM
8545 C                   EVAL      W_CAPHO1  = CAPHO1
8545 C                   EVAL      W_PHFL  = '1'
8545 C                   ENDIF
8545 C                   ENDIF
8545 C     KADDL0        READE     CADDRL0
8545 C                   ENDDO
8545 C                   IF        W_CAPHO1 <> *ZEROS
8545 C                   EVAL      W_CAMPOCAR = W_CAPHO1
8545 C                   ENDIF
8545  *
8545 C*                  EVAL      W_CAMPOCAR = CNNOBP
     C                   ENDIF
     C                   LEAVESR
     C                   ELSE
     C                   READ      CXREFLC                                39
     C                   ENDIF
     C                   ENDDO
     C
      // PMO15301
      /free
        on-error;
          W_TipoTrama = W_Trama102Op;
          Monitoreo();
        endmon;
      /end-free
     C                   ENDSR
     C* Fin Rutina.
     C*________________________________________________________________________
     C* Rutina para validar el nit y tipo de nit del beneficiario
     C
     C     VALIDA_NIT    BEGSR
     C
      // PMO15301
      /free
         monitor;
      /end-free
     C                   MOVE      NITBENEF      ZCNNOSS
     C                   SELECT
     C     W_TIPONIT     WHENEQ    '2'
     C                   EVAL      ZCNCDTI = '1'
     C     W_TIPONIT     WHENEQ    '6'
     C                   EVAL      ZCNCDTI = '2'
     C     W_TIPONIT     WHENEQ    '1'
     C                   EVAL      ZCNCDTI = '3'
     C     W_TIPONIT     WHENEQ    '4'
     C                   EVAL      ZCNCDTI = '4'
     C     W_TIPONIT     WHENEQ    '3'
     C                   EVAL      ZCNCDTI = '5'
     C     W_TIPONIT     WHENEQ    '6'
     C                   EVAL      ZCNCDTI = '6'
     C     W_TIPONIT     WHENEQ    '7'
     C                   EVAL      ZCNCDTI = '7'
     C                   ENDSL
     C
     C                   SETOFF                                       51
     C     KNAMELZ       CHAIN     @CNAMELZ                           51
     C                   IF        *IN51 = *OFF
     C                   MOVE      ZCNNOOB       SUCURBEN
     C                   ELSE
     C                   MOVE      25            SUCURBEN
     C                   ENDIF
     C
      // PMO15301
      /free
        on-error;
          W_TipoTrama = W_TramaTrnOp;
          Monitoreo();
        endmon;
      /end-free
     C                   ENDSR
     C* Fin Rutina.
     C*________________________________________________________________________
     C* Rutina para validar el nit y tipo de nit del beneficiario
     C
     C     SRVALAOBLC    BEGSR
     C
     C                   CLEAR                   wNombreAOB
     C                   CLEAR                   wCIUBEN
     C                   CLEAR                   wFECNBEN
     C                   CLEAR                   W_CTACHAR
     C
     C                   MOVE      NITBENEF      IDCLIENTE
     C                   MOVE      TIPONITBEN    W_TIPOID
     C     IDCLIENTE     CHAIN     CIBFFUSRNC
     C                   IF        %FOUND(CIBFFUSRNC)
     C                   IF        PRIMERNOM <>*BLANKS
     C                   EVAL      W_CTACHAR =%TRIMR(%TRIM(PRIMERNOM))
     C     wNombreAOB    CAT       W_CTACHAR:0   wNombreAOB
     C                   ENDIF
     C                   IF        SEGUNDONOM<>*BLANKS
     C                   EVAL      W_CTACHAR =%TRIMR(%TRIM(SEGUNDONOM))
     C     wNombreAOB    CAT       W_CTACHAR:1   wNombreAOB
     C                   ENDIF
     C                   IF        PRIMERAPE <>*BLANKS
     C                   EVAL      W_CTACHAR =%TRIMR(%TRIM(PRIMERAPE))
     C     wNombreAOB    CAT       W_CTACHAR:1   wNombreAOB
     C                   ENDIF
     C                   IF        SEGUNDOAPE<>*BLANKS
     C                   EVAL      W_CTACHAR =%TRIMR(%TRIM(SEGUNDOAPE))
     C     wNombreAOB    CAT       W_CTACHAR:1   wNombreAOB
     C                   ENDIF
     C                   EVAL      wFECNBEN = FECHANAC
     C                   EVAL      wCIUBEN  = LUGARNAC
     C                   ELSE
     C                   MOVE      NITBENEF      ZCNNOSS
     C                   SELECT
     C     W_TIPOID      WHENEQ    '2'
     C                   EVAL      ZCNCDTI = '1'
     C     W_TIPOID      WHENEQ    '6'
     C                   EVAL      ZCNCDTI = '2'
     C     W_TIPOID      WHENEQ    '1'
     C                   EVAL      ZCNCDTI = '3'
     C     W_TIPOID      WHENEQ    '4'
     C                   EVAL      ZCNCDTI = '4'
     C     W_TIPOID      WHENEQ    '3'
     C                   EVAL      ZCNCDTI = '5'
     C     W_TIPOID      WHENEQ    '6'
     C                   EVAL      ZCNCDTI = '6'
     C     W_TIPOID      WHENEQ    '7'
     C                   EVAL      ZCNCDTI = '7'
     C                   ENDSL
     C
     C                   SETOFF                                       51
     C     KNAMELZ       CHAIN     @CNAMELZ                           51
     C                   IF        *IN51 = *OFF
     C                   EVAL      wNombreAOB=%TRIM(ZCNNAME)
     C                   EVAL      wNombreAOB=%TRIMR(wNombreAOB)
     C     ZCNNAMK       CHAIN     CINDVL0
     C                   IF        %FOUND(CINDVL0)
     C                   EVAL      wFECNBEN = CIDTBR
     C                   IF        CIPLBR <> *BLANKS
     C                   EVAL      wCITY    = %INT(CIPLBR)
     C     wCITY         CHAIN     XECITYL0
     C                   IF        %FOUND(XECITYL0)
     C                   EVAL      wCIUBEN  = XENACI
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ELSE
     C                   EVAL      wNombreAOB=%TRIM(NOMBENEF)
     C                   EVAL      wNombreAOB=%TRIMR(wNombreAOB)
     C                   IF        wNombreAOB = *BLANKS
     C                   EVAL      ESTADO = '44'
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C
     C                   IF        ESTADO = '00'
     C                   EVAL      LK_NIT   =    NITBENEF
     C                   EVAL      LK_TIPOID=    W_TIPOID
     C                   EVAL      LK_NAME  =    wNombreAOB
     C                   EVAL      LK_FUNC  =    'A'
     C                   CLEAR                   LK_STATUS
     C                   CLEAR                   LK_MESSAGE
     C                   CALL      'CIBCECL'
     C                   PARM                    LK_NIT
     C                   PARM                    LK_TIPOID
     C                   PARM                    LK_NAME
     C                   PARM                    LK_FUNC
     C                   PARM                    LK_STATUS
     C                   PARM                    LK_MESSAGE
     C
     C                   IF        LK_STATUS = 'N'
     C                   MOVE      REFODEPA      BAREFOP
     C                   MOVE      NITBENEF      BANITBEN
     C     KEYAOB        CHAIN     CIBFFBAAOB
     C                   IF        %FOUND(CIBFFBAAOB)
     C                   IF        BAESTADO <> 'P'
     C                   SELECT
     C     BAESTADO      WHENEQ    'D'
     C                   EVAL      ESTADO  = '43'
     C     BAESTADO      WHENEQ    ' '
     C                   EVAL      ESTADO  = '42'
     C                   ENDSL
     C                   ENDIF
     C                   ELSE
     C                   IF        LK_MESSAGE = 'CB' OR
     C                             LK_MESSAGE = 'CA'
     C                   EVAL      ESTADO  = '42'
     C                   EXSR      GEN_AOB
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDSR
     C* Fin Rutina.
     C*________________________________________________________________________
     C* Rutina para generar registro en OP bloqueadas AOB
     C     GEN_AOB       BEGSR
     C
     C                   CLEAR                   REGBAAOB
     C                   IF        LK_MESSAGE = 'CB'
     C                   EVAL      BACATEGOR =   'BLOQUEO'
     C                   ELSE
     C                   EVAL      BACATEGOR =   'ALERTA'
     C                   ENDIF
     C                   MOVE      *DATE         W_FECHASIS
     C                   MOVE      W_FECHASIS    BAFECHAB
     C                   EVAL      BAFECING  =   FECHAING
     C                   EVAL      BAREFOP   =   REFODEPA
     C                   EVAL      BAREMENV  =   CODCONV
     C                   EVAL      BAVALOROP =   VLRODEPA
     C                   EVAL      BATIPIDBEN=   LK_TIPOID
     C                   EVAL      BANITBEN  =   LK_NIT
     C                   EVAL      BANOMBEN  =   wNombreAOB
     C                   EVAL      BAFECNBEN =   wFECNBEN
     C                   EVAL      BACIUBEN  =   wCIUBEN
     C                   EVAL      BATELEFO1 =   TELBEN1
     C                   EVAL      BATELEFO2 =   TELBEN2
     C                   EVAL      BACIUDBENE=   CIUBENEF
     C                   EVAL      BBANCO    =   DISPONIB3
     C     BBANCO        CHAIN     STFFFBANCO                         25
     C                   IF        %FOUND(STFFFBANCO)
     C                   EVAL      BABANCODES=   BDESCRIP
     C                   ENDIF
     C                   EVAL      BAORDENANT=   NOMORDEN
     C                   EVAL      BAPAISORIG=   CEDUORDEN
     C                   EVAL      WNIT15    = %INT(NITBENEF)
     C     WNIT15        CHAIN     CIBL1USAOB
     C                   IF        %FOUND(CIBL1USAOB)
     C                   EVAL      BAFECINAOB=   L1FECHCREA
     C                   EVAL      BAUSRINS  =   L1USERCREA
     C                   ENDIF
     C                   EVAL      BAPAGAEN  =   'AOB'
     C                   WRITE     REGBAAOB
     C
     C                   ENDSR
     C* Fin Rutina.
     C*________________________________________________________________________
     C* Rutina para que halla oficinas
     C     HALLA_OFICINA BEGSR
     C
      // PMO15301
      /free
         monitor;
      /end-free
     C* Se halla oficina origen de la orden de pago
     C                   MOVE      SUCURBEN      W_OFICINA
     C                   EVAL      W_OFCCIO  = 25
     C                   EVAL      W_OFCORIG = 25
     C                   IF        W_OFICINA <> 0
     C                   MOVE      W_OFICINA     CODOFC
     C     KOFI          CHAIN     REGCIBCI                           31
     C                   IF        *IN31 = *OFF
     C                   MOVE      CODOFCCI      W_OFCORIG
     C                   MOVE      CODOFCME      W_OFCCIO
     C                   ENDIF
     C                   ENDIF
     C                   EVAL      OFORIGPA=W_OFCORIG
     C                   EVAL      OFCCIOPA=W_OFCCIO
      // PMO15301
      /free
        on-error;
          W_TipoTrama = W_TramaTrnOp;
          Monitoreo();
        endmon;
      /end-free
     C                   ENDSR
     C* Fin Rutina.
     C*______________________________________________________________________
     C*
     C     VALIDA_CAMPOS BEGSR
     C
     C
      // PMO15301
      /free
         monitor;
      /end-free
     C* Se valida si orden de pago está lista para ser avisada
     C                   IF        DISPONIB3=  0
     C                   IF        (NITBENEF <> ' '  AND CTABENEF  = 0) OR
     C                             (NITBENEF <> ' '  AND CTABENEF <> 0)
     C                   EVAL      ESTADO = '03'
     C                   ENDIF
     C                   ENDIF
     C
     C* Se valida la moneda de la Orden de Pago
     C*                  IF        MONODEPA<>MONEDA1 AND MONODEPA<>MONEDA2 AND
     C*                            MONODEPA<>MONEDA3 AND MONODEPA<>MONEDA4 AND
     C*                            MONODEPA<>MONEDA5
     C*                  EVAL      ESTADO = '10'
     C*                  ENDIF
     C
     C* PMO11659
     C* Se condicionan validaciones para que se realicen solo para OP no AOB
     C                   IF        DISPONIB3=  0
     C* Se valida si orden de pago es para ser investigada
     C                   IF        NITBENEF = ' '  AND CTABENEF  = 0
     C                   EVAL      ESTADO = '02'
     C                   ENDIF
     C* Se valida que orden de pago posea un nit de benefic
     C                   IF        NITBENEF = ' '  AND CTABENEF <> 0
     C                   EVAL      ESTADO = '16'
     C                   ENDIF
     C
     C* Se valida que pagos de no clientes posean teléfono
     C                   IF        NITBENEF = ' ' AND TELBEN1 = 0 AND
     C                             TELBEN2 = 0
     C                   EVAL      ESTADO = '17'
     C                   ENDIF
     C*                  IF        SUCURBEN <> 0
     C*                  EXSR      HALLA_OFICINA
     C*                  ENDIF
     C
     C*PMO11659
     C                   ELSE
     C* Validaciones para que se realicen solo para OP AOB
     C
     C* Se valida si NIT inscrito a pago ágil
     C                   IF        NITBENEF <> ' '
     C                   EVAL      IDUSUARIO =NITBENEF
     C     IDUSUARIO     CHAIN     CIBFFDCAUT                         25
     C                   IF        *IN25
     C                   EVAL      ESTADO = '30'
     C                   ENDIF
     C                   ENDIF

     C* Se valida si cuenta  inscrita a abonos
     C                   EVAL      ANROCUENTA = *BLANKS
     C                   IF        MSGBENEF <> *blanks
     C                   MOVE      *Blanks       W_CUENTAOB
     C                   EVAL      ANROCUENTA =%SUBST(MSGBENEF:1:17)
     C                   EVAL      ACODBANCO = DISPONIB3
     C     CLAVEAOB      CHAIN     CIBFFUSAOB                         35
     C                   IF        NOT *IN35 and
     C                             AEstado = 'A'
     C                   Else
     C                   EVAL      ESTADO = '30'
     C                   ENDIF
     C* No tiene cuenta
     C                   ELSE
     C                   EVAL      ESTADO = '34'
     C                   ENDIF
     C* Se valida que pagos de no clientes posean teléfono
     C
     C                   IF        TELBEN1 = 0 AND
     C                             TELBEN2 = 0
     C                   EVAL      ESTADO = '31'
     C                   ENDIF
     C* Se valida banco
     C                   EVAL      BBANCO = DISPONIB3
     C     BBANCO        CHAIN     STFFFBANCO                         25
     C                   IF        *IN25
     C                   EVAL      ESTADO = '32'
     C                   ENDIF
     C* Se valida que venga ciudad del beneficiario
     C
     C                   IF        CIUBENEF = ' '
     C                   EVAL      ESTADO = '37'
     C                   ENDIF

     C* Se valida Cupo del cliente y estado
     C                   CLEAR                   WRESP
     C                   IF        CODMONODP <> 1
     C                   MOVE      CODMONODP     WCODCAM
     C                   MOVE      DIAACT        WDIA
     C                   MOVE      MESACT        WMES
     C                   MOVE      YEAACT        WANO
     C                   CALL      'STACVALCOD'
     C                   PARM                    WANO
     C                   PARM                    WMES
     C                   PARM                    WDIA
     C                   PARM                    WCODCAM
     C                   PARM                    WCODSWF
     C                   PARM                    WNOMBRE
     C                   PARM                    WPAIS
     C                   PARM                    WVLRCAM
     C                   PARM                    WRESP
     C                   ENDIF

     C                   IF        (WRESP='0')OR(WRESP='4')
     C                   EVAL      FACTOR    = WVLRCAM
     C                   ELSE
     C                   EVAL      FACTOR = 1
     C                   ENDIF

     C                   EVAL      ECVALOR = VLRODEPA / FACTOR

     C                   IF        NITBENEF <> ' '
     C                   MOVE      NITBENEF      W_NIT
     C                   Z-ADD     W_NIT         ECNIT
     C                   MOVE      TIPONITBEN    ECTIPO
     C                   CLEAR                   WRESP

     C                   CALL      'CIBCESTCLI'
     C                   PARM                    ECNIT
     C                   PARM                    ECTIPO
     C                   PARM                    ECVALOR
     C                   PARM                    WRESP
     C                   PARM                    ECTARIFA
     C                   PARM                    ECOPERACION

     C                   IF        WRESP       = '2'
     C                   EVAL      ESTADO = '36'
     C                   ENDIF
     C*
     C                   IF        WRESP       = '1'
     C                   EVAL      ESTADO = '35'
     C                   ENDIF
     C                   ENDIF
     C*
     C* Consultar valor tope para no clientes
     C*
     C                   Z-ADD     0             WTOPE
     C                   MOVE      'C'           TTIPOOPR
     C                   Z-ADD     0             TNIT
     C     TCLAVETOPE    CHAIN     CIBFFTOPES                         80
     C                   IF        *IN80 = *OFF
     C                   MOVE      TMNOCLIEN     WTOPE
     C                   ENDIF

     C*
     C* Verificar si no es cliente
     C*
     C                   IF        NITBENEF <> ' '
     C     NITBENEF      CHAIN     @CNAMEL3                           81
     C                   IF        *IN81 = *ON  AND ECVALOR > WTOPE
     C                   EVAL      ESTADO = '36'
     C                   ENDIF
     C                   ENDIF

     C                   IF        NITBENEF =  ' '  AND ECVALOR > WTOPE
     C                   EVAL      ESTADO = '36'
     C                   ENDIF
     C*PMO11659
     C                   ENDIF
     C
     C* Se valida la sucursal de la orden de pagos
     C                   IF        (SUCURBEN=0 OR OFORIGPA=0 OR OFCCIOPA=0)
     C                             AND NITBENEF <> '  '
     C                   EVAL      ESTADO = '15'
     C                   ENDIF
     C
     C
     C* Se valida si beneficiario no se encuentra en listas
     c* de control
     C                   IF        NITBENEF <> ' '
     C                   IF        DISPONIB3 <> 0
     C                   IF        ESTADO = '00'
     C                   EXSR      SRVALAOBLC
     C                   ENDIF
     C                   ELSE
     C                   MOVE      NITBENEF      W_NIT
     C                   Z-ADD     W_NIT         WNIT15
     C                   MOVE      TIPONITBEN    WTPONIT
     C*                  CALL      'CDCCBLQDO'
     C*                  PARM                    WTPONIT
     C*                  PARM                    WNIT15
     C*                  PARM                    WEXISTE
     C
     C*                  IF        WEXISTE     = 'S'
     C*                            AND DISPONIB3=  0
     C*                  EVAL      ESTADO = '20'
     C*                  ENDIF
     C                   ENDIF
     C                   ENDIF
     C* Marzo de 2016  PMO17759 SARLAFT
     C
     C*Validación de país en listas de control solo para remesadoras
     C*parametrizadas
      /free
                        W_ESTADO=*BLANKS;
                        w_Lista = CODCONV;
                        w_Codigo = 'VALPA';
                        w_Nombre1 = *Blanks;
                        CIBOBANGOS(W_Lista:w_Codigo:w_Nombre1);
                        If %Trim(w_Nombre1) = 'S'        ;
                          IF  CEDUORDEN = *BLANKS ;
                            w_Lista = CODCONV;
                            w_Codigo = 'NPAIS';
                            w_Nombre1 = *Blanks;
                            CIBOBANGOS(W_Lista:w_Codigo:w_Nombre1);
                            If %Trim(w_Nombre1)<> *blanks and
                               %Trim(w_Nombre1)<> *zeros;
                               CEDUORDEN = %Trim(w_Nombre1) ;
                            Else;
                               W_ESTADO = '50';
                            EndIf;
                          EndIf;
      /end-free
     C                   IF        CEDUORDEN <> *BLANKS
     C                   MOVE      REFODEPA      w_Refppal
     C                   MOVE      CEDUORDEN     w_nompais
     C                   MOVE      'V'           w_Func
     C                   MOVE      *BLANKS       w_EstVal
     C                   MOVE      *BLANKS       w_msg
     C                   MOVE      *BLANKS       w_codrec
     C                   CALL      'CIBRVALPAI'
     C                   PARM                    w_Refppal
     C                   PARM                    w_nompais
     C                   PARM                    w_Func
     C                   PARM                    w_EstVal
     C                   PARM                    w_msg
     C                   PARM                    w_codrec
     C                   IF        w_EstVal <>'S'
     C                   IF        w_codrec <>'0'  AND w_codrec <>'5'
     C                   EVAL      W_ESTADO = '50'
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   IF        W_ESTADO = '50'
     C                   MOVE      CODCONV       w_codmod
     C                   MOVE      REFODEPA      w_Refmod
     C                   MOVE      REFPALOP      w_Refarmod
     C                   MOVE      NITBENEF      w_nitmod
     C                   MOVE      CTABENEF      w_ctamod
     C                   MOVE      NOMBENEF      w_nommod
     C                   MOVE      TELBEN1       w_tel1mod
     C                   MOVE      TELBEN2       w_tel2mod
     C                   MOVE      FECHAING      w_fecingmod
     C                   MOVE      VLRODEPA      w_valmod
     C                   MOVE      DIRBENEF      w_dirmod
     C                   MOVE      ' '           w_indmod
     C                   CALL      'CIBRDEVORD'
     C                   PARM                    w_codmod
     C                   PARM                    w_Refmod
     C                   PARM                    w_Refarmod
     C                   PARM                    w_nitmod
     C                   PARM                    w_ctamod
     C                   PARM                    w_nommod
     C                   PARM                    w_tel1mod
     C                   PARM                    w_tel2mod
     C                   PARM                    w_fecingmod
     C                   PARM                    w_valmod
     C                   PARM                    w_dirmod
     C                   PARM                    w_indmod
     C                   IF        w_indmod  = 'S'
     C                   MOVE      W_ESTADO      ESTADO
     C                   ENDIF
     C                   ENDIF
      // PMO15301
      /free
          // Marzo 2016  PMO17759 SARLAFT
                       Endif;

        on-error;
          W_TipoTrama = W_Trama102Op;
          Monitoreo();
        endmon;
      /end-free
     C                   ENDSR
     C* Fin Rutina.
     C*________________________________________________________________________
     C* Rutina para elimina caracteres especiales de la referencia en O de P
     C*
     C     ELIM_CAR      BEGSR
     C
      // PMO15301
      /free
         monitor;
      /end-free
     C                   EVAL      SPC = 0
     C                   EVAL      W_POSCAMPO   =  1
     C                   EVAL      W_VALIDOS = *BLANKS
     C                   EVAL      LNG = %LEN(%TRIM(W_CHEQUEO)) + 1
     C                   DOW       LNG > W_POSCAMPO
     C                   EVAL      W_CAMPO1 = %SUBST(W_CHEQUEO:W_POSCAMPO:1)
     C     W_Caracter    CHECK     W_CAMPO1      NUMERO
     C                   IF        NUMERO = 0 AND W_CAMPO1 <> ' '
     C     W_VALIDOS     CAT       W_CAMPO1:SPC  W_VALIDOS
     C                   EVAL      SPC = 0
     C                   ENDIF
     C                   IF        NUMERO <> 0 AND W_BANDERA = '2'
     C     W_otros       CHECK     W_CAMPO1      NUMERO
     C                   IF        NUMERO = 0
     C     W_VALIDOS     CAT       W_CAMPO1:SPC  W_VALIDOS
     C                   EVAL      SPC = 0
     C                   ENDIF
     C                   ENDIF
     C                   IF        W_BANDERA = '1' AND W_CAMPO1 = ','
     C     W_VALIDOS     CAT       W_CAMPO1:SPC  W_VALIDOS
     C                   EVAL      SPC = 0
     C                   ENDIF
     C                   IF        W_BANDERA = '2' AND W_CAMPO1 = ' '
     C                   EVAL      SPC = spc + 1
     C                   ENDIF
     C                   EVAL      W_POSCAMPO = W_POSCAMPO + 1
     C                   ENDDO
     C
      // PMO15301
      /free
        on-error;
          W_TipoTrama = W_TramaTrnOp;
          Monitoreo();
        endmon;
      /end-free
     C                   ENDSR
     C* Fin Rutina.
     C*________________________________________________________________________
     C*
     C     VALIDA_TASA   BEGSR
     C
      // PMO15301
      /free
         monitor;
      /end-free
     C
     C                   EVAL      W_AA  = %SUBST(W_FEC:1:4)
     C                   EVAL      W_M   = %SUBST(W_FEC:5:2)
     C                   EVAL      W_D   = %SUBST(W_FEC:7:2)
     C                   EVAL      W_CODCAM=41
     C
     C                   MOVEL     W_AA          W_AAAA
     C                   MOVEL     W_M           W_MM
     C                   MOVEL     W_D           W_DD
     C
     C                   CALL      'STACVALCOD'
     C                   PARM                    W_AAAA
     C                   PARM                    W_MM
     C                   PARM                    W_DD
     C                   PARM                    W_CODCAM
     C                   PARM                    W_CODSWI          3
     C                   PARM                    W_NOMBRE         25
     C                   PARM                    W_PAIS           20
     C                   PARM                    W_VALCAM
     C                   PARM                    W_INDIC           1
     C
     C                   IF        TASACAMB=0
     C                   MOVEL     '23'          ESTADO
     C                   ELSE
     C     CODCONV       CHAIN     CIBFFBANOP                         25
     C                   IF        NOT *IN25 AND B_TASAFIJA='S'
     C                   EVAL      W_RESTINF= B_LIMINFTA / (100)
     C                   IF        W_RESTINF<0
     C                   EVAL      W_RESTINF= W_RESTINF*(-1)
     C                   ENDIF
     C                   EVAL      W_RESTSUP= B_LIMSUPTA / (100)
     C                   IF        W_RESTSUP<0
     C                   EVAL      W_RESTSUP= W_RESTSUP*(-1)
     C                   ENDIF
     C                   EVAL      W_RESTSUP= B_LIMSUPTA / (100)
     C                   EVAL      W_LIMINF =W_VALCAM - ( W_VALCAM * W_RESTINF )
     C                   EVAL      W_LIMSUP =W_VALCAM + ( W_VALCAM * W_RESTINF )
     C                   IF        TASACAMB<W_LIMINF OR TASACAMB>W_LIMSUP
     C                   MOVEL     '23'          ESTADO
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C
      // PMO15301
      /free
        on-error;
          W_TipoTrama = W_TramaTrnOp;
          Monitoreo();
        endmon;
      /end-free
     C                   ENDSR
     C* Fin Rutina.
     C*________________________________________________________________________
     C*
     C     EXTR_CAMP     BEGSR
     C
      // PMO15301
      /free
         monitor;
      /end-free
     C                   EXSR      BORRAR
     C
     C                   IF        T_INFBENEF=' '
     C                   EXSR      EXT_ORD
     C                   LEAVESR
     C                   ENDIF
     C
     C     W_CARDEL      SCAN      T_INFBENEF    VSPOS
     C                   IF        VSPOS > 0
     C                   IF        VSPOS <> 1
     C                   EVAL      *IN50=*ON
     C                   EVAL      W_POS = 1
     C                   ELSE
     C                   EVAL      W_POS = 2
     C                   ENDIF
     C                   ENDIF
     C
     C                   EVAL      W_LONG=%LEN(%TRIM(T_INFBENEF))
     C
     C                   CLEAR                   W_REG1
     C                   DOW       W_CAMP=0
     C                   EVAL      W_REG = %SUBST(T_INFBENEF:W_POS:1)
     C                   IF        W_REG =  W_CARDEL OR W_POS = W_LONG + 1
     C                   EVAL      W_REG1= %TRIM(W_REG1)
     C                   EVAL      W_POS = W_POS + 1
     C                   EXSR      ADDREG
      // PMO15301
      /free
         if  W_TipoTrama <> *blanks;
             leavesr;
         endif;
      /end-free
     C                   ELSEIF    W_REG <> W_CARDEL AND W_POS <= W_LONG
     C                   EVALR     W_REG1= W_REG1 + W_REG
     C                   EVAL      W_POS = W_POS + 1
     C                   ENDIF
     C                   IF        W_POS>= W_LONG
     C                   EVAL      W_CAMP=1
     C                   ENDIF
     C                   ENDDO
     C                   EXSR      EXT_ORD
      // PMO15301
      /free
        if  W_TipoTrama <> *blanks;
            leavesr;
        endif;

        on-error;
          W_TipoTrama = W_TramaTrnOp;
          Monitoreo();
        endmon;
      /end-free
     C
     C                   ENDSR
     C* Fin Rutina.
     C*________________________________________________________________________
     C*
     C     ADDREG        BEGSR
     C
      // PMO15301
      /free
         monitor;
      /end-free
     C                   IF        *IN50
     C     W_Digit       CHECK     T_INFBENEF    W_POS1            3 0
     C                   EVAL      *IN50=*OFF
     C                   IF        W_POS1=2
     C                   EVAL      W_CONT = W_CONT+1
     C                   CLEAR                   W_REG1
     C                   LEAVESR
     C                   ELSE
     C                   EVAL      W_CONT = W_CONT+1
     C                   ENDIF
     C                   ELSE
     C                   EVAL      W_CONT = W_CONT+1
     C                   ENDIF
     C
     C                   IF        W_CONT=1
     C                   EVAL      T_CTABENEF=W_REG1
     C                   EVAL      W_CHEQUEO=' '
     C                   MOVEL     T_CTABENEF    W_CHEQUEO
     C                   EXSR      ELIM_CAR
      // PMO15301
      /free
         if  W_TipoTrama <> *blanks;
             leavesr;
         endif;
      /end-free
     C                   MOVEL     W_VALIDOS     T_CTABENEF
     C                   CLEAR                   W_REG1
     C                   LEAVESR
     C                   ENDIF
     C
     C                   IF        W_CONT=2
     C                   EVAL      T_NOMBENEF=W_REG1
     C                   CLEAR                   W_REG1
     C                   LEAVESR
     C                   ENDIF
     C
     C                   IF        W_CONT=3
     C                   EVAL      T_DIRBENEF=W_REG1
     C                   CLEAR                   W_REG1
     C                   LEAVESR
     C                   ENDIF
     C
     C                   IF        W_CONT=4
     C                   EVAL      T_CIUBENEF=W_REG1
     C                   CLEAR                   W_REG1
     C                   LEAVESR
     C                   ENDIF
     C
      // PMO15301
      /free
        on-error;
          W_TipoTrama = W_TramaTrnOp;
          Monitoreo();
        endmon;
      /end-free
     C                   ENDSR
     C* Fin Rutina.
     C*________________________________________________________________________
     C*
     C     EXT_ORD       BEGSR
      // PMO15301
      /free
         monitor;
      /end-free
     C                   EXSR      BORRAR
     C
     C                   IF        T_INFORDEN=' '
     C                   LEAVESR
     C                   ENDIF
     C
     C     W_CARDEL      SCAN      T_INFORDEN    VSPOS
     C                   IF        VSPOS > 0
     C                   IF        VSPOS <> 1
     C                   EVAL      *IN50=*ON
     C                   EVAL      W_POS = 1
     C                   ELSE
     C                   EVAL      W_POS = 2
     C                   ENDIF
     C                   ENDIF
     C                   EVAL      W_LONG=%LEN(%TRIM(T_INFORDEN))
     C
     C                   EVAL      W_REG1 = ' '
     C                   DOW       W_CAMP=0
     C                   EVAL      W_REG = %SUBST(T_INFORDEN:W_POS:1)
     C                   IF        W_REG =  W_CARDEL OR W_POS = W_LONG + 1
     C                   EVAL      W_POS = W_POS + 1
     C                   EVAL      W_REG1= %TRIM(W_REG1)
     C                   EXSR      ADDREGORD
      // PMO15301
      /free
         if  W_TipoTrama <> *blanks;
             leavesr;
         endif;
      /end-free
     C                   ELSEIF    W_REG <> W_CARDEL AND W_POS <= W_LONG
     C                   EVALR     W_REG1= W_REG1 + W_REG
     C                   EVAL      W_POS = W_POS + 1
     C                   ENDIF
     C                   IF        W_POS>= W_LONG
     C                   EVAL      W_CAMP=1
     C                   ENDIF
     C                   ENDDO
     C
      // PMO15301
      /free
        on-error;
          W_TipoTrama = W_TramaTrnOp;
          Monitoreo();
        endmon;
      /end-free
     C                   ENDSR
     C* Fin Rutina.
     C*________________________________________________________________________
     C*
     C     ADDREGORD     BEGSR
     C
      // PMO15301
      /free
         monitor;
      /end-free
     C                   IF        *IN50
     C     W_Digit       CHECK     T_INFORDEN    W_POS1
     C                   EVAL      *IN50=*OFF
     C                   IF        W_POS1=2
     C                   EVAL      W_CONT = W_CONT+1
     C                   ELSE
     C                   EVAL      W_CONT = W_CONT+1
     C                   ENDIF
     C                   ELSE
     C                   EVAL      W_CONT = W_CONT+1
     C                   ENDIF
     C
     C                   IF        W_CONT=1
     C                   EVAL      T_IDORDEN =W_REG1
 HP  C                   EVAL      W_CHEQUEO=' '
 |   C                   MOVEL     T_IDORDEN     W_CHEQUEO
 |   C                   EXSR      ELIM_CAR
      // PMO15301
      /free
         if  W_TipoTrama <> *blanks;
             leavesr;
         endif;
      /end-free
 HP  C                   MOVEL     W_VALIDOS     T_IDORDEN
     C                   CLEAR                   W_REG1
     C                   LEAVESR
     C                   ENDIF
     C
     C                   IF        W_CONT=2
     C                   EVAL      T_NOMORDEN=W_REG1
 HP  C                   EVAL      LNG = %LEN(%TRIM(T_NOMORDEN)) + 1
 |   C     W_Digit       CHECK     T_NOMORDEN    W_POS1
 |   C                   EVAL      LNG = LNG - W_POS1
 HP  C                   EVAL      T_NOMORDEN= %SUBST(T_NOMORDEN:W_POS1:LNG)
     C                   CLEAR                   W_REG1
     C                   LEAVESR
     C                   ENDIF
     C
     C                   IF        W_CONT=3
     C                   EVAL      T_DIRORDEN=W_REG1
 HP  C                   EVAL      LNG = %LEN(%TRIM(T_DIRORDEN)) + 1
 |   C     W_Digit       CHECK     T_DIRORDEN    W_POS1
 |   C                   EVAL      LNG = LNG - W_POS1
 HP  C                   EVAL      T_DIRORDEN= %SUBST(T_DIRORDEN:W_POS1:LNG)
     C                   CLEAR                   W_REG1
     C                   LEAVESR
     C                   ENDIF
     C
     C                   IF        W_CONT=4
     C                   EVAL      T_CIUORDEN=W_REG1
     C                   EVAL      LNG = %LEN(%TRIM(T_CIUORDEN)) + 1
     C     W_Digit       CHECK     T_CIUORDEN    W_POS1
     C                   EVAL      LNG = LNG - W_POS1
     C                   EVAL      T_CIUORDEN= %SUBST(T_CIUORDEN:W_POS1:LNG)
     C                   CLEAR                   W_REG1
     C                   LEAVESR
     C                   ENDIF
     C
      // PMO15301
      /free
        on-error;
          W_TipoTrama = W_TramaTrnOp;
          Monitoreo();
        endmon;
      /end-free
     C                   ENDSR
     C* Fin Rutina.
     C*________________________________________________________________________
     C*
     C     BORRAR        BEGSR
     C                   CLEAR                   VSPOS
     C                   CLEAR                   W_POS
     C                   CLEAR                   W_LONG
     C                   CLEAR                   W_REG
     C                   CLEAR                   W_CONT
     C                   CLEAR                   W_CAMP
     C                   ENDSR
     C* Fin Rutina.
     C*________________________________________________________________________
     C* Conversión automática de números de cuenta Conavi a Bancolombia
     C*
     C     CONVER_CTA    BEGSR
     C
      // PMO15301
      /free
         monitor;
      /end-free
     C                   MOVE      CTABENEF      W_CTACHAR
     C                   CLEAR                   W_TIPOCTA
     C                   CLEAR                   W_CTANUME
     C                   CALL      'CIBRCTACON'
     C                   PARM                    W_CTACHAR
     C                   PARM                    W_TIPOCTA
     C                   PARM                    W_CTANUME
     C                   IF        W_TIPOCTA <> *BLANKS
     C                   EVAL      TIPOCTABEN = W_TIPOCTA
     C                   EVAL      CTABENEF = W_CTANUME
     C* Se busca la sucursal del beneficiario
     C                   MOVE      W_CTANUME     W_CUENTA

       // If %Trim(w_Nombre1) = '1';

          CIBROFICTA(W_CUENTA:W_TIPOCTA:W_Sucursal:W_Inderr);
          IF W_Sucursal = *Blanks;
             SUCURBEN = *zeros;
          ELSE;
            SUCURBEN = %Int(W_Sucursal);
          ENDIF;


       // Else;
     C*                   MOVE      TIPOCTABEN    DMAPCD
     C*                   Z-ADD     CTABENEF      DMACCT
     C*     KDBAL         CHAIN     @DBAL                              30
     C*                   IF        *IN30 = *OFF
     C*                   MOVE      DMBRN         SUCURBEN
     C*                   ENDIF
     C*                   ENDIF
     C
     C                   ENDIF
      // PMO15301
      /free
        on-error;
          W_TipoTrama = W_Trama102Op;
          Monitoreo();
        endmon;
      /end-free
     C                   ENDSR
     C*________________________________________________________________________
     pMonitoreo        b
     dMonitoreo        pi

      /free
        select;
          when W_TipoTrama = W_TramaTrnOp;
             MnRutina   = DsInfdsTrnOP.RoutinRpg;
             MnArchivo  = %trim(DsInfdsTrnOp.Library) +
                          '/' + %trim(DsInfdsTrnOP.FileName);
             MnNrr      = DsInfdsTrnOP.DbRrn;
             MnDescrip  = DsPsds.ExcptType + DsPsDs.ExcptNumber + ' ' +
                          %trim(DsPsds.ExcptData) + ' - ' +
                          %trim(DsPsds.JobName) + '/' + %trim(DsPsDs.JobUser) +
                          '/' + %editc(DsPsds.JobNumber: 'X') + ' - ' +
                          DsInfdsTrnOp.LastOpCode + DsInfdsTrnop.ListLine;
             MnTrama    = DsRegTrnOp;
          when W_TipoTrama = W_Trama102Op;
             MnRutina   = DsInfds102Op.RoutinRpg;
             MnArchivo  = %trim(DsInfds102Op.Library) +
                          '/' + %trim(DsInfds102Op.FileName);
             MnNrr      = DsInfds102Op.DbRrn;
             MnDescrip  = DsPsds.ExcptType + DsPsDs.ExcptNumber + ' ' +
                          %trim(DsPsds.ExcptData) + ' - ' +
                          %trim(DsPsds.JobName) + '/' + %trim(DsPsDs.JobUser) +
                          '/' + %editc(DsPsds.JobNumber: 'X') + ' - ' +
                          DsInfds102Op.LastOpCode + DsInfds102op.ListLine;
             MnTrama    = DsReg102Op;
        endsl;
          GeneraLogMon();
      /end-free

     pMonitoreo        e

     pGeneraLogMon     b
     dGeneraLogMon     pi
     d lProceso        s             10a   inz('CIBDI15')
     d lPrograma       s             10a   inz('CIBRREGOPT')
     d lOmite          s              1a   inz('S')

      /free
        MnProceso  = lProceso;
        MnPrograma = lPrograma;
        MnOmite    = lOmite;
        MnFecha    = %date();
        MnHora     = %time();
        monitor;
           write RegMonPr;
        on-error;
        endmon;
        clear RegMonPr;
      /end-free

     pGeneraLogMon     e
      *--------------------------------------------------------------------
      * Nombre Proc.: Fn_QuitaPunto
      * Propósito...: Elimina puntos de una variable
      *--------------------------------------------------------------------
     P Fn_QuitaPunto   B
     D Fn_QuitaPunto   Pi          2000a
     D  p_Valor                    2000a   Const
      *
     D w_Valor         s           2000a
     D w_PosAux        s              4s 0
      /Free
        //Elimina el punto decimal
        w_PosAux = %Scan('.': p_Valor);
        If w_PosAux > 0;
          w_Valor = %SubSt(p_Valor: 1: w_PosAux - 1) +
                    %SubSt(p_Valor: w_PosAux + 1);
        Else;
          w_Valor = p_Valor;
        EndIf;

        Return w_Valor;
      /End-Free
     P Fn_QuitaPunto   E   