*&---------------------------------------------------------------------*
*&  Include           ZSD_PRICING_UPLOAD_TOP
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------
* CLASS
*----------------------------------------------------------------------
CLASS lcl_cond DEFINITION DEFERRED.
*----------------------------------------------------------------------
* TYPES
*----------------------------------------------------------------------
TYPE-POOLS slis.
TYPES: BEGIN OF ty_main,
         kappl  TYPE kappl,
         kschl  TYPE kschl,
         table  TYPE tabname,
         fld1   TYPE fieldname,
         fld2   TYPE fieldname,
         fld3   TYPE fieldname,
         fld4   TYPE fieldname,
         fld5   TYPE fieldname,
         fld6   TYPE fieldname,
         fld7   TYPE fieldname,
         fld8   TYPE fieldname,
         fld9   TYPE fieldname,
         fld10  TYPE fieldname,
         fld11  TYPE fieldname,
         datab  TYPE kodatab,
         datbi  TYPE kodatbi,
         kbetr  TYPE kbetr_kond,
         kpein  TYPE kpein,
         meins  TYPE meins,
         krech  TYPE krech,
         waers  TYPE waers,
         kstbm  TYPE kstbm,
         kbetr2 TYPE kbetr_kond,
       END OF ty_main,
       BEGIN OF ty_t681a,
         kappl TYPE kappl, " Application
       END OF ty_t681a,
       BEGIN OF ty_t685,
         kschl TYPE kschl, " Condition Type
       END OF ty_t685,
       BEGIN OF ty_t006,
         msehi   TYPE msehi, " Uom
         isocode TYPE isocd_unit, " ISO Code
       END OF ty_t006,
       BEGIN OF ty_cexit,
         funcname  TYPE rs38l_fnam, " FunctionMod Name
         parameter TYPE rs38l_par_, " Parameter
         paramtype TYPE rs38l_kind, " Parameter Type
       END OF ty_cexit,
       BEGIN OF ty_table,
         table TYPE tabname,
         info  TYPE REF TO data,
       END OF ty_table,
       BEGIN OF ty_log,
         tabix TYPE sytabix,
         key   TYPE fc_text,
         type  TYPE msgty,
         msg   TYPE bapi_msg,
       END OF ty_log.
*----------------------------------------------------------------------
* CONSTANTS
*----------------------------------------------------------------------
CONSTANTS : c_i     TYPE char1 VALUE 'I', " Info
            c_s     TYPE char1 VALUE 'S', " Success
            c_e     TYPE char1 VALUE 'E', " Error
            c_segnm TYPE idocdsgnum VALUE '000001', " Segment Num
            c_hlevl TYPE edi_hlevel VALUE '01', " Higher Level
            c_komg  TYPE edilsegtyp VALUE 'E1KOMG', " Segment Name
            c_konh  TYPE edilsegtyp VALUE 'E1KONH', " Segment Name
            c_konp  TYPE edilsegtyp VALUE 'E1KONP', " Segment Name
            c_konm  TYPE edilsegtyp VALUE 'E1KONM', " Segment Name
            c_dctyp TYPE edi_doctyp VALUE 'COND_A04', " IDOC Type
            c_bstyp TYPE edi_idoctp VALUE 'COND_A04', " Basic Type
            c_mstyp TYPE edi_mestyp VALUE 'COND_A', " Message Type
            c_ls    TYPE edi_rcvprt VALUE 'LS', " Partner System
            c_in    TYPE edi_direct VALUE '2', " Direction
            c_sap   TYPE char3 VALUE 'SAP', " SAP
            c_53    TYPE edi_status VALUE '53', " Success Status
            c_a     TYPE stfkz VALUE 'A', " Scale Type
            c_b     TYPE kzbzg VALUE 'B', " Scale Type
            c_c     TYPE kzbzg VALUE 'C', " Scale basis indicator
            c_per   TYPE konwa VALUE '%', " Rate unit (currency   or percentage)
            c_conv  TYPE char16 VALUE 'CONVERSION_EXIT_',
            c_input TYPE char6 VALUE '_INPUT',
            c_vrkme TYPE fieldname VALUE 'VRKME', " Sales Unit
            c_sep   TYPE char1 VALUE '/', " Seperator
            c_u     TYPE char1 VALUE 'U', " Upper Case
            c_text  TYPE string VALUE 'TXT', " TXT
            c_inp   TYPE rs38l_par_ VALUE 'INPUT',
            c_out   TYPE rs38l_par_ VALUE 'OUTPUT',
            c_other TYPE abap_excpname VALUE 'OTHERS',
            c_h     TYPE char01 VALUE 'H'.
*----------------------------------------------------------------------
* VARIABLES
*----------------------------------------------------------------------
DATA : v_repid TYPE syrepid, " Report ID
       v_efile TYPE string, " Error File Path
       v_flag  TYPE char1, " Upload Failed
       v_tabix TYPE sytabix, " Record Index
       v_error TYPE char1, " Error in record
       v_vakey TYPE vakey_50, " Variable Key
       v_key   TYPE fc_text, " Key for SALV Disp
       v_segnm TYPE idocdsgnum, " Segment Num
       v_hlevl TYPE edi_hlevel, " Higher Level
       v_partn TYPE edi_rcvprn, " Partner Number
       v_port  TYPE edi_rcvpor, " Port
       v_len   TYPE ddleng, " Strlen
       v_leng  TYPE ddleng. " Strlen
*----------------------------------------------------------------------
* WORK AREAS
*----------------------------------------------------------------------
DATA : w_edidc   TYPE edidc, " Control Record
       w_edidd   TYPE edidd, " Data Record
       w_e1komg  TYPE e1komg, " Segment Data
       w_e1konh  TYPE e1konh, " Segment Data
       w_e1konp  TYPE e1konp, " Segment Data
       w_e1konm  TYPE e1konm. "scales
*----------------------------------------------------------------------
* INTERNAL TABLES
*----------------------------------------------------------------------
DATA : t_main     TYPE STANDARD TABLE OF ty_main, " Upload Data
       t_error    TYPE STANDARD TABLE OF ty_main, " Error Data
       t_t681a    TYPE STANDARD TABLE OF ty_t681a, " Application
       t_t685     TYPE STANDARD TABLE OF ty_t685, " Condition Types
       t_t006     TYPE STANDARD TABLE OF ty_t006, " ISO Codes
       t_edidc    TYPE STANDARD TABLE OF edidc, " Control Record
       t_edidd    TYPE STANDARD TABLE OF edidd, " Data Record
       t_edids    TYPE STANDARD TABLE OF bdidocstat, " Status Record
       t_return   TYPE STANDARD TABLE OF bdwfretvar, " Return
       t_bdi_ser  TYPE STANDARD TABLE OF bdi_ser, " IDOC Structure
       t_ddfields TYPE ddfields,
       t_cexit    TYPE STANDARD TABLE OF ty_cexit,
       t_table    TYPE STANDARD TABLE OF ty_table,
       t_log      TYPE STANDARD TABLE OF ty_log,
       t_fcat     TYPE slis_t_fieldcat_alv.
*----------------------------------------------------------------------
* OBJECTS
*----------------------------------------------------------------------
DATA : o_cond   TYPE REF TO lcl_cond,
       o_data   TYPE REF TO data,
       o_e1komg TYPE REF TO data,
       o_e1konh TYPE REF TO data,
       o_e1konp TYPE REF TO data,
       o_e1konm TYPE REF TO data.
*----------------------------------------------------------------------
* FIELD-SYMBOLS
*----------------------------------------------------------------------
FIELD-SYMBOLS : <fs_main>  TYPE ty_main,
                <fs_t681a> TYPE ty_t681a,
                <fs_t685>  TYPE ty_t685,
                <fs_t006>  TYPE ty_t006,
                <fs_edids> TYPE bdidocstat,
                <fs_dfies> TYPE dfies,
                <fs_field> TYPE any,
                <fs_value> TYPE any,
                <fs_komg>  TYPE any,
                <fs_konh>  TYPE any,
                <fs_konp>  TYPE any,
                <fs_konm>  TYPE any,
                <fs_cexit> TYPE ty_cexit,
                <fs_table> TYPE ty_table.
