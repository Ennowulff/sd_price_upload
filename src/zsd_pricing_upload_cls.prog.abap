*&---------------------------------------------------------------------*
*&  Include           ZSD_PRICING_UPLOAD_CLS
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* CLASS lcl_cond DEFINITION
*----------------------------------------------------------------------*
* Local Class
*----------------------------------------------------------------------*
CLASS lcl_cond DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          use_header_line TYPE abap_bool DEFAULT space,
      f4_help,
      f4_ehelp,
      validate_file,
      validate_efile,
      sos,
      eos,
      upload_data,
      get_partner_num,
      get_check_data,
      process_data,
      display_alv,
      download_error_records,
      create_fcat,
      append_fcat IMPORTING im_field TYPE fieldname
                            im_text  TYPE scrtext_l,
      alv,
      get_table_details,
      validate_data,
      pass_isocode_for_vrkme,
      pass_vistex,
      conversion_routine IMPORTING im_convexit TYPE convexit
                                   im_leng     TYPE ddleng,
      call_conv_exit IMPORTING im_func TYPE rs38l_fnam
                               im_leng TYPE ddleng,
      populate_idoc_data,
      pass_control_record,
      pass_data_record,
      post_data,
      direct_update,
      append_log IMPORTING im_tabix TYPE sytabix
                           im_key   TYPE fc_text
                           im_type  TYPE msgty
                           im_msg   TYPE bapi_msg,
      format_message IMPORTING  im_msgid TYPE sy-msgid
                                im_msgno TYPE sy-msgno
                                im_msgv1 TYPE sy-msgv1
                                im_msgv2 TYPE sy-msgv2
                                im_msgv3 TYPE sy-msgv3
                                im_msgv4 TYPE sy-msgv4
                     EXPORTING  ex_msg   TYPE any
                     EXCEPTIONS not_found,
      upload_pres_file IMPORTING  im_pfile TYPE localfile
                       EXPORTING  ex_data  TYPE STANDARD TABLE
                       EXCEPTIONS upload_failed,
      validate_txt_file IMPORTING  im_file TYPE localfile
                        EXCEPTIONS invalid_txt,
      validate_pres_file IMPORTING  im_pfile TYPE localfile
                         EXCEPTIONS filenotsplitted
                                    invalid_directory
                                    invalid_filename,
      split_file_path IMPORTING  im_file  TYPE localfile
                      EXPORTING  ex_dir   TYPE any
                                 ex_fname TYPE any
                      EXCEPTIONS filenotsplitted.
  PRIVATE SECTION.
    DATA use_header_line TYPE abap_bool.
ENDCLASS. "lcl_cond DEFINITION
*----------------------------------------------------------------------*
* CLASS lcl_cond IMPLEMENTATION
*----------------------------------------------------------------------*
* Local Class
*----------------------------------------------------------------------*
CLASS lcl_cond IMPLEMENTATION.
  METHOD constructor.
    v_repid = sy-repid.
    me->use_header_line = use_header_line.
  ENDMETHOD. "constructor
  METHOD f4_help.
    DATA: lt_table TYPE filetable,
          lv_rc    TYPE i.
    FIELD-SYMBOLS: <lfs_table> TYPE file_table.
    CALL METHOD cl_gui_frontend_services=>file_open_dialog
      EXPORTING
        default_extension       = c_text
      CHANGING
        file_table              = lt_table
        rc                      = lv_rc
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        OTHERS                  = 5.
    IF sy-subrc <> 0.
      MESSAGE 'Error in value-request for upload file'(020) TYPE c_i.
      LEAVE LIST-PROCESSING.
    ELSE.
      READ TABLE lt_table ASSIGNING <lfs_table> INDEX 1.
      IF <lfs_table> IS ASSIGNED.
        p_file = <lfs_table>-filename.
      ENDIF.
    ENDIF.
  ENDMETHOD. "f4_help
  METHOD f4_ehelp.
    DATA lv_path TYPE string.
    CALL METHOD cl_gui_frontend_services=>directory_browse
      CHANGING
        selected_folder      = lv_path
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4.
    IF sy-subrc = 0.
      p_efile = lv_path.
    ELSE.
      MESSAGE 'Error in value-request for download path'(028) TYPE c_i.
    ENDIF.
  ENDMETHOD. "f4_ehelp
  METHOD validate_file.
    DATA: lv_result TYPE abap_bool,
          lv_file   TYPE string.
    lv_file = p_file.
    CALL METHOD cl_gui_frontend_services=>file_exist
      EXPORTING
        file                 = lv_file
      RECEIVING
        result               = lv_result
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        wrong_parameter      = 3
        not_supported_by_gui = 4
        OTHERS               = 5.
    IF sy-subrc <> 0 OR ( sy-subrc = 0 AND lv_result = space ).
      MESSAGE 'Invalid file path'(024) TYPE c_e.
    ENDIF.
  ENDMETHOD. "validate_file
  METHOD validate_efile.
    DATA: lv_file   TYPE string,
          lv_result TYPE abap_bool.
    IF p_error = abap_true.
      IF p_efile IS INITIAL.
        MESSAGE 'Enter a valid file path'(029) TYPE c_e.
      ENDIF.
      lv_file = p_efile.
      CALL METHOD cl_gui_frontend_services=>directory_exist
        EXPORTING
          directory            = lv_file
        RECEIVING
          result               = lv_result
        EXCEPTIONS
          cntl_error           = 1
          error_no_gui         = 2
          wrong_parameter      = 3
          not_supported_by_gui = 4
          OTHERS               = 5.
      IF sy-subrc <> 0 OR ( sy-subrc = 0 AND lv_result = space ).
        MESSAGE 'Invalid error file path'(030) TYPE c_e.
      ENDIF.
    ENDIF.
  ENDMETHOD. "validate_efile
  METHOD sos.
*~~No File Path is given
    IF p_file IS INITIAL.
      MESSAGE 'File path missing'(010) TYPE c_s DISPLAY LIKE c_e.
      LEAVE LIST-PROCESSING.
    ENDIF.
    upload_data( ).
    CHECK v_flag IS INITIAL.
    get_partner_num( ).
    IF t_main IS NOT INITIAL AND " upload data
    v_error IS INITIAL. " error flag
*~~Get Check Data for Condition Type & Iso codes
      get_check_data( ).
*~~Process Upload Records
      process_data( ).
    ENDIF.
  ENDMETHOD. "sos
  METHOD eos.
    CHECK t_main IS NOT INITIAL.
*~~If no upload error occured
    IF v_flag = space.
*~~If error record(s) download is requested
      IF p_error = abap_true.
        IF t_error IS INITIAL.
          MESSAGE 'No error record(s) to be downloaded'(033) TYPE c_i.
        ELSE.
          download_error_records( ).
        ENDIF.
      ENDIF.
*~~If no log is there to be displayed
      IF t_log IS INITIAL.
        MESSAGE 'No data to display'(002) TYPE c_i.
      ELSE.
*~~Display complete log in ALV
        display_alv( ).
      ENDIF.
    ENDIF.
  ENDMETHOD. "eos
  METHOD upload_data.
    CLEAR : t_main, v_flag.

    DATA t_file TYPE STANDARD TABLE OF string.
    DATA entry TYPE string.
    DATA l_main TYPE ty_main.
    DATA l_fields TYPE STANDARD TABLE OF string.
    DATA l_field TYPE string.
    DATA l_start_index TYPE i.
    FIELD-SYMBOLS <value> TYPE any.

    IF use_header_line = abap_true.
      l_start_index = 2.
    ELSE.
      l_start_index = 1.
    ENDIF.

    CALL METHOD me->upload_pres_file
      EXPORTING
        im_pfile      = p_file
      IMPORTING
        ex_data       = t_file
      EXCEPTIONS
        upload_failed = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE c_i NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      v_flag = abap_true.
    ELSE.
      LOOP AT t_file INTO entry FROM l_start_index.
        CLEAR l_main.
        SPLIT entry AT ';' INTO TABLE l_fields.
        LOOP AT l_fields INTO l_field.
          ASSIGN COMPONENT sy-tabix OF STRUCTURE l_main TO <value>.
          IF sy-subrc = 0.
            <value> = l_field.
          ENDIF.
        ENDLOOP.
        APPEND l_main TO t_main.
      ENDLOOP.
    ENDIF.
  ENDMETHOD. "upload_data
  METHOD get_partner_num.
    CLEAR : v_partn, v_port, v_error.
    CONCATENATE c_sap sy-sysid INTO v_port.
    SELECT SINGLE logsys FROM t000 INTO v_partn
    WHERE mandt = sy-mandt.
    IF sy-subrc <> 0.
      v_error = abap_true.
      CLEAR v_key.
      CALL METHOD append_log
        EXPORTING
          im_tabix = '0'
          im_key   = v_key
          im_type  = c_e
          im_msg   = 'No Partner Function Found'(003).
      t_error = t_main.
    ENDIF.
  ENDMETHOD. "get_partner_num
  METHOD get_check_data.
    DATA li_main TYPE STANDARD TABLE OF ty_main.
    CLEAR : t_t685, t_t006.
*~~Application
    li_main = t_main.
    SORT li_main BY kappl.
    DELETE ADJACENT DUPLICATES FROM li_main COMPARING kappl.
    IF li_main IS NOT INITIAL.
      SELECT kappl FROM t681a INTO TABLE t_t681a
      FOR ALL ENTRIES IN li_main
      WHERE kappl = li_main-kappl.
      IF sy-subrc = 0.
        SORT t_t681a BY kappl.
      ENDIF.
    ENDIF.
*~~Condition Type
    CLEAR li_main.
    li_main = t_main.
    SORT li_main BY kappl kschl.
    DELETE ADJACENT DUPLICATES FROM li_main COMPARING kappl kschl.
    IF li_main IS NOT INITIAL.
      SELECT kschl FROM t685 INTO TABLE t_t685
      FOR ALL ENTRIES IN li_main
      WHERE kappl = li_main-kappl
      AND kschl = li_main-kschl.
      IF sy-subrc = 0.
        SORT t_t685 BY kschl.
      ENDIF.
    ENDIF.
*~~Iso codes
    CLEAR li_main.
    li_main = t_main.
    SORT li_main BY meins.
    DELETE ADJACENT DUPLICATES FROM li_main COMPARING meins.
    IF li_main IS NOT INITIAL.
      SELECT msehi isocode FROM t006 INTO TABLE t_t006
      FOR ALL ENTRIES IN li_main
      WHERE msehi = li_main-meins.
      IF sy-subrc = 0.
        SORT t_t006 BY msehi.
      ENDIF.
    ENDIF.
  ENDMETHOD. "get_check_data
  METHOD process_data.
    UNASSIGN <fs_main>.
    CLEAR : o_e1komg, o_e1konh, o_e1konp, o_e1konm.
    CREATE DATA:
      o_e1komg TYPE (c_komg),
      o_e1konh TYPE (c_konh),
      o_e1konp TYPE (c_konp),
      o_e1konm TYPE (c_konm).
    IF  o_e1komg IS NOT INITIAL
    AND o_e1konh IS NOT INITIAL
    AND o_e1konp IS NOT INITIAL
    AND o_e1konm IS NOT INITIAL.
      UNASSIGN: <fs_komg>, <fs_konh>, <fs_konp>, <fs_konm>.
      ASSIGN:
        o_e1komg->* TO <fs_komg>,
        o_e1konh->* TO <fs_konh>,
        o_e1konp->* TO <fs_konp>,
        o_e1konm->* TO <fs_konm>.
    ENDIF.
    CLEAR t_error.
    LOOP AT t_main ASSIGNING <fs_main>.
      CLEAR : v_tabix, v_error, v_vakey.
      IF  <fs_komg> IS ASSIGNED
      AND <fs_konh> IS ASSIGNED
      AND <fs_konp> IS ASSIGNED
      AND <fs_konm> IS ASSIGNED.
        CLEAR:
          <fs_komg>,
          <fs_konh>,
          <fs_konp>,
          <fs_konm>.
      ENDIF.
      v_tabix = sy-tabix.
      get_table_details( ).
      validate_data( ).
      IF v_error = space.
        populate_idoc_data( ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD. "process_data
  METHOD download_error_records.
    CLEAR v_efile.
    CONCATENATE p_efile '\Error_' sy-datum sy-uzeit '.txt'
    INTO v_efile.
    CALL METHOD cl_gui_frontend_services=>gui_download
      EXPORTING
        filename                = v_efile
        write_field_separator   = abap_true
      CHANGING
        data_tab                = t_error
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        not_supported_by_gui    = 22
        error_no_gui            = 23
        OTHERS                  = 24.
    IF sy-subrc <> 0.
      MESSAGE 'Error downloading error record(s)'(031) TYPE c_i.
    ENDIF.
  ENDMETHOD. "download_error_records
  METHOD display_alv.
    IF t_log IS INITIAL.
      MESSAGE 'No log to be displayed'(017) TYPE c_i.
      LEAVE LIST-PROCESSING.
    ENDIF.
    create_fcat( ).
    alv( ).
  ENDMETHOD. "display_alv
  METHOD create_fcat.
    append_fcat( EXPORTING im_field = 'TYPE' im_text = 'Message Type'(018)
    ).
    append_fcat( EXPORTING im_field = 'TABIX' im_text = 'Record No'(019) ).
    append_fcat( EXPORTING im_field = 'KEY' im_text = 'Record Key'(021) ).
    append_fcat( EXPORTING im_field = 'MSG' im_text = 'Message'(022) ).
  ENDMETHOD. "create_fcat
  METHOD append_fcat.
    DATA lw_fcat TYPE slis_fieldcat_alv.
    lw_fcat-fieldname = im_field.
    lw_fcat-seltext_s = lw_fcat-seltext_m = lw_fcat-seltext_l = im_text.
    lw_fcat-ddictxt = 'L'.
    lw_fcat-tabname = 'T_LOG'.
    APPEND lw_fcat TO t_fcat.
    CLEAR lw_fcat.
  ENDMETHOD. "append_fcat
  METHOD alv.
    DATA : lw_layout TYPE slis_layout_alv.
    lw_layout-colwidth_optimize = abap_true.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program     = v_repid
        i_callback_top_of_page = 'TOP_OF_PAGE'
        is_layout              = lw_layout
        it_fieldcat            = t_fcat
      TABLES
        t_outtab               = t_log
      EXCEPTIONS
        program_error          = 1
        OTHERS                 = 2.
    IF sy-subrc <> 0.
      MESSAGE 'Error displaying ALV' TYPE c_i.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDMETHOD. "alv
  METHOD get_table_details.
    FIELD-SYMBOLS <lfs_ddfields> TYPE STANDARD TABLE.
    CLEAR t_ddfields.
    UNASSIGN <fs_t006>.
    IF <fs_main> IS ASSIGNED.
      READ TABLE t_t006 ASSIGNING <fs_t006> WITH KEY msehi = <fs_main>-meins BINARY SEARCH.
      IF <fs_t006> IS ASSIGNED.
        <fs_main>-meins = <fs_t006>-isocode.
      ENDIF.
      UNASSIGN <fs_table>.
      READ TABLE t_table ASSIGNING <fs_table> WITH KEY table = <fs_main>-table.
      IF <fs_table> IS ASSIGNED.
        UNASSIGN <lfs_ddfields>.
        ASSIGN <fs_table>-info->* TO <lfs_ddfields>.
        IF <lfs_ddfields> IS ASSIGNED.
          t_ddfields = <lfs_ddfields>.
        ENDIF.
      ELSE.
        APPEND INITIAL LINE TO t_table ASSIGNING <fs_table>.
        IF <fs_table> IS ASSIGNED.
          <fs_table>-table = <fs_main>-table.
          UNASSIGN <lfs_ddfields>.
          CREATE DATA <fs_table>-info TYPE ddfields.
          ASSIGN <fs_table>-info->* TO <lfs_ddfields>.
        ENDIF.
      ENDIF.
      IF t_ddfields IS NOT INITIAL.
        RETURN.
      ENDIF.
      CALL FUNCTION 'CATSXT_GET_DDIC_FIELDINFO'
        EXPORTING
          im_structure_name = <fs_main>-table
        IMPORTING
          ex_ddic_info      = t_ddfields
        EXCEPTIONS
          failed            = 1
          OTHERS            = 2.
      IF sy-subrc <> 0.
        v_error = abap_true.
        CLEAR v_key.
        v_key = <fs_main>-table.
        CALL METHOD me->append_log
          EXPORTING
            im_tabix = v_tabix
            im_key   = v_key
            im_type  = c_e
            im_msg   = 'Invalid Condition Table'(004).
*~~Append Error
        APPEND <fs_main> TO t_error.
      ENDIF.
      IF <lfs_ddfields> IS ASSIGNED.
        <lfs_ddfields> = t_ddfields.
      ENDIF.
    ENDIF.
  ENDMETHOD. "get_table_details
  METHOD validate_data.
    DATA : lv_count TYPE i,
           lv_tabix TYPE sytabix,
           lv_flag  TYPE char1.
    FIELD-SYMBOLS <lfs_dfies> TYPE any.
*~~Check for APPLICATION
    UNASSIGN <fs_t681a>.
    READ TABLE t_t681a ASSIGNING <fs_t681a> WITH KEY kappl = <fs_main>-kappl BINARY SEARCH.
    IF <fs_t681a> IS NOT ASSIGNED.
      v_error = abap_true.
      CLEAR v_key.
      CONCATENATE <fs_main>-kschl <fs_main>-table INTO v_key SEPARATED BY c_sep.
      CALL METHOD me->append_log
        EXPORTING
          im_tabix = v_tabix
          im_key   = v_key
          im_type  = c_e
          im_msg   = 'Invalid Application'(005).
*~~Append Error
      APPEND <fs_main> TO t_error.
    ENDIF.
*~~Check for CONDITION TYPE
    UNASSIGN <fs_t685>.
    READ TABLE t_t685 ASSIGNING <fs_t685> WITH KEY kschl = <fs_main>-kschl BINARY SEARCH.
    IF <fs_t685> IS NOT ASSIGNED.
      v_error = abap_true.
      CLEAR v_key.
      CONCATENATE <fs_main>-kschl <fs_main>-table INTO v_key SEPARATED BY c_sep.
      CALL METHOD me->append_log
        EXPORTING
          im_tabix = v_tabix
          im_key   = v_key
          im_type  = c_e
          im_msg   = 'Invalid Condition Type'(006).
*~~Append Error
      APPEND <fs_main> TO t_error.
    ENDIF.
*~~Check for FIELD CONTINUITY
    lv_count = 4.
    lv_tabix = 4.
    CLEAR : sy-index, lv_flag, v_leng, v_len.
    WHILE sy-index <= 11.
      UNASSIGN <fs_field>.
      ASSIGN COMPONENT lv_count OF STRUCTURE <fs_main> TO <fs_field>.
      IF <fs_field> IS ASSIGNED.
        IF <fs_field> IS INITIAL.
          lv_flag = abap_true.
        ELSE.
          IF lv_flag = abap_true.
            CLEAR v_key.
            v_key = <fs_main>-kschl.
            CALL METHOD me->append_log
              EXPORTING
                im_tabix = v_tabix
                im_key   = v_key
                im_type  = c_e
                im_msg   = 'Discontinuity in Variable Key Fields'(007).
            v_error = abap_true.
*~~Append Error
            APPEND <fs_main> TO t_error.
            EXIT.
          ELSE.
            CLEAR o_data.
            READ TABLE t_ddfields ASSIGNING <fs_dfies> INDEX lv_tabix.
            IF <fs_dfies> IS ASSIGNED.
              CREATE DATA o_data TYPE (<fs_dfies>-rollname).
              IF o_data IS BOUND.
                ASSIGN o_data->* TO <fs_value>.
                IF <fs_value> IS ASSIGNED.
                  <fs_value> = <fs_field>.
                  IF sy-index = 1.
                    CLEAR v_leng.
                  ELSE.
                    v_leng = v_leng + v_len.
                  ENDIF.
                  IF <fs_dfies>-convexit IS NOT INITIAL.
                    conversion_routine( EXPORTING im_convexit = <fs_dfies>-convexit
                    im_leng = <fs_dfies>-leng ).
                  ELSE.
                    CLEAR v_len.
                    v_len = <fs_dfies>-leng.
                    v_vakey+v_leng(v_len) = <fs_value>.
                  ENDIF.
*~~Replace value of VRKME by T006-ISOCODE value
                  pass_isocode_for_vrkme( ).
*~~Assign the field value in KOMG structure
                  UNASSIGN <lfs_dfies>.
                  ASSIGN COMPONENT <fs_dfies>-fieldname OF STRUCTURE <fs_komg> TO <lfs_dfies>.
                  IF <lfs_dfies> IS ASSIGNED.
                    <lfs_dfies> = <fs_value>.
                  ENDIF.
*~~Assign the field value in KONH structure
                  UNASSIGN <lfs_dfies>.
                  ASSIGN COMPONENT <fs_dfies>-fieldname OF STRUCTURE <fs_konh> TO <lfs_dfies>.
                  IF <lfs_dfies> IS ASSIGNED.
                    <lfs_dfies> = <fs_value>.
                  ENDIF.
*~~Assign the field value in KONP structure
                  UNASSIGN <lfs_dfies>.
                  ASSIGN COMPONENT <fs_dfies>-fieldname OF STRUCTURE <fs_konp> TO <lfs_dfies>.
                  IF <lfs_dfies> IS ASSIGNED.
                    <lfs_dfies> = <fs_value>.
                  ENDIF.
*~~Assign the field value in KONM structure
                  UNASSIGN <lfs_dfies>.
                  ASSIGN COMPONENT <fs_dfies>-fieldname OF STRUCTURE <fs_konm> TO <lfs_dfies>.
                  IF <lfs_dfies> IS ASSIGNED.
                    <lfs_dfies> = <fs_value>.
                  ENDIF.
*~~Pass Vistex Data if required
                  pass_vistex( ).
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
      lv_count = lv_count + 1.
      lv_tabix = lv_tabix + 1.
    ENDWHILE.
*~~Check variable key not more than 100 digits
    CLEAR lv_count.
    lv_count = strlen( v_vakey ).
    IF lv_count > 100.
      v_error = abap_true.
      CLEAR v_key.
      CONCATENATE <fs_main>-kschl <fs_main>-table v_vakey
      INTO v_key SEPARATED BY c_sep.
      CALL METHOD me->append_log
        EXPORTING
          im_tabix = v_tabix
          im_key   = v_key
          im_type  = c_e
          im_msg   = 'Variable Key too big'(008).
*~~Append Error
      APPEND <fs_main> TO t_error.
    ENDIF.
  ENDMETHOD. "validate_data
  METHOD pass_isocode_for_vrkme.
    IF <fs_dfies>-fieldname = c_vrkme.
      UNASSIGN <fs_t006>.
      READ TABLE t_t006 ASSIGNING <fs_t006>
      WITH KEY msehi = <fs_value> BINARY SEARCH.
      IF <fs_t006> IS ASSIGNED.
        <fs_value> = <fs_t006>-isocode.
      ENDIF.
    ENDIF.
  ENDMETHOD. "pass_isocode_for_vrkme
  METHOD pass_vistex.
    FIELD-SYMBOLS <lx_idoc> TYPE any.
    UNASSIGN <lx_idoc>.
    ASSIGN COMPONENT <fs_dfies>-fieldname OF STRUCTURE <fs_komg> TO <lx_idoc>.
    IF <lx_idoc> IS ASSIGNED.
      <lx_idoc> = <fs_value>.
    ENDIF.
    UNASSIGN <lx_idoc>.
    ASSIGN COMPONENT <fs_dfies>-fieldname OF STRUCTURE <fs_konh> TO <lx_idoc>.
    IF <lx_idoc> IS ASSIGNED.
      <lx_idoc> = <fs_value>.
    ENDIF.
    UNASSIGN <lx_idoc>.
    ASSIGN COMPONENT <fs_dfies>-fieldname OF STRUCTURE <fs_konp> TO <lx_idoc>.
    IF <lx_idoc> IS ASSIGNED.
      <lx_idoc> = <fs_value>.
    ENDIF.
    ASSIGN COMPONENT <fs_dfies>-fieldname OF STRUCTURE <fs_konm> TO <lx_idoc>.
    IF <lx_idoc> IS ASSIGNED.
      <lx_idoc> = <fs_value>.
    ENDIF.
  ENDMETHOD. "pass_vistex
  METHOD conversion_routine.
    DATA lv_func TYPE rs38l_fnam.
    CONCATENATE c_conv im_convexit c_input INTO lv_func.
    UNASSIGN <fs_cexit>.
    READ TABLE t_cexit ASSIGNING <fs_cexit> WITH KEY funcname = lv_func.
    IF <fs_cexit> IS NOT ASSIGNED.
      SELECT funcname parameter paramtype FROM fupararef
      APPENDING TABLE t_cexit
      WHERE funcname = lv_func.
      IF sy-subrc = 0.
        SORT t_cexit BY funcname.
      ENDIF.
    ENDIF.
    call_conv_exit(
      EXPORTING
        im_func = lv_func
        im_leng = im_leng ).
  ENDMETHOD. "conversion_routine
  METHOD call_conv_exit.
    DATA lv_msg TYPE bapi_msg.
    DATA : li_ptab TYPE abap_func_parmbind_tab,
           lw_ptab TYPE abap_func_parmbind,
           li_etab TYPE abap_func_excpbind_tab,
           lw_etab TYPE abap_func_excpbind.
    UNASSIGN <fs_cexit>.
    LOOP AT t_cexit ASSIGNING <fs_cexit> WHERE funcname = im_func.
      CASE <fs_cexit>-paramtype.
        WHEN 'I'.
          IF <fs_cexit>-parameter = c_inp.
            lw_ptab-name = <fs_cexit>-parameter.
            lw_ptab-kind = abap_func_exporting.
            GET REFERENCE OF <fs_value> INTO lw_ptab-value.
            INSERT lw_ptab INTO TABLE li_ptab.
          ENDIF.
        WHEN 'E'.
          IF <fs_cexit>-parameter = c_out.
            lw_ptab-name = <fs_cexit>-parameter.
            lw_ptab-kind = abap_func_importing.
            GET REFERENCE OF <fs_value> INTO lw_ptab-value.
            INSERT lw_ptab INTO TABLE li_ptab.
          ENDIF.
        WHEN 'X'.
          lw_etab-name = c_other.
          lw_etab-value = 1.
          INSERT lw_etab INTO TABLE li_etab.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.
    IF sy-subrc = 0.
      CALL FUNCTION im_func
        PARAMETER-TABLE
        li_ptab
        EXCEPTION-TABLE
        li_etab.
      CASE sy-subrc.
        WHEN 0.
          CLEAR v_len.
          v_len = im_leng.
          v_vakey+v_leng(v_len) = <fs_value>.
        WHEN 1.
          v_error = abap_true.
          CALL METHOD me->format_message
            EXPORTING
              im_msgid  = sy-msgid
              im_msgno  = sy-msgno
              im_msgv1  = sy-msgv1
              im_msgv2  = sy-msgv2
              im_msgv3  = sy-msgv3
              im_msgv4  = sy-msgv4
            IMPORTING
              ex_msg    = lv_msg
            EXCEPTIONS
              not_found = 1
              OTHERS    = 2.
          IF sy-subrc <> 0.
            CLEAR lv_msg.
            CONCATENATE 'Error occured while using Conversion Exit'(009) im_func
            INTO lv_msg SEPARATED BY space.
          ENDIF.
          CLEAR v_key.
          CONCATENATE <fs_main>-kschl <fs_main>-table INTO v_key SEPARATED BY c_sep.
          CALL METHOD me->append_log
            EXPORTING
              im_tabix = v_tabix
              im_key   = v_key
              im_type  = c_e
              im_msg   = lv_msg.
*~~Append Error
          APPEND <fs_main> TO t_error.
      ENDCASE.
    ENDIF.
  ENDMETHOD. "call_conv_exit
  METHOD populate_idoc_data.
*~~Populate IDOC Control Record
    pass_control_record( ).
*~~Populate IDOC Data Record
    pass_data_record( ).
*~~Post the data in IDOC
    post_data( ).
  ENDMETHOD. "populate_idoc_data
  METHOD pass_control_record.
    CLEAR : w_edidc, t_edidc.
    w_edidc-rcvpor = v_port.
    w_edidc-rcvprt = c_ls.
    w_edidc-rcvprn = v_partn.
    w_edidc-sndpor = v_port.
    w_edidc-sndprt = c_ls.
    w_edidc-sndprn = v_partn.
    w_edidc-doctyp = c_dctyp.
    w_edidc-mestyp = c_mstyp.
    w_edidc-idoctp = c_bstyp.
    w_edidc-direct = c_in.
    APPEND w_edidc TO t_edidc.
    CLEAR w_edidc.
  ENDMETHOD. "pass_control_record
  METHOD pass_data_record.
    CLEAR : w_edidd, t_edidd, w_e1komg, w_e1konh, w_e1konp,
    v_segnm, v_hlevl.
    IF <fs_komg> IS ASSIGNED.
      w_e1komg = <fs_komg>.
    ENDIF.
    IF <fs_konh> IS ASSIGNED.
      w_e1konh = <fs_konh>.
    ENDIF.
    IF <fs_konp> IS ASSIGNED.
      w_e1konp = <fs_konp>.
    ENDIF.
    IF <fs_konm> IS ASSIGNED.
      w_e1konm = <fs_konm>.
    ENDIF.
*~~Initialize the Segment Number and Higher Level
    v_segnm = c_segnm.
    v_hlevl = c_hlevl.
*~~Pass the data for E1KOMG
    w_edidd-segnum = v_segnm.
    w_edidd-segnam = c_komg.
    w_edidd-hlevel = v_hlevl.
    w_e1komg-kvewe = <fs_main>-table+0(1).
    w_e1komg-kotabnr = <fs_main>-table+1(3).
    w_e1komg-kappl = <fs_main>-kappl.
    w_e1komg-kschl = <fs_main>-kschl.
    w_e1komg-vakey = v_vakey.
    w_edidd-sdata = w_e1komg.
    APPEND w_edidd TO t_edidd.
    CLEAR w_edidd.
*~~Increment the Segment Number
    v_segnm = v_segnm + 1.
*~~Pass the data for E1KONH
    w_edidd-segnum = v_segnm.
    w_edidd-segnam = c_konh.
    w_edidd-hlevel = v_hlevl.
    w_e1konh-datab = <fs_main>-datab.
    w_e1konh-datbi = <fs_main>-datbi.
    w_edidd-sdata = w_e1konh.
    APPEND w_edidd TO t_edidd.
    CLEAR w_edidd.
*~~Increment the Segment Number & Higher Level
    v_segnm = v_segnm + 1.
    v_hlevl = v_hlevl + 1.
*~~Pass the data for E1KONP
    w_edidd-segnum = v_segnm.
    w_edidd-segnam = c_konp.
    w_edidd-hlevel = v_hlevl.
    w_e1konp-kschl = <fs_main>-kschl.
    w_e1konp-kbetr = <fs_main>-kbetr.
    w_e1konp-kpein = <fs_main>-kpein.
    w_e1konp-kmein = <fs_main>-meins.
    w_e1konp-krech = <fs_main>-krech.
    CASE w_e1konp-krech.
      WHEN c_a.
        w_e1konp-kzbzg = c_b.
        w_e1konp-konwa = c_per.
      WHEN c_c.
        w_e1konp-kzbzg = c_c.
        w_e1konp-konwa = <fs_main>-waers.
    ENDCASE.
    w_edidd-sdata = w_e1konp.
    APPEND w_edidd TO t_edidd.
    CLEAR w_edidd.
    CLEAR w_edidd.
    v_segnm = v_segnm + 1.
    v_hlevl = v_hlevl + 1.
    w_edidd-segnum = v_segnm.
    w_edidd-segnam = c_konm.
    w_edidd-hlevel = v_hlevl.
    w_e1konm-kstbm = <fs_main>-kstbm.
    w_e1konm-kbetr = <fs_main>-kbetr.
    w_edidd-sdata = w_e1konm.
    APPEND w_edidd TO t_edidd.
    CLEAR w_edidd.
  ENDMETHOD. "pass_data_record
  METHOD post_data.
*~~Update using DIRECT UPDATE
    direct_update( ).
  ENDMETHOD. "post_data
  METHOD direct_update.
    DATA: lv_msg  TYPE bapi_msg,
          lv_type TYPE msgty.
    CLEAR : t_edids, t_return, t_bdi_ser.
*~~Pass the details to the generated IDOC Number
    CALL FUNCTION 'IDOC_INPUT_COND_A'
      EXPORTING
        input_method          = space
        mass_processing       = space
      TABLES
        idoc_contrl           = t_edidc
        idoc_data             = t_edidd
        idoc_status           = t_edids
        return_variables      = t_return
        serialization_info    = t_bdi_ser
      EXCEPTIONS
        wrong_function_called = 1
        OTHERS                = 2.
    IF sy-subrc <> 0.
      CLEAR lv_msg.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
      INTO lv_msg.
      CLEAR v_key.
      CONCATENATE <fs_main>-kschl <fs_main>-table v_vakey
      INTO v_key SEPARATED BY c_sep.
      CALL METHOD me->append_log
        EXPORTING
          im_tabix = v_tabix
          im_key   = v_key
          im_type  = c_e
          im_msg   = lv_msg.
*~~Append Error
      APPEND <fs_main> TO t_error.
    ELSE.
      UNASSIGN <fs_edids>.
      LOOP AT t_edids ASSIGNING <fs_edids>.
        CLEAR lv_msg.
        IF <fs_edids>-status = c_53.
          lv_msg = 'Changes done successfully'(011).
          lv_type = c_s.
        ELSE.
          MESSAGE ID <fs_edids>-msgid
                TYPE <fs_edids>-msgty
              NUMBER <fs_edids>-msgno
                WITH <fs_edids>-msgv1
                     <fs_edids>-msgv2
                     <fs_edids>-msgv3
                     <fs_edids>-msgv4
          INTO lv_msg.
          lv_type = c_e.
*~~Append Error
          APPEND <fs_main> TO t_error.
        ENDIF.
        CLEAR v_key.
        CONCATENATE <fs_main>-kschl <fs_main>-table v_vakey
        INTO v_key SEPARATED BY c_sep.
        CALL METHOD me->append_log
          EXPORTING
            im_tabix = v_tabix
            im_key   = v_key
            im_type  = lv_type
            im_msg   = lv_msg.
      ENDLOOP.
    ENDIF.
  ENDMETHOD. "direct_update
  METHOD append_log.
    DATA lw_log TYPE ty_log.
    lw_log-tabix = im_tabix.
    lw_log-key = im_key.
    lw_log-type = im_type.
    lw_log-msg = im_msg.
    APPEND lw_log TO t_log.
    CLEAR lw_log.
  ENDMETHOD. "append_log
  METHOD format_message.
    CALL FUNCTION 'FORMAT_MESSAGE'
      EXPORTING
        id        = im_msgid
        lang      = sy-langu
        no        = im_msgno
        v1        = im_msgv1
        v2        = im_msgv2
        v3        = im_msgv3
        v4        = im_msgv4
      IMPORTING
        msg       = ex_msg
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      CLEAR ex_msg.
      RAISE not_found.
    ENDIF.
  ENDMETHOD. "format_message
  METHOD upload_pres_file.
    DATA : lv_file TYPE string.
*~~Clear Log & File Name
    CLEAR : t_log, lv_file.
    lv_file = im_pfile.
*~~Check file for a TXT File
    CALL METHOD me->validate_txt_file
      EXPORTING
        im_file     = im_pfile
      EXCEPTIONS
        invalid_txt = 1
        OTHERS      = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
      RAISING upload_failed.
    ENDIF.
*~~Validate File Path and File Name
    CALL METHOD me->validate_pres_file
      EXPORTING
        im_pfile          = im_pfile
      EXCEPTIONS
        filenotsplitted   = 1
        invalid_directory = 2
        invalid_filename  = 3
        OTHERS            = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
      RAISING upload_failed.
    ENDIF.
    CALL METHOD cl_gui_frontend_services=>gui_upload
      EXPORTING
        filename                = lv_file
        has_field_separator     = abap_true
      CHANGING
        data_tab                = ex_data
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        not_supported_by_gui    = 17
        error_no_gui            = 18
        OTHERS                  = 19.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
      RAISING upload_failed.
    ENDIF.
*~~Check for number of records upload
    IF ex_data IS INITIAL.
      MESSAGE 'No data found in the upload file'(012) TYPE c_i.
    ENDIF.
  ENDMETHOD. "upload_pres_file
  METHOD validate_txt_file.
    DATA : lv_len  TYPE int4,
           lv_file TYPE char4.
    lv_len = strlen( im_file ).
    lv_len = lv_len - 3.
    lv_file = im_file+lv_len(3).
    CALL FUNCTION '/SAPDII/CONVERT_CASE'
      EXPORTING
        if_input  = lv_file
        if_langu  = sy-langu
        if_tocase = c_u
      IMPORTING
        ef_output = lv_file.
    IF lv_file <> c_text.
      MESSAGE 'Invalid Text File'(013) TYPE c_i RAISING invalid_txt.
    ENDIF.
  ENDMETHOD. "VALIDATE_TXT_FILE
  METHOD validate_pres_file.
    DATA : lv_res   TYPE abap_bool,
           lv_fname TYPE string,
           lv_dir   TYPE string.
*~~Split FilePath and FileName
    CALL METHOD me->split_file_path
      EXPORTING
        im_file         = im_pfile
      IMPORTING
        ex_dir          = lv_dir
        ex_fname        = lv_fname
      EXCEPTIONS
        filenotsplitted = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
      RAISING filenotsplitted.
    ENDIF.
*--Method to Validate Directory
    CALL METHOD cl_gui_frontend_services=>directory_exist
      EXPORTING
        directory            = lv_dir
      RECEIVING
        result               = lv_res
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        wrong_parameter      = 3
        not_supported_by_gui = 4
        OTHERS               = 5.
    IF lv_res IS INITIAL.
      MESSAGE 'Invalid Directory'(014) TYPE c_i RAISING invalid_directory.
    ELSE.
      CLEAR lv_res.
*--Method to Validate File
      CALL METHOD cl_gui_frontend_services=>file_exist
        EXPORTING
          file                 = lv_fname
        RECEIVING
          result               = lv_res
        EXCEPTIONS
          cntl_error           = 1
          error_no_gui         = 2
          wrong_parameter      = 3
          not_supported_by_gui = 4
          OTHERS               = 5.
      IF lv_res IS INITIAL.
        MESSAGE 'Invalid Filename'(015) TYPE c_i RAISING invalid_filename.
      ENDIF.
    ENDIF.
  ENDMETHOD. "validate_pres_file
  METHOD split_file_path.
*--FM to split File Path into Directory and File Name
    CALL FUNCTION 'SO_SPLIT_FILE_AND_PATH'
      EXPORTING
        full_name     = im_file
      IMPORTING
        stripped_name = ex_fname
        file_path     = ex_dir
      EXCEPTIONS
        x_error       = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      MESSAGE 'File and Path not splitted'(016) TYPE c_i RAISING filenotsplitted.
    ENDIF.
  ENDMETHOD. "split_file_path
ENDCLASS. "lcl_cond IMPLEMENTATION
