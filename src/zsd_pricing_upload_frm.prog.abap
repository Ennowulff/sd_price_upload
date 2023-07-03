*&---------------------------------------------------------------------*
*&  Include           ZSD_PRICING_UPLOAD_FRM
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Include ZSD_PRICING_UPLOAD_FRM
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* FORM TOP_OF_PAGE
*----------------------------------------------------------------------*
* Sub-Routine for Top Of Page for ALV
*----------------------------------------------------------------------*
FORM top_of_page.
  DATA: lt_top   TYPE slis_t_listheader,
        lw_top   TYPE slis_listheader,
        lv_datum TYPE char10.
*~~Header
  lw_top-typ = c_h.
  lw_top-info = 'Pricing Upload'(023).
  APPEND lw_top TO lt_top. CLEAR lw_top.
*~~Username
  lw_top-typ = c_s.
  lw_top-key = 'Username:'(025).
  lw_top-info = sy-uname.
  APPEND lw_top TO lt_top. CLEAR lw_top.
*~~Date
  lw_top-typ = c_s.
  lw_top-key = 'Date:'(026).
  WRITE sy-datum TO lv_datum.
  lw_top-info = lv_datum.
  APPEND lw_top TO lt_top. CLEAR lw_top.
*~~Error Log (if dowloaded)
  IF p_error = abap_true AND v_efile IS NOT INITIAL.
    lw_top-typ = c_s.
    lw_top-key = 'Error File Path:'(032).
    lw_top-info = v_efile.
    APPEND lw_top TO lt_top. CLEAR lw_top.
  ENDIF.
*~~Print Top of page
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_top.
ENDFORM. "top_of_page
