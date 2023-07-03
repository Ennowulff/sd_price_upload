*&---------------------------------------------------------------------*
*& Report ZSD_PRICING_UPLOAD
*&---------------------------------------------------------------------*
*& https://blogs.sap.com/2015/08/31/easy-and-efficient-way-of-uploading-pricing-conditions-in-sap-system-using-a-single-exclusively-designed-program/
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------
* Object ID : XXXXXX
* Program : ZSD_PRICING_UPLOAD
* Author : Tarun Gambhir
* Date : 04/28/2015
* Description: Pricing Conditions Upload Conversion for VK11/VK12/VK13
*----------------------------------------------------------------------
* MODIFICATIONS LOG
*----------------------------------------------------------------------
* Request Text
* ---------- ----------------------------------------------------------
* Initial Version
*----------------------------------------------------------------------
REPORT zsd_pricing_upload.
*----------------------------------------------------------------------
* INCLUDES
*----------------------------------------------------------------------
INCLUDE:
  zsd_pricing_upload_top, " Global Declaration
  zsd_pricing_upload_scr, " Selection Screen
  zsd_pricing_upload_cls, " Local Class
  zsd_pricing_upload_frm. " Sub-Routines

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  CREATE OBJECT o_cond
    EXPORTING
      use_header_line = p_header.
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  o_cond->f4_help( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_efile.
  o_cond->f4_ehelp( ).

AT SELECTION-SCREEN ON p_file.
  o_cond->validate_file( ).

AT SELECTION-SCREEN ON p_efile.
  o_cond->validate_efile( ).
*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  o_cond->sos( ).
*----------------------------------------------------------------------*
* END-OF-SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
  o_cond->eos( ).
