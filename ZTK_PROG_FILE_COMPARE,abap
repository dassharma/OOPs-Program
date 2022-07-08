*&---------------------------------------------------------------------*
*& Report ZTK_PROG_FILE_COMPARE
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
REPORT ZTK_PROG_FILE_COMPARE .

*/--------------------------------------------------------------------\*
*| LCX_EXCEPTION  = Exception Handling                                |*
*| LCL_SEL_SCREEN = Selection Screen inputs and modifications         |*
*| LCL_FILE       = All file related operations & creation of int tab |*
*| LCL_MODEL      = Data operations for that file                     |*
*| LCL_ALV_GRID   = ALV Grid                                          |*
*| LCL_VIEW       = Creation of UI to get Translation Table & Values  |*
*| LCL_LOG        = Handle the 'Compare' operation log                |*
*\--------------------------------------------------------------------/*

***********************************
* Class Definitions deferred
***********************************
CLASS lcx_exception  DEFINITION DEFERRED.
CLASS lcl_sel_screen DEFINITION DEFERRED.
CLASS lcl_file       DEFINITION DEFERRED.
CLASS lcl_model      DEFINITION DEFERRED.
CLASS lcl_alv_grid   DEFINITION DEFERRED.
CLASS lcl_view       DEFINITION DEFERRED.
CLASS lcl_log        DEFINITION DEFERRED.

***********************************
* Class: Exception Handling
***********************************
CLASS lcx_exception DEFINITION FINAL
                    INHERITING FROM cx_static_check.

*- ||
  PUBLIC SECTION.
*-- Interfaces
    INTERFACES:
      if_t100_message.

*-- Constants
    CONSTANTS:
      BEGIN OF gc_msg_type,
        success TYPE symsgty VALUE 'S',
        error   TYPE symsgty VALUE 'E',
        warning TYPE symsgty VALUE 'W',
      END OF gc_msg_type.

*-- Class Structures
    CLASS-DATA:
          ls_t100key LIKE if_t100_message=>t100key.

*-- Methods
    METHODS:
      " Constructor message
      constructor   " Raise exceptions using textid = type <SCX_T100KEY>
        IMPORTING
          !textid   LIKE if_t100_message=>t100key OPTIONAL
          !previous LIKE previous OPTIONAL.

*-- Class Methods
    CLASS-METHODS:
      " Generates the Message Text
      get_message_text
        IMPORTING
                  is_t100key        TYPE scx_t100key
        RETURNING VALUE(rv_message) TYPE bapi_msg.

ENDCLASS.

CLASS lcx_exception IMPLEMENTATION.

  METHOD constructor.

    " Calling the super method
    super->constructor(
      EXPORTING
        previous = previous
    ).

    " Maintaining the TextId
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.

    " Storing the Message details
    ls_t100key = if_t100_message~t100key.

  ENDMETHOD.

  METHOD get_message_text.

    " Generates the text for the message
    CALL FUNCTION 'MESSAGE_PREPARE'
      EXPORTING
        language               = sy-langu
        msg_id                 = is_t100key-msgid
        msg_no                 = CONV msgnr( is_t100key-msgno )
        msg_var1               = CONV balm-msgv1( is_t100key-attr1 )
        msg_var2               = CONV balm-msgv2( is_t100key-attr2 )
        msg_var3               = CONV balm-msgv3( is_t100key-attr3 )
        msg_var4               = CONV balm-msgv4( is_t100key-attr4 )
      IMPORTING
        msg_text               = rv_message
      EXCEPTIONS
        function_not_completed = 1
        message_not_found      = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      CLEAR rv_message.
    ENDIF.

  ENDMETHOD.

ENDCLASS.


***********************************
* Class: Selection Screen
***********************************
CLASS lcl_sel_screen DEFINITION FINAL.

  PUBLIC SECTION.
*- Structure Types

*- Range Types

*- Structure

*- Variables
    DATA:
      gv_file_path1  TYPE hrpadkz_filename,
      gv_file_path2  TYPE hrpadkz_filename,
      gv_headlines_1 TYPE line_num,
      gv_headlines_2 TYPE line_num,
      gv_separator   TYPE usmd_sep.

*- Range Internal Table
    DATA:
      gt_r_keyfields1 TYPE RANGE OF cifkeydesc,
      gt_r_keyfields2 TYPE RANGE OF cifkeydesc.

*- Class Data
    CLASS-DATA:
      gv_keyfields TYPE cifkeydesc.

*- Constants
    CONSTANTS:
      lc_ucomm_onli TYPE syucomm VALUE 'ONLI'.

*- Methods
    METHODS:
      " Maintains the Selection Screen inputs into instance attributes
      maintain_selection_inputs,

      " Restricts the Select Option functionality
      restrict_select_options,

      " Validates the Keyfields
      validate_keyfields
        IMPORTING
          iv_ucomm TYPE syucomm
        RAISING
          lcx_exception.


*-||
  PRIVATE SECTION.
*- Internal Tables
    DATA:
      lt_selopts_255 TYPE STANDARD TABLE OF rsparamsl_255
                          WITH KEY selname.

*- Method
    METHODS:
      " Gets Range values
      get_value_range
        IMPORTING
          iv_name  TYPE rsparamsl_255-selname
        CHANGING
          ct_range TYPE STANDARD TABLE,

      " Gets Parameter value
      get_value_param
        IMPORTING
          iv_name  TYPE rsparamsl_255-selname
        CHANGING
          cv_value TYPE any.

ENDCLASS.

CLASS lcl_sel_screen IMPLEMENTATION.

  METHOD maintain_selection_inputs.

*--- Reading the Selection Screen inputs
    REFRESH lt_selopts_255.
    DATA(lt_selopts) = VALUE rsparams_tt( ).
    CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
      EXPORTING
        curr_report         = sy-repid
*     IMPORTING
*       SP                  =
      TABLES
        selection_table     = lt_selopts
        selection_table_255 = lt_selopts_255
      EXCEPTIONS
        not_found           = 1
        no_report           = 2
        OTHERS              = 3.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

*--- Generating the PARAMETER values
    me->get_value_param(
      EXPORTING
        iv_name = :
        'P_FILE1' CHANGING cv_value = gv_file_path1 ),
        'P_FILE2' CHANGING cv_value = gv_file_path2 ),
        'P_HDLN1' CHANGING cv_value = gv_headlines_1 ),
        'P_HDLN2' CHANGING cv_value = gv_headlines_2 ),
        'P_SEP'   CHANGING cv_value = gv_separator ).

*--- Generating the SELECT-OPTION values
    me->get_value_range(
      EXPORTING
        iv_name  = :
        'S_KEYFD1' CHANGING ct_range = gt_r_keyfields1 ),
        'S_KEYFD2' CHANGING ct_range = gt_r_keyfields2 ).

  ENDMETHOD.

  METHOD get_value_range.

    " Returns the values
    CLEAR ct_range.
    LOOP AT lt_selopts_255 REFERENCE INTO DATA(lo_selopts)
                                     WHERE selname EQ iv_name.
      CHECK lo_selopts->option IS NOT INITIAL.
      APPEND INITIAL LINE TO ct_range ASSIGNING FIELD-SYMBOL(<lfs_range>).
      <lfs_range> = CORRESPONDING #( lo_selopts->* ).
    ENDLOOP.

  ENDMETHOD.

  METHOD get_value_param.

    " Returns the value
    cv_value = VALUE #( lt_selopts_255[ selname = iv_name ]-low OPTIONAL ).

  ENDMETHOD.

  METHOD restrict_select_options.

    CONSTANTS:
      lc_opt_name1 TYPE rsrest_opl VALUE 'OBJECTKEY1',
      lc_opt_name2 TYPE rsrest_opl VALUE 'OBJECTKEY2'.

*-----Generates the data for restricting the behavior of 'Material Doc.'
    DATA(ls_restrict) =
      VALUE sscr_restrict(
      opt_list_tab = VALUE sscr_opt_list_tab(
                     ( name = lc_opt_name1 options = VALUE #( eq = abap_true cp = abap_true ) )
                     ( name = lc_opt_name2 options = VALUE #( eq = abap_true cp = abap_true ) )
                     )
      ass_tab      = VALUE sscr_ass_tab(
                     ( kind = 'S' name = 'S_KEYFD1' sg_main = 'I' sg_addy = space op_main = lc_opt_name1 )
                     ( kind = 'S' name = 'S_KEYFD2' sg_main = 'I' sg_addy = space op_main = lc_opt_name2 )
                     )
      ).
*--- Restricts the select option functionalities
    CALL FUNCTION 'SELECT_OPTIONS_RESTRICT'
      EXPORTING
*       PROGRAM                =
        restriction            = ls_restrict
*       DB                     = ' '
      EXCEPTIONS
        too_late               = 1
        repeated               = 2
        selopt_without_options = 3
        selopt_without_signs   = 4
        invalid_sign           = 5
        empty_option_list      = 6
        invalid_kind           = 7
        repeated_kind_a        = 8
        OTHERS                 = 9.
    IF sy-subrc IS NOT INITIAL.
      CLEAR ls_restrict.
    ENDIF.

  ENDMETHOD.

  METHOD validate_keyfields.

*-----Checks the USER COMMAND
    IF iv_ucomm NE lc_ucomm_onli. RETURN. ENDIF.

*--- Checks if the user has provided the KEYFIELDS or not
    IF gt_r_keyfields1 IS INITIAL
       OR gt_r_keyfields2 IS INITIAL.
      RAISE EXCEPTION TYPE lcx_exception
        EXPORTING
          textid = VALUE #(
                   msgid = 'AQ'
                   msgno = 309
          ).
      " Specify values for the key fields
    ENDIF.

*--- Checks that we have the same number of KEYFIELDS
    IF lines( gt_r_keyfields1 ) NE lines( gt_r_keyfields2 ).
      RAISE EXCEPTION TYPE lcx_exception
        EXPORTING
          textid = VALUE #(
                   msgid = 'FINB_TR'
                   msgno = 066
                   attr1 = 'File 1: KeyFields'(003)
                   attr2 = 'File 2: KeyFields'(004)
          ).
      " The key fields in the tables are not the same (&1, &2)
    ENDIF.

  ENDMETHOD.

ENDCLASS.


***********************************
* Class: File Handling
***********************************
CLASS lcl_file DEFINITION FINAL.

*-||
  PUBLIC SECTION.
*-- Structure Types
    TYPES:
      BEGIN OF gty_data,
        file_path  TYPE string,
        file_data  TYPE REF TO data,
        components TYPE cl_abap_structdescr=>component_table,
      END OF gty_data.

*-- Table Types
    TYPES:
    gtty_file_data TYPE STANDARD TABLE OF string
                        WITH EMPTY KEY.

*-- Constants
    CONSTANTS:
      lc_regex_filter_field TYPE string VALUE '[*]'.

*-- Methods
    METHODS:
      " Maintains the File Details
      constructor
        IMPORTING
          iv_file_path TYPE string
          iv_separator TYPE c
          iv_headlines TYPE line_num,

      " Get the file data
      get_data
        RETURNING VALUE(rs_data) TYPE gty_data,

      " Uploads data from file into internal table
      upload_file
        RETURNING VALUE(ro_data) TYPE REF TO data
        RAISING
                  lcx_exception,

      " Decodes the data from the file and populates into internal table
      decode_data_file
        CHANGING
          co_tab TYPE REF TO data.

*-- Class Data: Variables
    CLASS-DATA:
      lv_file_type TYPE string.

*-- Class Methods
    CLASS-METHODS:
      " Class Constructor
      class_constructor,

      " F4 Help for Filename
      f4_file_path
        CHANGING
          cv_file_path TYPE hrpadkz_filename,

      " Validates the data input for Filename
      validate_file_path
        IMPORTING
          iv_file_path TYPE hrpadkz_filename
        RAISING
          lcx_exception.

*-||
  PRIVATE SECTION.
*-- Internal Table
    DATA:
      lt_file_data TYPE gtty_file_data.

*-- Structure
    DATA:
      ls_data TYPE gty_data.

*--- Variables
    DATA:
      lv_separator    TYPE usmd_sep,
      lv_header_lines TYPE line_num.

*-- Methods
    METHODS:
      " Generats the Components from the file
      generate_components_from_file
        IMPORTING
                  is_file_data         TYPE string
        RETURNING VALUE(rt_components) TYPE cl_abap_structdescr=>component_table.

ENDCLASS.

CLASS lcl_file IMPLEMENTATION.

  METHOD class_constructor.

    " Maintaining the File Type
    lv_file_type = 'csv Files (*.CSV)|*.CSV|'(017).

  ENDMETHOD.

  METHOD constructor.

*--- Maintaining the File Details
    " File Path
    ls_data = VALUE #(
              file_path = iv_file_path
              ).
    " Separator
    lv_separator    = iv_separator.
    " Number of Header Lines
    lv_header_lines = iv_headlines.

  ENDMETHOD.

  METHOD get_data.

    rs_data = ls_data.

  ENDMETHOD.

  METHOD f4_file_path.

    DATA:
      lt_file TYPE filetable.

    " File Opne dialog
    DATA(lv_rc) = VALUE i( ).
    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
*          window_title            =
        default_extension         = lv_file_type
*          default_file_name         =
        file_filter               = lv_file_type
*          with_encoding           =
*          initial_directory       =
*          multiselection          =
      CHANGING
        file_table              = lt_file
        rc                      = lv_rc
*          user_action             =
*          file_encoding           =
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        OTHERS                  = 5
    ).
    IF sy-subrc = 0.
      cv_file_path = VALUE #( lt_file[ 1 ]-filename OPTIONAL ).
    ENDIF.

  ENDMETHOD.

  METHOD validate_file_path.

*--- Checks if the process is UPLOAD or DOWNLOAD
    DATA(lv_result) = VALUE abap_bool( ).
    " Validates where the File exists or not
    cl_gui_frontend_services=>file_exist(
      EXPORTING
        file                 = iv_file_path
      RECEIVING
        result               = lv_result
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        wrong_parameter      = 3
        not_supported_by_gui = 4
        OTHERS               = 5
    ).
    IF sy-subrc <> 0
       OR lv_result EQ abap_false.
      RAISE EXCEPTION TYPE lcx_exception
        EXPORTING
          textid = VALUE #(
                   msgid = sy-msgid
                   msgno = sy-msgno
                   attr1 = sy-msgv1
                   attr2 = sy-msgv2
                   attr3 = sy-msgv3
                   attr4 = sy-msgv4
          ).
    ENDIF.

  ENDMETHOD.

  METHOD upload_file.

    FIELD-SYMBOLS:
                   <lft_data> TYPE STANDARD TABLE.

*** Uploads the data into internal table
    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename                = ls_data-file_path
*      filetype                = 'ASC'
*      has_field_separator     = SPACE
*      header_length           = 0
*      read_by_line            = 'X'
*      dat_mode                = SPACE
*      codepage                =
*      ignore_cerr             = ABAP_TRUE
*      replacement             = '#'
*      virus_scan_profile      =
*    IMPORTING
*      filelength              =
*      header                  =
    CHANGING
      data_tab                = lt_file_data
*      isscanperformed         = SPACE
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
      OTHERS                  = 19
  ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_exception
        EXPORTING
          textid = VALUE #(
                   msgid = sy-msgid
                   msgno = sy-msgno
                   attr1 = sy-msgv1
                   attr2 = sy-msgv2
                   attr3 = sy-msgv3
                   attr4 = sy-msgv4
          ).
    ENDIF.

*--- Generates the COMPONENTS of the internal table using the first line of the file
    ls_data-components = me->generate_components_from_file(
                             is_file_data = VALUE #( lt_file_data[ 1 ] OPTIONAL )
                             ).

*--- Creates the internal table using the COMPONENTS from above
    TRY.
        DATA(lo_tab) = cl_abap_tabledescr=>create(
                       p_line_type          = cl_abap_structdescr=>create(
                                                p_components = ls_data-components
                                              )
                       ).
        CREATE DATA ls_data-file_data TYPE HANDLE lo_tab.
      CATCH cx_sy_table_creation INTO DATA(lco_table).
        RAISE EXCEPTION TYPE lcx_exception
          EXPORTING
            textid = VALUE #(
                     msgid = 'UJT'
                     msgno = 027
                     attr1 = CONV #( lco_table->get_text( ) )
                     ).
        " Error occured while creating dynamic internal table

      CATCH cx_sy_struct_creation INTO DATA(lco_struct).
        RAISE EXCEPTION TYPE lcx_exception
          EXPORTING
            textid = VALUE #(
                     msgid = 'GG'
                     msgno = 785
                     attr1 = lco_struct->component_name
                     ).
        " Check field &

    ENDTRY.

*--- Deletes the Header Lines
    DELETE lt_file_data FROM 1 TO lv_header_lines.

  ENDMETHOD.

  METHOD generate_components_from_file.

*--- Generates the components from the input line
    DO.
      TRY.
          " Splits the line data to generate the Field name
          DATA(lv_field) = segment(
                           val   = is_file_data
                           index = sy-index
                           sep   = lv_separator
                           ).
          " Normalizes the Field name
          lv_field = replace(
                     val    = lv_field
                     regex  = lc_regex_filter_field
                     with   = space
                     ).
          " Adds this to the Components table
          rt_components = VALUE #(
                          BASE rt_components
                          (
                            name = lv_field
                            type = cl_abap_elemdescr=>get_string( )
                          )
                          ).
        CATCH cx_sy_strg_par_val.
          EXIT.
      ENDTRY.
    ENDDO.

  ENDMETHOD.

  METHOD decode_data_file.

    FIELD-SYMBOLS:
                   <lft_data> TYPE STANDARD TABLE.

    " De-reference the data
    ASSIGN co_tab->* TO <lft_data>.
    IF sy-subrc IS NOT INITIAL. RETURN. ENDIF.

*--- Processes the file data
    LOOP AT lt_file_data ASSIGNING FIELD-SYMBOL(<lfv_file_data>).

      " Adding a new line to internal table
      APPEND INITIAL LINE TO <lft_data> ASSIGNING FIELD-SYMBOL(<lfs_data>).

      DO.
        TRY.
            " Splits the line data to get the field value into proper field
            ASSIGN COMPONENT sy-index OF STRUCTURE <lfs_data> TO FIELD-SYMBOL(<lfv_value>).
            <lfv_value> = segment(
                          val   = <lfv_file_data>
                          index = sy-index
                          sep   = lv_separator
                          ).
          CATCH cx_sy_strg_par_val.
            EXIT.
        ENDTRY.
      ENDDO.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.


***********************************
* Class: Log
***********************************
CLASS lcl_log DEFINITION.

*-||
  PUBLIC SECTION.
*- Structure Types
    TYPES:
      BEGIN OF gty_key_translation,
        fld1 TYPE lvc_fname,
        fld2 TYPE lvc_fname,
      END OF gty_key_translation,

      BEGIN OF gty_field_values,
        field TYPE lvc_fname,
        value TYPE string,
      END OF gty_field_values,

      BEGIN OF gty_log_data,
        msg_type   TYPE bapi_mtype,
        flds_file1 TYPE lvc_t_fnam,
        data_file1 TYPE REF TO data,
        flds_file2 TYPE lvc_t_fnam,
        data_file2 TYPE REF TO data,
      END OF gty_log_data,

      BEGIN OF gty_data,
        icon_file1  TYPE icon_d,
        row_file1   TYPE i,
        field_file1 TYPE lvc_fname,
        value_file1 TYPE string,
        icon_file2  TYPE icon_d,
        row_file2   TYPE i,
        field_file2 TYPE lvc_fname,
        value_file2 TYPE string,
      END OF gty_data.

*- Table Types
    TYPES:
      gtty_key_translation TYPE STANDARD TABLE OF gty_key_translation
                                WITH EMPTY KEY,
      gtty_field_values    TYPE STANDARD TABLE OF gty_field_values
                                WITH EMPTY KEY,
      gtty_log_data        TYPE STANDARD TABLE OF gty_log_data
                                WITH EMPTY KEY.

*- Constants
    CONSTANTS:
      BEGIN OF gc_msgtype,
        success TYPE bapi_mtype VALUE 'S',
        error   TYPE bapi_mtype VALUE 'E',
      END OF gc_msgtype,

      BEGIN OF gc_icon,
        success TYPE icon_d VALUE icon_green_light,
        error   TYPE icon_d VALUE icon_red_light,
        warning TYPE icon_d VALUE icon_yellow_light,
      END OF gc_icon.

*- Methods
    METHODS:
      " Maintains the instance attributes
      constructor
        IMPORTING
          iv_path_file1      TYPE string
          iv_path_file2      TYPE string
          iv_row_fieldname   TYPE lvc_fname
          it_key_translation TYPE gtty_key_translation
        RAISING
          lcx_exception,

      " Adds the message to the LOG table
      add_message
        IMPORTING
          is_log_data TYPE gty_log_data,

      " Displays the transaction Log
      display_log_salv
        RAISING
          lcx_exception.


*-||
  PRIVATE SECTION.
*- Internal Tables
    DATA:
      lt_log_data        TYPE gtty_log_data,
      lt_key_translation TYPE gtty_key_translation.

*- Variables
    DATA:
      lv_path_file1    TYPE string,
      lv_path_file2    TYPE string,
      lv_row_fieldname TYPE lvc_fname.

*- Instances
    DATA:
      lo_data_tab       TYPE REF TO data,
      lo_type_key_file1 TYPE REF TO cl_abap_structdescr,
      lo_type_key_file2 TYPE REF TO cl_abap_structdescr,
      lo_salv           TYPE REF TO cl_salv_table.

*- Data Manipuation Methods
    METHODS:
      " Generates the LOG data into internal table
      create_tab_from_log
        IMPORTING
          it_log_data TYPE gtty_log_data,

      " Generates the KEY values
      generate_key_value
        IMPORTING
                  io_data       TYPE REF TO data
                  io_str_type   TYPE REF TO cl_abap_structdescr
        RETURNING VALUE(ro_key) TYPE REF TO data,

      " Generates the ROW index
      generate_row_index
        IMPORTING
                  io_data         TYPE REF TO data
        RETURNING VALUE(rv_index) TYPE i,

      " Generates the fields & its values
      generate_field_n_values
        IMPORTING
                  it_fields              TYPE lvc_t_fnam
                  io_data                TYPE REF TO data
        RETURNING VALUE(rt_field_values) TYPE gtty_field_values.

*- SALV Methods
    METHODS:
      " Maintains the PF STATUS
      maintain_status,

      " Maintains the FIELD CATALOG
      maintain_field_catalog
        RAISING
          cx_salv_data_error
          cx_salv_not_found,

      " Maintains the DISPLAY SETTINGS
      maintain_display_settings,

      " Maintains the SORT ORDERS
      maintain_sorts,

      " Maintains the TOP-OF-PAGE data
      maintain_top_of_page.

ENDCLASS.

CLASS lcl_log IMPLEMENTATION.

  METHOD constructor.

**********************************************************************
* There are repeated iterations for the KEY TRANSLATION table
* However, there are few records & hence performance impact is low
**********************************************************************

*--- Maintains the File Path
    lv_path_file1 = iv_path_file1.
    lv_path_file2 = iv_path_file2.

*--- Maintains the Key Translation values
    lt_key_translation = it_key_translation.

*--- Maintains the ROW fieldname
    lv_row_fieldname = iv_row_fieldname.

    TRY.
***** Structure Type: KEYS = File 1
        lo_type_key_file1 = cl_abap_structdescr=>create(
                            p_components          = VALUE #(
                                                    FOR ls_key_translation IN it_key_translation
                                                     (
                                                      name = ls_key_translation-fld1
                                                      type = cl_abap_elemdescr=>get_string( )
                                                     )
                                                    )
                            ).
***** Structure Type: KEYS = File 2
        lo_type_key_file2 = cl_abap_structdescr=>create(
                            p_components          = VALUE #(
                                                    FOR ls_key_translation IN it_key_translation
                                                     (
                                                      name = ls_key_translation-fld2
                                                      type = cl_abap_elemdescr=>get_string( )
                                                     )
                                                    )
                            ).

***** Table: LOG
        DATA(lo_tab) = cl_abap_tabledescr=>create(
                       p_line_type          = cl_abap_structdescr=>create(
                                                  p_components = VALUE #(
                                                  FOR ls_key_translation IN it_key_translation
                                                    (
                                                      name = ls_key_translation-fld1
                                                      type = cl_abap_elemdescr=>get_string( )
                                                    )
                                                  (
                                                    LINES OF CAST cl_abap_structdescr(
                                                      cl_abap_typedescr=>describe_by_name( p_name = 'GTY_DATA' )
                                                      )->get_components( )
                                                  )
                                                  )
                                              )
                        ).
        CREATE DATA lo_data_tab TYPE HANDLE lo_tab.

      CATCH cx_sy_table_creation INTO DATA(lco_table).
        RAISE EXCEPTION TYPE lcx_exception
          EXPORTING
            textid = VALUE #(
                     msgid = 'UJT'
                     msgno = 027
                     ).
        " Error occured while creating dynamic internal table

      CATCH cx_sy_struct_creation INTO DATA(lco_struct).
        RAISE EXCEPTION TYPE lcx_exception
          EXPORTING
            textid = VALUE #(
                     msgid = 'GG'
                     msgno = 785
                     attr1 = lco_struct->component_name
                     ).
        " Check field &

    ENDTRY.

  ENDMETHOD.

  METHOD add_message.

    APPEND is_log_data TO lt_log_data.

  ENDMETHOD.

  METHOD create_tab_from_log.

    DATA:
      lo_key      TYPE REF TO data,
      lo_str_data TYPE REF TO data.
    FIELD-SYMBOLS:
      <lft_data> TYPE STANDARD TABLE.

*-----De-reference the final internal table
    ASSIGN lo_data_tab->* TO <lft_data>.

*--- Generates WorkArea for final internal table
    DATA(lo_str) = CAST cl_abap_tabledescr(
                   cl_abap_typedescr=>describe_by_data_ref( p_data_ref = lo_data_tab )
                   )->get_table_line_type( ).
    CREATE DATA lo_str_data TYPE HANDLE lo_str.
    ASSIGN lo_str_data->* TO FIELD-SYMBOL(<lfs_data>).

*--- Processes the LOG data
    LOOP AT it_log_data ASSIGNING FIELD-SYMBOL(<lfs_log_data>).

      " Processes based on the operation
      DATA(ls_data) = VALUE gty_data( ).
      CASE <lfs_log_data>-msg_type.
        WHEN gc_msgtype-success.
          " * [S] = Match found
          " --> Key Values
          lo_key = me->generate_key_value(
                   io_data      = <lfs_log_data>-data_file1
                   io_str_type  = lo_type_key_file1
                   ).
          ls_data = VALUE #( icon_file1 = gc_icon-success icon_file2 = gc_icon-success ).

        WHEN gc_msgtype-error.
          IF <lfs_log_data>-data_file2 IS NOT BOUND.
            " * [E] = No records in FILE2
            " --> Key Values
            lo_key = me->generate_key_value(
                     io_data      = <lfs_log_data>-data_file1
                     io_str_type  = lo_type_key_file1
                     ).
            ls_data = VALUE #( icon_file1 = gc_icon-success icon_file2 = gc_icon-error ).

          ELSEIF <lfs_log_data>-data_file1 IS NOT BOUND.
            " * [E] = No records in FILE1
            " --> Key Values
            lo_key = me->generate_key_value(
                     io_data      = <lfs_log_data>-data_file2
                     io_str_type  = lo_type_key_file2
                     ).
            ls_data = VALUE #( icon_file1 = gc_icon-error icon_file2 = gc_icon-success ).

          ELSE.
            " * [E] = Match found but difference in some fields of FILE1 & FILE2
            " --> Key Values
            lo_key = me->generate_key_value(
                     io_data      = <lfs_log_data>-data_file1
                     io_str_type  = lo_type_key_file1
                     ).
            ls_data = VALUE #( icon_file1 = gc_icon-error icon_file2 = gc_icon-error ).

          ENDIF.

        WHEN OTHERS.
          " Use for other operations

      ENDCASE.

      " Generating the Row Index
      ls_data = VALUE #( BASE ls_data
                row_file1 = me->generate_row_index( io_data = <lfs_log_data>-data_file1 )
                row_file2 = me->generate_row_index( io_data = <lfs_log_data>-data_file2 )
                ).

      " Maintaining the Key Values
      ASSIGN lo_key->* TO FIELD-SYMBOL(<lfs_key>).
      <lfs_data> = CORRESPONDING #( <lfs_key> ).
      UNASSIGN <lfs_key>.

      " Generating the Fields & Values
      DATA(lt_field_values1) = me->generate_field_n_values(
                               it_fields = <lfs_log_data>-flds_file1
                               io_data   = <lfs_log_data>-data_file1
                               ).
      DATA(lt_field_values2) = me->generate_field_n_values(
                               it_fields = <lfs_log_data>-flds_file2
                               io_data   = <lfs_log_data>-data_file2
                               ).

*-----Maintaining the data into internal table
      LOOP AT lt_field_values1 INTO DATA(ls_field_value1).

        READ TABLE lt_field_values2 INTO DATA(ls_field_value2)
                                    INDEX sy-tabix.
        CHECK sy-subrc IS INITIAL.

        <lfs_data> =  CORRESPONDING #( BASE ( <lfs_data> )
                                       VALUE gty_data(
                                            BASE ls_data
                                            field_file1 = ls_field_value1-field
                                            value_file1 = ls_field_value1-value
                                            field_file2 = ls_field_value2-field
                                            value_file2 = ls_field_value2-value
                                            )
                                      ).
        APPEND <lfs_data> TO <lft_data>.

        " Clearing the data in the last iteration to check further
        AT LAST. CLEAR <lfs_data>. ENDAT.

      ENDLOOP.

      " Checks if we need to add the data back into the structure
      IF <lfs_data> IS NOT INITIAL.
        <lfs_data> = CORRESPONDING #( BASE ( <lfs_data> ) ls_data ).
        APPEND <lfs_data> TO <lft_data>.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD generate_field_n_values.

*-----De-reference the data
    IF io_data IS NOT BOUND. RETURN. ENDIF.
    ASSIGN io_data->* TO FIELD-SYMBOL(<lfs_data>).

*--- Generate the values
    LOOP AT it_fields ASSIGNING FIELD-SYMBOL(<lfv_field>).
      ASSIGN COMPONENT <lfv_field> OF STRUCTURE <lfs_data>
                                   TO FIELD-SYMBOL(<lfv_value>).
      rt_field_values = VALUE #(
                        BASE rt_field_values
                        (
                          field = <lfv_field>
                          value = <lfv_value>
                        )
      ).
    ENDLOOP.

  ENDMETHOD.

  METHOD generate_row_index.

*-----De-reference the data
    IF io_data IS NOT BOUND. RETURN. ENDIF.
    ASSIGN io_data->* TO FIELD-SYMBOL(<lfs_data>).

*--- Generates the ROW index
    ASSIGN COMPONENT lv_row_fieldname OF STRUCTURE <lfs_data>
                                      TO FIELD-SYMBOL(<lfv_row>).
    rv_index = <lfv_row>.

  ENDMETHOD.

  METHOD generate_key_value.

**********************************************************************
* No need to check for structure inconsistency as this is exclusive
* to this class only and should be called responsibly
**********************************************************************

    DATA:
      lo_key TYPE REF TO data.

*-----De-reference the data
    IF io_data IS NOT BOUND. RETURN. ENDIF.
    ASSIGN io_data->* TO FIELD-SYMBOL(<lfs_data>).

*--- Generates the KEY value holder from the input data
    CREATE DATA lo_key TYPE HANDLE io_str_type.
    ASSIGN lo_key->* TO FIELD-SYMBOL(<lfs_key_value>).
    <lfs_key_value> = CORRESPONDING #( <lfs_data> ).

*--- Generates the returning parameter KEY
    CREATE DATA ro_key TYPE HANDLE lo_type_key_file1.
    ASSIGN ro_key->* TO FIELD-SYMBOL(<lfs_key>).
    <lfs_key> = <lfs_key_value>.

  ENDMETHOD.

  METHOD display_log_salv.

    FIELD-SYMBOLS:
                   <lft_data> TYPE ANY TABLE.

*-----De-reference the data
    me->create_tab_from_log( it_log_data = lt_log_data ).
    ASSIGN lo_data_tab->* TO <lft_data>.

*--- Displays the data in an SALV
    TRY.
        " Creates the SALV instance
        cl_salv_table=>factory(
*      EXPORTING
*        list_display   = IF_SALV_C_BOOL_SAP=>FALSE    " ALV Displayed in List Mode
*        r_container    =     " Abstract Container for GUI Controls
*        container_name =
          IMPORTING
            r_salv_table   = lo_salv
          CHANGING
            t_table        = <lft_data>
        ).

        " PF Status
        me->maintain_status( ).

        " Field Catalog
        me->maintain_field_catalog( ).

        " Sort Objects
        me->maintain_sorts( ).

        " Display
        me->maintain_display_settings( ).

        " TOP-OF-PAGE data
        me->maintain_top_of_page( ).

        " Displays the data
        lo_salv->display( ).

      CATCH cx_salv_error INTO DATA(lco_salv_error).
        RAISE EXCEPTION TYPE lcx_exception
          EXPORTING
            textid = CORRESPONDING #( lco_salv_error->get_message( )
                     MAPPING
                      attr1 = msgv1
                      attr2 = msgv2
                      attr3 = msgv3
                      attr4 = msgv4
                     ).

    ENDTRY.

  ENDMETHOD.

  METHOD maintain_status.

*** Generates the PF Status instance
    DATA(lo_functions) = lo_salv->get_functions( ).

*-----Activates all the Functions in the PF Status
    lo_functions->set_all( ).

  ENDMETHOD.

  METHOD maintain_field_catalog.

*** Generates the Field Catalog instance
    DATA(lo_columns) = lo_salv->get_columns( ).
    lo_columns->set_optimize( ).

*--- Modifies the Column properties
    LOOP AT lo_columns->get( ) ASSIGNING FIELD-SYMBOL(<ls_column_ref>).

*-----Maintains the LONG Text descriptions
      CASE <ls_column_ref>-columnname.
        WHEN 'ICON_FILE1'.        <ls_column_ref>-r_column->set_long_text( 'File 1: Status'(009) ).
        WHEN 'ROW_FILE1'.         <ls_column_ref>-r_column->set_long_text( 'File 1: RowIndex'(010) ).
        WHEN 'FIELD_FILE1'.       <ls_column_ref>-r_column->set_long_text( 'File 1: Field'(011) ).
        WHEN 'VALUE_FILE1'.       <ls_column_ref>-r_column->set_long_text( 'File 1: Value'(012) ).
        WHEN 'ICON_FILE2'.        <ls_column_ref>-r_column->set_long_text( 'File 2: Status'(013) ).
        WHEN 'ROW_FILE2'.         <ls_column_ref>-r_column->set_long_text( 'File 2: RowIndex'(014) ).
        WHEN 'FIELD_FILE2'.       <ls_column_ref>-r_column->set_long_text( 'File 2: Field'(015) ).
        WHEN 'VALUE_FILE2'.       <ls_column_ref>-r_column->set_long_text( 'File 2: Value'(016) ).
        WHEN OTHERS.
          " Key Values
          <ls_column_ref>-r_column->set_long_text(
          | { <ls_column_ref>-columnname } / { VALUE #( lt_key_translation[ fld1 = <ls_column_ref>-columnname ]-fld2 OPTIONAL ) } |
          ).
      ENDCASE.

*-----Clears SHORT & MEDIUM text so that system displays LONG text only
      <ls_column_ref>-r_column->set_short_text( space ).
      <ls_column_ref>-r_column->set_medium_text( space ).

    ENDLOOP.

  ENDMETHOD.

  METHOD maintain_sorts.

*** Maintains the SORT Order
    DATA(lo_sorts) = lo_salv->get_sorts( ).
    IF lo_sorts->is_sort_defined( ) IS NOT INITIAL. RETURN. ENDIF.
    TRY.
        lo_sorts->add_sort(
          EXPORTING :
            columnname = 'ROW_FILE1' position = 1 ),
            columnname = 'ROW_FILE2' position = 2 ).
      CATCH cx_salv_data_error cx_salv_not_found cx_salv_existing.
        CLEAR lo_sorts.
    ENDTRY.

  ENDMETHOD.

  METHOD maintain_display_settings.

*** Generates the Layout instance
    DATA(lo_display) = lo_salv->get_display_settings( ).
    lo_display->set_striped_pattern( abap_true ).

  ENDMETHOD.

  METHOD maintain_top_of_page.

    " Maintains the data
    DATA(lo_layout) = NEW cl_salv_form_layout_grid( ).

    " Line 1
    lo_layout->create_label( row = 1 column = 1 )->set_text( 'Path - File 1:'(007) ).
    lo_layout->create_flow( row = 1 column = 2 )->create_text( text = lv_path_file1 ).
    " Line 2
    lo_layout->create_label( row = 2 column = 1 )->set_text( 'Path - File 2:'(008) ).
    lo_layout->create_flow( row = 2 column = 2 )->create_text( text = lv_path_file2 ).
    " Blank Line
    lo_layout->create_flow( row = 3 column = 1 )->create_text(  ).

*** Maintains the TOP-OF-PAGE data
    lo_salv->set_top_of_list( lo_layout ).
    lo_salv->set_top_of_list_print( lo_layout ).

  ENDMETHOD.

ENDCLASS.


***********************************
* Class: Data Model
***********************************
CLASS lcl_model DEFINITION.

*-||
  PUBLIC SECTION.
*- Structure Types
    TYPES:
      BEGIN OF gty_file_data,
        file_obj  TYPE REF TO lcl_file,
        keyfields TYPE wrf_ppw_fname_rtty,
      END OF gty_file_data,

      BEGIN OF gty_translation,
        fld_file1 TYPE lvc_fname,
        fld_file2 TYPE lvc_fname,
        cellstyle TYPE lvc_t_styl,
      END OF gty_translation.

*- Table Types
    TYPES:
      gtty_translation TYPE STANDARD TABLE OF gty_translation
                            WITH EMPTY KEY.

*- Constants
    CONSTANTS:
      lc_fld_key TYPE lvc_fname VALUE 'KEY',
      lc_fld_row TYPE lvc_fname VALUE 'ROW_INDEX'.

*- Structures
    DATA:
      ls_data_file1 TYPE gty_file_data,
      ls_data_file2 TYPE gty_file_data.

*- Instances
    DATA:
      lo_values      TYPE REF TO data,
      lo_translation TYPE REF TO data,
      lo_log         TYPE REF TO lcl_log.

*- Methods
    METHODS:
      " Creates the FILE instances
      constructor
        IMPORTING
                  io_sel_screen TYPE REF TO lcl_sel_screen
        RAISING   lcx_exception,

      " Processes the FILE data
      populate_data
        RAISING
          lcx_exception,

      " Creates the TRANSLATION model
      create_translation_data
        IMPORTING
                  is_file1              TYPE gty_file_data
                  is_file2              TYPE gty_file_data
        RETURNING VALUE(ro_translation) TYPE REF TO data,

      " Creates the VALUE model
      create_value_data
        IMPORTING
                  is_file         TYPE gty_file_data
        RETURNING VALUE(ro_value) TYPE REF TO data
        RAISING   lcx_exception,

      " Compares the data of the two file instances
      compare_file_data
        IMPORTING
          it_selected_rows TYPE lvc_t_roid
        RAISING
          lcx_exception.


*-||
  PRIVATE SECTION.
*- Methods
    METHODS:
      " Checks if the input KeyField exists or not
      check_keyfield_exists
        IMPORTING
          is_data_file TYPE gty_file_data
        RAISING
          lcx_exception,

      " Creates sorted internal table from the data
      create_sorted_table
        IMPORTING
                  is_data_file   TYPE gty_file_data
                  it_fields      TYPE lvc_t_fnam
        RETURNING VALUE(ro_data) TYPE REF TO data
        RAISING
                  lcx_exception,

      " Filters the data from the two internal tables based on the Translation table
      filter_data_tab
        IMPORTING
          it_translation TYPE gtty_translation
          io_values      TYPE REF TO data
        CHANGING
          co_tab1        TYPE REF TO data
          co_tab2        TYPE REF TO data.

ENDCLASS.

CLASS lcl_model IMPLEMENTATION.

  METHOD constructor.

***************** FILE1
***************************
* Indicator: File 1
***************************
    cl_progress_indicator=>progress_indicate(
      EXPORTING
        i_text               = 'Processing data for file 1'(018)
        i_processed          = 1
        i_total              = 2
        i_output_immediately = abap_true
    ).
***************************
    TRY.
*--- Generates the file instance
        ls_data_file1 = VALUE #(
                        file_obj = NEW #(
                                   iv_file_path = io_sel_screen->gv_file_path1
                                   iv_separator = io_sel_screen->gv_separator
                                   iv_headlines = io_sel_screen->gv_headlines_1
                                   )
                        keyfields = VALUE #(
                                    FOR ls_keyfields1 IN io_sel_screen->gt_r_keyfields1
                                    sign = 'I' option = 'EQ'
                                     ( low = CONV #(
                                            replace(
                                            val    = ls_keyfields1-low
                                            regex  = lcl_file=>lc_regex_filter_field
                                            with   = space
                                            )
                                       )
                                     )
                                    )
                        ).

*--- Populates the data
        ls_data_file1-file_obj->upload_file( ).

*--- Checks if the KEYFIELD exists in the list of components
        me->check_keyfield_exists( is_data_file = ls_data_file1 ).

      CATCH lcx_exception.
        RAISE EXCEPTION TYPE lcx_exception.

    ENDTRY.


***************** FILE2
***************************
* Indicator: File 2
***************************
    cl_progress_indicator=>progress_indicate(
      EXPORTING
        i_text               = 'Processing data for file 2'(019)
        i_processed          = 2
        i_total              = 2
        i_output_immediately = abap_true
    ).
***************************
    TRY.
*--- Generates the file instance
        ls_data_file2 = VALUE #(
                        file_obj = NEW #(
                                   iv_file_path = io_sel_screen->gv_file_path2
                                   iv_separator = io_sel_screen->gv_separator
                                   iv_headlines = io_sel_screen->gv_headlines_2
                                   )
                        keyfields = VALUE #(
                                    FOR ls_keyfields2 IN io_sel_screen->gt_r_keyfields2
                                    sign = 'I' option = 'EQ'
                                     ( low = CONV #(
                                            replace(
                                            val    = ls_keyfields2-low
                                            regex  = lcl_file=>lc_regex_filter_field
                                            with   = space
                                            )
                                       )
                                     )
                                    )
                        ).

*--- Populates the data
        ls_data_file2-file_obj->upload_file( ).

*--- Checks if the KEYFIELD exists in the list of components
        me->check_keyfield_exists( is_data_file = ls_data_file2 ).

      CATCH lcx_exception.
        RAISE EXCEPTION TYPE lcx_exception.

    ENDTRY.

  ENDMETHOD.

  METHOD check_keyfield_exists.

    " Checks if KeyField exists in the list of Components
    DATA(lt_components) = is_data_file-file_obj->get_data( )-components.
    LOOP AT is_data_file-keyfields ASSIGNING FIELD-SYMBOL(<lfs_keyfield>).
      IF NOT line_exists( lt_components[ name = <lfs_keyfield>-low ] ).
        " Exception that field does not exist
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD populate_data.

    DATA:
      lo_data TYPE REF TO data.

*--- Populates the data for FILE1
    lo_data = ls_data_file1-file_obj->get_data( )-file_data.
    ls_data_file1-file_obj->decode_data_file(
      CHANGING
        co_tab = lo_data
    ).

*--- Populates the data for FILE2
    lo_data = ls_data_file2-file_obj->get_data( )-file_data.
    ls_data_file2-file_obj->decode_data_file(
      CHANGING
        co_tab = lo_data
    ).

*** Creates the TRANSLATION model
    lo_translation = me->create_translation_data(
                     is_file1 = ls_data_file1
                     is_file2 = ls_data_file2
                     ).

*** Creates the VALUE model
    TRY.
        lo_values = me->create_value_data( is_file = ls_data_file1 ).
      CATCH lcx_exception.
        RAISE EXCEPTION TYPE lcx_exception.
    ENDTRY.

  ENDMETHOD.

  METHOD create_translation_data.

    FIELD-SYMBOLS:
                   <lft_data> TYPE gtty_translation.

    " Generates the Components for the two file instances
    DATA(lt_components_1) = is_file1-file_obj->get_data( )-components.
    DATA(lt_components_2) = is_file2-file_obj->get_data( )-components.

*--- Processes the data
    DATA(lt_translation) = VALUE gtty_translation( ).
    LOOP AT lt_components_1 ASSIGNING FIELD-SYMBOL(<lfs_component>).
      DATA(lv_index) = sy-tabix.

      " Checks if it is a Key Component
      DATA(lv_key_index) = line_index( is_file1-keyfields[ low = <lfs_component>-name ] ).
      IF lv_key_index <> 0.
        " Key Component
        INSERT VALUE #(
        fld_file1 = <lfs_component>-name
        fld_file2 = VALUE #( is_file2-keyfields[ lv_key_index ]-low OPTIONAL )
        cellstyle = VALUE #(
                    ( fieldname = 'FLD_FILE1' style = cl_gui_alv_grid=>mc_style_disabled )
                    ( fieldname = 'FLD_FILE2' style = cl_gui_alv_grid=>mc_style_disabled )
                    )
        ) INTO lt_translation INDEX lv_key_index.

      ELSE.
        " Non-key Component
        APPEND VALUE #(
        fld_file1 = <lfs_component>-name
        fld_file2 = VALUE #( lt_components_2[ lv_index ]-name OPTIONAL )
        cellstyle = VALUE #(
                    ( fieldname = 'FLD_FILE1' style = cl_gui_alv_grid=>mc_style_disabled )
                    )
        ) TO lt_translation.

      ENDIF.

    ENDLOOP.

    " Maintaining the reference of the Translation table
    CREATE DATA ro_translation LIKE lt_translation.
    ASSIGN ro_translation->* TO <lft_data>.
    <lft_data> = lt_translation.

  ENDMETHOD.

  METHOD create_value_data.

*--- Creates the Components for the internal table
    DATA(lt_components_tab) = REDUCE cl_abap_structdescr=>component_table(
                              INIT components = VALUE cl_abap_structdescr=>component_table( )
                              FOR ls_component IN is_file-file_obj->get_data( )-components
                                               WHERE ( name IN is_file-keyfields )
                              NEXT components = VALUE #(
                                                BASE components
                                                ( ls_component )
                                                )

                              ).

*--- Creates internal table using the above components
    TRY.
        DATA(lo_tab) = cl_abap_tabledescr=>create(
                       p_line_type = cl_abap_structdescr=>create(
                                     p_components = lt_components_tab
                                     )
                       ).
        CREATE DATA ro_value TYPE HANDLE lo_tab.

      CATCH cx_sy_table_creation INTO DATA(lco_table).
        RAISE EXCEPTION TYPE lcx_exception
          EXPORTING
            textid = VALUE #(
                     msgid = 'UJT'
                     msgno = 027
                     ).
        " Error occured while creating dynamic internal table

    ENDTRY.

  ENDMETHOD.

  METHOD compare_file_data.

    FIELD-SYMBOLS:
      <lft_translation> TYPE gtty_translation,
      <lft_data1>       TYPE ANY TABLE,
      <lft_data2>       TYPE ANY TABLE.

*-----Generates the Translation data
    ASSIGN lo_translation->* TO <lft_translation>.
    DATA(lt_translation) = <lft_translation>.

*-----Generates the list of selected fields
    DATA(lt_rows_range) = VALUE satr_t_hier_ranges_index(
                          FOR ls_row IN it_selected_rows
                          LET sign = 'I' option = 'EQ' IN
                          ( sign = sign option = option low = ls_row-row_id )
                          ).
    LOOP AT lt_translation ASSIGNING FIELD-SYMBOL(<lfs_translation>).
      CHECK sy-tabix NOT IN lt_rows_range.
      CLEAR <lfs_translation>.
    ENDLOOP.
    DELETE lt_translation WHERE fld_file1 IS INITIAL OR fld_file2 IS INITIAL.



*--- Creates sorted internal table as per inputs from user in UI for FILE1
    DATA(lo_data_file1) = me->create_sorted_table(
                          is_data_file = ls_data_file1
                          it_fields    = VALUE #(
                                         FOR ls_translation IN lt_translation
                                         ( ls_translation-fld_file1 )
                                         )
                          ).

*--- Creates sorted internal table as per inputs from user in UI for FILE2
    DATA(lo_data_file2) = me->create_sorted_table(
                          is_data_file = ls_data_file2
                          it_fields    = VALUE #(
                                         FOR ls_translation IN lt_translation
                                         ( ls_translation-fld_file2 )
                                         )
                          ).

*--- Filters the data (as per user inputs)
    me->filter_data_tab(
      EXPORTING
        it_translation = lt_translation
        io_values      = lo_values
      CHANGING
        co_tab1        = lo_data_file1
        co_tab2        = lo_data_file2
    ).

*-----Dereference the data
    ASSIGN:
      lo_data_file1->* TO <lft_data1>,
      lo_data_file2->* TO <lft_data2>.

*** ||| Log instance
    lo_log = NEW #(
                 iv_path_file1      = CONV #( ls_data_file1-file_obj->get_data( )-file_path )
                 iv_path_file2      = CONV #( ls_data_file2-file_obj->get_data( )-file_path )
                 iv_row_fieldname   = lc_fld_row
                 it_key_translation = VALUE #(
                                      FOR ls_keyfield1 IN ls_data_file1-keyfields
                                                       INDEX INTO lv_index
                                      LET ls_keyfield2 = ls_data_file1-keyfields[ lv_index ] IN
                                      ( fld1 = ls_keyfield1-low fld2 = ls_keyfield2-low )
                                      )
             ).

*** Compares the data within the two internal tables created earlier
    DATA(lt_rows_found) = VALUE satr_t_hier_ranges_index( ).
    LOOP AT <lft_data1> ASSIGNING FIELD-SYMBOL(<lfs_data1>).

      " Generating the KeyValue from ITAB1
      ASSIGN COMPONENT lc_fld_key OF STRUCTURE <lfs_data1> TO FIELD-SYMBOL(<lfv_key_value>).
      CHECK sy-subrc IS INITIAL.

      " Checks if entry exists in ITAB2
      READ TABLE <lft_data2> ASSIGNING FIELD-SYMBOL(<lfs_data2>)
                                       WITH KEY (lc_fld_key) = <lfv_key_value>.
      IF sy-subrc IS NOT INITIAL.
        " [ERROR] = --> ITAB2 entry not found
        lo_log->add_message(
                is_log_data = VALUE #(
                              msg_type    = lcl_log=>gc_msgtype-error
                              data_file1  = REF #( <lfs_data1> )
                              )
                ).
        CONTINUE.
      ENDIF.

      " Maintaining the Row Number found in second internal table
      ASSIGN COMPONENT lc_fld_row OF STRUCTURE:
             <lfs_data1> TO FIELD-SYMBOL(<lfv_row_1>),
             <lfs_data2> TO FIELD-SYMBOL(<lfv_row_2>).
      lt_rows_found = VALUE #( BASE lt_rows_found ( sign = 'I' option = 'EQ' low = <lfv_row_2> ) ).

      " Checks individual fields
      DATA(ls_log_data) = VALUE lcl_log=>gty_log_data( ).
      LOOP AT lt_translation ASSIGNING <lfs_translation>.
        ASSIGN COMPONENT:
          <lfs_translation>-fld_file1 OF STRUCTURE <lfs_data1> TO FIELD-SYMBOL(<lfv_value1>),
          <lfs_translation>-fld_file2 OF STRUCTURE <lfs_data2> TO FIELD-SYMBOL(<lfv_value2>).
        IF <lfv_value1> <> <lfv_value2>.
          DATA(lv_error) = abap_true.     " --> ERROR indicator
          ls_log_data = VALUE #( BASE ls_log_data
                        flds_file1 = VALUE #( BASE ls_log_data-flds_file1 ( <lfs_translation>-fld_file1 ) )
                        flds_file2 = VALUE #( BASE ls_log_data-flds_file2 ( <lfs_translation>-fld_file2 ) )
                        ).
        ENDIF.
        UNASSIGN: <lfv_value1>, <lfv_value2>.
      ENDLOOP.

      " Checks the ERROR insicator
      IF lv_error IS NOT INITIAL.
        CLEAR lv_error.
        ls_log_data = VALUE #( BASE ls_log_data
                      msg_type = lcl_log=>gc_msgtype-error
                      data_file1 = REF #( <lfs_data1> )
                      data_file2 = REF #( <lfs_data2> )
                      ).
        " [ERROR] = --> Match not found
        lo_log->add_message( is_log_data = ls_log_data ).

      ELSE.
        " [SUCCESS] = --> Match found
        lo_log->add_message(
                is_log_data = VALUE #(
                              msg_type = lcl_log=>gc_msgtype-success
                              data_file1 = REF #( <lfs_data1> )
                              )
                ).

      ENDIF.

    ENDLOOP.

*--- Checks if we have any records in ITAB2 which were not referenced/ read
    LOOP AT <lft_data2> ASSIGNING <lfs_data2>.
      ASSIGN COMPONENT lc_fld_row OF STRUCTURE <lfs_data2> TO <lfv_row_2>.
      CHECK <lfv_row_2> NOT IN lt_rows_found.
      " [ERROR] = --> ITAB1 entry not found
      lo_log->add_message(
              is_log_data = VALUE #(
                            msg_type = lcl_log=>gc_msgtype-error
                            data_file2 = REF #( <lfs_data2> )
                            )
              ).
    ENDLOOP.

  ENDMETHOD.

  METHOD create_sorted_table.

    DATA:
      lo_structure TYPE REF TO data.
    FIELD-SYMBOLS:
      <lft_standard> TYPE STANDARD TABLE,
      <lft_sorted>   TYPE SORTED TABLE,
      <lfs_sorted>   TYPE any.

*-----Component tables for the structure
    DATA(lt_components_key)     = VALUE cl_abap_structdescr=>component_table( ).
    DATA(lt_components_nonkey)  = VALUE cl_abap_structdescr=>component_table( ).

*--- Processes the user entries to generate the components of the new internal table
    DATA(lt_components) = is_data_file-file_obj->get_data( )-components.
    LOOP AT it_fields ASSIGNING FIELD-SYMBOL(<lfv_field>).

      " Generates the properties of the component
      TRY .
          DATA(ls_component) = VALUE #( lt_components[ name = <lfv_field> ] ).
        CATCH cx_sy_itab_line_not_found.
          CONTINUE.
      ENDTRY.

      " Checks if it is a key field
      IF line_exists( is_data_file-keyfields[ low = <lfv_field> ] ).
        " Key Field
        lt_components_key = VALUE #( BASE lt_components_key ( ls_component ) ).
      ELSE.
        " Non-Key Field
        lt_components_nonkey = VALUE #( BASE lt_components_nonkey ( ls_component ) ).
      ENDIF.

    ENDLOOP.

    TRY.
***** Creates the Sorted Internal Table
*--- Structure Type
        DATA(lo_str_sorted) =
        cl_abap_structdescr=>create(
                                   p_components = VALUE #(
                                                   (
                                                   name       = lc_fld_key
                                                   as_include = abap_true
                                                   type       = cl_abap_structdescr=>create(
                                                                p_components = lt_components_key
                                                                )
                                                   )
                                                  ( LINES OF lt_components_nonkey )
                                                   (
                                                   name = lc_fld_row
                                                   type = CAST cl_abap_elemdescr( cl_abap_typedescr=>describe_by_data( p_data = sy-tabix ) )
                                                   )
                                                  )
                                   ).
        " Structure
        CREATE DATA lo_structure TYPE HANDLE lo_str_sorted.
        ASSIGN lo_structure->* TO <lfs_sorted>.

*--- Table Type
        DATA(lo_tab_sorted) =
        cl_abap_tabledescr=>create(
        p_line_type          = lo_str_sorted
        p_table_kind         = cl_abap_tabledescr=>tablekind_sorted
*        p_unique             = ABAP_FALSE
        p_key                = VALUE abap_keydescr_tab(
                                     FOR ls_component_key IN lt_components_key
                                     ( CONV #( ls_component_key-name ) )
                                     )
        p_key_kind           = cl_abap_tabledescr=>keydefkind_user
        ).
        " Internal Table
        CREATE DATA ro_data TYPE HANDLE lo_tab_sorted.
        ASSIGN ro_data->* TO <lft_sorted>.

*** Maintaining the data into sorted internal table
        " Data = Standard Table
        DATA(lo_data_standard) = is_data_file-file_obj->get_data( )-file_data.
        ASSIGN lo_data_standard->* TO <lft_standard>.
        " Moving the data with row index
        LOOP AT <lft_standard> ASSIGNING FIELD-SYMBOL(<lfs_standard>).
          <lfs_sorted> = CORRESPONDING #( <lfs_standard> ).
          ASSIGN COMPONENT lc_fld_row OF STRUCTURE <lfs_sorted> TO FIELD-SYMBOL(<lfv_row>).
          IF sy-subrc IS INITIAL. <lfv_row> = sy-tabix. ENDIF.
          INSERT <lfs_sorted> INTO TABLE <lft_sorted>.
        ENDLOOP.

      CATCH cx_sy_table_creation.
        RAISE EXCEPTION TYPE lcx_exception
          EXPORTING
            textid = VALUE #(
                     msgid = 'UJT'
                     msgno = 027
                     ).
        " Error occured while creating dynamic internal table

      CATCH cx_sy_struct_creation INTO DATA(lco_struct).
        RAISE EXCEPTION TYPE lcx_exception
          EXPORTING
            textid = VALUE #(
                     msgid = 'GG'
                     msgno = 785
                     attr1 = lco_struct->component_name
                     ).
        " Check field &

    ENDTRY.

  ENDMETHOD.

  METHOD filter_data_tab.

    DATA:
      lo_data_temp TYPE REF TO data.
    FIELD-SYMBOLS:
      <lft_values>    TYPE STANDARD TABLE,
      <lft_data>      TYPE SORTED TABLE,
      <lft_data_temp> TYPE SORTED TABLE.

*-----De-reference the VALUES data
    ASSIGN io_values->* TO <lft_values>.

******************
* Internal Table 1
******************
    " Table data
    ASSIGN co_tab1->* TO <lft_data>.

    " Temporary copy
    CREATE DATA lo_data_temp LIKE <lft_data>.
    ASSIGN lo_data_temp->* TO <lft_data_temp>.

    " Filters the data
    LOOP AT <lft_values> ASSIGNING FIELD-SYMBOL(<lfs_values>).
      READ TABLE <lft_data> ASSIGNING FIELD-SYMBOL(<lfs_data>)
                            WITH KEY (lc_fld_key) = <lfs_values>.
      IF sy-subrc IS INITIAL.
        INSERT <lfs_data> INTO TABLE <lft_data_temp>.
      ENDIF.
      AT LAST.
        <lft_data> = <lft_data_temp>.
        UNASSIGN: <lft_data>, <lft_data_temp>.
      ENDAT.
    ENDLOOP.

******************
* Internal Table 2
******************
    " Table data
    ASSIGN co_tab2->* TO <lft_data>.

    " Temporary copy
    CREATE DATA lo_data_temp LIKE <lft_data>.
    ASSIGN lo_data_temp->* TO <lft_data_temp>.

    " Filters the data
    LOOP AT <lft_values> ASSIGNING <lfs_values>.
      READ TABLE <lft_data> ASSIGNING <lfs_data>
                            WITH KEY (lc_fld_key) = <lfs_values>.
      IF sy-subrc IS INITIAL.
        INSERT <lfs_data> INTO TABLE <lft_data_temp>.
      ENDIF.
      AT LAST.
        <lft_data> = <lft_data_temp>.
        UNASSIGN: <lft_data>, <lft_data_temp>.
      ENDAT.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.


***********************************
* Class: ALV Grid
***********************************
CLASS lcl_alv_grid DEFINITION.

*-||
  PUBLIC SECTION.
*- Methods
    METHODS:
      " Maintains the ALV data model
      constructor
        IMPORTING
          io_container TYPE REF TO cl_gui_container
          io_data      TYPE REF TO data,

      " Returns the GRID instance
      get_grid_instance
        RETURNING VALUE(ro_grid) TYPE REF TO cl_gui_alv_grid,

      " Maintains the FieldCatalog
      set_fieldcatalog
        IMPORTING
                  it_fcat            TYPE lvc_t_fcat
        RETURNING VALUE(ro_alv_grid) TYPE REF TO lcl_alv_grid,

      " Maintains the Layout
      set_layout
        IMPORTING
                  is_layout          TYPE lvc_s_layo
        RETURNING VALUE(ro_alv_grid) TYPE REF TO lcl_alv_grid,

      " Displays the ALV
      display
        RETURNING VALUE(ro_alv_grid) TYPE REF TO lcl_alv_grid
        RAISING
                  lcx_exception.

*- Class-Methods
    CLASS-METHODS:
      " Generates Field Catalog with the properties of data model
      generate_fcat_model
        IMPORTING
                  io_data        TYPE REF TO data
        RETURNING VALUE(rt_fcat) TYPE lvc_t_fcat.


*-||
  PRIVATE SECTION.
*- Structure
    DATA:
          ls_layo TYPE lvc_s_layo.

*- Internal Tables
    DATA:
          lt_fcat TYPE lvc_t_fcat.

*- Instance
    DATA:
      lo_data TYPE REF TO data,
      lo_grid TYPE REF TO cl_gui_alv_grid.


ENDCLASS.

CLASS lcl_alv_grid IMPLEMENTATION.

  METHOD constructor.

    " Creates the Grid reference
    lo_grid = NEW cl_gui_alv_grid( i_parent = io_container ).

    " Maintains the data model
    lo_data = io_data.

  ENDMETHOD.

  METHOD get_grid_instance.

    ro_grid = lo_grid.

  ENDMETHOD.

  METHOD set_fieldcatalog.

    " Maintains the FieldCatalog
    lt_fcat = it_fcat.

    " Returns the class instance for chaining
    ro_alv_grid = me.

  ENDMETHOD.

  METHOD set_layout.

    " Maintains the Layout
    ls_layo = is_layout.

    " Returns the class instance for chaining
    ro_alv_grid = me.

  ENDMETHOD.

  METHOD display.

    FIELD-SYMBOLS:
                   <lft_data> TYPE ANY TABLE.

    " De-reference the data
    ASSIGN lo_data->* TO <lft_data>.

*--- Checks if the FieldCatalog is maintained or not
    IF lt_fcat IS INITIAL. lt_fcat = me->generate_fcat_model( io_data = lo_data ). ENDIF.

******************
*--- Event Handling
******************

*--- Displays the ALV grid
    lo_grid->set_table_for_first_display(
      EXPORTING
*        i_buffer_active               =
*        i_bypassing_buffer            =
*        i_consistency_check           =
*        i_structure_name              =
*        is_variant                    =
*        i_save                        =
*        i_default                     = 'X'
        is_layout                     = ls_layo
*        is_print                      =
*        it_special_groups             =
*        it_toolbar_excluding          =
*        it_hyperlink                  =
*        it_alv_graphics               =
*        it_except_qinfo               =
*        ir_salv_adapter               =
      CHANGING
        it_outtab                     = <lft_data>
        it_fieldcatalog               = lt_fcat
*        it_sort                       =
*        it_filter                     =
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_exception
        EXPORTING
          textid = VALUE #(
                   msgid = sy-msgid
                   msgno = sy-msgno
                   attr1 = sy-msgv1
                   attr2 = sy-msgv2
                   attr3 = sy-msgv3
                   attr4 = sy-msgv4
                   ).
    ENDIF.

    " Returns the class instance for chaining
    ro_alv_grid = me.

  ENDMETHOD.

  METHOD generate_fcat_model.

    FIELD-SYMBOLS:
                   <lft_data> TYPE ANY TABLE.

    " Generates the components of the internal table
    ASSIGN io_data->* TO <lft_data>.
    DATA(lt_components) = CAST cl_abap_structdescr(
                               CAST cl_abap_tabledescr(
                                    cl_abap_typedescr=>describe_by_data(
                                                       p_data = <lft_data>
                                                       )
                                    )->get_table_line_type( )
                          )->get_components( ).

    " Generates the FIELDCATALOG properties from above - modify this for other FieldCatalogs
    rt_fcat = VALUE #(
              FOR ls_component IN lt_components
                  INDEX INTO lv_index
              (
              VALUE #(
                      col_pos   = lv_index
                      fieldname = ls_component-name
                      scrtext_l = CONV #( ls_component-name )
                      col_opt   = abap_true
                     )
              )
    ).

  ENDMETHOD.

ENDCLASS.


***********************************
* Class: View
***********************************
CLASS lcl_view DEFINITION.

*-||
  PUBLIC SECTION.
*- Instances
    INTERFACES:
      if_ldap_generic_dynpro.

*- Methods
    METHODS:
      constructor
        IMPORTING
          io_model TYPE REF TO lcl_model
        RAISING
          lcx_exception,

      " Displays UI for user to maintain translations and filter values
      display_ui
        RAISING
          lcx_exception,

      " Frees the container instances
      free_container_instances.


*-||
  PRIVATE SECTION.
*- Instances
    DATA:
      lo_model              TYPE REF TO lcl_model,
      lo_custom_container   TYPE REF TO cl_gui_custom_container,
      lo_splitter_container TYPE REF TO cl_gui_splitter_container,
      lo_grid_values        TYPE REF TO lcl_alv_grid,
      lo_grid_translation   TYPE REF TO lcl_alv_grid.

*- Variables
    DATA:
      lv_filter TYPE flag.

*- Methods
    METHODS:
      " Maintains the Container Width
      maintain_container_width
        IMPORTING
          iv_container_id TYPE i
          iv_filter       TYPE flag
        RAISING
          lcx_exception.

ENDCLASS.

CLASS lcl_view IMPLEMENTATION.

  METHOD constructor.

    " Maintains the MODEL instance
    lo_model = io_model.

    " Generates the Container for the data display
    CALL FUNCTION 'RS_LDAP_GENERIC_DYNPRO_GET'
      IMPORTING
        eo_container  = lo_custom_container
      EXCEPTIONS
        control_error = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_exception
        EXPORTING
          textid = VALUE #(
                   msgid = sy-msgid
                   msgno = sy-msgno
                   attr1 = sy-msgv1
                   attr2 = sy-msgv2
                   attr3 = sy-msgv3
                   attr4 = sy-msgv4
                   ).
    ENDIF.

  ENDMETHOD.

  METHOD free_container_instances.

*--- Frees the Container instances
    FREE:
      lo_model,
      lo_custom_container,
      lo_splitter_container.

  ENDMETHOD.

  METHOD display_ui.

*--- Creates the Splitter Container and accrodingly the child Containers
    lo_splitter_container = NEW cl_gui_splitter_container(
                            parent  = CAST cl_gui_container( lo_custom_container )
                            rows    = 1
                            columns = 2
                            ).

    TRY.
*--- Maintains the Column Width
        me->maintain_container_width(
          EXPORTING
            iv_container_id = 1
            iv_filter       = abap_false
        ).

*** Generates the ALV Grids
        " Values
        lo_grid_values = NEW lcl_alv_grid(
        io_container = lo_splitter_container->get_container(
                       row       = 1
                       column    = 1
                       )
        io_data      = lo_model->lo_values
        )->set_layout( is_layout = VALUE #(
                                   edit       = abap_true
                                   cwidth_opt = abap_true
                                   )
        )->display( ).

        " Translation
        lo_grid_translation = NEW lcl_alv_grid(
        io_container = lo_splitter_container->get_container(
                       row       = 1
                       column    = 2
                       )
        io_data      = lo_model->lo_translation
        )->set_fieldcatalog( it_fcat = VALUE #(
                                       ( col_pos = 1 fieldname = 'FLD_FILE1' scrtext_l = 'Field - File 1'(005) )
                                       ( col_pos = 2 fieldname = 'FLD_FILE2' scrtext_l = 'Field - File 2'(006) )
                                       ( col_pos = 3 fieldname = 'CELLSTYLE' tech = abap_true  )
                                       )
        )->set_layout( is_layout = VALUE #(
                                   edit       = abap_true
                                   cwidth_opt = abap_true
                                   stylefname = 'CELLSTYLE'
                                   )
        )->display( ).

*--- Event Handling = to be identified with the help of 'SENDER' parameter


      CATCH lcx_exception INTO DATA(lco_exception).
        RAISE EXCEPTION TYPE lcx_exception.

    ENDTRY.

*--- Displays the DYNPRO
    CALL FUNCTION 'RS_LDAP_GENERIC_DYNPRO'
      EXPORTING
        io_container        = lo_custom_container
        ii_dynpro_interface = me.

  ENDMETHOD.

  METHOD maintain_container_width.

*--- Maintains the container widths based on the user input
    lo_splitter_container->set_column_width(
      EXPORTING
        id                = iv_container_id
        width             = SWITCH #( iv_filter
                            WHEN abap_true THEN 60
                            ELSE 0
                            )
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_exception
        EXPORTING
          textid = VALUE #(
                   msgid = sy-msgid
                   msgno = sy-msgno
                   attr1 = sy-msgv1
                   attr2 = sy-msgv2
                   attr3 = sy-msgv3
                   attr4 = sy-msgv4
                   ).
    ENDIF.

*--- Sets the Row Sash and disables any resizing of the containers
    lo_splitter_container->set_column_sash(
      EXPORTING
        id                = 1
        type              = cl_gui_splitter_container=>type_sashvisible
        value             = SWITCH #( iv_filter
                            WHEN abap_true THEN cl_gui_splitter_container=>false
                            ELSE cl_gui_splitter_container=>true
                            )
*      IMPORTING
*        result            =     " Result Code
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_exception
        EXPORTING
          textid = VALUE #(
                   msgid = sy-msgid
                   msgno = sy-msgno
                   attr1 = sy-msgv1
                   attr2 = sy-msgv2
                   attr3 = sy-msgv3
                   attr4 = sy-msgv4
                   ).
    ENDIF.

*--- Maintains the FILTER indicator
    lv_filter = iv_filter.

  ENDMETHOD.

  METHOD if_ldap_generic_dynpro~pbo.

*--- Titlebar
    ef_titlebar_text = sy-title.
    ef_pf_status     = 'STATUS'.
    ef_pf_program    = sy-repid.

  ENDMETHOD.

  METHOD if_ldap_generic_dynpro~pai.

*--- Handles the OKCODE
    CASE if_okcode.
      WHEN 'BACK' OR 'CANCEL' OR 'EXIT'.
        SET SCREEN 0.

      WHEN 'COMPARE'.
        " Refreshes the data content from the screen
        IF lv_filter IS NOT INITIAL. lo_grid_values->get_grid_instance( )->check_changed_data( ). ENDIF.
        lo_grid_translation->get_grid_instance( )->check_changed_data( ).

*--- Generates the selected rows
        lo_grid_translation->get_grid_instance( )->get_selected_rows(
          IMPORTING
            et_row_no     = DATA(lt_rows)
        ).

        TRY.
            " Compares the data
            me->lo_model->compare_file_data( lt_rows ).

            " Displays the log for the operation
            me->lo_model->lo_log->display_log_salv( ).

          CATCH lcx_exception.
            MESSAGE ID lcx_exception=>ls_t100key-msgid TYPE lcx_exception=>gc_msg_type-success NUMBER lcx_exception=>ls_t100key-msgno
                       WITH lcx_exception=>ls_t100key-attr1 lcx_exception=>ls_t100key-attr2
                            lcx_exception=>ls_t100key-attr3 lcx_exception=>ls_t100key-attr4
                            DISPLAY LIKE lcx_exception=>gc_msg_type-error.
        ENDTRY.

      WHEN 'FILTER'.
        " Maintains the UI for FILTER criteria
        TRY.
            me->maintain_container_width(
              EXPORTING
                iv_container_id = 1
                iv_filter       = SWITCH #( lv_filter
                                  WHEN abap_true THEN abap_false
                                  ELSE abap_true
                                  )
            ).
          CATCH lcx_exception.
            MESSAGE ID lcx_exception=>ls_t100key-msgid TYPE lcx_exception=>gc_msg_type-success NUMBER lcx_exception=>ls_t100key-msgno
                       WITH lcx_exception=>ls_t100key-attr1 lcx_exception=>ls_t100key-attr2
                            lcx_exception=>ls_t100key-attr3 lcx_exception=>ls_t100key-attr4
                            DISPLAY LIKE lcx_exception=>gc_msg_type-error.

        ENDTRY.

      WHEN OTHERS.
        " Do nothing
    ENDCASE.

  ENDMETHOD.

ENDCLASS.


**********************************************************************
* SELECTION SCREEN
**********************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS:
  p_file1 TYPE hrpadkz_filename OBLIGATORY.
SELECT-OPTIONS:
  s_keyfd1 FOR lcl_sel_screen=>gv_keyfields NO INTERVALS.
PARAMETERS:
  p_hdln1 TYPE line_num.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
PARAMETERS:
  p_file2 TYPE hrpadkz_filename OBLIGATORY.
SELECT-OPTIONS:
  s_keyfd2 FOR lcl_sel_screen=>gv_keyfields NO INTERVALS.
PARAMETERS:
  p_hdln2 TYPE line_num.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN SKIP.
PARAMETERS:
  p_sep TYPE usmd_sep DEFAULT ';' OBLIGATORY.

**********************************************************************
* INITIALIZATION
**********************************************************************
INITIALIZATION.
*--- Maintains the Selection Screen instance
  DATA(go_sel_screen) = NEW lcl_sel_screen( ).
  " Restricts the Select Option functionalities
  go_sel_screen->restrict_select_options( ).

**********************************************************************
* AT SELECTION SCREEN
**********************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file1.
  " F4 Help for the upload file
  lcl_file=>f4_file_path(
    CHANGING
      cv_file_path = p_file1
  ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file2.
  " F4 Help for the upload file
  lcl_file=>f4_file_path(
    CHANGING
      cv_file_path = p_file2
  ).

AT SELECTION-SCREEN ON p_file1.
  TRY .
      " Validates the file path
      IF sy-ucomm EQ 'ONLI'.  lcl_file=>validate_file_path( iv_file_path = p_file1 ). ENDIF.

    CATCH lcx_exception.
      MESSAGE ID lcx_exception=>ls_t100key-msgid TYPE lcx_exception=>gc_msg_type-error NUMBER lcx_exception=>ls_t100key-msgid
                 WITH lcx_exception=>ls_t100key-attr1 lcx_exception=>ls_t100key-attr2
                      lcx_exception=>ls_t100key-attr3 lcx_exception=>ls_t100key-attr4.

  ENDTRY.

AT SELECTION-SCREEN.
  " Maintains the Selection Screen data
  go_sel_screen->maintain_selection_inputs( ).

  TRY .
      " Validates the KeyFields
      go_sel_screen->validate_keyfields( sy-ucomm ).

    CATCH lcx_exception.
      MESSAGE ID lcx_exception=>ls_t100key-msgid TYPE lcx_exception=>gc_msg_type-error NUMBER lcx_exception=>ls_t100key-msgid
                 WITH lcx_exception=>ls_t100key-attr1 lcx_exception=>ls_t100key-attr2
                      lcx_exception=>ls_t100key-attr3 lcx_exception=>ls_t100key-attr4.

  ENDTRY.

**********************************************************************
* START-OF-SELECTION
**********************************************************************
START-OF-SELECTION.

**********************************************************************
* END-OF-SELECTION
**********************************************************************
END-OF-SELECTION.
  TRY.
*--- Creates the MODEL instance
      DATA(go_model) = NEW lcl_model( go_sel_screen ).

*--- Populates the data from the files and creates the UI models
      go_model->populate_data( ).

*--- Displays the data in a UI
      NEW lcl_view( go_model )->display_ui( ).

*--- Populates the data from the files
    CATCH lcx_exception.
      MESSAGE ID lcx_exception=>ls_t100key-msgid TYPE lcx_exception=>gc_msg_type-success NUMBER lcx_exception=>ls_t100key-msgid
                 WITH lcx_exception=>ls_t100key-attr1 lcx_exception=>ls_t100key-attr2
                      lcx_exception=>ls_t100key-attr3 lcx_exception=>ls_t100key-attr4
                      DISPLAY LIKE lcx_exception=>gc_msg_type-error.

  ENDTRY.


001	Details: File 1
002	Details: File 2
003	File 1: KeyFields
004	File 2: KeyFields
005	Field - File 1
006	Field - File 2
007	Path - File 1:
008	Path - File 2:
009	File 1: Status
010	File 1: RowIndex
011	File 1: Field
012	File 1: Value
013	File 2: Status
014	File 2: RowIndex
015	File 2: Field
016	File 2: Value
018	Processing data for file 1
019	Processing data for file 2
P_HDLN1	Header Line (skip)
P_HDLN2	Header Line (skip)
