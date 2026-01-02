CLASS ycl_aaic_util DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_importing_params_s,
             name        TYPE string,
             type        TYPE string,
             format      TYPE string,
             required    TYPE abap_bool,
             description TYPE string,
           END OF ty_importing_params_s,

           ty_importing_params_tt TYPE STANDARD TABLE OF ty_importing_params_s WITH EMPTY KEY,

           ty_splitted_string_tt  TYPE STANDARD TABLE OF string WITH EMPTY KEY.

    METHODS serialize
      IMPORTING
                i_data        TYPE data
                i_camel_case  TYPE abap_bool DEFAULT abap_false
      RETURNING VALUE(r_json) TYPE string.

    METHODS deserialize
      IMPORTING
        i_json       TYPE string
        i_camel_case TYPE abap_bool DEFAULT abap_false
      EXPORTING
        e_data       TYPE data.

    METHODS get_method_importing_params
      IMPORTING
        i_class_name         TYPE csequence
        i_method_name        TYPE csequence
      EXPORTING
        e_t_importing_params TYPE ty_importing_params_tt
        e_t_components       TYPE cl_abap_structdescr=>component_table.

    METHODS get_parameter_type
      IMPORTING
                i_o_type_descr TYPE REF TO cl_abap_elemdescr
      RETURNING VALUE(r_type)  TYPE string.

    METHODS get_parameter_format
      IMPORTING
                i_o_type_descr  TYPE REF TO cl_abap_datadescr
      RETURNING VALUE(r_format) TYPE string.

    METHODS get_data_element_description
      IMPORTING
                i_name               TYPE csequence
      RETURNING VALUE(r_description) TYPE string.

    METHODS get_structure_description
      IMPORTING
                i_name               TYPE csequence
      RETURNING VALUE(r_description) TYPE string.

    METHODS get_json_schema
      IMPORTING
                i_class_name         TYPE csequence
                i_method_name        TYPE csequence
      RETURNING VALUE(r_json_schema) TYPE string.

    METHODS split_string
      IMPORTING
        i_string            TYPE string
        i_length            TYPE i
      EXPORTING
        e_t_splitted_string TYPE ty_splitted_string_tt.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ycl_aaic_util IMPLEMENTATION.


  METHOD serialize.

    FREE r_json.

    DATA(l_pretty_name) = /ui2/cl_json=>pretty_mode-low_case.

    IF i_camel_case = abap_true.
      l_pretty_name = /ui2/cl_json=>pretty_mode-camel_case.
    ENDIF.

    r_json = /ui2/cl_json=>serialize(
     EXPORTING
       data = i_data
       compress         = abap_false
*       name             =
       pretty_name      = l_pretty_name
*       type_descr       =
*       assoc_arrays     =
*       ts_as_iso8601    =
*       expand_includes  =
*       assoc_arrays_opt =
*       numc_as_string   =
*       name_mappings    =
*       conversion_exits =
    ).

  ENDMETHOD.


  METHOD deserialize.

    CLEAR e_data.

    DATA(l_pretty_name) = /ui2/cl_json=>pretty_mode-low_case.

    IF i_camel_case = abap_true.
      l_pretty_name = /ui2/cl_json=>pretty_mode-camel_case.
    ENDIF.

    /ui2/cl_json=>deserialize(
      EXPORTING
        json            = i_json           " JSON string
*      jsonx            =                  " JSON XString
       pretty_name      = l_pretty_name    " Pretty Print property names
*      assoc_arrays     =                  " Deserialize associative array as tables with unique keys
*      assoc_arrays_opt =                  " Optimize rendering of name value maps
*      name_mappings    =                  " ABAP<->JSON Name Mapping Table
*      conversion_exits =                  " Use DDIC conversion exits on deserialize of values
      CHANGING
        data             = e_data          " Data to serialize
    ).

  ENDMETHOD.


  METHOD get_data_element_description.

    CLEAR r_description.

    TRY.

        xco_cp_abap_dictionary=>data_element(
          EXPORTING
            iv_name         = CONV #( i_name )
          RECEIVING
            ro_data_element = DATA(lo_data_element)
        ).

        DATA(lo_content) = lo_data_element->content( ).

        r_description = lo_content->get_short_description( ).

      CATCH cx_sy_dyn_call_illegal_type ##NO_HANDLER.

      CATCH cx_xco_runtime_exception ##NO_HANDLER.

    ENDTRY.

  ENDMETHOD.


  METHOD get_json_schema.

    TYPES: BEGIN OF ty_properties,
             name      TYPE c LENGTH 30,
             fieldname TYPE c LENGTH 30,
             kind      TYPE c LENGTH 1,
             required  TYPE abap_bool,
             json      TYPE string,
           END OF ty_properties.

    DATA lo_tabledescr  TYPE REF TO cl_abap_tabledescr.
    DATA lo_structdescr TYPE REF TO cl_abap_structdescr.
    DATA lo_elemdescr   TYPE REF TO cl_abap_elemdescr.

    DATA lt_json_properties TYPE STANDARD TABLE OF ty_properties.

    DATA: l_json                TYPE string,
          l_escaped_description TYPE string,
          l_fieldtext           TYPE string,
          l_fieldname           TYPE string,
          l_description         TYPE string,
          l_required            TYPE string,
          l_is_required         TYPE abap_bool.

    CLEAR r_json_schema.

    me->get_method_importing_params(
      EXPORTING
        i_class_name         = i_class_name
        i_method_name        = i_method_name
      IMPORTING
        e_t_components       = DATA(lt_components)
        e_t_importing_params = DATA(lt_importing_params)
    ).

    LOOP AT lt_components ASSIGNING FIELD-SYMBOL(<ls_components>).

      CLEAR l_description.

      CASE <ls_components>-type->kind.

        WHEN 'E'. " Element

          lo_elemdescr ?= <ls_components>-type.

          APPEND INITIAL LINE TO lt_json_properties ASSIGNING FIELD-SYMBOL(<ls_json_properties>).

          READ TABLE lt_importing_params INTO DATA(ls_importing_params)
            WITH KEY name = <ls_components>-name.

          l_description = /ui2/cl_json=>serialize( EXPORTING data = ls_importing_params-description ).

          <ls_json_properties>-name = <ls_components>-name.
          <ls_json_properties>-fieldname = <ls_components>-name.
          <ls_json_properties>-kind = <ls_components>-type->kind.
          <ls_json_properties>-required = ls_importing_params-required.
          <ls_json_properties>-json = '"' && <ls_components>-name && '":{"type":"' && me->get_parameter_type( lo_elemdescr ) && '", "description":' && l_description && '}'.

        WHEN 'S'. " Structure

          lo_structdescr ?= <ls_components>-type.

          LOOP AT lo_structdescr->components ASSIGNING FIELD-SYMBOL(<ls_struct_field>).

            CLEAR l_is_required.

            lo_structdescr->get_component_type(
              EXPORTING
                p_name                 = <ls_struct_field>-name
              RECEIVING
                p_descr_ref            = DATA(lo_component_type)
              EXCEPTIONS
                component_not_found    = 1
                unsupported_input_type = 2
                OTHERS                 = 3
            ).

            IF sy-subrc <> 0.
              CONTINUE.
            ENDIF.

            lo_elemdescr ?= lo_component_type.

            IF lo_elemdescr->is_ddic_type( ) = abap_true.

              DATA(l_relative_name) = lo_elemdescr->get_relative_name( ).

              l_description = me->get_data_element_description(
                EXPORTING
                  i_name = l_relative_name
              ).

            ENDIF.

            IF l_description IS NOT INITIAL AND l_description(1) = '*'.

              l_description = l_description+1.

              l_is_required = abap_true.

            ENDIF.

            l_description = /ui2/cl_json=>serialize( EXPORTING data = l_description ).

            APPEND INITIAL LINE TO lt_json_properties ASSIGNING <ls_json_properties>.

            <ls_json_properties>-name = <ls_components>-name.
            <ls_json_properties>-fieldname = <ls_struct_field>-name.
            <ls_json_properties>-kind = <ls_components>-type->kind.
            <ls_json_properties>-required = l_is_required.
            <ls_json_properties>-json = '"' && <ls_struct_field>-name && '":{"type":"' && me->get_parameter_type( lo_elemdescr ) && '", "description":' && l_description && '}'.

          ENDLOOP.

        WHEN 'T'. " Table Type

          lo_tabledescr ?= <ls_components>-type.

          lo_structdescr ?= lo_tabledescr->get_table_line_type( ).

          LOOP AT lo_structdescr->components ASSIGNING <ls_struct_field>.

            CLEAR l_is_required.

            lo_structdescr->get_component_type(
              EXPORTING
                p_name                 = <ls_struct_field>-name
              RECEIVING
                p_descr_ref            = lo_component_type
              EXCEPTIONS
                component_not_found    = 1
                unsupported_input_type = 2
                OTHERS                 = 3
            ).

            IF sy-subrc <> 0.
              CONTINUE.
            ENDIF.

            lo_elemdescr ?= lo_component_type.

            IF lo_elemdescr->is_ddic_type( ) = abap_true.

              l_relative_name = lo_elemdescr->get_relative_name( ).

              l_description = me->get_data_element_description(
                EXPORTING
                  i_name = l_relative_name
              ).

            ENDIF.

            IF l_description IS NOT INITIAL AND l_description(1) = '*'.

              l_description = l_description+1.

              l_is_required = abap_true.

            ENDIF.

            l_description = /ui2/cl_json=>serialize( EXPORTING data = l_description ).

            APPEND INITIAL LINE TO lt_json_properties ASSIGNING <ls_json_properties>.

            <ls_json_properties>-name = <ls_components>-name.
            <ls_json_properties>-fieldname = <ls_struct_field>-name.
            <ls_json_properties>-kind = <ls_components>-type->kind.
            <ls_json_properties>-required = l_is_required.
            <ls_json_properties>-json = '"' && <ls_struct_field>-name && '":{"type":"' && me->get_parameter_type( lo_elemdescr ) && '", "description":' && l_description && '}'.

          ENDLOOP.

      ENDCASE.

    ENDLOOP.

    DATA(l_index) = sy-tabix.

    LOOP AT lt_components ASSIGNING <ls_components>.

      CLEAR: l_json,
             l_index,
             l_required.

      LOOP AT lt_json_properties ASSIGNING <ls_json_properties> WHERE name = <ls_components>-name.

        l_index = l_index + 1.

        IF <ls_json_properties>-required = abap_true AND
          ( <ls_json_properties>-kind = 'S' OR <ls_json_properties>-kind = 'T' ).
          IF l_required IS INITIAL.
            l_required = |"{ <ls_json_properties>-fieldname }"|.
          ELSE.
            l_required = |{ l_required }, "{ <ls_json_properties>-fieldname }"|.
          ENDIF.
        ENDIF.

        IF l_json IS INITIAL.

          IF <ls_json_properties>-kind = 'S'.

            READ TABLE lt_importing_params INTO ls_importing_params
              WITH KEY name = <ls_components>-name.

            l_escaped_description = me->serialize( i_data = ls_importing_params-description ).

            l_json = '"' && <ls_components>-name && '": { "type": "object", "description":' && l_escaped_description && ', "properties": {'.

          ENDIF.

          IF <ls_json_properties>-kind = 'T'.

            READ TABLE lt_importing_params INTO ls_importing_params
              WITH KEY name = <ls_components>-name.

            l_escaped_description = me->serialize( i_data = ls_importing_params-description ).

            l_json = '"' && <ls_components>-name && '": { "type": "array", "description":' && l_escaped_description && ', "items": { "type": "object", "properties": {'.

          ENDIF.

          l_json = l_json && <ls_json_properties>-json.

        ELSE.
          l_json = l_json && ', ' && <ls_json_properties>-json.
        ENDIF.

      ENDLOOP.

      IF sy-subrc = 0.
        IF <ls_json_properties>-kind = 'S'.
          l_json = l_json && '}, "required": [' && l_required && ']}'.
        ENDIF.
        IF <ls_json_properties>-kind = 'T'.
          l_json = l_json && '}, "required": [' && l_required && ']}}'.
        ENDIF.
      ENDIF.

      IF r_json_schema IS INITIAL.
        r_json_schema = r_json_schema && l_json.
      ELSE.
        r_json_schema = r_json_schema && ', ' && l_json.
      ENDIF.

    ENDLOOP.

    r_json_schema = '{' && r_json_schema && '}'.

  ENDMETHOD.


  METHOD get_method_importing_params.

    DATA: lo_class_descr TYPE REF TO cl_abap_classdescr,
          lo_tabledescr  TYPE REF TO cl_abap_tabledescr,
          lo_structdescr TYPE REF TO cl_abap_structdescr,
          lo_elem_descr  TYPE REF TO cl_abap_elemdescr.

    DATA: lt_methods    TYPE abap_methdescr_tab,
          lt_components TYPE cl_abap_structdescr=>component_table.

    DATA: ls_component TYPE cl_abap_structdescr=>component.

    DATA: l_description TYPE string.

    FREE e_t_importing_params.

    " Get the class descriptor
    CALL METHOD cl_abap_classdescr=>describe_by_name
      EXPORTING
        p_name         = to_upper( i_class_name )     " Type name
      RECEIVING
        p_descr_ref    = DATA(lo_descr)   " Reference to description object
      EXCEPTIONS
        type_not_found = 1                " Type with name p_name could not be found
        OTHERS         = 2.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    lo_class_descr ?= lo_descr.

    " Get all methods of the class
    lt_methods = lo_class_descr->methods.

    READ TABLE lt_methods ASSIGNING FIELD-SYMBOL(<ls_method>) WITH KEY name = to_upper( i_method_name ).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT <ls_method>-parameters ASSIGNING FIELD-SYMBOL(<ls_parameter>).

      IF <ls_parameter>-parm_kind <> 'I'. " Importing parameters
        CONTINUE.
      ENDIF.

      CLEAR l_description.

      lo_class_descr->get_method_parameter_type(
        EXPORTING
          p_method_name       = i_method_name         " Method name
          p_parameter_name    = <ls_parameter>-name   " Parameter Name
        RECEIVING
          p_descr_ref         = DATA(lo_descr_ref)    " Description object
        EXCEPTIONS
          parameter_not_found = 1                     " Parameter could not be found
          method_not_found    = 2                     " Method was not found
          OTHERS              = 3
      ).

      IF sy-subrc = 0.

        ls_component-name = <ls_parameter>-name.
        ls_component-type ?= lo_descr_ref.

        APPEND ls_component TO e_t_components.

      ENDIF.

      IF e_t_importing_params IS REQUESTED.

        CASE ls_component-type->kind.

          WHEN 'E'. "Element

            lo_elem_descr ?= lo_descr_ref.

            IF lo_elem_descr->is_ddic_type( ) = abap_true.

              DATA(l_relative_name) = lo_elem_descr->get_relative_name( ).

              l_description = me->get_data_element_description(
                EXPORTING
                  i_name = l_relative_name
              ).

            ENDIF.

            APPEND VALUE #( name = <ls_parameter>-name
                            type = me->get_parameter_type( i_o_type_descr = lo_elem_descr )
                            format = me->get_parameter_format( i_o_type_descr = lo_elem_descr )
                            required = COND #( WHEN <ls_parameter>-is_optional IS INITIAL THEN abap_true ELSE abap_false )
                            description = l_description ) TO e_t_importing_params.

          WHEN 'S'. " Structure

            lo_structdescr ?= lo_descr_ref.

            IF lo_structdescr->is_ddic_type( ) = abap_true.

              l_relative_name = lo_structdescr->get_relative_name( ).

              l_description = me->get_structure_description(
                EXPORTING
                  i_name = l_relative_name
              ).

            ENDIF.

            APPEND VALUE #( name = <ls_parameter>-name
                            type = 'object'
                            format = space
                            required = COND #( WHEN <ls_parameter>-is_optional IS INITIAL THEN abap_true ELSE abap_false )
                            description = l_description ) TO e_t_importing_params.

          WHEN 'T'. " Table Type

            lo_tabledescr ?= lo_descr_ref.

            IF lo_tabledescr->is_ddic_type( ) = abap_true.

            ENDIF.

            APPEND VALUE #( name = <ls_parameter>-name
                            type = 'array'
                            format = space
                            required = COND #( WHEN <ls_parameter>-is_optional IS INITIAL THEN abap_true ELSE abap_false )
                            description = l_description ) TO e_t_importing_params.

        ENDCASE.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_parameter_format.

    CLEAR r_format.

    IF i_o_type_descr->type_kind = cl_abap_typedescr=>typekind_date.
      r_format = 'date'.
    ENDIF.

  ENDMETHOD.


  METHOD get_parameter_type.

    CLEAR r_type.

    CASE i_o_type_descr->type_kind.

      WHEN cl_abap_typedescr=>typekind_string OR
           cl_abap_typedescr=>typekind_char OR
           cl_abap_typedescr=>typekind_date OR
           cl_abap_typedescr=>typekind_csequence OR
           cl_abap_typedescr=>typekind_clike.

        r_type = 'string'.

        IF i_o_type_descr->absolute_name CS 'ABAP_BOOL' AND i_o_type_descr->output_length = 1.
          r_type = 'boolean'.
        ENDIF.

      WHEN cl_abap_typedescr=>typekind_int OR
           cl_abap_typedescr=>typekind_int1 OR
           cl_abap_typedescr=>typekind_int2 OR
           cl_abap_typedescr=>typekind_int8 OR
           cl_abap_typedescr=>typekind_decfloat OR
           cl_abap_typedescr=>typekind_decfloat16 OR
           cl_abap_typedescr=>typekind_decfloat34 OR
           cl_abap_typedescr=>typekind_float OR
           cl_abap_typedescr=>typekind_num OR
           cl_abap_typedescr=>typekind_numeric OR
           cl_abap_typedescr=>typekind_packed.

        r_type = 'number'.

    ENDCASE.

  ENDMETHOD.


  METHOD get_structure_description.

    CLEAR r_description.

    TRY.

        xco_cp_abap_dictionary=>structure(
          EXPORTING
            iv_name = CONV #( i_name )
          RECEIVING
            ro_structure = DATA(lo_structure)
        ).

        DATA(lo_content) = lo_structure->content( ).

        r_description = lo_content->get_short_description( ).

      CATCH cx_sy_dyn_call_illegal_type ##NO_HANDLER.

      CATCH cx_xco_runtime_exception ##NO_HANDLER.

        TRY.

            xco_cp_abap_dictionary=>database_table(
              EXPORTING
                iv_name = CONV #( i_name )
              RECEIVING
                ro_database_table = DATA(lo_database_table)
            ).

            DATA(lo_content_table) = lo_database_table->content( ).

            r_description = lo_content_table->get_short_description( ).

          CATCH cx_sy_dyn_call_illegal_type ##NO_HANDLER.

          CATCH cx_xco_runtime_exception ##NO_HANDLER.

        ENDTRY.

    ENDTRY.

  ENDMETHOD.

  METHOD split_string.

    DATA lt_split TYPE STANDARD TABLE OF string WITH EMPTY KEY.

    DATA: l_line      TYPE string,
          l_remaining TYPE string.

    SPLIT i_string AT space INTO TABLE lt_split.

    LOOP AT lt_split ASSIGNING FIELD-SYMBOL(<l_word>).

      DATA(l_len) = strlen( l_line ) + strlen( <l_word> ) + 1.

      IF l_len <= i_length.

        IF l_line IS INITIAL.
          l_line = <l_word>.
        ELSE.
          l_line = |{ l_line } { <l_word> }|.
        ENDIF.

        CONTINUE.

      ENDIF.

      IF l_line IS INITIAL.

        WHILE strlen( <l_word> ) >= i_length.

          l_line = <l_word>(i_length).

          APPEND l_line TO e_t_splitted_string.

          FREE l_line.

          <l_word> = <l_word>+i_length.

        ENDWHILE.

        l_line = <l_word>.

        CONTINUE.

      ELSE.

        IF strlen( <l_word> ) > i_length.

          WHILE strlen( <l_word> ) >= i_length.

            DATA(l_room) = i_length - strlen( l_line ) - 1.

            IF l_line IS NOT INITIAL.
              l_line = |{ l_line } { <l_word>(l_room) }|.
            ELSE.
              l_line = <l_word>(l_room).
            ENDIF.

            <l_word> = <l_word>+l_room.

            APPEND l_line TO e_t_splitted_string.

            FREE l_line.

          ENDWHILE.

          l_line = <l_word>.

          CONTINUE.

        ELSE.

          APPEND l_line TO e_t_splitted_string.

          FREE l_line.

          l_line = <l_word>.

          CONTINUE.

        ENDIF.

      ENDIF.

      APPEND l_line TO e_t_splitted_string.

      FREE l_line.

    ENDLOOP.

    IF l_line IS NOT INITIAL.
      APPEND l_line TO e_t_splitted_string.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
